/* Ocamlyacc parser for QWEB */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA FUNCTION
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ
%token AND OR IN
%token DISPLAY OUTPUT IF ENDIF OTHERWISE ENDOTHERWISE OTHERWISEIF ENDOTHERWISEIF FOR ENDFOR REPEAT ENDREPEAT IN SET CONTINUE PASS
%token INT BOOL STR FLOAT CHAR RECT CIRC TRI SQRE ELPS POLY POINT LINE DATE
%token LBRACKET RBRACKET COLON DOT END
%token LENGTH APPEND REMOVE
%token CREATEHEADER CREATEPARAGRAPH CREATETABLE CREATEUNORDEREDLIST 
%token OBJECT
%token <string> ID
%token EOF

%token <float> INT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token <float> FLOAT_LITERAL

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ IS
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%nonassoc DOT

%%

program:
	  func_decl_list EOF { List.rev $1 }

func_decl_list:
	/* nothing */ { [] }
	| func_decl_list func_decl {$2 :: $1}

obj_list:
	 typ ID { [$1] }
	| obj_list COMMA ID {$3 :: $1}

stmt:
	  expr END {Expr $1}
	| REPEAT expr COLON loop_stmt_list ENDREPEAT {Repeat($2, Block(List.rev $4))}
	| FOR ID IN expr COLON loop_stmt_list ENDFOR {For(Id($2), $4, Block(List.rev $6))}
	| IF expr COLON stmt_list ENDIF otherwiseif_list {If($2, Block(List.rev $4), Block([]))}
	| IF expr COLON stmt_list ENDIF otherwiseif_list OTHERWISE COLON stmt_list ENDOTHERWISE {If($2, Block(List.rev $4), Block(List.rev $9))}
	| DISPLAY expr END {Display($2)}
	| OBJECT obj_list END {Object($2)}
	| OUTPUT END {Output Noexpr}
	| OUTPUT expr END {Output $2}	

stmt_list:
	  /* nothing */ { [] }
	| stmt_list stmt {$2 :: $1}

loop_stmt_list:
	  /* nothing */ { [] }
	| loop_stmt_list loop_stmt {$2 :: $1}

loop_stmt:
	  stmt { $1 }
	| PASS END { Pass }
	| CONTINUE END { Continue }

otherwiseif_list:
	  /* nothing */ { [] }
	| otherwiseif_list otherwiseif_stmt {$2 :: $1}

otherwiseif_stmt:
	| OTHERWISEIF expr COLON stmt_list ENDOTHERWISEIF {If($2, Block(List.rev $4), Block([]))}

expr:
	  literal {$1}
	| arith_op {$1}
	| bool_op {$1}
	| LBRACKET qweb_list RBRACKET {ListDecl(List.rev $2)}
	| list_op {$1}
	| html_op {$1}
	| obj_op {$1}
	| typ ID {Id($1)}
	| ID ASSIGN expr {Assign($1, $3)}
	| LPAREN expr RPAREN {$2}
	| LBRACE dict RBRACE {DictDecl(List.rev $2)}
	| ID LPAREN actuals_opt RPAREN {Call($1, $3)}

arith_op:
	  MINUS expr		{Unop(Neg, $2)}
	| expr PLUS expr	{Binop($1, Add, $3)}
	| expr MINUS expr	{Binop($1, Minus, $3)}
	| expr TIMES expr	{Binop($1, Times, $3)}
	| expr DIVIDE expr	{Binop($1, Divide, $3)}

bool_op:
	  NOT expr		{Unop(Not, $2)}
	| expr EQ expr 	{Binop($1, Eq, $3)}
	| expr NEQ expr {Binop($1, Neq, $3)}
	| expr LT expr 	{Binop($1, Less, $3)}
	| expr LEQ expr {Binop($1, Leq, $3)}
	| expr GT expr 	{Binop($1, Greater, $3)}
	| expr GEQ expr {Binop($1, Geq, $3)}
	| expr AND expr {Binop($1, And, $3)}
	| expr OR expr	{Binop($1, Or, $3)}

qweb_list:
	  /* nothing */ { [] }
	| expr { [$1] }
	| qweb_list COMMA expr { $3 :: $1 }

dict: 
	  /* nothing */ { [] }
	| expr COLON expr { [($1, $3)] }
	| dict COMMA expr COLON expr { ($3, $5) :: $1 }

list_op:
	| expr DOT APPEND LPAREN expr RPAREN { ListAppend($1, $5) }
	| expr DOT REMOVE LPAREN expr RPAREN { ListRemove($1, $5) }
	| expr DOT LENGTH LPAREN RPAREN { ListLength($1) }

html_op:
	| expr DOT CREATEHEADER LPAREN expr RPAREN { HtmlHeader($1, $5) }
	| expr DOT CREATEPARAGRAPH LPAREN expr RPAREN { HtmlParagraph($1, $5) }
	| expr DOT CREATETABLE LPAREN expr RPAREN { HtmlTable($1, $5) }
	| expr DOT CREATEUNORDEREDLIST LPAREN expr RPAREN { HtmlUnorderedList($1, $5) }

obj_op:
    | expr DOT ID {ObjectField($1, $3)}
    | expr DOT ID ASSIGN expr {ObjectAssign($1, $3, $5)}

func_decl:
	  FUNCTION ID LPAREN formal_opt RPAREN COLON stmt_list END
	  	{ {
	  		fname = $2;
	  		formals = $4;
	  		body = List.rev $7
	  	} }

actuals_opt:
	  /* nothing */ { [] }
	| actuals_list { List.rev $1 }

actuals_list:
      expr                    { [$1] }
  	| actuals_list COMMA expr { $3 :: $1 }

formal_opt:
	  /* nothing */ { [] }
	| formal_list { List.rev $1 }

formal_list:
	  typ ID { [$1] }
	| formal_list COMMA typ ID {$3 :: $1}

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | CHAR  { Char  }
	| RECT  { Rect  }
	| CIRC  { Circ  }
	| TRI  { Tri  }
	| SQRE  { Sqre  }
	| ELPS  { Elps  }
	| POLY  { Poly  }
	| POINT  { Point  }
	| LINE  { Line  }

literal:
	| INT_LITERAL 		{Int_lit($1)}
	| STRING_LITERAL	{String_lit($1)}
	| BOOL_LITERAL		{Bool_lit($1)}
  | FLOAT_LITERAL		{Float_lit($1)}
