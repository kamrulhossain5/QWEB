type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | FUNCTION
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | IN
  | DISPLAY
  | OUTPUT
  | IF
  | ENDIF
  | OTHERWISE
  | ENDOTHERWISE
  | OTHERWISEIF
  | ENDOTHERWISEIF
  | FOR
  | ENDFOR
  | REPEAT
  | ENDREPEAT
  | SET
  | CONTINUE
  | PASS
  | INT
  | BOOL
  | STR
  | FLOAT
  | CHAR
  | RECT
  | CIRC
  | TRI
  | SQRE
  | ELPS
  | POLY
  | POINT
  | LINE
  | DATE
  | LBRACKET
  | RBRACKET
  | COLON
  | DOT
  | END
  | LENGTH
  | APPEND
  | REMOVE
  | CREATEHEADER
  | CREATEPARAGRAPH
  | CREATETABLE
  | CREATEUNORDEREDLIST
  | OBJECT
  | ID of (string)
  | EOF
  | INT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | FLOAT_LITERAL of (float)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
