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
  | RETURN
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
  | TO
  | WHILE
  | INT
  | BOOL
  | STR
  | FLOAT
  | CHAR
  | VOID
  | RECT
  | CIRC
  | TRI
  | SQRE
  | ELPS
  | POLY
  | POINT
  | LINE
  | DATE
  | COLOR
  | LBRACKET
  | RBRACKET
  | COLON
  | DOT
  | END
  | LENGTH
  | APPEND
  | REMOVE
  | PRINT
  | CREATEHEADER
  | CREATEPARAGRAPH
  | CREATETABLE
  | CREATEUNORDEREDLIST
  | OBJECT
  | ID of (string)
  | EOF
  | LITERAL of (int)
  | INT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | BOOL_LITERAL of (bool)
  | FLOAT_LITERAL of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
