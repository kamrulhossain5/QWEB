(* Ocamllex scanner for QWEB *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ','      { COMMA }
| '.'      { DOT }
| ':'      { COLON }
| ';'      { END }

(* BINOPS *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "to"      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&" | "and"    { AND }
| "||" | "or"     { OR }
| "!" | "not"      { NOT }

(* STRUCTURE KEYWORDS *)
| "function"    { FUNCTION }
| "if"     { IF }
| "end if"     { ENDIF }
| "otherwise"   { OTHERWISE }
| "end otherwise"   { ENDOTHERWISE }
| "otherwise if"   { OTHERWISEIF }
| "end otherwise if"   { ENDOTHERWISEIF }
| "output" { OUTPUT }
| "display"  { DISPLAY }
| "to"     { TO }
| "in"     { IN }
| "continue" { CONTINUE }
| "pass"  { PASS }
| "FOR each"    { FOR }
| "end for"    { FOR }
| "REPEAT until"  { REPEAT }
| "END REPEAT"  { ENDREPEAT }

(* DATA TYPES *)
| "int"    { INT }
| "bool"   { BOOL }
| "str"   { STR }
| "float"  { FLOAT }
| "char"  { CHAR }
| "color"    { COLOR }
| "rect"   { RECT }
| "circ"   { CIRC }
| "tri"   { TRI }
| "sqre"   { SQRE }
| "elps"   { ELPS }
| "poly"  { POLY }
| "point"  { POINT }
| "line"  { LINE }
| "date"  { DATE }

(* LIST KEYWORDS *)
| "length" { LENGTH }
| "append" { APPEND }
| "remove" { REMOVE }

(* QWEB KEYWORDS *)
| "createHeader" { CREATEHEADER }
| "createParagraph" { CREATEPARAGRAPH }
| "createTable" { CREATETABLE }
| "createUnorderedList" { CREATEUNORDEREDLIST  }

(* OBJECT KEYWORDS *)
| "object" { OBJECT }

| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }