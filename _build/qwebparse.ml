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

open Parsing;;
let _ = parse_error;;
# 4 "qwebparse.mly"
open Ast
# 85 "qwebparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* FUNCTION *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* ASSIGN *);
  269 (* NOT *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* LEQ *);
  274 (* GT *);
  275 (* GEQ *);
  276 (* AND *);
  277 (* OR *);
  278 (* IN *);
  279 (* RETURN *);
  280 (* DISPLAY *);
  281 (* OUTPUT *);
  282 (* IF *);
  283 (* ENDIF *);
  284 (* OTHERWISE *);
  285 (* ENDOTHERWISE *);
  286 (* OTHERWISEIF *);
  287 (* ENDOTHERWISEIF *);
  288 (* FOR *);
  289 (* ENDFOR *);
  290 (* REPEAT *);
  291 (* ENDREPEAT *);
  292 (* SET *);
  293 (* CONTINUE *);
  294 (* PASS *);
  295 (* TO *);
  296 (* WHILE *);
  297 (* INT *);
  298 (* BOOL *);
  299 (* STR *);
  300 (* FLOAT *);
  301 (* CHAR *);
  302 (* VOID *);
  303 (* RECT *);
  304 (* CIRC *);
  305 (* TRI *);
  306 (* SQRE *);
  307 (* ELPS *);
  308 (* POLY *);
  309 (* POINT *);
  310 (* LINE *);
  311 (* DATE *);
  312 (* COLOR *);
  313 (* LBRACKET *);
  314 (* RBRACKET *);
  315 (* COLON *);
  316 (* DOT *);
  317 (* END *);
  318 (* LENGTH *);
  319 (* APPEND *);
  320 (* REMOVE *);
  321 (* PRINT *);
  322 (* CREATEHEADER *);
  323 (* CREATEPARAGRAPH *);
  324 (* CREATETABLE *);
  325 (* CREATEUNORDEREDLIST *);
  326 (* OBJECT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  327 (* ID *);
  328 (* LITERAL *);
  329 (* INT_LITERAL *);
  330 (* STRING_LITERAL *);
  331 (* BOOL_LITERAL *);
  332 (* FLOAT_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\007\000\007\000\003\000\010\000\010\000\010\000\008\000\
\008\000\013\000\013\000\014\000\012\000\012\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\005\000\005\000\005\000\
\005\000\005\000\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\000\000\002\000\003\000\002\000\003\000\003\000\000\000\
\002\000\000\000\002\000\001\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\004\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\039\000\000\000\030\000\031\000\034\000\032\000\
\033\000\001\000\003\000\004\000\000\000\000\000\012\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\010\000\000\000\
\000\000\009\000\011\000\000\000\000\000\000\000\000\000\016\000\
\005\000\000\000\000\000\023\000\026\000\025\000\024\000\017\000\
\000\000\000\000\000\000\022\000\000\000\000\000\013\000\028\000\
\014\000\015\000\037\000\000\000\000\000\029\000\000\000\038\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\018\000\025\000\029\000\
\019\000\040\000\041\000\045\000\000\000\000\000\052\000\053\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\195\254\021\255\000\000\244\254\
\198\254\013\255\018\255\000\000\022\255\244\254\000\000\210\254\
\244\254\000\000\000\000\221\254\254\254\037\255\016\255\000\000\
\000\000\016\255\039\255\000\000\000\000\000\000\000\000\000\000\
\038\255\040\255\004\255\000\000\041\255\016\255\000\000\000\000\
\000\000\000\000\000\000\042\255\043\255\000\000\016\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\044\255\
\000\000\000\000\045\255\000\000\000\000\000\000\000\000\000\000\
\010\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\049\255\034\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\255\000\000\000\000\
\000\000\000\000\000\000\000\000\050\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\019\000\000\000\251\255\000\000\000\000\014\000\
\000\000\000\000\229\255\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 303
let yytable = "\031\000\
\010\000\032\000\033\000\042\000\001\000\031\000\044\000\032\000\
\049\000\014\000\017\000\016\000\020\000\016\000\016\000\021\000\
\024\000\031\000\051\000\028\000\034\000\015\000\016\000\022\000\
\026\000\023\000\034\000\056\000\005\000\006\000\007\000\008\000\
\016\000\009\000\027\000\030\000\027\000\015\000\047\000\027\000\
\046\000\050\000\048\000\027\000\054\000\043\000\006\000\007\000\
\055\000\021\000\035\000\000\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\036\000\000\000\037\000\
\038\000\039\000\035\000\036\000\000\000\037\000\038\000\039\000\
\016\000\016\000\000\000\016\000\016\000\016\000\035\000\036\000\
\000\000\037\000\038\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\006\000\007\000\008\000\000\000\009\000"

let yycheck = "\002\001\
\000\000\004\001\005\001\031\000\001\000\002\001\034\000\004\001\
\005\001\071\001\016\000\002\001\071\001\004\001\005\001\003\001\
\022\000\002\001\046\000\025\000\023\001\001\001\002\001\006\001\
\071\001\004\001\023\001\055\000\041\001\042\001\043\001\044\001\
\023\001\046\001\001\001\071\001\003\001\001\001\001\001\006\001\
\002\001\001\001\003\001\025\000\003\001\032\000\003\001\003\001\
\006\001\001\001\003\001\255\255\003\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\071\001\072\001\255\255\074\001\
\075\001\076\001\071\001\072\001\255\255\074\001\075\001\076\001\
\071\001\072\001\255\255\074\001\075\001\076\001\071\001\072\001\
\255\255\074\001\075\001\076\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\041\001\042\001\043\001\044\001\255\255\046\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  FUNCTION\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  IN\000\
  RETURN\000\
  DISPLAY\000\
  OUTPUT\000\
  IF\000\
  ENDIF\000\
  OTHERWISE\000\
  ENDOTHERWISE\000\
  OTHERWISEIF\000\
  ENDOTHERWISEIF\000\
  FOR\000\
  ENDFOR\000\
  REPEAT\000\
  ENDREPEAT\000\
  SET\000\
  CONTINUE\000\
  PASS\000\
  TO\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  STR\000\
  FLOAT\000\
  CHAR\000\
  VOID\000\
  RECT\000\
  CIRC\000\
  TRI\000\
  SQRE\000\
  ELPS\000\
  POLY\000\
  POINT\000\
  LINE\000\
  DATE\000\
  COLOR\000\
  LBRACKET\000\
  RBRACKET\000\
  COLON\000\
  DOT\000\
  END\000\
  LENGTH\000\
  APPEND\000\
  REMOVE\000\
  PRINT\000\
  CREATEHEADER\000\
  CREATEPARAGRAPH\000\
  CREATETABLE\000\
  CREATEUNORDEREDLIST\000\
  OBJECT\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  LITERAL\000\
  INT_LITERAL\000\
  STRING_LITERAL\000\
  BOOL_LITERAL\000\
  FLOAT_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 43 "qwebparse.mly"
           ( _1 )
# 389 "qwebparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "qwebparse.mly"
                 ( ([], [])               )
# 395 "qwebparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 48 "qwebparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 403 "qwebparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 49 "qwebparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 411 "qwebparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 53 "qwebparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 426 "qwebparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "qwebparse.mly"
                  ( [] )
# 432 "qwebparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 61 "qwebparse.mly"
                  ( _1 )
# 439 "qwebparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "qwebparse.mly"
                             ( [(_1,_2)]     )
# 447 "qwebparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "qwebparse.mly"
                             ( (_3,_4) :: _1 )
# 456 "qwebparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "qwebparse.mly"
                     ( [] )
# 462 "qwebparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 69 "qwebparse.mly"
                     ( _2 :: _1 )
# 470 "qwebparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "qwebparse.mly"
               ( (_1, _2) )
# 478 "qwebparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "qwebparse.mly"
                                              ( Expr _1               )
# 485 "qwebparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 86 "qwebparse.mly"
                                           ( Block(List.rev _2)    )
# 492 "qwebparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 87 "qwebparse.mly"
                                           ( Return _2             )
# 499 "qwebparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "qwebparse.mly"
                 ( [] )
# 505 "qwebparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "qwebparse.mly"
                  (_2 :: _1)
# 513 "qwebparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "qwebparse.mly"
                 ( [] )
# 519 "qwebparse.ml"
               : 'loop_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'loop_stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'loop_stmt) in
    Obj.repr(
# 103 "qwebparse.mly"
                            (_2 :: _1)
# 527 "qwebparse.ml"
               : 'loop_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "qwebparse.mly"
        ( _1 )
# 534 "qwebparse.ml"
               : 'loop_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "qwebparse.mly"
                  ( Noexpr )
# 540 "qwebparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "qwebparse.mly"
                  ( _1 )
# 547 "qwebparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 122 "qwebparse.mly"
               ( Literal(_1) )
# 554 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "qwebparse.mly"
                      ( Fliteral(_1) )
# 561 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 124 "qwebparse.mly"
                      ( BoolLit(_1)  )
# 568 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "qwebparse.mly"
                   ( Sliteral(_1) )
# 575 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "qwebparse.mly"
                 ( Id(_1) )
# 582 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 127 "qwebparse.mly"
                      ( _2                   )
# 589 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 128 "qwebparse.mly"
                             ( Call(_1, _3)  )
# 597 "qwebparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 211 "qwebparse.mly"
           ( Int   )
# 603 "qwebparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 212 "qwebparse.mly"
           ( Bool  )
# 609 "qwebparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 213 "qwebparse.mly"
           ( Float )
# 615 "qwebparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 214 "qwebparse.mly"
           ( Void  )
# 621 "qwebparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 215 "qwebparse.mly"
           ( Str   )
# 627 "qwebparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 234 "qwebparse.mly"
                  ( [] )
# 633 "qwebparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 235 "qwebparse.mly"
               ( List.rev _1 )
# 640 "qwebparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 238 "qwebparse.mly"
                            ( [_1] )
# 647 "qwebparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 239 "qwebparse.mly"
                         ( _3 :: _1 )
# 655 "qwebparse.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
