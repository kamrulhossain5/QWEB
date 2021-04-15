(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SOutput of sexpr
  | SIF of sexpr * sstmt * sstmt
  | SFOR of sexpr * sexpr * sstmt
  (*| SREPEAT of sexpr * sstmt*)
  | SWHILE of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SFliteral(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " to " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ "\n";
  | SOutput(expr) -> "output " ^ string_of_sexpr expr ^ "\n";
  | SIF(e, s, SBlock([])) ->
      "IF (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIF(e, s1, s2) ->  "IF (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "OTHERWISE\n" ^ string_of_sstmt s2
  | SFOR(e1, e2, s) ->
      "FOR each " ^ string_of_sexpr e1  ^ " in " ^ string_of_sexpr e2 ^ " END FOR " ^ string_of_sstmt s
  (*| SREPEAT(e, s) -> "REPEAT until " ^ string_of_sexpr e ^ string_of_sstmt s ^ " END REPEAT "*)
  | SWHILE(e, s) -> "WHILE (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s ^ " END WHILE "

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
