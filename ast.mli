
(* Arbre de syntaxe abstraite de Mini C *)

type loc = Lexing.position * Lexing.position

type ident = string

type loc_ident = { ident : ident; loc : loc }

type binop =
  | Bplus | Bminus | Btimes | Bdiv | Bmod
  | Band | Bor | Bband | Bbor | Bbxor | BshiftL | BshiftR
  | Beq | Bneq | Bleq | Blt | Bgeq | Bgt

type unop = 
  | Uneg | Unot | Ubnot

type typ =
  | VTarray of loc_expr * typ
  | FTarray of typ
  | TTint
  | FTvoid

and constant =
  | Carray of int * loc_expr array
  | Cint of int

and left =
  | Lterm of loc_ident * loc_expr
  | Lident of loc_ident

and loc_expr = { expr : expr; loc : loc }

and expr =
  | Eleft of left
  | Eassign of left * loc_expr
  | Ecall of loc_ident * loc_expr list
  | Ebinop of binop * loc_expr * loc_expr
  | Eunop of unop * loc_expr
  | Econst of constant

type stat =
  | Sexpr of loc_expr
  | Sdo of stat list
  | Sreturn of loc_expr
  | SreturnVoid
  | Sif of loc_expr * stat
  | Sifelse of loc_expr * stat * stat
  | Swhile of loc_expr * stat
  | Sdecl of loc_ident list * typ

type declaration =
  | Dident of loc_ident list * typ
  | DstaticIdent of loc_ident list * typ
  | Dfun of loc_ident * typ * (loc_ident * typ) list * stat

type file = declaration list
