
(* Arbre de syntaxe abstraite de Mini C *)

type ident = string

type binop =
  | Bplus | Bminus | Btimes | Bdiv | Bmod
  | Band | Bor | Bband | Bbor | Bbxor | Bshiftl | Bshiftr
  | Beq | Bneq | Bleq | Blt | Bgeq | Bgt

type unop = 
  | Unot | Ubnot

type typ =
  | Tarray of int * typ
  | Tint

type constant =
  | Carray of expr array
  | Cint of int

and left =
  | Lterm of ident * expr
  | Lident of ident

and expr =
  | Eleft of left
  | Eassign of left * expr
  | Ecall of ident * expr list
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Econst of constant

type stat =
  | Sexpr of expr
  | Sdo of stat list
  | Sreturn of expr
  | Sif of expr * stat
  | Sifelse of expr * stat * stat
  | Swhile of expr * stat
  | Sdecl of ident * typ

type declaration =
  | Dident of ident * typ
  | Dfun of ident * typ * (ident * typ) list * stat

type file = declaration list
