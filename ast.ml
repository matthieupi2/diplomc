
(* Arbre de syntaxe abstraite de Mini C *)


exception SyntaxError of string

type ident = string

type binop =
  | Bplus | Bminus | Btimes | Bdiv | Bmod
  | Band | Bor | Bband | Bbor | Bbxor | BshiftL | BshiftR
  | Beq | Bneq | Bleq | Blt | Bgeq | Bgt

type unop = 
  | Uneg | Unot | Ubnot

type typ =
  | Tarray of expr * typ
  | Tint

and constant =
  | Carray of int * expr array
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
  | Dident of ident list * typ
  | Dfun of ident * typ * (ident * typ) list * stat

type file = declaration list
