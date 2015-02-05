
(* Interpréteur pour Mini C *)

open Ast

exception Error of loc * string

type var =
  | Vint of int ref
  | Varray of var array

type tfun = { typ : typ; args : (loc_ident * typ) list; body : stat }

module Mstr = Map.Make(String)

(* Interprétation... *)

exception Return of var
exception ReturnVoid

let rec newVar =
  assert false

let rec interpExpr =
  assert false

and interpStat vars = function
  | Sexpr e -> let _ = interpExpr vars e in vars
  | Sdo ls -> List.fold_right (fun vars s -> interpStat s vars) ls vars ; vars
  | Sreturn e -> raise (Return (interpExpr vars e))
  | SreturnVoid -> raise ReturnVoid
  | Sif (e, s) -> ( let cdt = interpExpr vars e in
    match cdt with
      | Vint n -> if !n = 0 then
          ()
        else
          interpStat vars s
      | _ -> assert false ) ;
    vars
  | Sifelse (e, s1, s2) -> ( let cdt = interpExpr vars e in
    match cdt with
      | Vint n -> if !n = 0 then
          interpStat vars s2
        else
          interpStat vars s1
      | _ -> assert false ) ;
    vars
  | Swhile (e, s) as w -> interpStat vars (Sif (e, Sdo [s ; w]))
  | Sdecl (lid, t) -> assert false (* List.fold_right newVar t lid vars *)

(** Lecture du fichier **)

let rec newGlobalVar = function
  | VTarray ({ expr = Econst (Cint n) }, t) when n > 0 ->
      Varray (Array.init n (fun _ -> newGlobalVar t))
  | TTint -> Vint (ref 0)
  | VTarray _ -> assert false
  | _ -> assert false

let addGlobalVar t id vars =
  if Mstr.mem id.ident vars then
    assert false ;
  Mstr.add id.ident (newGlobalVar t) vars

let addFun t id args body funs =
  if Mstr.mem id.ident funs then
    assert false ;
  match t with
    | VTarray _ -> assert false
    | _ -> Mstr.add id.ident { typ = t; args = args; body = body } funs
  
let addStaticVar t id statics =
  if Mstr.mem id.ident statics then
    statics
  else
    Mstr.add id.ident (newGlobalVar t) statics


let interpFile ldecl statics =
  let rec aux statics vars funs = function    (* Ast.file -> Mstr *)
    | [] -> (statics, vars, funs)
    | Dident (lid, t)::q ->
      aux statics (List.fold_right (addGlobalVar t) lid vars) funs q
    | DstaticIdent (lid, t)::q ->
      aux (List.fold_right (addStaticVar t) lid statics)
        (List.fold_right (addGlobalVar t) lid vars) funs q
    | Dfun (id, t, args, body)::q ->
        aux statics vars (addFun t id args body funs) q in
  let statics = aux ldecl in
  assert false
