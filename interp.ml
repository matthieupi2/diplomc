
(* Interpréteur pour Mini C *)

open Ast

exception Error of loc * string

type var_typ =
  | Tarray of int * var_typ
  | Tint

type var =
  | Vint of int ref
  | Varray of tvar array (* TODO très lourd *)

and tvar = { typ : var_typ; var : var }

type tfun = { ftyp : typ; args : (loc_ident * typ) list; body : stat }

module Mvar = Map.Make(String)

let funs = Hashtbl.create 13
let vars = Hashtbl.create 53

(** Lecture du fichier **)

let rec newGlobalVar = function
  | VTarray ({ expr = Econst (Cint n) }, t) when n > 0 ->
      let arr = Array.init n (fun _ -> newGlobalVar t) in
      { typ = Tarray (n, arr.(0).typ); var = Varray arr }
  | TTint -> { typ = Tint; var = Vint (ref 0) }
  | VTarray _ -> assert false
  | _ -> assert false

let addGlobalVar t id =
  if Hashtbl.mem vars id.ident then
    assert false ;
  Hashtbl.add vars id.ident (newGlobalVar t)

let addFun t id args body =
  if Hashtbl.mem funs id.ident then
    assert false ;
  match t with
    | VTarray _ -> assert false
    | _ -> Hashtbl.add funs id.ident { ftyp = t; args = args; body = body }
  
let addStaticVar t id statics =
  if Mvar.mem id.ident statics then
    statics
  else
    Mvar.add id.ident (newGlobalVar t) statics


let interpFile ldecl statics =
  let rec aux = function    (* Ast.file -> Mvar *)
    | [] -> statics
    | Dident (lid, t)::q -> List.iter (addGlobalVar t) lid ; aux q
    | DstaticIdent (lid, t)::q -> List.iter (addGlobalVar t) lid ;
      List.fold_right (addStaticVar t) lid (aux q)
    | Dfun (id, t, args, body)::q -> addFun t id args body ; aux q in
  let statics = aux ldecl in
  assert false
