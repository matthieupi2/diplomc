
(* Interpréteur pour Mini C *)

open Ast

exception Error of loc * string

type var =
  | Vint of int ref
  | Varray of var array

module Mvar = Map.Make(String)

type tvar = { typ : typ; var : var }

type tfun = { typ : typ; args : (loc_ident * typ) list; body : stat }

let funs = Hashtbl.create 13
let vars = Hashtbl.create 53

(** Lecture du fichier **)

let rec newGlobalVar = function
  | Tarray ({ expr = Econst (Cint n) }, t) ->
      Varray (Array.init n (fun _ -> newGlobalVar t))
  | Tint -> Vint (ref 0)
  | Tarray _ -> assert false
  | Tvoid -> assert false

let addGlobalVar t id =
  if Hashtbl.mem vars id.ident then
    assert false ;
  Hashtbl.add vars id.ident (newGlobalVar t)

let addFun t id args body =
  if Hashtbl.mem funs id.ident then
    assert false ;
  Hashtbl.add funs id.ident { typ = t; args = args; body = body }
  
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
