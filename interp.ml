
(* Interpréteur pour Mini C *)

open Ast

exception Error of loc * string

type var =
  | Vint of int ref
  | Varray of var array

type tfun = { typ : typ; args : (loc_ident * typ) list; body : stat }

module Mstr = Map.Make(String)

let add_list f t lid map =
  List.fold_right (fun id map -> Mstr.add id.ident (f t) map) lid map

(* Interprétation... *)

exception Return of var
exception ReturnVoid

let rec newVar vars funs = function
  | VTarray (e, t) -> ( match interpExpr vars funs e with
    | Vint i when !i > 0 -> Varray (Array.init !i (fun _ -> newVar vars funs t))
    | _ -> assert false )
  | TTint -> Vint (ref 0)
  | _ -> assert false

and interpExpr _ _ _ =
  assert false

and interpStat vars funs = function
  | Sexpr e -> let _ = interpExpr vars funs e in vars
  | Sdo ls -> let _ =
      List.fold_right (fun s vars -> interpStat vars funs s) ls vars in 
    vars
  | Sreturn e -> raise (Return (interpExpr vars funs e))
  | SreturnVoid -> raise ReturnVoid
  | Sif (e, s) -> let cdt = interpExpr vars funs e in
    let _ = match cdt with
      | Vint n -> if !n = 0 then
          ()
        else
          ignore (interpStat vars funs s)
      | _ -> assert false in
    vars
  | Sifelse (e, s1, s2) -> let cdt = interpExpr vars funs e in
    let _ = match cdt with
      | Vint n -> if !n = 0 then
          interpStat vars funs s2
        else
          interpStat vars funs s1
      | _ -> assert false in
    vars
  | Swhile (e, s) as w -> interpStat vars funs (Sif (e, Sdo [s ; w]))
  | Sdecl (lid, t) -> add_list (newVar vars funs) t lid vars

(** Lecture du fichier **)

let rec newGlobalVar = function
  | VTarray ({ expr = Econst (Cint n) }, t) when n > 0 ->
      Varray (Array.init n (fun _ -> newGlobalVar t))
  | TTint -> Vint (ref 0)
  | VTarray _ -> assert false
  | _ -> assert false

let newFun t args body =
  match t with
    | VTarray _ -> assert false
    | _ -> { typ = t; args = args; body = body }
  
let addStaticVar t id statics =
  if Mstr.mem id.ident statics then (* TODO compare type *)
    statics
  else
    Mstr.add id.ident (newGlobalVar t) statics

let interpFile ldecl statics =
  let rec aux statics vars funs = function    (* Ast.file -> Mstr *)
    | [] -> (statics, vars, funs)
    | Dident (lid, t)::q ->
        aux statics (add_list newGlobalVar t lid vars) funs q
    | DstaticIdent (lid, t)::q ->
      aux (List.fold_right (addStaticVar t) lid statics)
        (add_list newGlobalVar t lid vars) funs q
    | Dfun (id, t, args, body)::q ->
        aux statics vars (Mstr.add id.ident (newFun t args body) funs) q in
  let statics, vars, funs = aux statics Mstr.empty Mstr.empty ldecl in
  assert false
