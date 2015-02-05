
(* Interpréteur pour Mini C *)

open Ast

exception Error of loc * string

type var =
  | Vint of int ref
  | Varray of var array
  | Vvoid                   (* TODO ... *)

type tfun = { typ : typ; args : (loc_ident * typ) list; body : stat }

module Mstr = Map.Make(String)

let add_list f t lid map =
  List.fold_right (fun id map -> Mstr.add id.ident (f t) map) lid map

(* Interprétation... *)

exception Return of var
exception ReturnVoid

let rec areSameTypes a b = match a, b with
  | Vint _, Vint _ -> true
  | Varray a, Varray b -> areSameTypes a.(0) b.(0)
  | _ -> false

let rec isOfType typ a = match a, typ with
  | Vint _, TTint -> true
  | Varray a, FTarray typ -> isOfType typ a.(0)
  | _ -> false

let rec newVar vars funs = function
  | VTarray (e, t) -> ( match interpExpr vars funs e with
    | Vint i when !i > 0 -> Varray (Array.init !i (fun _ -> newVar vars funs t))
    | _ -> assert false )
  | TTint -> Vint (ref 0)
  | _ -> assert false

and interpLeft _ _ _ =
  assert false

(* TODO ajouter \0 à la fin des chaînes de caractères *)
(* TODO assuré 32 bit *)

and interpExpr vars funs e = match e.expr with
  | Eleft l -> interpLeft vars funs l
  | Eassign (l, e) -> let l = interpLeft vars funs l in
    let e = interpExpr vars funs e in
    if areSameTypes l e then
      match l, e with
        | Vint refl, Vint v -> refl := !v ; l
        | Varray arrl, Varray arr ->
          for i = 0 to min (Array.length arrl) (Array.length arr) - 1 do
            arrl.(i) <- arr.(i)
          done ;
          l
        | _ -> assert false
    else
      assert false
  | Ecall (id, args) -> (
    try
      let f = Mstr.find id.ident funs in
      let rec add_args vars formals args = match formals, args with
        | [], [] -> vars
        | formal::qf, arg::qa -> 
          let v = interpExpr vars funs arg in
          if isOfType (snd formal) v then
            add_args (Mstr.add (fst formal).ident v vars) qf qa
          else
            assert false
        | _ -> assert false in
      try
        ignore (interpStat (add_args vars f.args args) funs f.body) ;
        if f.typ = FTvoid then
          raise ReturnVoid
        else
          assert false
      with
        | Return v -> v
        | ReturnVoid -> Vvoid
    with Not_found -> assert false )
  | _ -> assert false

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
