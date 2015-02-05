
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

let int_of_bool b = if b then 1 else 0

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

and interpExpr vars funs e =
  let interpExpr = interpExpr vars funs in
  match e.expr with
    | Eleft l -> interpLeft vars funs l
    | Eassign (l, e) -> let l = interpLeft vars funs l in
      let v = interpExpr e in
      if areSameTypes l v then
        match l, v with
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
            let v = interpExpr arg in
            if isOfType (snd formal) v then
              add_args (Mstr.add (fst formal).ident v vars) qf qa
            else
              assert false
          | _ -> assert false in
        try
          ignore (interpStat f.typ (add_args vars f.args args) funs f.body) ;
          if f.typ = FTvoid then
            raise ReturnVoid
          else
            assert false
        with
          | Return v -> v
          | ReturnVoid -> Vvoid
      with Not_found -> assert false )
    | Ebinop (op, e1, e2) -> ( (* TODO vérification typage malgré paresseux *)
      match op, interpExpr e1 with
        | Band, Vint i1 when !i1 == 0 -> Vint (ref 0)
        | Bor, Vint i1 when !i1 <> 0 -> Vint (ref 1)
        | _, Vint i1 -> ( let i1 = !i1 in
          match interpExpr e2 with
            | Vint i2 -> let i2 = !i2 in
              Vint (ref ( match op with
                | Bplus       -> i1 + i2
                | Bminus      -> i1 - i2
                | Btimes      -> i1 * i2
                | Bdiv        -> i1 / i2
                | Bmod        -> i1 mod i2
                | Band | Bor  -> int_of_bool (i2 <> 0)
                | Bband       -> i1 land i2
                | Bbor        -> i1 lor i2
                | Bbxor       -> i1 lxor i2
                | BshiftL     -> i1 lsl i2
                | BshiftR     -> i1 lsr i2
                | Beq         -> int_of_bool (i1 = i2)
                | Bneq        -> int_of_bool (i1 <> i2)
                | Bleq        -> int_of_bool (i1 <= i2)
                | Blt         -> int_of_bool (i1 < i2)
                | Bgeq        -> int_of_bool (i1 >= i2)
                | Bgt         -> int_of_bool (i1 > i2) ))
            | _ -> assert false )
        | _ -> assert false )
    | Eunop (op, e) -> (
      match interpExpr e with
        | Vint i -> let i = !i in
          Vint (ref (match op with
            | Uneg  -> -i
            | Unot  -> if i = 0 then 1 else 0
            | Ubnot -> lnot i ))
        | _ -> assert false )
    | Econst c -> match c with
      | Carray (n, arr) -> let e0 = interpExpr arr.(0) in
        let aux = function
          | 0 -> e0
          | n -> let e = interpExpr arr.(n) in
            if areSameTypes e e0 then
              e
            else
              assert false in
        Varray (Array.init n aux)
      | Cint i -> Vint (ref i)

and interpStat retTyp vars funs s =
  let interpExpr = interpExpr vars funs in
  let interpStat = (fun vars -> interpStat retTyp vars funs) in
  match s with
    | Sexpr e -> let _ = interpExpr e in vars
    | Sdo ls -> let _ =
        List.fold_right (fun s vars -> interpStat vars s) ls vars in 
      vars
    | Sreturn e -> let v = interpExpr e in
      if isOfType retTyp v then
        raise (Return v)
      else
        assert false
    | SreturnVoid ->
      if retTyp = FTvoid then
        raise ReturnVoid
      else
        assert false
    | Sif (e, s) -> let cdt = interpExpr e in
      let _ = match cdt with
        | Vint n -> if !n = 0 then
            ()
          else
            ignore (interpStat vars s)
        | _ -> assert false in
      vars
    | Sifelse (e, s1, s2) -> let cdt = interpExpr e in
      let _ = match cdt with
        | Vint n -> if !n = 0 then
            interpStat vars s2
          else
            interpStat vars s1
        | _ -> assert false in
      vars
    | Swhile (e, s) as w -> interpStat vars (Sif (e, Sdo [s ; w]))
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

let interpFile retTyp ldecl statics =
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
