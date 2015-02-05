
(* Interpréteur pour Mini C *)
(* Pour l'instant l'on n'admet pas le changement de programme conservant les
 * variables statiques, mais ça peut changer... *)

open Ast

exception Error of loc * string

type var =
  | Vint of int ref
  | Varray of var array
  | Vvoid                   (* TODO ... *)

type tfun = { typ : typ; args : (loc_ident * typ) list; body : stat }

module Mstr = Map.Make(String)

let add_list f t lid map =
  List.fold_right (fun id map -> Mstr.add id.ident (f id.iloc t) map) lid map

let int_of_bool b = if b then 1 else 0

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

(* Gestion des erreurs *)

let rec string_of_type = function
  | VTarray _ -> failwith "remaining VTarray"
  | FTarray t -> string_of_type t ^ "[]"
  | TTint -> "int"
  | FTvoid -> "void"

let rec type_of_var = function
  | Vint _ -> TTint
  | Varray arr -> FTarray (type_of_var arr.(0))
  | Vvoid -> FTvoid

let type_error loc a typ =
  let s1 = string_of_type (type_of_var a) in
  let s2 = string_of_type typ in
  raise (Error (loc, "type error : is of type " ^ s1 ^
    "\nbut should be of type " ^ s2))

let diff_types_error loc a b =
  let s1 = string_of_type (type_of_var a) in
  let s2 = string_of_type (type_of_var b) in
  raise (Error (loc, "type error : is of type " ^ s1 ^
    "\nbut should be of type " ^ s2))

let unbound_error id =
  raise (Error (id.iloc, "error: unbound value " ^ id.ident))

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

let rec newVar vars funs loc = function
  | VTarray (e, t) -> ( match interpExpr vars funs e with
    | Vint i when !i > 0 ->
      Varray (Array.init !i (fun _ -> newVar vars funs dummy_loc t))
    | Vint _ -> raise (Error (e.eloc, "error: size of array should be a" ^
        "positive integer"))
    | _ as a -> type_error e.eloc a TTint )
  | TTint -> Vint (ref 0)
  | _ as t -> raise (Error (loc, "type error: type " ^ string_of_type t ^
      " cannot be the type of a variable"))

and interpLeft vars funs = function
  | Lterm (id, e) -> (
    try
      match Mstr.find id.ident vars, interpExpr vars funs e with
        | Varray arr, Vint i-> (
          try
            arr.(!i)
          with Invalid_argument "index out of bounds" -> 
            raise (Error (e.eloc, "error: index out of bounds: " ^ id.ident ^
              "'s length is " ^ string_of_int (Array.length arr))) )
        | Varray _, (_ as a) -> type_error e.eloc a TTint
        | _ -> raise (Error (id.iloc, "type error: " ^ id.ident ^
            " should be an array"))
    with
      | Not_found -> unbound_error id
       )
  | Lident id ->
    try
      Mstr.find id.ident vars
    with
      | Not_found -> unbound_error id

(* TODO ajouter \0 à la fin des chaînes de caractères *)
(* TODO assurer 32 bit *)

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
          | _ -> failwith "areSameTypes failed"
      else
        diff_types_error e.eloc v l
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
              type_error arg.eloc v (snd formal)
          | _ -> raise (Error (id.iloc, "error: number of arguments does not " ^
            "match definition")) in
        try
          ignore (interpStat f.typ (add_args vars f.args args) funs f.body) ;
          if f.typ = FTvoid then
            raise ReturnVoid
          else
            raise (Error (id.iloc, "type error: function " ^ id.ident ^
              " should return something"))
        with
          | Return v -> v
          | ReturnVoid -> Vvoid
      with Not_found -> unbound_error id )
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
            | _ as a -> type_error e2.eloc a TTint )
        | _,(_ as a) -> type_error e1.eloc a TTint )
    | Eunop (op, e) -> (
      match interpExpr e with
        | Vint i -> let i = !i in
          Vint (ref (match op with
            | Uneg  -> -i
            | Unot  -> if i = 0 then 1 else 0
            | Ubnot -> lnot i ))
        | _ as a -> type_error e.eloc a TTint )
    | Econst c -> match c with
      | Carray (n, arr) -> let v0 = interpExpr arr.(0) in
        let aux = function
          | 0 -> v0
          | n -> let v = interpExpr arr.(n) in
            if areSameTypes v v0 then
              v
            else
              diff_types_error e.eloc v v0 in
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
        type_error e.eloc v retTyp
    | SreturnVoid loc ->
      if retTyp = FTvoid then
        raise ReturnVoid
      else 
        raise (Error (loc, "type error : this function should return " ^
          "something of type " ^ string_of_type retTyp))
    | Sif (e, s) -> let cdt = interpExpr e in
      let _ = match cdt with
        | Vint n -> if !n = 0 then
            ()
          else
            ignore (interpStat vars s)
        | _ -> type_error e.eloc cdt TTint in
      vars
    | Sifelse (e, s1, s2) -> let cdt = interpExpr e in
      let _ = match cdt with
        | Vint n -> if !n = 0 then
            interpStat vars s2
          else
            interpStat vars s1
        | _ -> type_error e.eloc cdt TTint in
      vars
    | Swhile (e, s) as w -> interpStat vars (Sif (e, Sdo [s ; w]))
    | Sdecl (lid, t) -> add_list (newVar vars funs) t lid vars

(** Lecture du fichier **)

let rec newGlobalVar loc = function
  | VTarray ({ expr = Econst (Cint n) }, t) when n > 0 ->
      Varray (Array.init n (fun _ -> newGlobalVar loc t))
  | TTint -> Vint (ref 0)
  | VTarray _ -> raise (Error (loc, "error: global array size should be " ^
      "a positive constant integer"))
  | _ as t -> raise (Error (loc, "type error: type " ^ string_of_type t ^
      " cannot be the type of a variable"))

let newFun t args body =
  match t with
    | VTarray _ -> failwith "there is a function of type VTarray"
    | _ -> { typ = t; args = args; body = body }
  
let addStaticVar t id (statics, vars) =
  try (* TODO compare type *)
    (statics, Mstr.add id.ident (Mstr.find id.ident statics) vars)
  with Not_found -> let v = newGlobalVar id.iloc t in
    (Mstr.add id.ident v statics, Mstr.add id.ident v vars)

let interpFile ldecl args statics =
  let rec aux statics vars funs = function    (* Ast.file -> Mstr *)
    | [] -> (statics, vars, funs)
    | Dident (lid, t)::q ->
        aux statics (add_list newGlobalVar t lid vars) funs q
    | DstaticIdent (lid, t)::q ->
     let statics, vars = List.fold_right (addStaticVar t) lid (statics, vars) in
      aux statics vars funs q
    | Dfun (id, t, args, body)::q ->
        aux statics vars (Mstr.add id.ident (newFun t args body) funs) q in
  let statics, vars, funs = aux statics Mstr.empty Mstr.empty ldecl in
  let ret = interpExpr vars funs { expr =
    Ecall ({ ident = "main"; iloc = dummy_loc }, args); eloc = dummy_loc } in
  (ret, statics)

