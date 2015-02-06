
open Format
open Lexing

module Mstr = Map.Make(String)

(** Gestion des prgorammes **)

let print_loc file (b, e) =
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file
    b.pos_lnum (b.pos_cnum - b.pos_bol + 1) (e.pos_cnum - b.pos_bol + 1)

let print_loc_lb file lb = print_loc file (lexeme_start_p lb, lexeme_end_p lb) 

(* Ouvertures des programmes et parsing *)

let getAst file =
  try 
    let c = open_in file in
    let lb = from_channel c in
    try 
      let ast = Parser.file Lexer.next_token lb in
      close_in c ; 
      Some ast
    with
      | Lexer.Error s -> print_loc_lb file lb ; eprintf "%s@." s ; None
      | Parser.Error -> print_loc_lb file lb ; eprintf "syntax error@." ; None
  with e -> eprintf "Anomaly: %s\n@." (Printexc.to_string e) ; None

exception NotADirectory of string

let openPrograms dir =
  if Sys.is_directory dir then
    let files = Array.to_list (Sys.readdir dir) in
    let progs =
      List.filter (fun name -> Filename.check_suffix name ".mc") files in
    let rec getAsts = function
      | [] -> Mstr.empty
      | prog::q -> match getAst prog with
        | Some ast -> Mstr.add (Filename.chop_suffix prog ".mc") ast (getAsts q)
        | None -> getAsts q in
    getAsts progs
  else
    raise (NotADirectory dir)

(* Fonction princiaple *)
let () =
  try
    let progs1 = openPrograms Sys.argv.(1) in
    let progs2 = openPrograms Sys.argv.(2) in
    assert false
  with _ ->
    assert false
