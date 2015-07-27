
open Format
open Lexing
open Parser
open Ast
open Interp

(* Définition des options proposées pour l'exécution *)

let usage = "usage : jeu [options] file.mc"

let spec = []

(* Ouverture du fichier à compiler *)
let rfile =
  let file = ref None in
  let set_file s = 
    if not (Filename.check_suffix s ".mc") then
      raise (Arg.Bad "no .mc extension") ;
    file := Some s in
  Arg.parse spec set_file usage ;
  match !file with
    | Some f -> f
    | None -> Arg.usage spec usage ; exit 1

let print_loc (b, e) =
  eprintf "File \"%s\", line %d, characters %d-%d:\n" rfile
    b.pos_lnum (b.pos_cnum - b.pos_bol + 1) (e.pos_cnum - b.pos_bol + 1)
    
let print_loc_lb lb = print_loc (lexeme_start_p lb, lexeme_end_p lb)

(* Fonction principale *)
let () =
  try
    let c = open_in rfile in
    let lb = from_channel c in
    try
      let ast = file Lexer.next_token lb in
      close_in c ;
      try
        let ret, _ = interpFile ast [] Mstr.empty in
        match ret with
          | Vint i -> printf "%d@." !i
          | _ -> printf "Pas entier !@."
      with
        | Interp.Error (loc, s) -> print_loc loc ; eprintf "%s@." s
    with
      | Lexer.Error s -> print_loc_lb lb ; eprintf "%s@." s
      | Parser.Error -> print_loc_lb lb ; eprintf "syntax error@."
  with e -> eprintf "Anomaly: %s\n@." (Printexc.to_string e)
