
open Format
open Lexing
open Parser
open Ast

(* Définition des options proposées pour l'exécution *)

let usage = "usage : jeu [options] file.mc"

let spec = []

(* Ouverture du fichier à compiler *)
let rfile =
  let file = ref None in
  let set_file s = 
    if not (Filename.check_suffix s ".mc") then
      raise (Arg.Bad "no .hs extension") ;
    file := Some s in
  Arg.parse spec set_file usage ;
  match !file with
    | Some f -> f
    | None -> Arg.usage spec usage ; exit 1

(* Fonction principale *)
let () =
  try
    let c = open_in rfile in
    let lb = from_channel c in
    try
      let _ = file Lexer.next_token lb in
      close_in c ;
        exit 0
    with SyntaxError s ->
      let b, e = lexeme_start_p lb, lexeme_end_p lb in
      eprintf "File \"%s\", line %d, characters %d-%d:\n%s@." rfile
        b.pos_lnum (b.pos_cnum - b.pos_bol + 1) (e.pos_cnum - b.pos_bol + 1) s
  with e -> eprintf "Anomaly: %s\n@." (Printexc.to_string e)
