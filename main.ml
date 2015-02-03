
open Format

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
    exit 0
  with e -> eprintf "Anomaly: %s\n@." (Printexc.to_string e)
