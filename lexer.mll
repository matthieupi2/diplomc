
(* Analyseur lexical pour Mini C *)

{
  open Ast
  open Parser
  open Lexing

  exception Error of string

  (* Définition des mots-clefs *)
  let kwd = Hashtbl.create 17
  let () = List.iter (fun (k,t) -> Hashtbl.add kwd k t)
    ["if", IF ;  "else", ELSE ; "return", RETURN ; "int", INT ; "while", WHILE]

  (* Différencie les identifiants des mots-clefs *)
  let id s =
    try
      Hashtbl.find kwd s
    with Not_found ->
      IDENT s
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let bdigit = ['0'-'1']
let hdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = digit+
let character = ['\032'-'\126']#['\\' '"'] | '\\'['0' '\\' 'n' '\'' '"' 't']
let binteger = "0b" bdigit+
let hinteger = "0x" hdigit+
let ident = letter(letter|digit)*

rule next_token = parse
  | '\n'        { new_line lexbuf ; next_token lexbuf }
  | '\t' | ' '  { next_token lexbuf }
  | "//"        { comment lexbuf }
  | "/*"        { multicomment lexbuf }

  | ident as s  { id s }

  | _ as c  { raise (Error ("illegal character: " ^ Char.escaped c)) }

and comment = parse
  | '\n'  { new_line lexbuf ; next_token lexbuf }
  | eof   { raise (Error "unterminated comment") }
  | _     { comment lexbuf }

and multicomment = parse
  | "*/"  { next_token lexbuf }
  | '\n'  { new_line lexbuf ; multicomment lexbuf }
  | eof   { raise (Error "unterminated comment") }
  | _     { multicomment lexbuf }
