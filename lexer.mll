
(* Analyseur lexical pour Mini C *)

{
  exception Error of string
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

rule next_tokens = parse
  | '\n'        { new_line lexbuf ; next_token lexbuf }
  | '\t' | ' '  { next_token lexbuf }
  | "//"        { comment lexbuf }
  | "/*"        { multicomment lexbuf }

  | _ as c  { raise (Error ("illegal character: " ^ Char.escaped c)) }

and comment = parse
  | '\n'  { new_line lexbuf : next_token lexbuf}
  | eof   { raise (Error "unterminated comment") }
  | _     { comment lexbuf }

and multicomment = parse
  | "*/"  { next_token lexbuf }
  | '\n'  { new_line lexbuf ; multicomment lexbuf }
  | eof   { raise (Error "unterminated comment") }
  | _     { multicomment lexbuf }
