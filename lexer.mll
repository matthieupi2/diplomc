
(* Analyseur lexical pour Mini C *)

{
  open Ast
  open Parser
  open Lexing

  exception Error of string

  (* Définition des mots-clefs *)
  let kwd = Hashtbl.create 17
  let () = List.iter (fun (k,t) -> Hashtbl.add kwd k t)
    [ "if", IF ;  "else", ELSE ; "return", RETURN ; "while", WHILE ;
      "static", STATIC ; "void", VOID ; "int", INT ]

  (* Différencie les identifiants des mots-clefs *)
  let id s =
    try
      Hashtbl.find kwd s
    with Not_found ->
      IDENT s

  let int_of_character = function
    | "\\0" -> 0
    | "\\\\" -> 92
    | "\\n" -> 10
    | "\\'" -> 39
    | "\\t" -> 9
    | s -> int_of_char s.[0]

  let carray_of_list l = 
    let arr = Array.of_list l in
    Carray (Array.length arr, arr)
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let integer = digit+ | '0' ( ('b'|'B')['0'-'1']+ | ('o'|'O')['0'-'7']+ |
  ('x'|'X')['0'-'9' 'a'-'f' 'A'-'F']+)
let character = ['\032'-'\126']#['\\' '"'] | '\\'['0' '\\' 'n' '\'' '"' 't']
let ident = letter(letter|digit)*

rule next_token = parse
  | '\n'        { new_line lexbuf ; next_token lexbuf }
  | '\t' | ' '  { next_token lexbuf }
  | "//"        { comment lexbuf }
  | "/*"        { multicomment lexbuf }

  | ident as s  { id s }

  | integer as s              { try
      CST (Cint (int_of_string s))
    with _ ->
      raise (Error ("constant too large")) }
  | '\''(character as s)'\''  { CST (Cint (int_of_character s)) }
  | '"'                       { CST (carray_of_list (lstring lexbuf)) }

  | '(' { LB }
  | ')' { RB }
  | '[' { LSB }
  | ']' { RSB }
  | '{' { LCB }
  | '}' { RCB }

  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '=' { ASSIGN }
  
  | "=="  { EQ }
  | "!="  { NEQ }
  | "<="  { LEQ }
  | '<'   { LT }
  | ">="  { GEQ }
  | '>'   { GT }

  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { TIMES }
  | '/'   { DIV }
  | '%'   { MOD }
  | "&&"  { AND }
  | "||"  { OR }
  | '&'   { BAND }
  | '|'   { BOR }
  | '^'   { BXOR }
  | "<<"  { SHIFTL }
  | ">>"  { SHIFTR }

  | '!' { NOT }
  | '~' { BNOT }

  | eof { EOF }

  | '\''character { raise (Error "missing \'") }
  | '\''('\\'_ as s) 
    { raise (Error ("illegal escape sequence " ^ s)) }
  | '\''(_ as c)
    { raise (Error ("illegal character between \': " ^ Char.escaped c)) }
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

and lstring = parse
  | '"'             { [] }
  | character as s  { { expr = Econst (Cint (int_of_character s));
    eloc = (dummy_pos, dummy_pos) }::lstring lexbuf }
  | eof             { raise (Error "unterminated string") }
  | '\\'_ as s       { raise (Error ("illegal escape sequence " ^ s)) }
  | _ as c
    { raise (Error ("illegal character in a string: " ^ Char.escaped c)) }
