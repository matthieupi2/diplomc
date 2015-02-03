
(* Analyseur syntaxique pour Petit Haskell *)

%{
  open Ast
%}

%token <Ast.ident> IDENT
%token <Ast.constant> CST
%token IF ELSE RETURN WHILE
%token LB RB LSB RSB LCB RCB
%token COLON ASSIGN
%token EQ NEQ LEQ LT GEQ GT
%token PLUS MINUS TIMES DIV MOD AND OR BAND BOR BXOR SHIFTL SHIFTR
%token NOT BNOT
%token EOF

%start file

%type <Ast.file> file

%%

file:
  | EOF { [] }
