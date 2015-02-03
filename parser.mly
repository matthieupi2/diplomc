
(* Analyseur syntaxique pour Petit Haskell *)

%{
  open Ast

  let syntax_error s =
    raise (SyntaxError s)

  let carray_of_list l = 
    let arr = Array.of_list l in
    Carray (Array.length arr, arr)
%}

%token <Ast.ident> IDENT
%token <Ast.constant> CST
%token IF ELSE RETURN WHILE INT
%token LB RB LSB RSB LCB RCB
%token SEMICOLON COMMA ASSIGN
%token EQ NEQ LEQ LT GEQ GT
%token PLUS MINUS TIMES DIV MOD AND OR BAND BOR BXOR SHIFTL SHIFTR
%token NOT BNOT
%token EOF

%start file

%type <Ast.file> file

%%

file:
  | ldecl=decl* EOF { ldecl } ;

decl:
  | t=typ lident=separated_nonempty_list(COMMA, IDENT) SEMICOLON
    { Dident (lident, t) }
  | t=typ f=IDENT LB args=separated_list(COMMA, decl_ident) RB s=stat
    { Dfun (f, t, args, s) } ;

decl_ident:
  | t=typ ident=IDENT { (ident, t) } ;

typ:
  | INT t=typ2 { t } ;

typ2:
  | LSB e=expr RSB t=typ2 { Tarray (e, t) }
  |                       { Tint } ;

stat:
  | e=expr SEMICOLON                      { Sexpr e }
  | LCB ls=separated_nonempty_list(SEMICOLON, stat) SEMICOLON RCB { Sdo ls }
  | RETURN e=expr                         { Sreturn e }
  | IF LB e=expr RB s=stat                { Sif (e, s) }
  | IF LB e=expr RB s1=stat ELSE s2=stat  { Sifelse (e, s1, s2) }
  | WHILE LB e=expr RB s=stat             { Swhile (e, s) }
  | t=typ ident=IDENT                     { Sdecl (ident, t) } ;

expr:
  | l=left  { Eleft l }
  | l=left ASSIGN e=expr  { Eassign (l, e) }
  | f=IDENT LB args=separated_list(COMMA, expr) RB  { Ecall (f, args) }
  | e1=expr op=binop e2=expr  { Ebinop (op, e1, e2) }
  | op=unop e=expr  { Eunop (op, e) }
  | LCB arr=separated_nonempty_list(COMMA, expr)
    { Econst (carray_of_list arr) } ;
  | c=CST { Econst c } ;

left:
  | ident=IDENT LSB e=expr RSB  { Lterm (ident, e) }
  | ident=IDENT                 { Lident ident }

%inline binop:
  | PLUS    { Bplus }
  | MINUS   { Bminus }
  | TIMES   { Btimes }
  | DIV     { Bdiv }
  | MOD     { Bmod }
  | AND     { Band }
  | OR      { Bor }
  | BAND    { Bband }
  | BOR     { Bbor }
  | BXOR    { Bbxor }
  | SHIFTL { BshiftL }
  | SHIFTR { BshiftR }
  | EQ     { Beq }
  | NEQ    { Bneq }
  | LEQ    { Bleq }
  | LT     { Blt }
  | GEQ    { Bgeq }
  | GT     { Bgt } ;

%inline unop:
  | MINUS { Uneg }
  | NOT   { Unot }
  | BNOT  { Ubnot } ;
