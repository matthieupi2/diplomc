
(* Analyseur syntaxique pour Petit Haskell *)

%{
  open Ast

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

%left     ASSIGN
%left     OR
%left     AND
%left     BOR
%left     BXOR
%left     BAND
%left     EQ NEQ
%left     LEQ LT GEQ GT
%left     SHIFTL SHIFTR
%left     PLUS MINUS
%left     TIMES DIV MOD
%right    NOT BNOT neg
%nonassoc RB    (* IF *)
%nonassoc ELSE

%start file

%type <Ast.file> file

%%

file:
  | ldecl=decl* EOF { ldecl } ;

loc_ident:
  | id=IDENT  { {ident = id; loc = ($startpos, $endpos)} }

decl:
  | t=typ lident=separated_nonempty_list(COMMA, loc_ident) SEMICOLON
    { Dident (lident, t) }
  | t=typ f=loc_ident LB args=separated_list(COMMA, decl_ident) RB s=stat
    { Dfun (f, t, args, s) } ;

decl_ident:
  | t=typ ident=loc_ident { (ident, t) } ;

typ:
  | INT t=typ2 { t } ;

typ2:
  | LSB e=loc_expr RSB t=typ2 { Tarray (e, t) }
  |                           { Tint } ;

stat:
  | e=loc_expr SEMICOLON                      { Sexpr e }
  | LCB ls=stat* RCB                          { Sdo ls }
  | RETURN e=loc_expr SEMICOLON               { Sreturn e }
  | IF LB e=loc_expr RB s=stat                { Sif (e, s) }
  | IF LB e=loc_expr RB s1=stat ELSE s2=stat  { Sifelse (e, s1, s2) }
  | WHILE LB e=loc_expr RB s=stat             { Swhile (e, s) }
  | t=typ ident=loc_ident SEMICOLON           { Sdecl (ident, t) } ;

loc_expr:
  | e=expr  { {expr = e; loc = ($startpos, $endpos)} }

expr:
  | l=left                              { Eleft l }
  | l=left ASSIGN e=loc_expr            { Eassign (l, e) }
  | f=loc_ident LB args=separated_list(COMMA, loc_expr) RB { Ecall (f, args) }
  | e1=loc_expr op=binop e2=loc_expr    { Ebinop (op, e1, e2) }
  | op=unop e=loc_expr                  { Eunop (op, e) }
  | MINUS e=loc_expr %prec neg          { Eunop (Uneg, e) }
  | LCB arr=separated_nonempty_list(COMMA, loc_expr) RCB
    { Econst (carray_of_list arr) }
  | c=CST                               { Econst c } ;

left:
  | ident=loc_ident LSB e=loc_expr RSB  { Lterm (ident, e) }
  | ident=loc_ident                     { Lident ident } ;

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
  | SHIFTL  { BshiftL }
  | SHIFTR  { BshiftR }
  | EQ      { Beq }
  | NEQ     { Bneq }
  | LEQ     { Bleq }
  | LT      { Blt }
  | GEQ     { Bgeq }
  | GT      { Bgt } ;

%inline unop:
  | NOT             { Unot }
  | BNOT            { Ubnot } ;
