%{
  open Ast
%}

%token LPAREN
%token RPAREN
%token <string> SYM
%token <string> PRIM
%token <int> NUM
%token EOL
%start main
%type <Ast.sexpr> main
%%

main:
  sexpr EOL         { $1 }
;

sexpr:
  | NUM             { Num $1 }
  | SYM             { Sym $1 }
  | LPAREN args RPAREN
                    { $2 }
  | LPAREN RPAREN   { Nil }

args:
  | sexpr           { Pair ($1, Nil) }
  | sexpr args      { Pair ($1, $2) }
