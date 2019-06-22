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
%type <Ast.ast> main
%%

main:
  expr EOL        { $1 }
;

expr:
  | NUM             { Num $1 }
  | SYM             { Sym $1 }
  | application     { $1 }

application:
  LPAREN PRIM args RPAREN { apply $2 $3 }

args:
  | expr            { [$1]   }
  | expr args       { $1::$2 }
