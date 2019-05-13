%{
  open Ast
%}

%token <int> INT
%token <string> SYM
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%nonassoc UMINUS

%start main
%type <Ast.ast> main
%%

main:
  expr EOL                      { $1 }
;

expr:
  | INT                         { Num $1 }
  | LPAREN sym expr_l RPAREN    { Call ($2, $3) }
  | MINUS INT %prec UMINUS      { Num (- $2) }
;

expr_l:
  | expr expr_l                 { $1::$2 }
  | expr                        { [$1] }
;

sym:
  | PLUS                        { "+" }
  | MINUS                       { "-" }
  | TIMES                       { "*" }
  | DIV                         { "/" }
  | SYM                         { $1 }
;
