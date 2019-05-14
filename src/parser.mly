%{
  open Ast
%}

%token <int> INT
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
  | INT                               { Num $1 }
  | MINUS INT %prec UMINUS            { Num (- $2) }
  | LPAREN PLUS   expr expr RPAREN    { Plus  ($3, $4) }
  | LPAREN MINUS  expr expr RPAREN    { Minus ($3, $4) }
  | LPAREN TIMES  expr expr RPAREN    { Times ($3, $4) }
  | LPAREN DIV    expr expr RPAREN    { Div   ($3, $4) }
;