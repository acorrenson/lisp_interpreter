%{
  (* caml code *)

%}

%token <int> INT
%token <string> SYM
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%nonassoc UMINUS

%start main
%type <string> main
%%

main:
  expr EOL                      { $1 }
;

expr:
  | INT                         { string_of_int $1 }
  | LPAREN sym expr_l RPAREN    { "("^$2^$3^")" }
  | MINUS INT %prec UMINUS      { "-"^(string_of_int $2) }
;

expr_l:
  | expr expr_l                 { $1^$2 }
  | expr                        { $1 }
;

sym:
  | PLUS                        { "+" }
  | MINUS                       { "-" }
  | TIMES                       { "*" }
  | DIV                         { "/" }
  | SYM                         { $1 }
;
