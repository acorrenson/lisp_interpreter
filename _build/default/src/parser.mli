type token =
  | LPAREN
  | RPAREN
  | SYM of (string)
  | PRIM of (string)
  | NUM of (int)
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast
