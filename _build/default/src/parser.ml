type token =
  | LPAREN
  | RPAREN
  | SYM of (string)
  | PRIM of (string)
  | NUM of (int)
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "src/parser.mly"
  open Ast
# 14 "src/parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  262 (* EOL *);
    0|]

let yytransl_block = [|
  259 (* SYM *);
  260 (* PRIM *);
  261 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\002\000\008\000\000\000\004\000\
\000\000\001\000\000\000\000\000\007\000\005\000"

let yydgoto = "\002\000\
\006\000\011\000\008\000\012\000"

let yysindex = "\002\000\
\255\254\000\000\253\254\000\000\000\000\000\000\000\255\000\000\
\255\254\000\000\255\254\003\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\007\000\000\000\254\255"

let yytablesize = 9
let yytable = "\003\000\
\009\000\004\000\001\000\005\000\014\000\010\000\006\000\007\000\
\013\000"

let yycheck = "\001\001\
\004\001\003\001\001\000\005\001\002\001\006\001\002\001\001\000\
\011\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  SYM\000\
  PRIM\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 16 "src/parser.mly"
                  ( _1 )
# 79 "src/parser.ml"
               : Ast.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "src/parser.mly"
                    ( Num _1 )
# 86 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 21 "src/parser.mly"
                    ( Sym _1 )
# 93 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'application) in
    Obj.repr(
# 22 "src/parser.mly"
                    ( _1 )
# 100 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 25 "src/parser.mly"
                          ( apply _2 _3 )
# 108 "src/parser.ml"
               : 'application))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 28 "src/parser.mly"
                    ( [_1]   )
# 115 "src/parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 29 "src/parser.mly"
                    ( _1::_2 )
# 123 "src/parser.ml"
               : 'args))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.ast)
