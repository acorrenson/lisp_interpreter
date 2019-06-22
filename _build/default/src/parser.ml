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
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\002\000\008\000\000\000\005\000\
\000\000\000\000\001\000\007\000\004\000"

let yydgoto = "\002\000\
\006\000\009\000\010\000"

let yysindex = "\005\000\
\002\255\000\000\255\254\000\000\000\000\000\000\003\255\000\000\
\002\255\006\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\010\000\003\000"

let yytablesize = 12
let yytable = "\003\000\
\008\000\004\000\003\000\005\000\004\000\001\000\005\000\013\000\
\011\000\006\000\007\000\012\000"

let yycheck = "\001\001\
\002\001\003\001\001\001\005\001\003\001\001\000\005\001\002\001\
\006\001\002\001\001\000\009\000"

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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sexpr) in
    Obj.repr(
# 16 "src/parser.mly"
                    ( _1 )
# 79 "src/parser.ml"
               : Ast.sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "src/parser.mly"
                    ( Num _1 )
# 86 "src/parser.ml"
               : 'sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 21 "src/parser.mly"
                    ( Sym _1 )
# 93 "src/parser.ml"
               : 'sexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 23 "src/parser.mly"
                    ( _2 )
# 100 "src/parser.ml"
               : 'sexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 24 "src/parser.mly"
                    ( Nil )
# 106 "src/parser.ml"
               : 'sexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexpr) in
    Obj.repr(
# 27 "src/parser.mly"
                    ( Pair (_1, Nil) )
# 113 "src/parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 28 "src/parser.mly"
                    ( Pair (_1, _2) )
# 121 "src/parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.sexpr)
