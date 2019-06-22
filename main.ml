open Ast

let _ =
  try
    let p = Pair (Sym "x", Pair (Num 2, Nil)) in
    psexpr (eval_list p (ref [("x", Num 1)]));
    flush stdout;
    print_endline "";
    let lexbuf = Lexing.from_channel (open_in "./test.lisp") in
    let env = ref [] in
    while true do
      let r = Parser.main Lexer.token lexbuf in
        ppast (eval r env); print_newline (); flush stdout
    done
  with Lexer.Eof ->
    exit 0