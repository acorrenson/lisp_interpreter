open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      print_string "λ "; flush stdout;
      let result = Parser.main Lexer.token lexbuf in
        (* pp_ast result; print_newline(); flush stdout;  *)
        print_int (eval result []); print_newline();
    done
  with Lexer.Eof ->
    exit 0