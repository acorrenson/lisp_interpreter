open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let r = Parser.main Lexer.token lexbuf in
        psexpr r; print_newline();
        ppast (eval r); flush stdout
    done
  with Lexer.Eof ->
    exit 0