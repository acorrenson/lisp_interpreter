open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let r = Parser.main Lexer.token lexbuf in
        ppast r; flush stdout
    done
  with Lexer.Eof ->
    exit 0