


type token = 
  | Op_par (* opening parenthesis *)
  | Cl_par (* closing parenthesis *)
  | Symbol of string
  | Number of int



let lex f =
  let rec lex_r ic current tokens =
    try let c = input_char ic in
      match c with
      | '(' -> print_endline "Parenthese"; lex_r ic "" (Op_par::tokens)
      | ')' -> print_endline "Parenthese"; lex_r ic "" (Cl_par::tokens)
      | 'a'..'z' | 'A'..'Z' -> lex_symbol ic (String.make 1 c)
      | _ -> print_endline "Token idk"; lex_r ic "" ("b"::tokens)

    with End_of_file -> tokens

  in
  
  let ic = open_in f in
  lex_r ic "" []
  

let _ =
  lex "test.lisp"
