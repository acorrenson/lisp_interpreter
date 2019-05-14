
type ast =
  (* Function call *)
  | Plus  of ast * ast
  | Minus of ast * ast
  | Div   of ast * ast
  | Times of ast * ast
  | Num of int


let pp_ast ast =
  let rec prec ast i =
    match ast with
    | Num n         -> print_endline ((String.make (2*i) ' ')^(string_of_int n))
    | Plus (a, b)   -> print_endline ((String.make (2*i) ' ')^"+"); prec a (i+1); prec b (i+1)
    | Minus (a, b)  -> print_endline ((String.make (2*i) ' ')^"-"); prec a (i+1); prec b (i+1)
    | Times (a, b)  -> print_endline ((String.make (2*i) ' ')^"*"); prec a (i+1); prec b (i+1)
    | Div (a, b)    -> print_endline ((String.make (2*i) ' ')^"/"); prec a (i+1); prec b (i+1)
  in
  prec ast 0


let rec eval t =
  match t with
  | Plus  (a, b) -> (eval a) + (eval b)
  | Minus (a, b) -> (eval a) - (eval b)
  | Div   (a, b) -> (eval a) / (eval b)
  | Times (a, b) -> (eval a) * (eval b)
  | Num n -> n