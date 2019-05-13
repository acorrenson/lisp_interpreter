
type ast =
  (* Function call *)
  | Call of string * ast list
  | List of ast list
  | Num of int
  | String of string
  | Var of string
  | End

let pp_ast ast =
  let rec prec ast i =
    let rec pp_list l = 
      match l with
      | [] -> ()
      | a::tail -> prec a (i+1); pp_list tail
    in
    match ast with
    | End         -> print_endline "End"
    | String s    -> print_endline ((String.make (2*i) ' ')^s)
    | Var s       -> print_endline ((String.make (2*i) ' ')^s)
    | Num n       -> print_endline ((String.make (2*i) ' ')^(string_of_int n))
    | Call (s, l) -> print_endline ((String.make (2*i) ' ')^s); pp_list l
    | List l      -> print_endline "List"; pp_list l
  in
  prec ast 0


let rec eval t env =
  match t with
  | Call ("+", [a; b]) -> (eval a env) + (eval b env)
  | Call ("-", [a; b]) -> (eval a env) - (eval b env)
  | Call ("*", [a; b]) -> (eval a env) * (eval b env)
  | Call ("/", [a; b]) -> (eval a env) / (eval b env)
  | Num n -> n
  | _ -> 0