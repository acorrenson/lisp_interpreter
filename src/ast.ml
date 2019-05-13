
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
        | [] -> print_newline ()
        | a::tail -> prec a (i+1); pp_list tail
      in
      match ast with
      | End -> print_endline "End"
      | String s -> print_endline ((String.make i ' ')^s)
      | Var s -> print_endline ((String.make i ' ')^s)
      | Num n -> print_endline ((String.make i ' ')^(string_of_int n))
      | Call (s, l) -> print_endline ((String.make i ' ')^s); pp_list l
      | List l -> print_endline "List"; pp_list l
    in
    prec ast 0