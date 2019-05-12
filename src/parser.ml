
open Lexer

type op = Plus | Minus | Times | Divide

let get_op c =
  match c with
  | '-' -> Minus
  | '+' -> Plus
  | '*' -> Times
  | '/' -> Divide
  | _ -> failwith ("unkown operator :"^(String.make 1 c))


type ast =
  (* Arithmetic expression *)
  | Expr of op * ast * ast
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
    | Expr (s, a, b) -> print_endline ((String.make i ' ')^"op"); prec a (i+1); prec b (i+1)
    | List l -> print_endline "List"; pp_list l
  in
  prec ast 0


let rec eval_arit a =
  match a with
  | Num n -> n
  | Expr (a, b, c) -> (
    match a with
    | Plus -> (eval_arit b) + (eval_arit c)
    | Times -> (eval_arit b) * (eval_arit c)
    | Minus -> (eval_arit b) - (eval_arit c)
    | Divide -> (eval_arit b) / (eval_arit c))
  | _ -> failwith "not an arithmetic expression"


let rec p_list l =
  match l with
  | [] -> print_newline ()
  | a::tail -> print_int a; print_char ' '; p_list tail


let append v l =
  List.rev (v::List.rev l)

let rec parser pks =
  let rec get_params pks p =
    match lex pks with
    | Lop_par   -> bwd pks; get_params pks ((parser pks)::p)
    | Lsymbol s -> get_params pks ((Var s)::p)
    | Lcl_par   -> p
    | Lnumber n -> get_params pks ((Num n)::p)
    | Lstring s -> get_params pks ((String s)::p)
    | Lend      -> failwith "reached EOF, Expected ) "
    | _         -> failwith "Expected expression"
  in
  match lex pks with
  | Lend        -> failwith "unexpected EOF"
  | Lop_par     ->
    ( match lex pks with
      | Lop c          ->
        Call (String.make 1 c, List.rev (get_params pks []))
      | Lsymbol "list" ->
        List (List.rev (get_params pks []))
      | Lsymbol s      ->
        Call (s, List.rev (get_params pks []))
      | _ ->
        failwith "Expected function id or arithmetic operator")
  | Lnumber n -> Num n
  | Lstring s -> String s
  | _ as t -> print_string "IDK "; pp_lexeme t; End


let _ =
  (* let a = eval_arit (Expr (Plus, (Expr (Times, Num 2, Num 3)), Num 1)) in *)
  let pks = fill_pks "test.lisp" in
  (* lex_all pks; *)
  (* print_int a; *)
  (* print_endline ""; *)
  (* pks.pos <- 0; *)
  pp_ast (parser pks)