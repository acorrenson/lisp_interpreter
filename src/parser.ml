
open Lexer

type ast =
  (* Function call *)
  | Call of string * ast list
  | List of ast list
  | Num of int
  | String of string
  | Var of string
  | End

type lstream = {stream : lexeme list; mutable pos : int; len : int }

let l_fwd lst = lst.pos <- lst.pos + 1
let l_bwd lst = lst.pos <- lst.pos - 1

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


let rec p_list print_f l =
  match l with
  | [] -> print_newline ()
  | a::tail -> print_f a; print_char ' '; p_list print_f tail


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
  in
  match lex pks with
  | Lend        -> failwith "unexpected EOF"
  | Lop_par     ->
    ( match lex pks with
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
  let pks = fill_pks "test.lisp" in
  pp_ast (parser pks)