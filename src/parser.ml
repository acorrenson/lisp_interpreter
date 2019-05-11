
open Lexer

type op = Plus | Minus | Times | Divide

type ast =
  (* Arithmetic expression *)
  | Expr of op * ast * ast
  (* Function call *)
  | Call of string * ast list
  | List of int list
  | Num of int
  | String of string


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


let _ =
  let a = eval_arit (Expr (Plus, (Expr (Times, Num 2, Num 3)), Num 1)) in
  print_int a;
  print_endline "";