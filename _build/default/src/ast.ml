(* ============================ *)
(* UTILS for AST manipulation   *)
(* ============================ *)

type sexpr =
  | Num of int
  | Sym of string
  | Pair of sexpr * sexpr
  | Nil

let rec eval sxp =
  match sxp with
  | Num _ -> sxp
  | Sym _ -> sxp
  | Pair (Sym s, body) ->
    begin
      match s with
      | "/"   -> divide (eval_list body) (* env *)
      | "-"   -> sub    (eval_list body) (* env *)
      | "+"   -> add    (eval_list body) (* env *)
      | "*"   -> mult   (eval_list body) (* env *)
      | "-1+" -> decr   (eval_list body) (* env *)
      | "1+"  -> incr   (eval_list body) (* env *)
      | "cond"  -> cond   (eval_list body) (* env *)
      | _ -> failwith "Unknown operation"
    end
  | _ -> failwith "Syntax error"

and eval_list l =
  match l with
  | Num n -> Num n
  | Sym s -> Sym s
  | Pair (s, Nil) -> Pair (eval_list s, Nil)
  | Pair (s1, s2) -> Pair (eval_list s1, eval_list s2)
  | _ -> failwith "Not a list"

and divide l =
  match l with
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a / b)
  | _ -> failwith "operand error"

and add l =
  match l with
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a + b)
  | Pair (a, b) -> add (Pair (eval a, eval b))
  | _ -> failwith "operand error"

and mult l =
  match l with
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a * b)
  | Pair (_, Nil) -> failwith "single operand for mult forbiden"
  | Pair (Num a, s) -> mult (Pair (Num a, mult s))
  | _ -> failwith "operand error"

and sub l =
  match l with
  | Pair (Num a, Nil) -> Num (- a)
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a - b)
  | Pair (Num a, b) -> sub (Pair (Num a, sub b))
  | _ -> failwith "operand error"

and incr l =
  match l with
  | Pair (Num a, Nil) -> Num (a + 1)
  | _ -> failwith "operand error"

and decr l =
  match l with
  | Pair (Num a, Nil) -> Num (a - 1)
  | _ -> failwith "operand error"

and cond l =
  match l with
  | Pair (pred, Pair (sxp1, Pair (sxp2, Nil))) ->
    begin
      match (eval pred) with
      | Num 0 -> eval sxp2
      | Num 1 -> eval sxp1
      | _     -> failwith "NOn boolean value"
    end
  | _ -> failwith "Incorrect body for cond"

(* ============================ *)
(* Printing                     *)
(* ============================ *)

let ppast a =
  match a with
  | Num n -> print_endline ("- int : " ^ (string_of_int n))
  | Sym s -> print_endline ("- sym : " ^ s)
  | _ -> print_endline ("- list")

let rec psexpr s =
  match s with
  | Pair (s1, s2) ->
    print_string "("; 
    psexpr s1;
    print_string " . ";
    psexpr s2;
    print_string ")"
  | Sym s -> print_string s
  | Num n -> print_int n
  | Nil -> print_string "Nil"