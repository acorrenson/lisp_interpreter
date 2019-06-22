(* ============================ *)
(* UTILS for AST manipulation   *)
(* ============================ *)

type sexpr =
  | Num of int
  | Sym of string
  | Pair of sexpr * sexpr
  | Nil

let ppast a =
  match a with
  | Num n -> print_endline ("- int : " ^ (string_of_int n))
  | Sym s -> print_endline ("- sym : " ^ s)
  | _ -> print_endline ("- list")


let rec list_of_sexpr s =
  match s with
  | Nil -> []
  | Pair (a, b) -> ppast a; a::(list_of_sexpr b)
  | _ -> failwith "this sexpr isn't a list"


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

let rec eval sxp =
  match sxp with
  | Num _ -> sxp
  | Sym _ -> sxp
  | Nil -> Nil
  | Pair (Sym s, body) ->
    psexpr (eval_list body); print_newline();
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
  | Pair (a, b) -> Pair (a, b)

and eval_list l =
  match l with
  | Num n -> Num n
  | Sym s -> Sym s
  | Pair (Sym _, _) -> eval l
  | Pair (s, Nil) -> Pair (eval s, Nil)
  | Pair (s1, s2) -> Pair (eval s1, eval_list s2)
  | Nil -> Nil

and divide l =
  match l with
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a / b)
  | _ -> failwith "operand error"

and add l =
  match l with
  | Pair (Num a, Nil) -> Num a
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a + b)
  | Pair (Num a, Pair (Num b, s)) -> add (Pair ( Num a, add (Pair (Num b, s))))
  | Pair (Num a, Num b) -> Num (a + b)
  | _ -> failwith "operand error"

and mult l =
  match l with
  | Pair (_, Nil) -> failwith "single operand for mult forbiden"
  | Pair (Num a, Pair (Num b, Nil)) -> Num (a * b)
  | Pair (Num a, Pair (Num b, s)) -> mult (Pair ( Num a, mult (Pair (Num b, s))))
  | Pair (Num a, Num b) -> Num (a * b)
  | _ -> failwith "operand error"

and sub l =
  match l with
  | Pair (Num a, Nil) -> Num (- a)
  | Pair (Num a, r) ->
    List.fold_left (
      fun a b -> match a, b with
      | Num a, Num b  -> Num (a - b)
      | Num a, Nil    -> Num a
      | _ -> failwith "operand error") (Num a) (list_of_sexpr r)
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
      | _     -> failwith "Non boolean value"
    end
  | _ -> failwith "Incorrect body for cond"

(* ============================ *)
(* Printing                     *)
(* ============================ *)

