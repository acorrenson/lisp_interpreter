(* ============================ *)
(* UTILS for AST manipulation   *)
(* ============================ *)

type sexpr =
  | Num of int
  | Sym of string
  | Pair of sexpr * sexpr
  | Nil

(* ============================ *)
(* Printing                     *)
(* ============================ *)

let rec ppast a =
  match a with
  | Num n -> print_endline ("- int : " ^ (string_of_int n))
  | Sym s -> print_endline ("- sym : " ^ s)
  | _ -> print_endline ("- list : ( " ^ (string_of_list a) ^ ")")
and string_of_list a =
  match a with
  | Pair (Sym s, t) -> s ^ " " ^ (string_of_list t)
  | Pair (Num s, t) -> (string_of_int s) ^ " " ^ (string_of_list t)
  | Nil -> ""
  | _ -> "..."

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


(* ============================ *)
(* utils                        *)
(* ============================ *)

let rec list_of_sexpr s =
  match s with
  | Nil -> []
  | Pair (a, b) -> a::(list_of_sexpr b)
  | _ -> failwith "this sexpr isn't a list"

let is_primitive s =
  match s with
  | "define"
  | "-"
  | "*"
  | "+"
  | "/"
  | "list" -> true
  | _ -> false

(* ============================ *)
(* Eval - Apply                 *)
(* ============================ *)

(* lookup for a value associated to a symbol in the current env *)
let lookup s env =
  try
    List.assoc s !env
  with Not_found -> failwith (s ^ " not defined")

(* bind a symbol to a value *)
let bind s v env = env := (s,v)::(!env)


let rec eval sxp env =
  match sxp with
  | Num _ -> sxp
  | Sym s -> lookup s env
  | Nil -> Nil
  | Pair (Sym s, body) ->
    begin
      match s with
      | "/"   -> divide (eval_list body env) (* env *)
      | "-"   -> sub    (eval_list body env) (* env *)
      | "+"   -> add    (eval_list body env) (* env *)
      | "*"   -> mult   (eval_list body env) (* env *)
      | "-1+" -> decr   (eval_list body env) (* env *)
      | "1+"  -> incr   (eval_list body env) (* env *)
      | "cond"  -> cond (eval_list body env) env
      | "list"  -> plist  (eval_list body env) (* env *)
      | "define" -> define body env
      | _ ->
        (try
          eval (lookup s env) env
        with e -> raise e)
    end
  | _ -> failwith "syntax error"

and eval_list l env =
  match l with
  | Num n -> Num n
  | Sym s -> lookup s env
  | Pair (Sym s, _) when (is_primitive s) -> eval l env
  | Pair (s, Nil) -> Pair (eval s env, Nil)
  | Pair (s1, s2) -> Pair (eval s1 env, eval_list s2 env)
  | Nil -> Nil

(* ============================ *)
(* Primitives                   *)
(* ============================ *)

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

and cond l env =
  match l with
  | Pair (pred, Pair (sxp1, Pair (sxp2, Nil))) ->
    begin
      match (eval pred env) with
      | Num 0 -> eval sxp2 env
      | Num 1 -> eval sxp1 env
      | _     -> failwith "Non boolean value"
    end
  | _ -> failwith "Incorrect body for cond"

and plist l = l
and define l e =
  try
    match l with
     | Pair (Sym s, Pair (v, Nil)) -> e := (s, v)::(!e); Nil
     | _ -> failwith "define error"
  with _ -> Nil