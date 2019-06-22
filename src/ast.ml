(* ============================ *)
(* UTILS for AST manipulation   *)
(* ============================ *)

type ast =
  | Num of int
  | Sym of string


(* ============================ *)
(* Primitives                   *)
(* ============================ *)

let divide l =
  match l with
  | [Num a; Num b] -> Num (a / b)
  | _ -> failwith "operand error"

let add l =
  match l with
  | [Num a; Num b] -> Num (a + b)
  | _ -> failwith "operand error"

let mult l =
  match l with
  | [Num a; Num b] -> Num (a * b)
  | _ -> failwith "operand error"

let sub l =
  match l with
  | [Num a; Num b] -> Num (a - b)
  | _ -> failwith "operand error"

let incr l =
  match l with
  | [Num a] -> Num (a + 1)
  | _ -> failwith "operand error"


let decr l =
  match l with
  | [Num a] -> Num (a - 1)
  | _ -> failwith "operand error"


(* ============================ *)
(* Print and apply              *)
(* ============================ *)


let apply s l =
  match s with
  | "/" -> divide l
  | "+" -> add l
  | "-" -> sub l
  | "*" -> mult l
  | "-1+" -> decr l
  | "1+" -> incr l
  | "define" -> print_endline "define operation"; Num 0
  | _ -> failwith "Unknown operation"

let ppast a =
  match a with
  | Num n -> print_endline ("- int : " ^ (string_of_int n))
  | Sym s -> print_endline ("- sym : " ^ s)