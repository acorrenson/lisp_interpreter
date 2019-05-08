(* ------------------------------- *)
(* An educationnal lexer           *)
(* for Common-lisp                 *)
(* ------------------------------- *)

(* Lexeme Type definition *)
type lexeme = 
  | Lend
  | Lop_par (* opening parenthesis *)
  | Lcl_par (* closing parenthesis *)
  | Lsymbol of string
  | Lstring of string
  | Lnumber of int
  | Lop of char

(* Print a lexeme *)
let pp_lexeme l =
  match l with
  | Lend -> print_endline "End"
  | Lop_par -> print_endline "("
  | Lcl_par -> print_endline ")"
  | Lsymbol s -> print_endline s
  | Lstring s -> print_endline s
  | Lnumber n -> print_endline (string_of_int n)
  | Lop c -> print_endline (String.make 1 c)

(* Custom peakable string *)
type peakable_string = {string : string; mutable pos : int; len : int}

(* Functions to manipulate peakable strings *)
let init_pks s =  {string = s; pos = 0; len = String.length s}
let fwd pks = pks.pos <- pks.pos + 1

(* Build a peakable string from a file *)
let fill_pks f =
  let rec getc ic s =
    try
      let nc = input_char ic in
      getc ic (s ^ (String.make 1 nc))
    with End_of_file -> s
  in
  let ic = open_in f in
  let s = getc ic "" in
  init_pks s

(* Jump to next end_of_line *)
let rec find_eol pks =
  if pks.pos < pks.len then
    match pks.string.[pks.pos] with
    | '\n' -> ()
    | _ -> fwd pks; find_eol pks
  else ()

(* extract a value mathing the property "pred" *)
let extract pred pks =
  let st = pks.string and pos = pks.pos in
  let rec ext n = if n < pks.len && (pred st.[n]) then ext (n+1) else n in
  let res = ext pos
  in
  pks.pos <- res; String.sub (pks.string) pos (res-pos)


(* extract a symbol *)
let extract_symbol pks =
  let is_alpha c = match c with 
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
    | _ -> false
  in extract is_alpha pks

(* extract a string *)
let extract_string pks =
  let is_alpha c = match c with 
    | 'a'..'z' | 'A'..'Z' | '\''
    | '0'..'9' | '_' | ' ' -> true
    | _ -> false
  in 
  let s = extract is_alpha pks in
  fwd pks; s


(* extract a number *)
let extract_number pks =
  let is_number c = match c with 
    | '0'..'9' -> true
    | _ -> false
  in int_of_string (extract is_number pks)


(* Extract the next lexeme of a peakable string *)
let rec lex pks =
  let lex_char c =
      match c with
      | '(' ->
        fwd pks; Lop_par
      | ')' ->
        fwd pks; Lcl_par
      | 'a'..'z' | 'A'..'Z' | '_' ->
        Lsymbol (extract_symbol pks)
      | '0'..'9' ->
        Lnumber (extract_number pks)
      | '\t' | ' ' | '\r' | '\n' ->
        fwd pks; lex pks
      | '"' ->
        fwd pks; Lstring (extract_string pks)
      | ';' ->
        find_eol pks; lex pks
      | '+' | '-' | '*' | '%' ->
        fwd pks; Lop (c)
      | _ -> failwith ("Unknown symbol : "^(String.make 1 c))

  in
  
  if pks.pos >= pks.len then Lend
  else lex_char pks.string.[pks.pos]
  

(* Lex all the peakable string *)
let rec lex_all pks =
  match lex pks with
  | Lend -> pp_lexeme Lend
  | _ as l -> pp_lexeme l; lex_all pks

(* TEST *)
let _ =
  let pks = fill_pks "test.lisp" in
  lex_all pks
