{
  open Parser
  exception Eof
}

let primitives = "+" | "-" | "-1+" | "1+" | "/" | "*" | "define"

rule token = parse
  | ['a'-'z' 'A'-'Z']+ as s       { SYM s }
  | ['0'-'9']+ as n               { NUM (int_of_string n) }
  | [' ' '\t']                    { token lexbuf }
  | ['\n']                        { EOL }
  | ['(']                         { LPAREN }
  | [')']                         { RPAREN }
  | primitives as p               { PRIM p }
  | eof                           { raise Eof }
  | _  as s                       { failwith (String.make 1 s) }
