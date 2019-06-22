{
  open Parser
  exception Eof
}

let symbol = "+" | "-" | "-1+" | "1+" | "/" | "*" | ['a'-'z' 'A'-'Z']+

rule token = parse
  | symbol as s                   { SYM s }
  | ['0'-'9']+ as n               { NUM (int_of_string n) }
  | [' ' '\t']                    { token lexbuf }
  | ['\n']                        { EOL }
  | ['(']                         { LPAREN }
  | [')']                         { RPAREN }
  | eof                           { raise Eof }
  | _  as s                       { failwith (String.make 1 s) }
