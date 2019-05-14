{
  open Parser
  exception Eof
}

rule token = parse
  | [' ' '\t']            { token lexbuf }
  | ['\n']                { EOL }
  | ['0'-'9']+ as n       { INT (int_of_string n) }
  | ['+']                 { PLUS }
  | ['-']                 { MINUS }
  | ['*']                 { TIMES }
  | ['/']                 { DIV }
  | ['(']                 { LPAREN }
  | [')']                 { RPAREN }
  | eof                   { raise Eof }
  | _ as c                { failwith ("Unknow token : "^(String.make 1 c))}