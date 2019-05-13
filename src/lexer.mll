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
  | ['a'-'z']+ as s       { SYM s }
  | eof                   { raise Eof }
  | _ as c                { failwith ("Unknow token : "^(String.make 1 c))}