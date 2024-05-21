{
  open Preparser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z' '0'-'9' '+' '-' '*' '/' '|' ',' '!' '&' '%' '#' ]+

rule token = parse
    "--" [^'\n']* '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']          { token lexbuf }
  | '-'?['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "bool"              { TBOOL }
  | "false"             { FALSE }
  | "fst"               { FST }
  | "fun"               { FUN }
  | "$cond"             { COND }
  | "int"               { TINT }
  | "is"                { IS }
  | "let"               { LET }  
  | "mixfix"            { MIXFIX }  
  | "left"              { LEFT }  
  | "right"             { RIGHT }  
  | "list"              { TLIST }
  | "match"             { MATCH }
  | "rec"               { REC }
  | "snd"               { SND }
  | "true"              { TRUE }
  | ":quit"             { QUIT }
  | "with"              { WITH }
  | "->"                { TARROW }
  | "=>"                { DARROW }
  | "$cons"             { CONS }
  | "$pair"             { PAIR }
  | ";;"                { SEMICOLON2 }
  | "$mod"              { MOD }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | "$mul"              { TIMES }
  | "$add"              { PLUS }
  | "$sub"              { MINUS } 
  | "$div"              { DIVIDE }
  | ':'                 { COLON }
  | '*'                 { STAR }
  | "$lt"               { LESS }
  | '='                 { SET_EQUAL }
  | "$eq"               { EQUALS }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '|'                 { ALTERNATIVE }
  | var                 { VAR (lexeme lexbuf) }
  | eof                 { EOF }

{
}
