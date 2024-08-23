{
  open Preparser
  open Lexing
}

let core = ['_' 'a'-'z' 'A'-'Z' '+' '-' '*' '/' '@' '&' '%' '#' '>' '<' ',' '!' '~' '?' '^' '.' '\'']
let non_fst = ['[' ']' '|' '=' '0'-'9']
let allowed = core | non_fst
let var = core (allowed | ':')* allowed*

rule token = parse
    "--" [^'\n']* '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']          { token lexbuf }
  | '-'?['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "$fst"              { FST }
  | "$cond"             { COND }
  | "$cons"             { CONS }
  | "$pair"             { PAIR }
  | "$mod"              { MOD }
  | "$mul"              { TIMES }
  | "$add"              { PLUS }
  | "$sub"              { MINUS } 
  | "$div"              { DIVIDE }
  | "$eq"               { EQUALS }
  | "$lt"               { LESS }
  | "$snd"              { SND }
  | "fun"               { FUN }
  | "int"               { TINT }
  | "is"                { IS }
  | "let"               { LET }  
  | "mixfix"            { MIXFIX }
  | "mixfixl"           { MIXFIXL }
  | "mixfixr"           { MIXFIXR }
  | "bool"              { TBOOL }
  | "false"             { FALSE }
  | "list"              { TLIST }
  | "match"             { MATCH }
  | "rec"               { REC }
  | "true"              { TRUE }
  | ":q""uit"?          { CMD_QUIT }
  | ":op""erators"?     { CMD_OPERATORS }
  | "with"              { WITH }
  | "->"                { TARROW }
  | "=>"                { DARROW }
  | ";;"                { SEMICOLON2 }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ':'                 { COLON }
  | '*'                 { STAR }
  | '='                 { SET_EQUAL }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '|'                 { ALTERNATIVE }
  | var                 { VAR (lexeme lexbuf) }
  | eof                 { EOF }

{
}
