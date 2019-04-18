{
  open Lexing
  open Parse
  exception SyntaxError of string
}

let number = '-'? ['0'-'9']+
let digit  = ['0'-'9']
let string = '\"' ([^ '\\' '\"'] | '\\'_ )* '\"'
let var = ['a'-'z']['a'-'z' '0'-'9' '_']*
let white  = (' ' | '\n' | '\t')*


rule read = parse
| white       {read lexbuf}
| '+'         {PLUS}
| '='         {EQUAL}
| '*'         {ASTERISK}
| ';'         {SEMICOL}
| "let"       {LET}
| "in"        {IN}
| "fun"       {FUN}
| "rec"       {REC}
| "if" {IF} | "then" {THEN} | "else" {ELSE}
| "true" {TRUE} | "false" {FALSE}
| "->"        {RARROW}
| number as n {NUMBER (int_of_string n)}
| '('         {LEFT_PAREN}
| ')'         {RIGHT_PAREN}
| var as s    {VARIABLE s}
| _           {raise @@ SyntaxError ("unexpected: " ^ Lexing.lexeme lexbuf)}
| eof         {EOF}

