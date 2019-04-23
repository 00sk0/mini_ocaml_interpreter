{
  open Lexing
  open Parse
  exception SyntaxError of string
}

let number_pos = ['0'-'9']+
let number_neg = '(' '-' ['0'-'9']+ ')'
let digit  = ['0'-'9']
let string = '\"' ([^ '\\' '\"'] | '\\'_ )* '\"'
let var = ['a'-'z']['a'-'z' '0'-'9' '_']*
let white  = (' ' | '\n' | '\t')*


rule read = parse
| white       {read lexbuf}
| "(*"        {comment lexbuf}
| '+'         {PLUS}
| '-'         {MINUS}
| '='         {EQUAL}
| '*'         {ASTERISK}
| '/'         {SLASH}
| ';'         {SEMICOL}
| '<'         {LT}
| "let"       {LET}
| "in"        {IN}
| "fun"       {FUN}
| "rec"       {REC}
| "if" {IF} | "then" {THEN} | "else" {ELSE}
| "true" {TRUE} | "false" {FALSE}
| "->"        {RARROW}
| number_pos as n {NUMBER (int_of_string n)}
| number_neg as n {NUMBER ~-(int_of_string @@ String.sub n 2 (String.length n - 3))}
| '('         {LEFT_PAREN}
| ')'         {RIGHT_PAREN}
| var as s    {VARIABLE s}
| _           {raise @@ SyntaxError ("unexpected: " ^ Lexing.lexeme lexbuf)}
| eof         {EOF}
and comment = parse
| "*)"        {read lexbuf}
| _           {comment lexbuf}

