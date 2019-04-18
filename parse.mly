%token <int> NUMBER
%token <string> VARIABLE
%token LEFT_PAREN RIGHT_PAREN EOF
%token PLUS LET EQUAL IN FUN RARROW REC IF THEN ELSE TRUE FALSE ASTERISK SEMICOL

/* low priority */
%nonassoc RARROW IN ELSE
/* %left SEMICOL */
%left EQUAL
%left PLUS
%left ASTERISK
%left VARIABLE NUMBER TRUE FALSE LEFT_PAREN
/* high priority */

%start <Eval.exp list option> prog

%%

prog:
  | EOF           {None}
  | p=prog0 EOF   {Some p}
;
prog0:
  | e=exp                 {[e]}
  | e=exp SEMICOL SEMICOL {[e]}
  | e=exp SEMICOL SEMICOL ls=prog0  {e::ls}
;
arg_exp:
  | x=VARIABLE  {Eval.Var  x}
  | n=NUMBER    {Eval.LInt n}
  | TRUE        {Eval.LBool true}
  | FALSE       {Eval.LBool false}
  | LEFT_PAREN exp=exp RIGHT_PAREN  {exp}
;
exp:
  | e=arg_exp   {e}
  | func=exp arg=arg_exp        {Eval.App (func,arg)}
  | e1=exp PLUS e2=exp  {Eval.LOpAdd (e1, e2)}
  | e1=exp ASTERISK e2=exp  {Eval.LOpMul (e1, e2)}
  | e1=exp EQUAL e2=exp         {Eval.Equal (e1, e2)}
  | FUN x=VARIABLE RARROW body=exp  {Eval.Fun (x,body)}
  | LET x=VARIABLE EQUAL v=exp IN body=exp  {Eval.Let (x,v,body)}
  | LET REC f=VARIABLE x=VARIABLE EQUAL v=exp IN body=exp {Eval.LetRec (f,x,v,body)}
  | IF cond=exp THEN csq=exp ELSE alt=exp {Eval.IF (cond,csq,alt)}
;

