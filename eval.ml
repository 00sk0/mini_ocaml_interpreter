module Frm = Map.Make(String)

exception VariableNotFound of string
exception TypeError
let sprintf = Printf.sprintf

type exp =
| LInt of int
| LBool of bool
| Var of string
| Fun of (string * typ * exp)
| App of (exp * exp)
| LOpAdd of (exp * exp)
| LOpMul of (exp * exp)
| IF of (exp * exp * exp)
| Equal of (exp * exp)
and value =
| VInt of int
| VBool of bool
| VProc of vproc
| VProcRec of vproc_rec
and ret = value
and env = {
  venv: ret Frm.t;
  tenv: typ Frm.t
} and vproc = {
  var: string; env: env;
  cont: exp }
and vproc_rec = {
  rvar: string; renv: env; rname: string;
  rcont: exp }
and typ =
| TInt | TBool | TArrow of (typ * typ)

module Env = struct
  type t = env
  let empty = {
    venv=Frm.empty; tenv=Frm.empty }
  let add key v t {venv;tenv} = {
    venv=Frm.add key v venv;
    tenv=Frm.add key t tenv }
  let fold f {venv;tenv} acc =
    Frm.fold (fun key v acc ->
      f key v (Frm.find key tenv) acc) venv acc
end

let rec string_of_exp = function
| LInt n -> string_of_int n
| LBool b -> string_of_bool b
| Var x -> x
(* | Let (x,v,body) -> sprintf "let %s = %s in %s" x (string_of_exp v) (string_of_exp body)
| LetRec (f,x,v,body) -> sprintf "let rec %s %s = %s in %s" f x (string_of_exp v) (string_of_exp body)
 *)
| Fun (var,typ,cont) -> sprintf "fun (%s:%s) -> %s" var (string_of_typ typ) (string_of_exp cont)
| App (e1,e2) -> sprintf "((%s) (%s))" (string_of_exp e1) (string_of_exp e2)
| LOpAdd (e1,e2) -> sprintf "%s + %s" (string_of_exp e1) (string_of_exp e2)
| LOpMul (e1,e2) -> sprintf "%s * %s" (string_of_exp e1) (string_of_exp e2)
| IF (cond,csq,alt) -> sprintf "if %s then %s else %s" (string_of_exp cond) (string_of_exp csq) (string_of_exp alt)
| Equal (e1,e2) -> sprintf "%s = %s" (string_of_exp e1) (string_of_exp e2)
and string_of_env env =
  sprintf "{%s}" @@ Env.fold (fun k v t acc ->
    acc ^ k ^ "=" ^ (string_of_value v) ^ ":" ^ (string_of_typ t) ^ ";") env ""
and string_of_value = function
| VInt n -> string_of_int n
| VBool b -> string_of_bool b
| VProc {var;env;cont} -> sprintf "fun%s %s -> %s" (string_of_env env) var (string_of_exp cont)
| VProcRec {rvar;renv;rcont;rname} ->
  sprintf "fun_rec(%s)[%s] %s -> %s"
    rname (string_of_env renv)
    rvar (string_of_exp rcont)
and string_of_typ = function
| TInt -> "int"
| TBool -> "bool"
| TArrow (u,v) -> sprintf "%s -> %s" (string_of_typ u) (string_of_typ v)

let string_ast_of_exp =
  let rec lp exp = match exp with
  | LInt _ | LBool _ | Var _ -> string_of_exp exp
  (* | Let (x,v,body) -> sprintf "Let(%s,%s,%s)" x (lp v) (lp body)
  | LetRec (f,x,v,body) -> sprintf "LetRec(%s,%s,%s,%s)" f x (lp v) (lp body) *)
  | Fun (var,typ,cont) -> sprintf "Fun(%s,%s,%s)" var (string_of_typ typ) (lp cont)
  | App (e1,e2) -> sprintf "App(%s,%s)" (lp e1) (lp e2)
  | LOpAdd (e1,e2) -> sprintf "Add(%s,%s)" (lp e1) (lp e2)
  | LOpMul (e1,e2) -> sprintf "Mul(%s,%s)" (lp e1) (lp e2)
  | IF (cond,csq,alt) -> sprintf "IF(%s,%s,%s)" (lp cond) (lp csq) (lp alt)
  | Equal (e1,e2) -> sprintf "%s = %s" (lp e1) (lp e2)
  in lp

let rec typechk exp env = match exp with
| LInt _ -> TInt
| LBool _ -> TBool
| Var x -> Frm.find x env.tenv
| Fun (x,tx,e) ->
  (* let tx = Frm.find x env.tenv in *)
  let te = typechk e env in
  TArrow (tx,te)
| App (e1,e2) ->
  let t1 = typechk e1 env in
  let t2 = typechk e2 env in (
    match t1 with
    | TArrow (u,v) when u=t2 -> v
    | _ -> raise TypeError )
| LOpAdd (e1,e2) | LOpMul (e1,e2) -> (
  match typechk e1 env,typechk e2 env with
  | TInt,TInt -> TInt | _ -> raise TypeError)
| IF (e1,e2,e3) ->
  let t,s = typechk e2 env,typechk e3 env in
  if typechk e1 env = TBool && t=s then t
  else raise TypeError
| Equal (e1,e2) ->
  let t,s = typechk e1 env,typechk e2 env in
  if t=s then TBool else raise TypeError

let rec eval exp (env:env) = match exp with
| LInt v -> VInt v
| LBool b -> VBool b
| Var x -> Frm.find x env.venv
(* | Let (x,v,body) ->
  let env' = Frm.add x (eval v env) env in
  eval body env'
| LetRec (f,x,v,body) ->
  let env' = Frm.add f (VProcRec {rname=f; rvar=x; rcont=v; renv=env}) env in
  eval body env' *)
| Fun (var,typ,cont) -> VProc {var; cont; env}
| App (e1,e2) -> (
  let ftyp  = typechk e1 env in
  let vtyp  = typechk e2 env in
  let func  = eval e1 env in
  let value = eval e2 env in
  match func with
  | VProc {var;cont;env} ->
    eval  cont @@ Env.add var value vtyp env
  | VProcRec {rvar; rcont; renv; rname} ->
    eval rcont @@ Env.add rvar value vtyp @@ Env.add rname func ftyp renv
  | e -> failwith @@ sprintf "not a function: %s" (string_of_value e))
| LOpAdd (e1,e2) -> (match eval e1 env, eval e2 env with
  | VInt u,VInt v -> VInt (u+v)
  | _ -> raise TypeError)
| LOpMul (e1,e2) -> (match eval e1 env, eval e2 env with
  | VInt u,VInt v -> VInt (u*v)
  | _ -> raise TypeError)
| IF (cond, csq, alt) -> (match eval cond env with
  | VBool true  -> eval csq env
  | VBool false -> eval alt env
  | _ -> raise TypeError)
| Equal (e1,e2) -> (match eval e1 env,eval e2 env with
  | VInt  u,VInt  v -> VBool (u=v)
  | VBool u,VBool v -> VBool (u=v)
  | _ -> raise TypeError)

let interpret ls0 =
  List.iter (fun ls ->
    List.iter (fun exp ->
      Printf.(
        eprintf "%s\n%!" @@ string_ast_of_exp exp;
        eprintf "t: %s\n%!" @@ string_of_typ   @@ typechk exp Env.empty;
        eprintf "-> %s\n%!" @@ string_of_value @@ eval exp Env.empty)
    ) ls) ls0








