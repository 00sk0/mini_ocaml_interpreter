exception VariableNotFound of string
exception TypeError
let sprintf = Printf.sprintf

module Frm = struct
  include Map.Make(String)
  let sprint printer frm =
    sprintf "[%s]" @@
      fold (fun k v acc ->
        acc ^ (sprintf "%s:%s;" k (printer v))
      ) frm ""
  let ( *> ) f g x = g (f x)
  let ( <* ) f g x = f (g x)
  let print printer frm =
    print_endline @@ sprint printer frm
end

type exp =
| LInt of int
| LBool of bool
| Var of string
| Fun of (string * exp)
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
| TVar of string

module Env = struct
  type t = env
  let empty = {
    venv=Frm.empty; tenv=Frm.empty }
  let add key v t {venv;tenv} = {
    venv=Frm.add key v venv;
    tenv=Frm.add key t tenv }
  let fold f {venv;tenv} acc =
    (* asserts #venv >= #tenv.
      #venv > #tenv when type-inferencing *)
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
| Fun (var,cont) -> sprintf "fun (%s) -> %s" var (string_of_exp cont)
| App (e1,e2) -> sprintf "((%s) (%s))" (string_of_exp e1) (string_of_exp e2)
| LOpAdd (e1,e2) -> sprintf "%s + %s" (string_of_exp e1) (string_of_exp e2)
| LOpMul (e1,e2) -> sprintf "%s * %s" (string_of_exp e1) (string_of_exp e2)
| IF (cond,csq,alt) -> sprintf "if %s then %s else %s" (string_of_exp cond) (string_of_exp csq) (string_of_exp alt)
| Equal (e1,e2) -> sprintf "%s = %s" (string_of_exp e1) (string_of_exp e2)
and string_of_env env =
  sprintf "{%s}" @@ Env.fold (fun k v t acc ->
    acc ^ k ^ "=" ^ (string_of_value v) ^ ":" ^ (string_of_typ t) ^ ";") env ""
and string_of_tenv {tenv;_} =
  sprintf "{%s}" @@ Frm.fold (fun k t acc ->
    acc ^ k ^ ":" ^ (string_of_typ t) ^ ";"
  ) tenv ""
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
| TArrow (TArrow _ as u, v) -> sprintf "(%s)->%s" (string_of_typ u) (string_of_typ v)
| TArrow (u,v) -> sprintf "%s->%s" (string_of_typ u) (string_of_typ v)
| TVar t -> t

let string_ast_of_exp =
  let rec lp exp = match exp with
  | LInt _ | LBool _ | Var _ -> string_of_exp exp
  (* | Let (x,v,body) -> sprintf "Let(%s,%s,%s)" x (lp v) (lp body)
  | LetRec (f,x,v,body) -> sprintf "LetRec(%s,%s,%s,%s)" f x (lp v) (lp body) *)
  | Fun (var,cont) -> sprintf "Fun(%s,%s)" var (lp cont)
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
| Fun (x,e) ->
  let tx = Frm.find x env.tenv in
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

exception UnifyError
let rec subst_ty t theta = (* simplify t using theta *)
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TArrow (t1,t2) -> TArrow (subst_ty t1 theta, subst_ty t2 theta)
  | TVar x -> (match Frm.find_opt x theta with
    | Some t -> t
    | None -> TVar x)
let rec subst_tfrm theta_new frm = (* rewrite frm *)
  Frm.map (fun t -> subst_ty t theta_new) frm
let compose_subst theta_new theta = (* rewrite + add what is only in theta_new *)
  let theta' = subst_tfrm theta_new theta in
  Frm.fold (fun x t tau ->
    match Frm.find_opt x theta with
    | Some _ -> tau
    | None -> Frm.add x t tau
  ) theta_new theta'
let rec occurs t1 t2 = (* t1 shouldn't occur in t2 *)
  if t1=t2 then true
  else match t2 with
  | TArrow (u,v) -> occurs t1 u || occurs t1 v
  | _ -> false
let unify ls =
  let of_ls = List.fold_left (fun acc (x,t) -> Frm.add x t acc) Frm.empty in
  let subst_eql theta =
    List.map (fun (t1,t2) -> subst_ty t1 theta, subst_ty t2 theta)
  in
  let rec solve ls theta = match ls with
  | [] -> theta
  | (t1,t2)::tl ->
    if t1=t2 then solve tl theta
    else (match t1,t2 with
    | TArrow (p,q), TArrow (r,s) ->
      solve ((p,r)::(q,s)::tl) theta
    | (TVar x as t1), t2 | t2, (TVar x as t1) ->
      if occurs t1 t2 then raise UnifyError
      else solve
        (subst_eql (of_ls [x,t2]) tl)
        (compose_subst (of_ls [x,t2]) theta)
    | _ -> raise UnifyError)
  in solve ls Frm.empty

let rec typeinf =
  let theta_def = Frm.empty in
  let count = ref @@ -1 in
  let new_tvar x =
    TVar (incr count; "'" ^ x ^ (string_of_int !count)) in
  fun exp env ->
  match exp with
  | LInt _  -> (env,TInt,theta_def)
  | LBool _ -> (env,TBool,theta_def)
  | Var x -> (match Frm.find_opt x env.tenv with
    | Some t1 -> (env,t1,theta_def)
    | None ->
      let t1 = new_tvar x in
      ({env with tenv=Frm.add x t1 env.tenv},t1,theta_def))
  | LOpAdd (e1,e2) | LOpMul (e1,e2) ->
    let env,t1,th1 = typeinf e1 env in
    let env,t2,th2 = typeinf e2 env in
    let t1' = subst_ty t1 th2 in (* simplify t1 using th2 *)
    let th3 = unify [t1',TInt; t2,TInt] in (* typecheck *)
    let env = {env with tenv=subst_tfrm th3 env.tenv} in (* rewrite tenv using th3 *)
    let th4 = compose_subst th3 (compose_subst th2 th1) (* compose th1,th2,th3 *)
    in (env,TInt,th4)
  | IF (cond,csq,alt) ->
    let env,t1,th1 = typeinf cond env in
    let th90 = unify [t1,TBool] in
    (* let t1' = subst_ty t1 th90 in *)
    let env = {env with tenv=subst_tfrm th90 env.tenv} in
    let env,t2,th2 = typeinf csq env in
    let env,t3,th3 = typeinf alt env in
    let th91 = unify [t2,t3] in
    let t2' = subst_ty t2 th91 in
    let env = {env with tenv=subst_tfrm th91 env.tenv} in
    let th = compose_subst th91 (compose_subst th90 (compose_subst th3 (compose_subst th2 th1))) in
    (env,t2',th)
  | Fun (x,e) ->
    let t = new_tvar x in
    let env = {env with tenv=Frm.add x t env.tenv} in (* define/undefine x in order to evaluate e *)
    let env,t1,th1 = typeinf e env in
    let t' = subst_ty t th1 in (* simplify t *)
    let env = {env with tenv=Frm.remove x env.tenv} in
    (env,TArrow(t',t1),th1)
  | App (e1,e2) ->
    let env,t1,th1 = typeinf e1 env in
    let env,t2,th2 = typeinf e2 env in
    let t = new_tvar "@" in
    let t1' = subst_ty t1 th2 in (* simplify t1 *)
    let th3 = unify [t1',TArrow(t2,t)] in (* exist t s.t. t1 ~ t2->t ? *)
    let t'  = subst_ty t th3 in (* specify t *)
    let env = {env with tenv=subst_tfrm th3 env.tenv} in
    let th4 = compose_subst th3 (compose_subst th2 th1) in
    (env,t',th4)
  | _ -> failwith "to be implemented"

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
| Fun (var,cont) -> VProc {var; cont; env}
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
  let string_of_typeinf_res exp =
    let env,typ,th = typeinf exp Env.empty in
    sprintf "  typ : %s
  th  : %s
  tenv : %s" (string_of_typ typ) (Frm.sprint string_of_typ th) (string_of_tenv env)
  in
  List.iter (fun ls ->
    List.iter (fun exp ->
      Printf.(
        printf "%s\n%!" @@ string_ast_of_exp exp;
        printf "%s\n%!" @@ string_of_typeinf_res exp;
        printf "-> %s\n%!" @@ string_of_value @@ eval exp Env.empty;
      )
    ) ls) ls0








