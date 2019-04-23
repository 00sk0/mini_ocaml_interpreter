exception VariableNotFound of string
exception TypeError
external ident : 'a -> 'a = "%identity"
let sprintf = Printf.sprintf

module Frm = struct
  include Map.Make(String)
  let sprint printer frm =
    sprintf "[%s]" @@
      fold (fun k v acc ->
        acc ^ (sprintf "%s:%s;" k (printer v))
      ) frm ""
end

type exp =
| LInt of int
| LBool of bool
| Var of string
| LUnit
| Let of (string * exp * exp)
| LetGlobal of (string * exp)
| LetRec of (string * string * exp * exp)
| Fun of (string * exp)
| App of (exp * exp)
| LOpAdd of (exp * exp)
| LOpSub of (exp * exp)
| LOpMul of (exp * exp)
| LOpDiv of (exp * exp)
| If of (exp * exp * exp)
| Equal of (exp * exp)
| LessT of (exp * exp)
and value =
| VInt of int
| VBool of bool
| VProc of vproc
| VProcRec of vproc_rec
| VUnit
and ret = value * env
and env = {
  venv: value Frm.t;
  tenv: typ Frm.t
} and vproc = {
  var: string; env: env;
  cont: exp
} and vproc_rec = {
  rvar: string; renv: env; rname: string;
  rcont: exp
} and typ =
| TInt | TBool | TArrow of (typ * typ)
| TVar of string
| TUnit

module Env = struct
  type t = env
  let empty = {
    venv=Frm.empty; tenv=Frm.empty }
  let add key v t {venv;tenv} = {
    venv=Frm.add key v venv;
    tenv=Frm.add key t tenv }
  let fold f {venv;tenv} (acc:string) : string =
    (* Printf.eprintf "#venv=%d v. #tenv=%d\n%!"
      (Frm.cardinal venv)
      (Frm.cardinal tenv)
    ; *)
    let m = Frm.merge (fun key v t -> match v,t with
    | Some v, Some t -> Some (Some v, Some t)
    | Some v, None   -> Some (Some v, None)
    | None,   Some t -> Some (None,   Some t)
    | None,   None   -> None
    ) venv tenv in
    Frm.fold (fun key (ov,ot) acc ->
      f key ov ot acc
    ) m acc
end

let rec string_of_exp = function
| LInt n -> string_of_int n
| LBool b -> string_of_bool b
| Var x -> x
| LUnit -> "()"
| Let (x,v,body) -> sprintf "let %s = %s in %s" x (string_of_exp v) (string_of_exp body)
| LetGlobal (x,v) -> sprintf "let %s = %s" x (string_of_exp v)
| LetRec (f,x,v,body) -> sprintf "let rec %s %s = %s in %s" f x (string_of_exp v) (string_of_exp body)
| Fun (var,cont) -> sprintf "(fun %s -> %s)" var (string_of_exp cont)
| App (e1,e2) -> sprintf "%s (%s)" (string_of_exp e1) (string_of_exp e2)
| LOpAdd (e1,e2) -> sprintf "%s + %s" (string_of_exp e1) (string_of_exp e2)
| LOpSub (e1,e2) -> sprintf "%s - %s" (string_of_exp e1) (string_of_exp e2)
| LOpMul (e1,e2) -> sprintf "(%s) * (%s)" (string_of_exp e1) (string_of_exp e2)
| LOpDiv (e1,e2) -> sprintf "(%s) / (%s)" (string_of_exp e1) (string_of_exp e2)
| If (cond,csq,alt) -> sprintf "if %s then %s else %s" (string_of_exp cond) (string_of_exp csq) (string_of_exp alt)
| Equal (e1,e2) -> sprintf "%s = %s" (string_of_exp e1) (string_of_exp e2)
| LessT (e1,e2) -> sprintf "%s < %s" (string_of_exp e1) (string_of_exp e2)
and string_of_env env =
  let pv = function Some v -> string_of_value v | None -> "?" in
  let pt = function Some t -> string_of_typ   t | None -> "?" in
  sprintf "{%s}" @@ Env.fold (fun k v t acc ->
    acc ^ k ^ "=(" ^ (pv v) ^ "):(" ^ (pt t) ^ ");"
  ) env ""
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
| VUnit -> "()"
and string_of_typ = function
| TInt -> "int"
| TBool -> "bool"
| TArrow (TArrow _ as u, v) -> sprintf "(%s)->%s" (string_of_typ u) (string_of_typ v)
| TArrow (u,v) -> sprintf "%s->%s" (string_of_typ u) (string_of_typ v)
| TVar t -> t
| TUnit -> "unit"

let string_ast_of_exp =
  let rec lp exp = match exp with
  | LInt v -> sprintf "LInt(%s)" @@ string_of_int v
  | LBool v -> sprintf "LBool(%s)" @@ string_of_bool v
  | Var v -> sprintf "Var (%s)" @@ v
  | LUnit -> "LUnit"
  | Let (x,v,body) -> sprintf "Let(%s,%s,%s)" x (lp v) (lp body)
  | LetGlobal (x,v) -> sprintf "LetGlobal(%s,%s)" x (lp v)
  | LetRec (f,x,v,body) -> sprintf "LetRec(%s,%s,%s,%s)" f x (lp v) (lp body)
  | Fun (var,cont) -> sprintf "Fun(%s,%s)" var (lp cont)
  | App (e1,e2) -> sprintf "App(%s,%s)" (lp e1) (lp e2)
  | LOpAdd (e1,e2) -> sprintf "Add(%s,%s)" (lp e1) (lp e2)
  | LOpSub (e1,e2) -> sprintf "Sub(%s,%s)" (lp e1) (lp e2)
  | LOpMul (e1,e2) -> sprintf "Mul(%s,%s)" (lp e1) (lp e2)
  | LOpDiv (e1,e2) -> sprintf "Div(%s,%s)" (lp e1) (lp e2)
  | If (cond,csq,alt) -> sprintf "IF(%s,%s,%s)" (lp cond) (lp csq) (lp alt)
  | Equal (e1,e2) -> sprintf "Equal(%s,%s)" (lp e1) (lp e2)
  | LessT (e1,e2) -> sprintf "LessT(%s,%s)" (lp e1) (lp e2)
  in lp

exception UnifyError
let rec subst_ty t theta = (* substantiate t using theta *)
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TArrow (t1,t2) -> TArrow (subst_ty t1 theta, subst_ty t2 theta)
  | TVar x -> (match Frm.find_opt x theta with
    | Some t -> t
    | None -> TVar x)
  | TUnit -> TUnit
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

let count_tvar = ref @@ -1
let new_tvar x =
  TVar (incr count_tvar; "'" ^ x ^ (string_of_int !count_tvar))
let rec typeinf =
  let theta_def = Frm.empty in
  fun exp env ->
  match exp with
  | LInt _  -> (env,TInt,theta_def)
  | LBool _ -> (env,TBool,theta_def)
  | Var x -> (match Frm.find_opt x env.tenv with
    | Some t1 -> (env,t1,theta_def)
    | None ->
      failwith @@ "variable " ^ x ^ " not found")
  | LUnit -> (env,TUnit,theta_def)
  | LOpAdd (e1,e2) | LOpMul (e1,e2) | LOpSub (e1,e2) | LOpDiv (e1,e2) ->
    let env,t1,th1 = typeinf e1 env in
    let env,t2,th2 = typeinf e2 env in
    let t1' = subst_ty t1 th2 in (* simplify t1 using th2 *)
    let th3 = unify [t1',TInt; t2,TInt] in (* typecheck *)
    (* let env = {env with tenv=subst_tfrm th3 env.tenv} in (* rewrite tenv using th3 *) *)
    let th4 = compose_subst th3 (compose_subst th2 th1) (* compose th1,th2,th3 *)
    in (env,TInt,th4)
  | If (cond,csq,alt) ->
    let env,t1,th1 = typeinf cond env in
    let th90 = unify [t1,TBool] in
    let th1 = compose_subst th90 th1 in
    let env = {env with tenv=subst_tfrm th1 env.tenv} in
    let env,t2,th2 = typeinf csq env in
    let env,t3,th3 = typeinf alt env in
    let t2 = subst_ty t2 th3 in
    let th91 = unify [t2,t3] in
    let th = compose_subst th91 (compose_subst th90 (compose_subst th3 (compose_subst th2 th1))) in
    let t2' = subst_ty t2 th in
    let env = {env with tenv=subst_tfrm th env.tenv} in
    (env,t2',th)
  | Fun (x,e) ->
    let t = new_tvar x in
    let xt_prev = Frm.find_opt x env.tenv in
    let env = {env with tenv=Frm.add x t env.tenv} in (* define/undefine x in order to evaluate e *)
    let env,t1,th1 = typeinf e env in
    let t' = subst_ty t th1 in
    let env = {env with tenv=
      (match xt_prev with
      | Some xt -> Frm.add x xt
      | None -> ident
      ) @@ Frm.remove x env.tenv} in
    (env,TArrow(t',t1),th1)
  | App (e1,e2) ->
    let env,t1,th1 = typeinf e1 env in
    let env,t2,th2 = typeinf e2 env in
    let t = new_tvar "@" in
    let t1' = subst_ty t1 th2 in
    let th3 = unify [t1',TArrow(t2,t)] in (* exist t s.t. t1 = t2->t ? *)
    let t'  = subst_ty t th3 in
    let env = {env with tenv=subst_tfrm th3 env.tenv} in
    let th4 = compose_subst th3 (compose_subst th2 th1) in
    (env,t',th4)
  | Equal (e1,e2) | LessT (e1,e2) ->
    let env,t1,th1 = typeinf e1 env in
    let env,t2,th2 = typeinf e2 env in
    let t1' = subst_ty t1 th2 in
    let th3 = unify [t1',t2] in
    let th4 = compose_subst th3 (compose_subst th2 th1) in
    (env,TBool,th4)
  | Let (x,v,body) ->
    let env,t1,th1 = typeinf v env in
    let env = {env with tenv=Frm.add x t1 env.tenv} in
    let env,t2,th2 = typeinf body env in
    let env = {env with tenv=Frm.remove x env.tenv} in
    let th3 = compose_subst th2 th1 in
    (env,t2,th3)
  | LetGlobal (x,v) ->
    let env,t1,th1 = typeinf v env in
    let env = {env with tenv=Frm.add x t1 env.tenv} in
    (env,TUnit,th1)
  | LetRec (f,x,v,body) as lr ->
    let env,t,th = fst @@ typeinf_lrec lr env
    in env,t,th
and typeinf_lrec exp env = match exp with
  | LetRec (f,x,v,body) ->
    let tx_prev = Frm.find_opt x env.tenv in
    let fx_prev = Frm.find_opt f env.tenv in

    let tx = new_tvar x in
    let tv = new_tvar "v" in
    let tf = TArrow (tx,tv) in
    let tb = new_tvar "b" in
    let env = {env with tenv=Frm.add x tx @@ Frm.add f tf env.tenv} in

    let env,tv2,th1 = typeinf v env in
    let th2 = unify [tv2,tv] in
    let thr = compose_subst th1 th2 in
    let tf = subst_ty tf thr in
    let tb = subst_ty tb thr in

    (* x cannot be refered from body
       since let rec f x = <v> in <body> *)
    let env = {env with tenv=Frm.add f tf env.tenv} in
    let env = {env with tenv=(match tx_prev with
    | Some tx -> Frm.add x tx | None -> ident)
    @@ Frm.remove x env.tenv} in

    let env,tb2,th1 = typeinf body env in
    let th2 = unify [tb,tb2] in
    let thr = compose_subst th2 (compose_subst th1 thr) in
    let tf = subst_ty tf thr in
    let tb = subst_ty tb thr in

    let env = {env with tenv=Frm.remove f env.tenv} in
    let env = {env with tenv=(
      match fx_prev with
      | Some fx -> Frm.add f fx | None -> ident
      ) @@ env.tenv
    } in
    let env = {env with tenv=subst_tfrm thr env.tenv} in
    (env,tb,thr),tf
  | _ -> raise @@ Invalid_argument "typeinf_lrec"


let rec eval exp (env:env) : ret =
  let rec eval_noext exp env : value = match exp with
  | LetGlobal _ -> failwith "eval_noext doesnt extend env"
  | LInt v -> VInt v
  | LBool b -> VBool b
  | Var x -> Frm.find x env.venv
  | LUnit -> VUnit
  | Let (x,v,body) ->
    let env,t,_ = typeinf v env in
    let v = eval_noext v env in
    let env' = Env.add x v t env in
    eval_noext body env'
  | LetRec (f,x,v,body) as lrec ->
    let _,tf = typeinf_lrec lrec env in
    let env' = Env.add f
      (VProcRec {rname=f; rvar=x; rcont=v; renv=env}) tf env in
    eval_noext body env'
  | Fun (var,cont) -> VProc {var; cont; env}
  | App (e1,e2) -> (
    let env,ftyp,_  = typeinf e1 env in
    let env,vtyp,_  = typeinf e2 env in
    let func  = eval_noext e1 env in
    let value = eval_noext e2 env in
    match func with
    | VProc {var;cont;env} ->
      eval_noext  cont @@ Env.add var value vtyp env
    | VProcRec {rvar; rcont; renv; rname} ->
      eval_noext rcont @@ Env.add rvar value vtyp @@ Env.add rname func ftyp renv
    | e -> failwith @@ sprintf "not a function: %s" (string_of_value e))
  | LOpAdd (e1,e2) -> (match eval_noext e1 env, eval_noext e2 env with
    | VInt u,VInt v -> VInt (u+v)
    | _ -> raise TypeError)
  | LOpSub (e1,e2) -> (match eval_noext e1 env, eval_noext e2 env with
    | VInt u,VInt v -> VInt (u-v)
    | _ -> raise TypeError)
  | LOpMul (e1,e2) -> (match eval_noext e1 env, eval_noext e2 env with
    | VInt u,VInt v -> VInt (u*v)
    | _ -> raise TypeError)
  | LOpDiv (e1,e2) -> (match eval_noext e1 env, eval_noext e2 env with
    | VInt u,VInt v -> VInt (u/v)
    | _ -> raise TypeError)
  | If (cond, csq, alt) -> (match eval_noext cond env with
    | VBool true  -> eval_noext csq env
    | VBool false -> eval_noext alt env
    | _ -> raise TypeError)
  | Equal (e1,e2) -> (match eval_noext e1 env, eval_noext e2 env with
    | VInt  u,VInt  v -> VBool (u=v)
    | VBool u,VBool v -> VBool (u=v)
    | VProc _,VProc _
    | VProcRec _,VProcRec _
      -> failwith "do not compare functions"
    | _ -> raise TypeError)
  | LessT (e1,e2) -> (match eval_noext e1 env, eval_noext e2 env with
    | VInt  u,VInt  v -> VBool (u<v)
    | VBool u,VBool v -> VBool (u<v)
    | VProc _,VProc _
    | VProcRec _,VProcRec _
      -> failwith "do not compare functions"
    | _ -> raise TypeError)
  in
  match exp with
| LetGlobal (x,v) ->
  let env_def = env in
  let _,t,_ = typeinf v env in
  let v = eval_noext v env_def in
  let env' = Env.add x v t env_def in VUnit, env'
| _ as exp -> eval_noext exp env, env

let interpret ?(loop=false) env ls0 =
  let string_of_typeinf_res exp env =
    let env,typ,th = typeinf exp env in
    sprintf "  typ : %s
  th  : %s" (string_of_typ typ) (Frm.sprint string_of_typ th)
  in
  List.fold_left (fun env ls ->
    List.fold_left (fun env exp ->
      Printf.(
        if not loop then printf "%s\n%!" @@ string_of_exp exp;
        printf "%s\n%!" @@ string_ast_of_exp exp;
        printf "%s\n%!" @@ string_of_typeinf_res exp env;
        (* printf "%s\n%!" @@ string_of_env env; *)
        let value,env = eval exp env in
        printf "  tenv : %s\n%!" (string_of_tenv env);
        (* printf "  env : %s\n%!" (string_of_env env); *)
        printf "-> %s\n%!" @@ string_of_value @@ value;
        (* printf "%s\n%!" @@ string_of_env env; *)
        printf "\n%!";
        env
      )
    ) env ls) env ls0

let create_env () = ref @@ Env.empty





