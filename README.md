# interpreter

Toy OCaml interpreter with type inference

```shell
$ make
...

(1 + 2 * 3) * 6;;
Mul(Add(1,Mul(2,3)),6)
  typ : int
  th  : []
  tenv : {}
-> 42

fun x -> fun y -> fun z -> (x y) (z (y+1));;
Fun(x,Fun(y,Fun(z,App(App(x,y),App(z,Add(y,1))))))
  typ : (int->'@4->'@5)->int->(int->'@4)->'@5
  th  : ['@3:'@4->'@5;'x0:int->'@4->'@5;'y1:int;'z2:int->'@4;]
  tenv : {}
-> fun{} x -> fun y -> fun z -> ((x) (y)) ((z) (y + 1))

let x = fun u -> fun v -> u v in fun u -> u x;;
Let(x,Fun(u,Fun(v,App(u,v))),Fun(u,App(u,x)))
  typ : ((('v7->'@8)->'v7->'@8)->'@10)->'@10
  th  : ['u6:'v7->'@8;'u9:(('v7->'@8)->'v7->'@8)->'@10;]
  tenv : {}
-> fun{x=(fun{} u -> fun v -> (u) (v)):('v12->'@13)->'v12->'@13;} u -> (u) (x)

let rec f x = if x = 0 then 0 else x + f (x-1) in f;;
LetRec(f,x,IF(x = 0,0,Add(x,App(f,Sub(x,1)))),f)
  typ : int->int
  th  : ['@17:int;'b16:int->int;'v15:int;'x14:int;]
  tenv : {}
-> fun_rec(f)[{}] x -> if x = 0 then 0 else x + (f) (x - 1)

let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f (* tail recursive *);;
LetRec(f,x,LetRec(g,a,IF(x = 0,a,App(App(f,Sub(x,1)),Add(a,x))),g),f)
  typ : int->int->int
  th  : ['@28:int->int;'@29:int;'a25:int;'b24:int->int->int;'b27:int->int;'v23:int->int;'v26:int;'x22:int;]
  tenv : {}
-> fun_rec(f)[{}] x -> let rec g a = if x = 0 then a else ((f) (x - 1)) (a + x) in g

let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f 10 0;;
LetRec(f,x,LetRec(g,a,IF(x = 0,a,App(App(f,Sub(x,1)),Add(a,x))),g),App(App(f,10),0))
  typ : int
  th  : ['@44:int->int;'@45:int;'@46:int->int;'@47:int;'a41:int;'b40:int;'b43:int->int;'v39:int->int;'v42:int;'x38:int;]
  tenv : {}
-> 55
```

Reference: http://logic.cs.tsukuba.ac.jp/jikken/index.html
