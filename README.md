# interpreter

Toy OCaml interpreter with type inference

```
fun x -> fun y -> fun z -> (x y) (z (y+1))
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
-> fun{x=fun{} u -> fun v -> (u) (v):('v12->'@13)->'v12->'@13;} u -> (u) (x)



```

Reference: http://logic.cs.tsukuba.ac.jp/jikken/index.html
