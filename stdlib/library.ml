let abs = fun x -> if x < 0 then (0 - x) else x;;
let ceildiv = fun n -> fun d -> (n + d - 1) / d;;
let fmod = fun n -> fun k -> let w = n - (n / k) * k in if w<0 then k+w else w;;
let pow = fun n -> fun k -> let rec pow a = let rec lp1 n = let rec lp2 i = if i=0 then a else if i=1 then n*a else pow (if fmod i 2 = 0 then a else a*n) (n*n) (i/2) in lp2 in lp1 in pow 1 n k;;

