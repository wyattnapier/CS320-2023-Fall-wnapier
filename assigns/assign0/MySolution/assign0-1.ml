(* factorial function *)
let rec
fact(x: int): int =
if x > 0 then x * fact(x-1) else 1
;;

(* ****** loop that calls fact() until it returns 0 ****** *)
let rec
myloop(n: int): int = 
if fact(n) = 0 then n else myloop(n+1)
;;