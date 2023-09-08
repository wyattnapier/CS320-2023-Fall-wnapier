let rec
fact(x: int): int =
if x > 0 then x * fact(x-1) else 1
;;

(* ****** ****** *)

let fact10 = fact(10)
;;
let fact100 = fact(100)
;;
let fact 1M = fact(1000000000)
;;

(* ****** ****** *)

let rec
myloop(x: int): int = 
if fact(x) = 0 then x else myloop(x+1)
;;

(* 
MacCarthy's 91-function to test compilers
should always return 91
*)
let rec
f91(x: int): int =
if x > 100 then x - 10 else f91(f91(x+11))
;;