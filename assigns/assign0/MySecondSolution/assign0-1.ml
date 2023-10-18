(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml returns '0' (due
to arithmetic overflow.
*)


let rec fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1


let rec myloop(n: int): int = 
  if fact(n) = 0 then n
  else myloop(n + 1)