(*
Assign0-2: 10 points
Please implement a function that tests whether
a given natural number is a prime:
fun isPrime(n0: int): bool
*)

let isPrime(n0: int): bool = 
  if n0 < 2 then false else
  let rec helper(div: int): bool =
    if div <= 1 then true
    else 
      if (n0 mod div = 0) then false
      else helper(div - 1 ) in
  helper(n0/2)
