let rec
primeHelper(x: int) (div: int) =
if div <= 1 then true
else if x mod div = 0 then false (* this number isn't prime *)
else primeHelper x (div-1) (* try next version *)
;;

let isPrime(x: int) =
  if x < 2 then false
  else primeHelper x (x-1)
;;