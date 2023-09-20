(* 
WOOHOO I FIXED IT!! (I did have to basically restart though)
the acc*10 opens the rightmost slot (the lowest digit) in acc
num mod 10 fills it with rightmost digit of num
num/10 chops off the rightmost digit for next iteration
*)
let intrev10(n: int): int =
  if n < 10 then n
  else
    let rec accumulator (acc: int) (num: int): int =
      if num <= 0 then acc
      else accumulator (acc*10 + num mod 10) (num/10)
    in
    accumulator 0 n
;;