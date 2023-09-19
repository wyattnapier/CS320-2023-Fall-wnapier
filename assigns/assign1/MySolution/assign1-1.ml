(* #use "./../../../classlib/OCaml/MyOCaml.ml";; *)
(* breaks for numbers with 0 in the middle *)

let rec int_length (digits: int) (len_counter: int): int = 
  if digits mod 10 > 0 then int_length(digits/10)(len_counter+1)
  else len_counter

let rec pow (base: int) (expo: int) (prod: int): int =
  if expo <= 0 then prod
  else pow (base) (expo-1) (prod*base)

let rec int_each (input: int)(n_length:int) (acc: int) (count: int) (prev0: bool): int = 
  if input > 0 then int_each (input / 10)(n_length) (acc + (input mod 10) * pow(10) (n_length - count) (1)) (count + 1) false
    (* if input mod 10 = 0 then int_each (input/10)(n_length) (acc) (count + 1) true
    else 
      if prev0 then int_each (input / 10)(n_length) (acc + (input mod 10) * pow(10) (n_length - count + 1) (1)) (count + 1) false
      else int_each (input / 10)(n_length) (acc + (input mod 10) * pow(10) (n_length - count) (1)) (count + 1) false why does n_length - count - 1 need the -1? *)
  else acc

let intrev10(n: int): int =
  if n mod 10 = 0 then n
  else
    let n_length = int_length(n) (0) in
    int_each(n)(n_length)(0)(1)false (* might need a 1 for the last arg*)
;;