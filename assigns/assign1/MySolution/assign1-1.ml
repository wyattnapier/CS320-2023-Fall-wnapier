(* #use "./../../../classlib/OCaml/MyOCaml.ml";; *)

let intrev10(n: int): int =
  let rec int_length (digits: int) (len_counter: int): int = 
    if digits mod 10 > 0 then int_length(digits/10)(len_counter+1)
    else len_counter
  in
  let n_length = int_length(n) (0) in
  let rec pow (base: int) (expo: int) (prod: int): int =
    if expo = 0 then prod
    else pow (base) (expo-1) (prod*base)
  in
  let rec int_each (input: int) (acc: int) (count: int): int = 
    if input > 0 then 
      int_each (input / 10) (acc + (input mod 10) * pow(10) (n_length - count - 1) (1)) (count + 1) (* why does n_length - count - 1 need the -1? *)
    else acc
  in int_each(n)(0)(0) (* might need a 1 for the last arg*)
;;