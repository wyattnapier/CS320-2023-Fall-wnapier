#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* bonus *)
(* a < c < b < d *)
let string_avoid_1324(cs: string): bool =
  if string_length cs < 4 then true
  else
    let rec my_loop (a: int) (b: int) (c: int) (d: int): bool =
      if string_get_at cs a < string_get_at cs c && string_get_at cs c < string_get_at cs b && string_get_at cs b < string_get_at cs d then false
      else if a >= string_length cs - 4 then true
      else if b >= string_length cs - 3 then my_loop (a + 1) (a + 2) (a + 3) (a + 4)
      else if c >= string_length cs - 2 then my_loop a (b + 1) (b + 2) (b + 3)
      else if d >= string_length cs - 1 then my_loop a b (c + 1) (c + 2)
      else my_loop a b c (d + 1)
    in my_loop 0 1 2 3
;;