#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_avoid_132(cs: string): bool =
  if string_length cs < 3 then true
  else
    let rec my_loop (a: int) (b: int) (c: int): bool =
      if string_get_at cs a < string_get_at cs c && string_get_at cs c < string_get_at cs b then false
      else if a >= string_length cs - 3 then true
      else if b >= string_length cs - 2 then my_loop (a + 1) (a + 2) (a + 3)
      else if c>= string_length cs -1 then my_loop a (b + 1) (b + 2)
      else my_loop a b (c+1)
    in my_loop 0 1 2