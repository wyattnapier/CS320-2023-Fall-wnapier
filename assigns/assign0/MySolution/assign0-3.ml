(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let rec helper (n: int) (s: string): string =
   if n > 0 then
      helper (n/10) (string_init (string_length s + 1) (fun i -> 
      if i == 0 then chr((n mod 10) + 48) 
      else string_get_at s (i-1)))
   else s
;;

let int2str(i0: int): string = 
   if i0 = 0 then "0" 
   else if i0 < 0 then let positive = helper(-1 * i0) ("") in string_init (string_length positive + 1) (fun i -> if i == 0 then '-' else string_get_at positive (i-1))
   else helper(i0) ("")
;;