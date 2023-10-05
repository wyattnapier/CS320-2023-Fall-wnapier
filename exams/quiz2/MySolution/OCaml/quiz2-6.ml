(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)
#use "./../../MySolution/OCaml/assign3-1.ml";;
let list_reverse(xs: 'a list): 'a list = 
list_foldright(xs)([])(fun xs acc ->
    match xs with
    |[] -> []
    |x1 :: xs1 -> xs :: acc
  )