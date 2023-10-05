(* ************************************************ *)

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)
#use "./../../MySolution/OCaml/assign3-1.ml";;

exception Empty
let list_last(xs: 'a list): 'a = 
let collected = false in 
list_foldright(xs)(None)(fun xs acc ->
  if collected = false then match xs with
    |[] -> raise Empty
    |x1 :: xs1 -> collected = true; acc = Some x1
  else acc
  )
