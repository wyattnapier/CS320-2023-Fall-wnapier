(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)
#use "./../../MySolution/OCaml/assign3-1.ml";;


exception Empty
let list_last(xs: 'a list): 'a = 
  list_foldleft(xs)(None)(fun acc xs ->
    match xs with
    |[] -> raise Empty
    |x1::xs1 -> match xs1 with
      |[] -> Some acc = x1
      | _ -> match acc with None | Some acc
    )