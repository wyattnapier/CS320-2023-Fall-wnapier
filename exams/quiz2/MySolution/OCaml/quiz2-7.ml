(* ************************************************ *)

(*
Q2-7: 10 points

The following implementation of list_append is not tail-recursive.
Please give an implementation of list_append that is tail-recursive.

Note that you can only use pattern matching and list_foldleft in your
implementation.
 
let rec
list_append(xs: 'a list)(ys: 'a list) =
match xs with
  [] -> ys | x1 :: xs -> x1 :: list_append(xs)(ys)
*)

(* ************************************************ *)
#use "./../../MySolution/OCaml/assign3-1.ml";;
let list_append(xs: 'a list)(ys: 'a list): 'a list =
list_foldleft(xs1)([])(fun xs1 acc1 ->
    match xs1 with
    |x1::xs1 -> acc1 :: xs1
    |[] -> list_foldleft(xs2)([])(fun xs2 acc2 ->
      match xs2 with
      |[] -> acc1
      |x2::xs2 -> acc1::x2
      )
  )
