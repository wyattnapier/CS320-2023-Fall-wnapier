#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)
(* use list make fwork 
   take answer to two
   make a length function for the x0 then put it in fold left else don't put it in fold left
*)

let rec
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
match xs with
| [] -> []
| x1 :: xs -> fopr(x1) :: list_map(xs)(fopr)
;;

let list_subsets (xs: 'a list): 'a list list =
  list_foldright(xs)([[]])(fun x res -> 
    list_append(res)(list_map(res)(fun xs -> x :: xs)))
;;

let list_length =
  fun xs -> list_foldleft(xs)(0)(fun r0 x -> r0 + 1)
;;

let list_nchoose
(xs: 'a list)(n0: int): 'a list list =
  let subsets = list_subsets(xs) in
  list_foldleft(subsets)([])(fun res x0 -> 
    if list_length x0 = n0 then list_append(res)([x0]) else res)