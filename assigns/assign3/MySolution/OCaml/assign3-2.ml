#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*
Assign3-2:
HX-2023-09-26: 10 points
//
The function [list_subsets]
returns all the subsets of a given
set (where sets are represented as lists)
//
let rec
list_subsets
(xs: 'a list): 'a list list =
(
match xs with
  [] -> [[]]
| x1 :: xs ->
  let res = list_subsets(xs) in
    res @ list_map(res, fun(xs) -> x1 :: xs)
)
//
Please give a NON-RECURSIVE implementation of
list_subsets based on list-combinators. Note that
the order of the elements in a list representation
of a set is insignificant. For instance, [1,2] and
[2,1] represents the same set {1,2}.
//
*)

let rec
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
match xs with
| [] -> []
| x1 :: xs -> fopr(x1) :: list_map(xs)(fopr)
;;

let list_subsets (xs: 'a list): 'a list list =
  let res = [] in
  list_map (xs) (list_append(res)(list_foldright(xs)))
;;
  (* foldright append map *)
