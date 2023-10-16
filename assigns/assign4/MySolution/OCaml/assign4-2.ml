#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fun () -> ...
//
*)

(* let theNatPairs: (int*int) stream = fun () -> 
  let rec pairHelper(i: int)(j: int) = fun () -> 
    if j = i + j then pairHelper(i + j + 1)(0)
    else pairHelper(i - 1)(j + 1)
  in pairHelper(0)(0)
;; *)