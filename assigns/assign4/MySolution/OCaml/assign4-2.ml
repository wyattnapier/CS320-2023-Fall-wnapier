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

(* let the_ln2_stream: float stream = fun() ->
  let rec streamize(denom: float)(sum: float)(sign: float): float stream = fun () -> 
    let nextNum = sign /. denom in 
    let nextSum = sum +. nextNum in 
    StrCons(nextSum, streamize(denom +. 1.0)(nextSum)(sign *. (-1.0))) in
  StrCons(1.0, streamize(2.0)(1.0)(-1.0)) *)


(* let theNatPairs: (int*int) stream = fun () -> 
  let rec pairHelper(lastSum: int)(a: int)(b: int) = fun () ->
    if a + 1 > lastSum then 
      if b + 1 > lastSum then 
        let newb = lastSum + 1 in
        let newa = 0 in
        let newSum = a + b in 
        pairHelper(newSum)(newa)(newb)
      else
        let newb = b + 1 in
        let newa = a - 1 in
        let newSum = a + b in 
        pairHelper(newSum)(newa)(newb)
    else
      let newb = b - 1 in
      let newa = a + 1 in
      let newSum = a + b in 
      pairHelper(newSum)(newa)(newb)
  in pairHelper(0)(0)(0)
;; *)

let theNatPairs: (int*int) stream = fun () -> 
  let rec pairHelper(i: int)(j: int) = fun () ->
    if j = i + j then pairHelper(i + j + 1)(0)
    else pairHelper(i - 1)(j + 1)
  in pairHelper(0)(0)
;;