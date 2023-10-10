#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: float stream = fun() -> ...
//
*)

let the_ln2_stream: float stream = fun() ->
  let rec streamize(denom: float)(sum: float)(sign: float): float stream = fun () -> 
    let nextNum = sign /. denom in 
    let nextSum = sum +. nextNum in 
    StrCons(nextSum, streamize(denom +. 1.0)(nextSum)(sign *. (-1.0))) in
  StrCons(1.0, streamize(2.0)(1.0)(-1.0))