#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let the_ln2_stream: float stream = fun() ->
  let rec streamize(denom: float)(sum: float)(sign: float): float stream = fun () -> 
    let nextNum = sign /. denom in 
    let nextSum = sum +. nextNum in 
    StrCons(nextSum, streamize(denom +. 1.0)(nextSum)(sign *. (-1.0))) in
  StrCons(1.0, streamize(2.0)(1.0)(-1.0))