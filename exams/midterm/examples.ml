(* ************************************ *)
#use "./../../classlib/OCaml/MyOCaml.ml";;
(* ************************************ *)
let rec kf91 x k =
  if x > 100 then k(x - 10) (* put it inside k because that is the operation you're doing *)
    else kf91(x + 11) (fun res -> kf91(res)(k)) (* call inside lambda function maps to outer call *)
;;
let f91 n =
  let rec f91_tail_recursive n k =
    if n > 100 then
      n - 10
    else if k=true then
      f91_tail_recursive (n + 11) (false)
    else
      f91_tail_recursive (n) (true)
  in
  f91_tail_recursive n true  (* Initialize k with 1 when calling the helper function *)
;;
(* Example usage: *)
let result = f91 97  (* Call f91 with an initial value of 97 *)
(* ************************************ *)
(* 
  kfact(x-1)(fun res -> k(x * res))
  3 * res
  kfact(2)(fun res -> (x * res)) where 3 is stored in res
  ANSWER: 6 woohoo
*)
(* ************************************ *)
let whatisit1(f)(x) = f(x)(x)
(* first look at f and 
  see that it taakes in two values (two of the same) and returns a third so it is ('a -> 'a -> 'b) 
  look at x which from f must be 'a
  f return type of f is the return type of whole function so 'b
  ANSWER: ('a -> 'a -> 'b) -> 'a -> 'b
*)
let whatisit2(f)(x)(y) = f(y)(x)
(*
   f takes in two different parameters and returns a thirdd so ('a -> 'b -> 'c)
   then x and y are swapped in input order so it is 'b -> 'a and matches return type of f which is 'c
   ANSWER: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
*)
(* ************************************ *)

(* ************************************ *)
let rec
f(x) = f(g(x+1)) + 1
and
g(x) = g(f(x-2)) + 2
;;

let rec kf(x)(k) =
  kg(x+1)(fun r -> kf r k)
and kg(x)(k) =
  kg(x-2)(fun r -> kg r (fun r -> k(r + 1)))
(* ************************************ *)

(*
 The above
 CPS-translates into the following *)

(* ************************************ *)

let rec
kf(x)(k) =
kg(x+1)(fun r -> kf r (fun r -> k(r + 1)))
and
kg(x)(k) =
kf(x-2)(fun r -> kg r (fun r -> k(r + 2)))
;;

(* ************************************ *)
let fg = fun(f)(g)(x) -> f(g(x))
(* 
  look at f first - takes on input and outputs another: ('a -> 'b)
  then look at g: result of g is being passed to f so it must return type a: ('c -> 'a)
  x = 'c because it is the input to g
  overall output is 'b because it is output of f  
  ANSWER: ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
*)

(* What is the type of fg in OCaml?
1. (’a -> ’a) -> (’b -> ’b) -> (’a -> ’b)
2. (’a -> ’b) -> (’b -> ’c) -> (’a -> ’c)
3. (’a -> ’b) -> (’a -> ’b) -> (’a -> ’b)
4. (’b -> ’c) -> (’a -> ’b) -> (’a -> ’c)
5. None of the above *)
(* ************************************ *)
(*
 This question should reinforce the claim that
 references are to be avoided.
*)

let
tricky =
  let global = ref(0) in
  let rec f(i: int) : int =
  global := !global + i;
  if i > 0 then f(i-1) 
  else !global in fun(i:int) -> f(i)
;;
(*
 What is the value of the expression tricky(10)?
*)
let tricky10 = tricky(10);;
(* 
  it is just a loop that does global + i from 10 to 0 
  ANSWER: 55! woohoo
*)
(* ************************************ *)

let
mystream =
let
rec
fstream(n: int): int stream = fun() ->
StrCons(n, stream_map(fstream(n+1))(fun(x) -> x+x+1)) in
fstream(0)
;;
(*
 The first element of mystream is 0.
 What is the 5th (fifth) element in mystream?
 *)
;;
(* ************************************ *)

let rec
fff(n: int): int =
if n = 0 then 0 else 10*fff(n / 2) + n mod 2
;;
(*
 What is the value of fff(1023)?
*)

(* ************************************ *)

type intcont = (int) -> int;;

let rec
kf(n: int)(k1: intcont)(k2: intcont): int =
if n = 0
then k1(0) else
kf(n-1)(fun(res) -> k2(res+n))(fun(res) -> k1(res-n))
;;
let
kf0(n: int): int = kf(n)(fun res -> res)(fun res -> res)
;;

(*
 What is the value of kf0(10)?
*)
			     
(* ************************************ *)

