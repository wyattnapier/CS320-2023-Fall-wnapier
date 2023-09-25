#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

(* import from assign2-1.ml essentially *)
let rec mylist_length(xs: 'a mylist): int = 
match xs with
| MyNil -> 0
| MyCons(x1, xs) -> 1 + mylist_length(xs)
| MySnoc(xs, x1) -> 1 + mylist_length(xs)
| MyReverse(xs) -> mylist_length(xs)
| MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
;;
(* *********** *)

(* *********** *)
let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
    match xs with
    | MyNil -> mylist_subscript_exn()
    | MyCons(x, xs) -> if i0 <= 0 then x else mylist_get_at(xs)(i0-1)
    | MySnoc(xs1, x1) -> let n1 = mylist_length xs1 in if i0 < n1 then mylist_get_at xs1 i0 else (if i0 <= n1 then x1 else mylist_subscript_exn())
    | MyReverse(xs1) -> let n1 = mylist_length xs1 in if i0 <  n1 then mylist_get_at(xs1)(n1 - 1 - i0) else mylist_subscript_exn()
    | MyAppend2(xs1, xs2) -> if mylist_length(xs1) <= i0 then mylist_get_at(xs2)(i0 - mylist_length(xs1)) else mylist_get_at(xs1)(i0)