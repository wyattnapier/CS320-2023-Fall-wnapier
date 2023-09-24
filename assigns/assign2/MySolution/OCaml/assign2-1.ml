#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int = 
match xs with
| MyNil -> 0
| MyCons(x1, xs) -> 1 + mylist_length(xs) (* works for these two *)
| MySnoc(xs, x1) -> 1 + mylist_length(xs)
| MyReverse(xs) -> mylist_length(xs)
| MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
;;