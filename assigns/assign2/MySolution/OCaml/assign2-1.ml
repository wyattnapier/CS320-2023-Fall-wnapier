#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

let rec
mylist_length(xs: 'a mylist): int = 
match xs with
| MyNil -> 0
| MyCons(x1, xs) -> 1 + mylist_length(xs)
;;