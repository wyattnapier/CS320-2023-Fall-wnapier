#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;
#use "./assign2-1.ml";;

let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a =
match xs with
| MyNil -> mylist_subscript_exn()
| MyCons(x, xs) -> if i0 = 0 then x else mylist_get_at(xs)(i0-1)