#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;
#use "./assign2-1.ml";;

let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
match xs with
| MyNil -> mylist_subscript_exn()
| MyCons(x, xs) -> if i0 = 0 then x else mylist_get_at(xs)(i0-1)
| MySnoc(xs, x1) -> if i0 = 0 then x1 else mylist_get_at(xs)(i0-1)
| MyReverse(xs) -> mylist_get_at(xs)(i0)
| MyAppend2(xs1, xs2) -> if mylist_length(xs1) <= i0 then mylist_get_at(xs2)(i0 - mylist_length(xs1)) else mylist_get_at(xs1)(i0)