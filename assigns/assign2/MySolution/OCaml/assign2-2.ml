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

(* import from assign2-1.ml essentially *)

let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
match xs with
| MyNil -> mylist_subscript_exn()
| MyCons(x, xs) -> if i0 = 0 then x else mylist_get_at(xs)(i0-1)
| MySnoc(xs, x1) -> if i0 = 0 then x1 else mylist_get_at(xs)(i0-1)
| MyReverse(xs) -> mylist_get_at(xs)(i0)
| MyAppend2(xs1, xs2) -> if mylist_length(xs1) <= i0 then mylist_get_at(xs2)(i0 - mylist_length(xs1)) else mylist_get_at(xs1)(i0)


(* let xs0 = MyNil
let xs1 = MyCons(1, xs0)
let xs2 = MyCons(2, xs1)
let xs3 = MyCons(3, xs0)
let xs4 = MyCons(4, xs3)
let xs5 = MyAppend2(xs2, xs4)
;;

let ( ) = assert(10 = mylist_get_at(xs1)(0)) *)

(* ****** ******
let xs0 = MyNil
let xs1 = MyCons(10, xs0)
let xs2 = MySnoc(xs0, -10)
let xs3 = MyAppend2(xs1, xs2)
let xs4 = MyReverse(xs3)
let xs5 = MyAppend2(xs4, xs4)
let xs6 = MyAppend2(xs5, xs5)
let xs7 = MyAppend2(xs6, xs6)
;;
(* ****** ****** *)
let ( ) = assert(10 = mylist_get_at(xs7)(1))
let ( ) = assert(10 = mylist_get_at(xs7)(3))
let ( ) = assert(10 = mylist_get_at(xs7)(5))
let ( ) = assert(10 = mylist_get_at(xs7)(7))
;;
(* ****** ****** *)
let ( ) = assert(10 = -mylist_get_at(xs7)(0))
let ( ) = assert(10 = -mylist_get_at(xs7)(2))
let ( ) = assert(10 = -mylist_get_at(xs7)(4))
let ( ) = assert(10 = -mylist_get_at(xs7)(6))
;;
(* ****** ****** *)
let () = print_string("Assign2-2-test passed!\n");; *)