#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

(* For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33" *)

(* import from assign2-1.ml essentially *)
let rec mylist_length(xs: 'a mylist): int = 
  match xs with
  | MyNil -> 0
  | MyCons(x1, xs) -> 1 + mylist_length(xs)
  | MySnoc(xs, x1) -> 1 + mylist_length(xs)
  | MyReverse(xs) -> mylist_length(xs)
  | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
  ;;
(* import from assign2-2.ml essentially *)
let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
    match xs with
    | MyNil -> mylist_subscript_exn()
    | MyCons(x, xs) -> if i0 <= 0 then x else mylist_get_at(xs)(i0-1)
    | MySnoc(xs1, x1) -> let n1 = mylist_length xs1 in if i0 < n1 then mylist_get_at xs1 i0 else (if i0 <= n1 then x1 else mylist_subscript_exn())
    | MyReverse(xs1) -> let n1 = mylist_length xs1 in if i0 <  n1 then mylist_get_at(xs1)(n1 - 1 - i0) else mylist_subscript_exn()
    | MyAppend2(xs1, xs2) -> if mylist_length(xs1) <= i0 then mylist_get_at(xs2)(i0 - mylist_length(xs1)) else mylist_get_at(xs1)(i0)
(* import from assign2-3.ml *)
let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work -> let _ = foldleft xs 0 (fun idx x0 -> ((work idx x0); idx + 1)) in ()
;;
(* ********* ********** *)
let
list_iforeach =
fun xs -> foldleft_to_iforeach(list_foldleft)(xs)
;;
(* ********* ********** *)
let rec foreach_to_length(foreach: ('xs, 'x0) foreach): 'xs -> int =
  foldleft_to_length(foreach_to_foldleft(foreach))
  and
  foldleft_to_length(foldleft: ('xs,'x0,'r0) foldleft): 'xs -> int = 
  (
    fun(xs) -> foldleft(xs)(0)(fun(r0)(x0) -> r0+1)
  )
;;
(* ********* ********** *)
let rec list_foreach(xs: 'a list) (work: 'a -> unit): unit =
  (
    match xs with
    | [] -> ()
    | x1 :: xs -> (
        work(x1); list_foreach(xs)(work)
      )
  )
;;
(* ********* ********** *)
let
string_sepjoin_list
(sep: string)(xs: string list): string =
  (* takes each string in list and then if the index isn't of the last element then append sep else just add last string chunk *)
  let length = foreach_to_length(list_foreach)(xs) in
  string_make_fwork
  (fun work -> 
    (list_iforeach xs 
      (fun i r0 -> 
        if i < ((length) - 1) then (string_foreach(r0)(work); string_foreach(sep)(work))
        else string_foreach(r0)(work)
      )
    )
  )
;;