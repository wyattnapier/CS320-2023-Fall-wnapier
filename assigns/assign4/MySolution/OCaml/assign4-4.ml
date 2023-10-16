#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)
(**************************************************************)
(* let rec list_streamize(xs: 'a list): 'a stream = fun () ->
  match xs with
  |[] -> StrNil
  |(x1::xs) -> StrCons(x1, list_streamize(xs))
;;

let list_permute(xs: 'a list): 'a list stream = fun () ->
  let rec permuter(xs: 'a list)(i: int)(j: int) = fun () ->
    (* list_make_fwork(xs) *)
  in permuter(xs)(0)list length? *)

let list_length(xs: 'a list): int = 
  list_foldleft(xs)(0)(fun acc xs -> acc + 1)
;;
    
let list_get_at(xs: 'a list)(index: int): 'a =
  list_make_fwork(fun work ->
    (int1_foreach)(list_length(xs)) (fun j ->
      
    )
  )

(* let list_copy_swap
let list_reverse_subset_from_index *)