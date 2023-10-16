(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

let rec
gtree_dfs
( nxs: 'b list)
( fchildren: 'b -> 'b list): 'b stream = fun() ->
( match nxs with
  | [] -> StrNil
  | nx1 :: nxs ->
    StrCons(nx1, gtree_dfs(fchildren(nx1) @ nxs)(fchildren))
)
;;
let rec
gtree_bfs
( nxs
: 'node list)
( fchildren
: 'node -> 'node list): 'node stream = fun() ->
(
match nxs with
  [] -> StrNil
| nx1 :: nxs ->
  StrCons(nx1, gtree_bfs(nxs @ fchildren(nx1))(fchildren))
)
;;
let fchildren (x0: 'a gtree): 'a gtree list =
match x0 with
| GTnil -> [] | GTcons(_, xs) -> xs

let rec helper(stream) = fun () ->
  match stream() with
  | StrNil -> StrNil
  | StrCons(fx1, fxs) -> 
    match fx1 with
    | GTnil -> helper(fxs)()
    | GTcons(gx1, gxs) -> StrCons(gx1, helper(fxs))
;;

let rec gtree_streamize_dfs(xs: 'a gtree): 'a stream =
  fun() -> helper(gtree_dfs([xs])(fchildren))()
;;

let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream =
  fun() -> helper(gtree_bfs([xs])(fchildren))()
;;

(* ****** TESTING CODE ****** *)
let xs0 = GTnil
let xs1 = GTcons(1, [])
let xs2 = GTcons(2, [])
let xs3 = GTcons(3, [xs1;xs2])
let xs4 = GTcons(4, [xs1;xs2;xs3])
let xs5 = GTcons(5, [xs1;xs2;xs3;xs4])
;;
(* ****** ****** *)
let ys0_dfs = gtree_streamize_dfs(xs5)
let ys1_bfs = gtree_streamize_bfs(xs5)
(* ****** ****** *)
let ys0_lst =
list_make_fwork
(fun work -> stream_foreach(ys0_dfs)(work))
let ys1_lst =
list_make_fwork
(fun work -> stream_foreach(ys1_bfs)(work))
let () = assert
(ys0_lst = [5; 1; 2; 3; 1; 2; 4; 1; 2; 3; 1; 2])
let () = assert
(ys1_lst = [5; 1; 2; 3; 4; 1; 2; 1; 2; 3; 1; 2])
(* ****** ****** *)
let () = print_string("Assign4-3-test passed!\n")
;;
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign4-3-test.sml] *)



(* let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream *)