(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:

USE ZIP!
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** from lecture ****** *)
let rec
list_z2foreach
(xs: 'a list)
(ys: 'b list)
(work: 'a -> 'b -> unit): unit =
(
match xs, ys with
| [], _ -> ()
| _, [] -> ()
| (x1 :: xs), (y1 :: ys) ->
  (work(x1)(y1); list_z2foreach(xs)(ys)(work)))
;;

(** transforms the work done by fwork into a list. **)
let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = (res := (x0 :: !res))
    in(*let*)(fwork(work); list_reverse(!res) )
;;

(* ****** ****** *)

let
list_zip2
(xs: 'a list)
(ys: 'b list): ('a * 'b) list =
list_make_fwork
(fun work ->
 list_z2foreach(xs)(ys)(fun x y -> work(x, y)))
;;

let
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
list_make_fwork
(fun work -> list_foreach(xs)(fun x -> work(fopr(x))))
;;

(* ************** *)

let rec grab_head(xs: 'a list list): 'a list =
    match xs with
    | [] -> []  (* raise done this thing is empty *)
    | [] :: _ -> []
    | (x :: _) :: xs1 -> x :: grab_head xs1 (* xs1 is the other rows *)
  

let rec grab_tail(xs: 'a list list): 'a list list =
  match xs with
  | [] -> [] (* raise done this thing is empty *)
  | [] :: xs1 -> grab_tail(xs1)
  | (_ :: xs) :: xs1 -> xs :: grab_tail(xs1)

let rec matrix_transpose(xss: 'a list list): 'a list list =
  match xss with
  | [[]] -> [[]]
  | [] :: _ -> []
  | _ ->  
    let heads = grab_head(xss) in 
    let tails = grab_tail(xss) in
    heads :: matrix_transpose(tails) 
    

(* ************** *)

  (* 
  (* is this backwards?? *)
  for_each(
    list_foldright(
      list_append(
        list_map(
          (xss)
        )
      ))) *)

  (* list_map(xss)(list_append(list_foldright))) *)
(*
THINKING SPACE:

list subset
list fold right
list append add temp to final
list map to map a number onto result

input:
[[1;2;3];
[4;5;6];
[7;8;9]]   

output:
[[1;4;7];
[2;5;8];
[3;6;9]]   
*)