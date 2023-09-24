#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign2.ml";;

(* Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop: *)

let
foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  