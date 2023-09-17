#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* concat string directly and then *)

(* use string_make_fwork *)
let string_merge(cs1)(cs2): string = 
  (* merge strings using a list *)
  let list_cs1 = 
;;

(* ****************** ********************* *)

(* APPROACH MERGING TWO STRINGS THEN SORTING *)
  (* merge strings *)
  (* let result =
    string_make_fwork
    ( fun work -> 
      (string_foreach(cs1)(work); string_foreach(cs2)(work))
    ) in
    (* sort result *)
    string_foreach(C) *)

(* ****************** ********************* *)

(* APPROACH USING FOR LOOP AND APPENDING SMALLER VALUE EACH TIME *)
  (* string_make_fwork
  ( fun work -> 
    (* instead of 'b' I need to compare to the value of the curr char in cs2 then take whatever is lower *)
    string_foreach(cs1)
    (fun c ->
      if c >= 'b' then work(c)
      else work('z')
    )
  ) *)
    