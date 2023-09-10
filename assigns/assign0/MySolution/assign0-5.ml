(*
   reverse a given string
   hella recurison
*)

(* ****** ****** *)

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)

let rec
helper (old: string) (rev: string): string = 
   if string_length old = string_length rev then rev
   else
      helper(old) (string_init (string_length rev + 1) (fun i -> 
         if i == 0 then string_get(old, string_length rev)
         else string_get(rev, (i - 1))
      ))
;;

let
stringrev(cs: string): string = 
   helper (cs) ""
;;

(* stringrev("hello");; *)