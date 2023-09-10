(*
   convert string to an integer
   if not a valid integer, raise an exception
*)
(* is String.length the same as string_length? *)

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

(* always set sum to 1 to start *)
let rec power (base: int) (expo: int) (prod: int): int =
   if expo = 0 then prod
   else power (base) (expo-1) (prod*base)
   ;;

let rec helper (sum: int) (s: string) (count: int): int =
   if count <= 0 then sum
   else if ord(string_get(s, count-1)) = 45 then sum * -1
   else
      helper (sum + power (10) (string_length s - count) (ord(string_get(s, count-1)) -48)) (s) (count - 1) (* flips order *)
      (* helper (sum + (power (10) (count) (1) * (ord(string_get(s, count-1))))-48) (s) (count+1) *)

      (* 
         THIS WORKS FOR SUMMING DIGITS
         helper (sum + ord(string_get(s, count-1)) -48) (s) (count + 1) *)
      (* 
         THIS WORKS FOR GETTING ONE CHAR
         ord(string_get(s, count-1)) -48  
      *)
;;

let str2int(cs: string): int =
   helper 0 (cs) (string_length cs)
;;