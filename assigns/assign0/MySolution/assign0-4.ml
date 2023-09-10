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
let rec power (base: int) (expo: int) (sum: int): int =
   if expo = 0 then sum
   else power (base) (expo-1) (sum*base)
   ;;

let rec helper (sum: int) (s: string) (count: int): int =
   if count > string_length (s) then sum
   else 
      helper (sum + (power (10) (count) (1) * (ord(string_get(s, count-1))))-48) (s) (count+1)
   (* else let increase = get_string(count) in helper *)
;;

let str2int(cs: string): int =
   helper 0 (cs) 1
;;