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

(* 
raises a base to a power
@params base, the power its raised to, and product (should default to 1)
@returns an int
*)
let rec power (base: int) (expo: int) (prod: int): int =
   if expo = 0 then prod
   else power (base) (expo-1) (prod*base)
   ;;

(* 
   helper function for str 2 int
   handles negative symbol and checks if input string is valid then constructs int from string
   @params sum is final result (initally 1), s = string; count is number of repetitions (initially 1)
   @returns an int from the string
*)
let rec helper (sum: int) (s: string) (count: int): int =
   if count <= 0 then sum
   else if ord(string_get(s, count-1)) = 45 then sum * -1
   else if ord(string_get(s, count-1)) < 48 || ord(string_get(s, count-1)) > 58 then invalid_arg s
   else
      helper (sum + power (10) (string_length s - count) (ord(string_get(s, count-1)) -48)) (s) (count - 1) (* flips order *)
;;

let str2int(cs: string): int =
   helper 0 (cs) (string_length cs)
;;