#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(*
GRAMMAR (<prog> is starting symbol):
<prog> ::= <coms>
<coms> ::= empty | <com>;<coms> // note each command ends with semi-colon
<com> ::= push <const> | Pop | Trace
         | Add | Sub | Div | Mul
         | And | Or | Not
         | Lt | Gt
<const> ::= <int> | <bool> | <unit>
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<nat> ::= <digit> | <digit><nat> // can use the thing from assign6 probably
<int> ::= <nat> | -<nat>
<bool> ::= True | False
*)

(* OH tips: 
   turn the string into a coms list (hard)
   pattern match coms list and stack for all the cases (easier)
*)

type const = 
| Int of int | Bool of bool | Unit of unit

type com = 
| Push of const | Pop | Trace
| And | Or | Not
| Lt | Gt

type coms = com list

(* ******** ********* *)

(* parse constants *)
let rec parse_const(): const parser = 
   parse_int() 
   <|> parse_bool()
   (* add unit parse later *)

(* turns digits into an actual integer value using natural function then removes following whitespace *)
and parse_int () : const parser =
   (let* _ = char '-' in let* x = natural in pure (Int (-x))) 
   <|> (let* x = natural in pure (Int x))

(* use binding to essentially do pattern matching *)
and parse_bool () : const parser =
   let* _ = keyword "True" in pure (Bool true)
   <|> let* _ = keyword "False" in pure (Bool false)
   <|> fail (* none or fail *)

(* 
use binding to parse unit as well
what is a unit though??
*)

(* *********** ********** *)

(*

(* try parsing the command in all the different way *)
(* first parsing command that is run *)
let rec parse_coms(): coms parser =
   parse_push()
   (* parse_and () <|> parse_or () <|> parse_not () *)

and parse_push(): coms parser = 
   let* _ = keyword "Push" in
   let* x = many1' parse_const in
   let* _ = keyword ";\n" in
   pure(Push x) (* maybe just parse push first and then parse const after that *)


(* and parse_and (): coms parser = 
   (* let* _ = whitespaces in *)
   let* _ = keyword "And" in
   let _ = parse_coms
   (* let* _ = whitespaces in *)
   pure(And x) *)

(* starting function to begin parsing *)
let interp (s : string) : string list option = (* YOUR CODE *)
   match string_parse (parse_coms ()) s with
   | Some (e, []) -> Some e
   | _ -> None
*)