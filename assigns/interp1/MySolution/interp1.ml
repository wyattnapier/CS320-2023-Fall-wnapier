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
| Int of int | Bool of bool | Unit (* of unit *)

type com = 
| Push of const | Pop | Trace
| And | Or | Not
| Lt | Gt

type coms = com list

(* ******** ********* *)

(* parse constants - mutually recursive because it simplifies it a bit in my mind *)
let rec parse_const(): const parser = 
   parse_int() <|> parse_bool() <|> parse_unit ()

and parse_int () : const parser =
   let* _ = char '-' in let* x = natural in pure (Int (-x)) 
   <|> let* x = natural in pure (Int x)
   (* do you have to clear the ;\n after? *)

and parse_bool () : const parser =
   let* _ = keyword "True;\n" in pure (Bool true)
   <|> let* _ = keyword "False;\n" in pure (Bool false)
   <|> fail

(* use binding to parse unit as well - what is a unit though?? *)
and parse_unit () : const parser = 
   let* _ = keyword "Unit" in pure Unit <|> fail

(* *********** ********** *)


let rec parse_com(): com parser =
   (* parse_push() <|> parse_pop () <|> parse_trace () <|> *)
   parse_and ()(* <|> parse_or () <|> parse_not () <|> *)
   (* parse_lt () <|> parse_gt *)

(*
and parse_push(): com parser = 
   let* _ = keyword "Push" in
   let* x = many1' parse_const in
   let* _ = keyword ";\n" in
   pure(Parse x) (* maybe just parse push first and then parse const after that *)
*)

and parse_and (): com parser = 
   let* _ = keyword "And;\n" in
   let _ = parse_coms in
   pure (And)

let rec parse_coms(): coms parser = 
   many parse_com ()

(* stringize *)
Exception NoCom
let com_stringize (c: com) : string = 
   match c with
   | Push (Int x) -> "Push int"
   | Push (Bool x) -> "Push bool"
   | Pop -> "Pop"
   | Trace -> "Trace"
   | And -> "And"
   | Or -> "Or"
   | Not -> "Not"
   | Lt -> "Lt"
   | Gt -> "Gt"
   | _ -> raise NoCom

(* ****** to string section ****** *)
(*
(* like list map but for sexpr - appends the result of map to list *)
let rec sexpr_map
(xs: sexpr list)(fopr: sexpr -> 'b): 'b list =
  match xs with
  | [] -> []
  | x1 :: xs -> fopr x1 :: sexpr_map xs fopr

let rec string_concat strictString strings =
  match strings with
  | [] -> ""
  | [s] -> s
  | s :: rest -> string_append s (string_append (strictString) (string_concat strictString rest))

(* matches the type of sexpr and then recursively builds the string *)
let rec sexpr_to_string s = 
  match s with
  | SInt n -> string_of_int n
  | SAdd exprs -> string_append ("(add ") ((string_append (string_concat " " (sexpr_map exprs sexpr_to_string)) ")"))
  | SMul exprs -> string_append ("(mul ") ((string_append (string_concat " " (sexpr_map exprs sexpr_to_string)) ")"))
  *)
(* **************************** ************************  *)

(* starting function to begin parsing // need to convert to string list *)
let interp (s : string) : string list option = (* YOUR CODE *)
   match string_parse (parse_coms ()) s with
   | Some (e, []) -> Some e
   | _ -> None