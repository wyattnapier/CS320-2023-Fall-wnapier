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
(* change return parser type to a specific const type e.g. (int, bool and unit)*)
let rec parse_const(): const parser = 
   parse_int() <|> parse_bool() <|> parse_unit ()

and parse_int () : const parser =
   let* _ = char '-' in let* x = natural in pure (Int (-x)) 
   <|> let* x = natural in pure (Int x)
   (* do you have to clear the ;\n after? *)

and parse_bool () : const parser =
   let* _ = keyword "True" in pure (Bool true)
   <|> let* _ = keyword "False" in pure (Bool false)
   <|> fail

and parse_unit () : const parser = 
   let* _ = keyword "Unit" in pure Unit <|> fail

(* *********** ********** *)

let rec parse_com(): com parser =
   parse_push() <|> parse_pop () <|> parse_trace () <|>
   parse_and () <|> parse_or () <|> parse_not () <|>
   parse_lt () <|> parse_gt()

and parse_push(): com parser = 
   let* _ = keyword "Push" in
   (* let* _ = whitespaces in *)
   let* c = parse_const() in
   (* let* _ = keyword ";\n" in *)
   pure (Push (Int 1))

and parse_pop (): com parser = 
   let* _ = keyword "Pop" in
   pure (Pop)

and parse_trace (): com parser = 
   let* _ = keyword "Trace" in
   pure (Trace)

and parse_and (): com parser = 
   let* _ = keyword "And" in
   pure (And)

and parse_or (): com parser = 
   let* _ = keyword "Or" in
   pure (Or)

and parse_not (): com parser = 
   let* _ = keyword "Not" in
   pure (Not)

and parse_lt (): com parser = 
   let* _ = keyword "Lt" in
   pure (Lt)

and parse_gt (): com parser = 
   let* _ = keyword "Gt" in
   pure (Gt)

let rec parse_coms(): coms parser = 
   many' parse_com  (* allows empty string I think? *)

(* 
pattern match on the type - do this before converting to a string
   pattern match and evaluate????????????????????
stringize  - option map this onto output from the match in interp
*)
(* exception NoCom
let com_stringize e : string = (* figure out input typing and pattern matching *)
   (* could make function instead of match *)
   match e with
   (* | Push (Int x) -> "Push int" ^ string_of_int x
   | Push (Bool x) -> "Push bool" ^ string_of_bool x *)
   | Pop -> "Pop"
   | Trace -> "Trace"
   | And -> "And"
   | Or -> "Or"
   | Not -> "Not"
   | Lt -> "Lt"
   | Gt -> "Gt"
   | _ -> raise NoCom *)

(* **************************** ************************  *)

(* starting function to begin parsing // need to convert to string list *)
(* let interp (s : string) : string list option = (* YOUR CODE *)
   string_parse (parse_coms ()) s  *)


   (* match string_parse (parse_coms ()) s with
   | Some (e, []) -> Some e (* map and stringize e *)
   | _ -> None *)