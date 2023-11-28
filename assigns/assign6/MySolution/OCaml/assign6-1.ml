(* ****** ****** *)
(*
//
Assign6:
Parsing and parsing combinators
//
DUE: the 13th of November, 2023
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])
//
Example (Rejected Strings):
parse "()" = None
parse "(add)" = None
parse "(add 1 2))" = None
parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* calls different parsers but only returns the sexpr that is successful *)
let rec parse_expr () : sexpr parser =
  parse_int () <|> parse_add () <|> parse_mul () (* disjoint returns the first successful parser *)

(* turns digits into an actual integer value using natural function then removes following whitespace *)
and parse_int () : sexpr parser =
  let* x = natural in 
    (if (0<=x) then pure (SInt x) << whitespaces else fail)

(* isolate keyword then parse arguments and then close parens and set type to SAdd *)
and parse_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SAdd es)

(* isolate keyword then parse arguments and then close parens and set type to SMul *)
and parse_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SMul es)

(* initial call to start parse - returns what is passed back from parse_expr without the useless info filtered into second list *)
let sexpr_parse (s : string) : sexpr option =
  match string_parse (parse_expr ()) s with
  | Some (e, []) -> Some e
  | _ -> None

(* ****** to string section ****** *)

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
(* end of [CS320-2023-Fall-assigns-assign6.ml] *)