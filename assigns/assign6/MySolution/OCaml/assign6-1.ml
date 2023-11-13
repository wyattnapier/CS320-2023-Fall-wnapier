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

(* type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

let rec digit () = 
  let* x = natural in 
    (if (0<=x) then pure (SInt x) << whitespaces else fail)
;; *)

(* let rec parse_digit e = 
  let* y = digit() in
  let* _ = whitespaces in
  pure (SAdd y)
  <|> digit() *)

(* let parse_expr e =
  string_parse (digit ()) e *)

(* ****** ****** *)

let rec parse_expr () : sexpr parser =
  parse_int () <|> parse_add () <|> parse_mul ()

and parse_int () : sexpr parser =
  let* n = natural in
  pure (SInt n) << whitespaces

and parse_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SAdd es)

and parse_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_expr in
  let* _ = keyword ")" in
  pure (SMul es)

let parse (s : string) : sexpr option =
  match string_parse (parse_expr ()) s with
  | Some (e, []) -> Some e
  | _ -> None


(* end of [CS320-2023-Fall-assigns-assign6.ml] *)