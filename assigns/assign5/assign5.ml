#use "./../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../lectures/lecture-11-02/code-2.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)

(* discussion answer *)


(*auxiliary functions*)

let explode (s: string) : char list = 
  let rec expl i l = 
    if i < 0 then l 
    else expl (i - 1) (String.get s i :: l) in 
  expl (String.length s - 1) []

(*end auxiliary functions*)

let parse (s : string) : expr option = (* YOUR CODE *)
  parse_expr (explode s)
;;

let parse_expr (c: char list) : expr option =

;;

let rec parse_exprs (c: char list) : exprs option =
  match c with
  | c1 ::  :: cs -> 
;;

(* let rec parse_num (c: char list) : int option =
  match c with
  | 
  | [] ->  *)

(* takes an int and needs to return an int option *)
let parse_digit (c: char list) : int option =
  if is_digit c then Some (int_of_char c - 48) else None


  (* 
  let t = int_of_char x - 48 in
  if (0 <= t) && (t <= 9) then Some (Digit t) else None    
  *)
