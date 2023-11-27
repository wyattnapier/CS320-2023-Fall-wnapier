#use "./../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../assign0/MySolution/assign0-3.ml";;

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

(* INSTRUCTIONS
   any program that can't be derived from the grammar should return none
   end with a trace
   trace takes the constant at top of stack and turns it into a string in the trace
*)

(* ******** begin types ********* *)

type const = 
| Int of int | Bool of bool | Unit (* of unit *)

type com = 
| Push of const | Pop | Trace
| Add | Sub | Mul | Div
| And | Or | Not
| Lt | Gt (* | Empty *)

type coms = com list

(* ******** end types, begin parser ********* *)

let parse_int : int parser =
   (let* _ = char '-' in let* x = natural in pure (-x))
   <|> (let* x = natural in pure x) <|> fail
let parse_bool : bool parser =
   (let* _ = keyword "True" in pure (true))
   <|> (let* _ = keyword "False" in pure (false)) <|> fail
let parse_unit: unit parser = 
   let* _ = keyword "Unit" in pure () <|> fail
let parse_const : const parser = 
   (parse_int >>= fun i -> pure(Int i)) <|> (parse_bool >>= fun b -> pure(Bool b)) 
   <|> (parse_unit >>= fun u -> pure (Unit)) <|> fail

(* *********** ********** *)

let rec parse_com(): com parser =
   parse_push() <|> parse_pop () <|> parse_trace () <|>
   parse_add () <|> parse_sub () <|> parse_mul() <|> parse_div () <|>
   parse_and () <|> parse_or () <|> parse_not () <|>
   parse_lt () <|> parse_gt()
   (* <|> parse_empty () *)

and parse_push(): com parser = 
   let* _ = keyword "Push" in
   let* c = parse_const in
   pure (Push c)

and parse_pop (): com parser = 
   let* _ = keyword "Pop" in
   pure (Pop)

and parse_trace (): com parser = 
   let* _ = keyword "Trace" in
   pure (Trace)

and parse_add (): com parser = 
   let* _ = keyword "Add" in
   pure (Add)

and parse_sub (): com parser = 
   let* _ = keyword "Sub" in
   pure (Sub)

and parse_mul (): com parser = 
   let* _ = keyword "Mul" in
   pure (Mul)

and parse_div (): com parser = 
   let* _ = keyword "Div" in
   pure (Div)

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

(* and parse_empty () : com parser =
   let* _ = keyword "" in
   pure (Empty) *)


let rec parse_coms(): coms parser = 
   many (parse_com () << keyword ";") 
   
   (* parse_empty() *)

(* ******** end parser, begin match ********* *)

let toString (c: const): string =
   match c with
   | Int c -> int2str c (* from assign0-3 *)
   | Bool false -> "False"
   | Bool true -> "True"
   | Unit -> "Unit"

let rec match_coms (s: const list) (t: string list) (p:coms): string list = 
   match p with
   (* | [] -> "end of program ":: t (* handles the empty string - but invalid inputs that parse as None match here too hmmmm *) *)
   | [] -> t
   | com :: rest ->
      match com with
      | Push c -> (* pattern match on the Push constructor and handle the const value c *)
         begin
            match c with
            | Int i -> match_coms ((Int i) :: s) t rest
            | Bool b -> match_coms ((Bool b) :: s) t rest
            | Unit -> match_coms ((Unit) :: s) t rest
         end
      | Pop ->
         begin
            match s with
            (* | [] -> "pop from empty stack" :: t *)
            | c1 :: cs -> match_coms (cs) (t) (rest)
            | _ -> "Panic" :: t
         end
      | Trace ->
         begin
            match s with
            (* | [] -> "trace from empty stack" :: t *)
            | c1 :: cs -> match_coms (Unit :: cs) (toString(c1):: t) (rest)
            | _ -> "Panic" :: t
         end
      | Add ->
         begin
            match s with
            (* | _ :: [] -> "add from insufficient stack" :: t (* can lump in with other case later *) *)
            | Int i :: Int j :: cs -> match_coms (Int(i + j) :: cs) (t) (rest)
            (* | _ -> "add with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Sub ->
         begin
            match s with
            (* | _ :: [] -> "sub from insufficient stack" :: t (* can lump in with other case later *) *)
            | Int i :: Int j :: cs -> match_coms (Int(i - j) :: cs) (t) (rest)
            (* | _ -> "sub with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Mul ->
         begin
            match s with
            (* | _ :: [] -> "mul from insufficient stack" :: t (* can lump in with other case later *) *)
            | Int i :: Int j :: cs -> match_coms (Int(i * j) :: cs) (t) (rest)
            (* | _ -> "mul with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Div ->
         begin
            match s with
            (* | _ :: [] -> "div from insufficient stack" :: t (* can lump in with other case later *) *)
            | Int i :: Int j :: cs -> if j != 0 then match_coms (Int(i / j) :: cs) (t) (rest) else "div by 0" :: t
            (* | _ -> "div with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | And ->
         begin
            match s with
            (* | _ :: [] -> "and from insufficient stack" :: t (* can lump in with other case later *) *)
            | Bool a :: Bool b :: cs -> match_coms (Bool(a && b) :: cs) (t) (rest)
            (* | _ -> "and with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Or ->
         begin
            match s with
            (* | _ :: [] -> "or from insufficient stack" :: t (* can lump in with other case later *) *)
            | Bool a :: Bool b :: cs -> match_coms (Bool(a || b) :: cs) (t) (rest)
            (* | _ -> "or with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Not ->
         begin
            match s with
            (* | [] -> "not from insufficient stack" :: t (* can lump in with other case later *) *)
            | Bool a :: cs -> match_coms (Bool(not a) :: cs) (t) (rest)
            (* | _ -> "not with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Lt ->
         begin
            match s with
            (* | _ :: [] -> "lt from insufficient stack" :: t (* can lump in with other case later *)  *)
            | Int i :: Int j :: cs -> match_coms (Bool(i < j) :: cs) (t) (rest)
            (* | _ -> "lt with wrong types" :: t *)
            | _ -> "Panic" :: t
         end
      | Gt ->
         begin
            match s with
            (* | _ :: [] -> "gt from insufficient stack" :: t (* can lump in with other case later *)  *)
            | Int i :: Int j :: cs -> match_coms (Bool(i > j) :: cs) (t) (rest)
            (* | _ -> "gt with wrong types" :: t *)
            | _ -> "Panic" :: t
         end

(* **************************** ************************  *)

(* starting function to begin parsing // need to convert to string list *)
let interp (s : string) : string list option = (* YOUR CODE *)
 (* add an if to handle the empty string that removes whitespaces *)
   (* let empty = string_parse s << whitespaces in *)
   match string_parse (whitespaces) s with
   |Some(_,[]) -> Some([])
   |_ -> 
      begin
         match string_parse (parse_coms ()) s with
         | Some ([], _) -> None
         | Some (coms, _) -> Some(match_coms [] [] coms)
         | None -> None
      end
;;   

(* push with invalid const still doesn't return the right thing hmmmmm *)