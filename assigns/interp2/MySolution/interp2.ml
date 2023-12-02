#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(*
SCOPE: implement static scope dynamically w/ the stack

when we are making our function definition, we are just logging it, but not running the code
   we run the code when we have  function call

function headers:
   fun <coms> End is the whole function
   formal parameter is not in the signature
      actual paarametr is the top element of the stack during call I think
   name of the function is the top element of the stack
   when the function definition is executed it will creaate a closure(?) on the stack

call:
   Call will use a clossure (for the function def) and 
   will also create a closure for the current closure in order to return from a function
   
*)

(*
GRAMMAR (<prog> is starting symbol):
<prog> ::= <coms>
<coms> ::= empty | <com>;<coms> // note each command ends with semi-colon
<com> ::= Push <const> | Pop | Trace
         | Add | Sub | Div | Mul
         | And | Or | Not
         | Lt | Gt
         | If <coms> Else <coms> End
         | Bind | Lookup
         | Fun <coms> End | Call | Return
<const> ::= <int> | <bool> | Unit | <sym>
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<nat> ::= <digit> | <digit><nat> // can use the thing from assign6 probably
<int> ::= <nat> | -<nat>
<bool> ::= True | False
<char> ::= a | b | ... | z
<sym> ::= <char> | <sym><char> | <sym><digit>
*)

(* ******** begin types ********* *)
type sym = string

and const = 
| Int of int | Bool of bool | Unit (* does sym go here or char? *)
| Sym of sym | Closure of sym * (sym * const) list * coms

and com = 
| Push of const | Pop | Swap | Trace
| Add | Sub | Mul | Div
| And | Or | Not
| Lt | Gt
(* have all of the above already implemented *)
| Bind | Lookup 
| Fun of coms (* match for End *) | IfElse of coms * coms (* match for End *)
| Call | Return

and coms = com list

(* ------------------------------------------------------------ *)

let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

let parse_sym = 
   let digit_test = satisfy char_isdigit in
   let char_test = satisfy char_islower in
   let* c = char_test in 
   let* rest = many (digit_test <|> char_test) in
   let result = string_make_fwork(list_foreach(c::rest)) in 
   pure(Sym result)

(**************************************** stopped working here *************************************************************)

let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_sym
let rec parse_com() = 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (let* _ = keyword "Fun" in
  let* c1 = many (parse_com() << keyword ";") in (* could this just be parse_coms xs? *)
  let* _ = keyword "End" in
  pure (Fun c1)) <|>
  (let* _ = keyword "If" in
  let* c1 = many (parse_com() << keyword ";") in
  let* _ = keyword "Else" in
  let* c2 = many (parse_com() << keyword ";") in
  let* _ = keyword "End" in 
  pure(IfElse (c1,c2))) <|> 
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return)
let parse_coms = many (parse_com() << keyword ";")

(* ------------------------------------------------------------ *)

(* interpreter *)

type stack = const list
type trace = string list
type environment = (sym * const) list
type prog = coms

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n

let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Sym s -> s
  | Closure (a, b, c) -> a (* should I use pattern matching instead? *)

let list_concatenation lst1 lst2 =
   let fwork work =
      list_foreach lst1 work;
      list_foreach lst2 work
   in
   list_make_fwork fwork
    

let rec eval (s : stack) (t : trace) (v: environment) (p : prog) : trace =
  match p with
  (* termination state returns the trace *)
   | [] -> t
   | Push c :: p0       (* PushStack *) -> eval (c :: s) t v p0
   | Pop :: p0 ->
     (match s with
     | _ :: s0          (* PopStack *) -> eval s0 t v p0
     | []               (* PopError *) -> eval [] ("Panic" :: t) [] [])
   | Swap :: p0 ->
      (match s with 
      | s0 :: s1 :: xs  (* SwapStack *) -> eval (s1 :: s0 :: xs) t v p0
      | []              (* SwapError1 *) -> eval [] ("Panic" :: t) [] []
      | s0 :: []        (* SwapError2 *) -> eval [] ("Panic" :: t) [] [])
   | Trace :: p0 ->
     (match s with
     | c :: s0          (* TraceStack *) -> eval (Unit :: s0) (toString c :: t) v p0
     | []               (* TraceError *) -> eval [] ("Panic" :: t) [] [])
   | Add :: p0 ->
     (match s with
     | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t v p0
     | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) [] [])
   | Sub :: p0 ->
     (match s with
     | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t v p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) [] [])
   | Mul :: p0 ->
     (match s with
     | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t v p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) [] [])
   | Div :: p0 ->
     (match s with
     | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) [] []
     | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t v p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) [] [])
   | And :: p0 ->
     (match s with
     | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t v p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) [] [])
   | Or :: p0 ->
     (match s with
     | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t v p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) [] [])
   | Not :: p0 ->
     (match s with
     | Bool a :: s0           (* NotStack  *) -> eval (Bool (not a) :: s0) t v p0
     | _ :: s0                (* NotError1 *) -> eval [] ("Panic" :: t) [] []
     | []                     (* NotError2 *) -> eval [] ("Panic" :: t) [] [])
   | Lt :: p0 ->
     (match s with
     | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t v p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) [] [])
   | Gt :: p0 ->
     (match s with
     | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t v p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) [] []
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) [] []
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) [] [])
   | IfElse (c1, c2) :: p0 -> 
      (match s with 
      | Bool true :: s0     (* IfElseStackTrue *) -> eval s0 t v (list_concatenation c1 p0) (* check logic *)
      | Bool false :: s0    (* IfElseStackFalse *) -> eval s0 t v (list_concatenation c2 p0) (* check logic *)
      | []                  (* IfElseError2 *) -> eval [] ("Panic" :: t) [] []
      | _                   (* IfElseError1 *) -> eval [] ("Panic" :: t) [] [])
   | Bind :: p0 ->
      (match s with
      | Sym x :: value :: xs  (* BindStack *) -> eval xs t ((x, value) :: v) p0
      | []                    (* BindError2 *) -> eval [] ("Panic" :: t) [] []
      | _ :: []               (* BindError3 *) -> eval [] ("Panic" :: t) [] []
      | _                     (* BindError4 *) -> eval [] ("Panic" :: t) [] [])
   | Lookup :: p0 ->
      (match s with 
      | Sym x :: s0 -> 
         let rec env_match (vs: environment) =
            match vs with
            | [] (* LookupError3 *) -> eval [] ("Panic" :: t) [] []
            | (name, value) :: rest -> if name = x then eval (value :: s0) t v p0
                                       else env_match rest
         in env_match v
      | []  (* LookupError2 *) -> eval [] ("Panic" :: t) [] []
      | _   (* LookupError2 *) -> eval [] ("Panic" :: t) [] [])
   | Fun c :: p0 -> 
      (match s with 
      | Sym x :: s0 (* FunStack *) -> let closure1 = Closure (x, v, c) in
                                      eval (closure1 :: s0) t v p0
      | []          (* FunError2 *) -> eval [] ("Panic" :: t) [] []
      | _           (* FunError1 *) -> eval [] ("Panic" :: t) [] [])
   | Call :: p0 ->
      (match s with 
      (* the entire next line is SO scuffed*)
      | Closure (f, vf, c) :: a :: s0 (* CallStack *) -> 
         eval [a :: (Sym "cc", v, p0) :: s0] t [(f, (f, vf c)) :: v] c
                                                         (* let ccSym = Sym "cc" in 
                                                         (match ccSym with
                                                         | Sym ccS -> let contClosure = Closure (ccS, v, p) in 
                                                                      let newEnvClosure = Closure (f, vf, c) in
                                                                      let newEnv = (f, newEnvClosure) in
                                                                      let newStack = Closure(ccS, v, p0) in 
                                                                      (* make newStack var? *)
                                                                      eval [a :: newStack :: s0] t [newEnv :: vf] c (* correct aarguments in right order, just type errors *)
                                                         | _ -> eval [] ("Expected sym" :: t) [] []) *)
      | []                          (* CallError2 *) -> eval [] ("Panic" :: t) [] []
      | s0 :: []                    (* CallError3 *) -> eval [] ("Panic" :: t) [] [])
   | _ ->  eval [] ("Nada mas" :: t) [] []

(* ------------------------------------------------------------ *)

(* putting it all together [input -> parser -> eval -> output] *)

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* ------------------------------------------------------------ *)

(* interp from file *)

(* let read_file (fname : string) : string =
  let fp = open_in fname in
  let s = string_make_fwork (fun work ->
      try
        while true do
          work (input_char fp)
        done
      with _ -> ())
  in
  close_in fp; s

let interp_file (fname : string) : string list option =
  let src = read_file fname in
  interp src *)

