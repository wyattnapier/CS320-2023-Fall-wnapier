#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* grab interp2 code and push stuff through it potentially *)
#use "./../../interp2/MySolution/interp2.ml";;

(*

Please implement the [compile] function following the
specifications described in CS320_Fall_2023_Project-3.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* ------------------------------------------------------------ *)

(* abstract syntax tree of high-level language *)

type uopr =
  | Neg | Not

type bopr =
  | Add | Sub | Mul | Div | Mod
  | And | Or
  | Lt  | Gt  | Lte | Gte | Eq

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | UOpr of uopr * expr
  | BOpr of bopr * expr * expr
  | Var of string
  | Fun of string * string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Seq of expr * expr
  | Ifte of expr * expr * expr
  | Trace of expr

(* ------------------------------------------------------------ *)

(* combinator for left-associative operators *)

let chain_left (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* init = p in
  let* fms = many (let* f = q in let* m = p in pure (f, m)) in
  let m = list_foldleft fms init (fun acc (f, m) -> f acc m) in
  pure m

let rec chain_right (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* m = p in
  (let* f = q in
   let* rest = chain_right p q in
   pure (f m rest)) <|> 
  (pure m)

let opt (p : 'a parser) : 'a option parser =
  (let* x = p in pure (Some x)) <|> pure None

(* basic constants *)

let parse_int : expr parser =
  let* n = natural in
  pure (Int n) << whitespaces

let parse_bool : expr parser =
  (keyword "true" >> pure (Bool true)) <|>
  (keyword "false" >> pure (Bool false))

let parse_unit : expr parser =
  keyword "(" >> keyword ")" >> pure Unit

(* names *)

let isReserved s =
  let reserved = 
    ["let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "trace"; "mod"; "not"] 
  in
  list_exists reserved (fun s0 -> s0 = s)

let parse_name : string parser =
  let lower = satisfy char_islower in
  let upper = satisfy char_isupper in
  let digit = satisfy char_isdigit in
  let quote = char '\'' in
  let wildc = char '_' in
  let* c = lower <|> wildc in
  let* cs = many (lower <|> upper <|> digit <|> wildc <|> quote) in
  let s = string_make_fwork (list_foreach (c :: cs)) in
  if isReserved s then fail
  else pure s << whitespaces

(* unary operators *)

let parse_neg : (expr -> expr) parser =
  keyword "-" >> pure (fun m -> UOpr (Neg, m))

(* binary operators *)

let parse_add : (expr -> expr -> expr) parser =
  keyword "+" >> pure (fun m n -> BOpr (Add, m, n))

let parse_sub : (expr -> expr -> expr) parser =
  keyword "-" >> pure (fun m n -> BOpr (Sub, m, n))

let parse_mul : (expr -> expr -> expr) parser =
  keyword "*" >> pure (fun m n -> BOpr (Mul, m, n))

let parse_div : (expr -> expr -> expr) parser =
  keyword "/" >> pure (fun m n -> BOpr (Div, m, n))

let parse_mod : (expr -> expr -> expr) parser =
  keyword "mod" >> pure (fun m n -> BOpr (Mod, m, n))

let parse_and : (expr -> expr -> expr) parser =
  keyword "&&" >> pure (fun m n -> BOpr (And, m, n))

let parse_or : (expr -> expr -> expr) parser =
  keyword "||" >> pure (fun m n -> BOpr (Or, m, n))

let parse_lt : (expr -> expr -> expr) parser =
  keyword "<" >> pure (fun m n -> BOpr (Lt, m, n))

let parse_gt : (expr -> expr -> expr) parser =
  keyword ">" >> pure (fun m n -> BOpr (Gt, m, n))

let parse_lte : (expr -> expr -> expr) parser =
  keyword "<=" >> pure (fun m n -> BOpr (Lte, m, n))

let parse_gte : (expr -> expr -> expr) parser =
  keyword ">=" >> pure (fun m n -> BOpr (Gte, m, n))

let parse_eq : (expr -> expr -> expr) parser =
  keyword "=" >> pure (fun m n -> BOpr (Eq, m, n))

let parse_neq : (expr -> expr -> expr) parser =
  keyword "<>" >> pure (fun m n -> UOpr (Not, BOpr (Eq, m, n)))

let parse_seq : (expr -> expr -> expr) parser =
  keyword ";" >> pure (fun m n -> Seq (m, n))

(* expression parsing *)

let rec parse_expr () = 
  let* _ = pure () in
  parse_expr9 ()

and parse_expr1 () : expr parser = 
  let* _ = pure () in
  parse_int <|> 
  parse_bool <|> 
  parse_unit <|>
  parse_var () <|>
  parse_fun () <|>
  parse_letrec () <|>
  parse_let () <|>
  parse_ifte () <|>
  parse_trace () <|>
  parse_not () <|>
  (keyword "(" >> parse_expr () << keyword ")")

and parse_expr2 () : expr parser =
  let* m = parse_expr1 () in
  let* ms = many' parse_expr1 in
  let m = list_foldleft ms m (fun acc m -> App (acc, m)) in
  pure m

and parse_expr3 () : expr parser =
  let* f_opt = opt parse_neg in
  let* m = parse_expr2 () in
  match f_opt with
  | Some f -> pure (f m)
  | None -> pure m

and parse_expr4 () : expr parser =
  let opr = parse_mul <|> parse_div <|> parse_mod in
  chain_left (parse_expr3 ()) opr

and parse_expr5 () : expr parser =
  let opr = parse_add <|> parse_sub in
  chain_left (parse_expr4 ()) opr

and parse_expr6 () : expr parser =
  let opr = 
    parse_lte <|> 
    parse_gte <|>
    parse_neq <|>
    parse_lt <|> 
    parse_gt <|>
    parse_eq
  in
  chain_left (parse_expr5 ()) opr

and parse_expr7 () : expr parser =
  chain_left (parse_expr6 ()) parse_and

and parse_expr8 () : expr parser =
  chain_left (parse_expr7 ()) parse_or

and parse_expr9 () : expr parser =
  chain_right (parse_expr8 ()) parse_seq

and parse_var () : expr parser =
  let* x = parse_name in
  pure (Var x)

and parse_fun () : expr parser =
  let* _ = keyword "fun" in
  let* xs = many1 parse_name in 
  let* _ = keyword "->" in
  let* body = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure m

and parse_let () : expr parser =
  let* _ = keyword "let" in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (x, m, n))

and parse_letrec () : expr parser =
  let* _ = keyword "let" in
  let* _ = keyword "rec" in
  let* f = parse_name in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (f, Fun (f, x, m), n))

and parse_ifte () : expr parser =
  let* _ = keyword "if" in
  let* m = parse_expr () in
  let* _ = keyword "then" in
  let* n1 = parse_expr () in
  let* _ = keyword "else" in
  let* n2 = parse_expr () in
  pure (Ifte (m, n1, n2))

and parse_trace () : expr parser =
  let* _ = keyword "trace" in
  let* m = parse_expr1 () in
  pure (Trace m) 

and parse_not () : expr parser =
  let* _ = keyword "not" in
  let* m = parse_expr1 () in
  pure (UOpr (Not, m))

exception SyntaxError
exception UnboundVariable of string

type scope = (string * string) list

let new_var =
  let stamp = ref 0 in
  fun x ->
    incr stamp;
    let xvar = string_filter x (fun c -> c <> '_' && c <> '\'') in
    string_concat_list ["v"; xvar; "i"; string_of_int !stamp]

let find_var scope s =
  let rec loop scope =
    match scope with
    | [] -> None
    | (s0, x) :: scope ->
      if s = s0 then Some x
      else loop scope
  in loop scope

let scope_expr (m : expr) : expr = 
  let rec aux scope m =
    match m with
    | Int i -> Int i
    | Bool b -> Bool b
    | Unit -> Unit
    | UOpr (opr, m) -> UOpr (opr, aux scope m)
    | BOpr (opr, m, n) -> 
      let m = aux scope m in
      let n = aux scope n in
      BOpr (opr, m, n)
    | Var s -> 
      (match find_var scope s with
       | None -> raise (UnboundVariable s)
       | Some x -> Var x)
    | Fun (f, x, m) -> 
      let fvar = new_var f in
      let xvar = new_var x in
      let m = aux ((f, fvar) :: (x, xvar) :: scope) m in
      Fun (fvar, xvar, m)
    | App (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      App (m, n)
    | Let (x, m, n) ->
      let xvar = new_var x in
      let m = aux scope m in
      let n = aux ((x, xvar) :: scope) n in
      Let (xvar, m, n)
    | Seq (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      Seq (m, n)
    | Ifte (m, n1, n2) ->
      let m = aux scope m in
      let n1 = aux scope n1 in
      let n2 = aux scope n2 in
      Ifte (m, n1, n2)
    | Trace m -> Trace (aux scope m)
  in
  aux [] m

(* ------------------------------------------------------------ *)

(* parser for the high-level language *)

let parse_prog (s : string) : expr =
  match string_parse (whitespaces >> parse_expr ()) s with
  | Some (m, []) -> scope_expr m
  | _ -> raise SyntaxError

let (@) = list_append
let(^) = string_append

(* 
MOD:
e1 - (e1 / e2 * e2)
Push 1; 
Push 2;
Push 1; 
Div;
Push 2;
Mul;
Swap;
Sub;
Trace;
*)

let rec expr_to_coms (x: expr): coms = 
  match x with
  | Int x -> [Push (Int x);]
  (* | UOpr(Neg, Int x) -> [Push (Int -x);] seems a bit sus change to UOpr? another case for negate *)
  (* | UOpr(Neg, e1) -> (- expr_to_coms e1) *)
  | Bool b -> [Push (Bool b);]  
  | Unit -> [Push (Unit);]
  | Var v -> [Push (Sym v);]
  | BOpr(Add, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Add;]
  | BOpr(Sub, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Sub;]
  | BOpr(Mul, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Mul;]
  | BOpr(Div, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Div;]
  | BOpr(Mod, e1, e2) -> (expr_to_coms (BOpr(Mul, e2, (BOpr (Div, e1, e2))))) @ (expr_to_coms e1) @ [Sub;] (* should be fixed *)
  | BOpr(Or, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Or;]
  | BOpr(And, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [And;]
  | UOpr(Not, e1) -> (expr_to_coms e1) @ [Not;] (* made UOpr *)
  | BOpr(Lt, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Lt;]
  | BOpr(Gt, e1, e2) -> (expr_to_coms e2) @ (expr_to_coms e1) @ [Gt;]
  | BOpr(Lte, e1, e2) -> (expr_to_coms (UOpr(Not, BOpr(Gt, e1, e2))))
  | BOpr(Gte, e1, e2) -> (expr_to_coms (UOpr(Not, BOpr(Lt, e1, e2))))
  | BOpr(Eq, e1, e2) -> (expr_to_coms (UOpr(Not, BOpr(Or, BOpr(Lt, e1, e2), BOpr(Gt, e1, e2)))))(* got rid of: @ [expr_to_coms e1] *)
  | Fun(s1, s2, ex) -> let param = [Push (Sym s2); Bind] @ (expr_to_coms ex) @ [Swap; Return;] in [Push (Sym s1); Fun param] (* interp2 solution uses "Ret" instead *)
  | App(e1, e2) -> (expr_to_coms e1) @ (expr_to_coms e2) @ [Swap; Call;]
  | Let(x, e1, e2) -> (expr_to_coms e1) @ [Push (Sym x);] @ [Bind;] @ (expr_to_coms e2)
  | Seq (e1, e2) -> (expr_to_coms e1) @ (expr_to_coms e2) (* 2 then 1 or 1 then 2? *)
  | Ifte(e1, e2, e3) -> let condition = (expr_to_coms e1) in 
                        let branchTrue = (expr_to_coms e2) in 
                        let branchFalse = (expr_to_coms e3) in 
                        condition @ [IfElse(branchTrue, branchFalse)]
  | Trace(e1) -> (expr_to_coms e1) @ [Trace; Pop;]

(* add "end" to this as well for function and ifte *)
let rec coms_to_slist (cs: coms) (sl: string list): string list =
  match cs with
  | [] -> sl
  | Push c :: xs -> let elem = "Push " ^ toString(c) ^ "; " in coms_to_slist(xs)(elem :: sl)
  | Pop :: xs -> let elem = ("Pop; ") in coms_to_slist(xs)(elem :: sl)
  | Add :: xs -> let elem = ("Add; ") in coms_to_slist(xs)(elem :: sl)
  | Sub :: xs -> let elem = ("Sub; ") in coms_to_slist(xs)(elem :: sl)
  | Mul :: xs -> let elem = ("Mul; ") in coms_to_slist(xs)(elem :: sl)
  | Div :: xs -> let elem = ("Div; ") in coms_to_slist(xs)(elem :: sl)
  | Or :: xs -> let elem = ("Or; ") in coms_to_slist(xs)(elem :: sl)
  | And :: xs -> let elem = ("And; ") in coms_to_slist(xs)(elem :: sl)
  | Not :: xs -> let elem = ("Not; ") in coms_to_slist(xs)(elem :: sl)
  | Lt :: xs -> let elem = ("Lt; ") in coms_to_slist(xs)(elem :: sl)
  | Gt :: xs -> let elem = ("Gt; ") in coms_to_slist(xs)(elem :: sl)
  | Swap :: xs -> let elem = ("Swap; ") in coms_to_slist(xs)(elem :: sl)
  | Bind :: xs -> let elem = ("Bind; ") in coms_to_slist(xs)(elem :: sl)
  | Lookup :: xs -> let elem = ("Lookup; ") in coms_to_slist(xs)(elem :: sl)
  | Call :: xs -> let elem = ("Call; ") in coms_to_slist(xs)(elem :: sl)
  | Return :: xs -> let elem = ("Trace; ") in coms_to_slist(xs)(elem :: sl)
  | Fun c :: xs -> let elem = ("Fun " ^ string_concat_list(list_reverse(coms_to_slist(c)([]))) ^" End ") in coms_to_slist(xs)(elem :: sl)
  | IfElse (t, f) :: xs -> let elem = ("If " ^ string_concat_list(list_reverse(coms_to_slist(t)([]))) ^" Else " ^ string_concat_list(list_reverse(coms_to_slist(f)([])))) ^ "End " in coms_to_slist(xs)(elem :: sl)
  | Trace :: xs -> let elem = ("Trace; ") in coms_to_slist(xs)(elem :: sl)
  (* | _ -> ["shat the bed" :: sl] *)

let compile (s : string) : string =
  let prog = parse_prog s in 
  (* let scoped = scope_expr prog in
  let matched_list = expr_to_coms scoped *)
  let matched_list = expr_to_coms prog
in string_concat_list(list_reverse(coms_to_slist(matched_list)([])))

(* no trace *)
let full (s: string) =
  let s1 = compile s in
  interp s1