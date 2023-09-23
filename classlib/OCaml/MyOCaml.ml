(* ****** ****** *)
(*
 MyOCaml.ml
 is a class library
 built for CS320, Fall, 2023
*)
(* ****** ****** *)
exception False;;
(* ****** ****** *)


(** Return the character with the given ASCII code. **)
let chr = Char.chr;;

(** return the ASCII code of the argument **)
let ord = Char.code;;

(** make n c is a string of length n with each index holding the character c **)
let str(c0) = String.make 1 c0;;

(** checks if a character is lowercase **)
let char_islower(ch: char) = (ch >= 'a' && ch <= 'z');;

(** checks if a character is uppercase **)
let char_isupper(ch: char) = (ch >= 'A' && ch <= 'Z');;

(** checks if a character is a alphabetical letter **)
let char_isletter(ch: char) = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');;

(** converts a character to lowercase if applicable **)
let char_tolower(ch: char) =
  if char_isupper(ch) then chr(ord(ch) - ord('A') + ord('a')) else ch
;;

(** converts a character to uppercase if applicable **)
let char_toupper(ch: char) =
  if char_islower(ch) then chr(ord(ch) - ord('a') + ord('A')) else ch
;;

(** converts int digit to a character **)
let char_of_digit (d0: int): char =
  let () = assert(d0 >= 0) in
    let () = assert(d0 <= 9) in 
      chr(ord('0') + d0)
;;(* end of [char_of_digit] *)


(** converts character to a digit**)
let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in 
      ord(ch) - ord('0')
;;(* end of [digit_of_char] *)

(* ****** TYPE ANNOTATIONS ****** *)

(** takes a collection 'xs and a predicate function ('x0 -> bool), and it returns a boolean value if all satisfy or not **)
type ('xs, 'x0) forall = 'xs -> ('x0 -> bool) -> bool

(** takes a generic collection 'xs and applies a function ('x0 -> unit) which may produce a side effect **)
type ('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

(** takes a generic collection 'xs and applies a function ('x0 -> unit) which may produce a side effect in reverse order **)
type ('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

(** takes a generic type 'xs and returns a list of values of type 'x0 **)
type ('xs, 'x0) listize = 'xs -> 'x0 list

(** takes a generic type 'xs and returns an array of values of type 'x0 **)
type ('xs, 'x0) arrnize = 'xs -> 'x0 array

(** takes a generic type 'xs and returns a list of values of type 'x0 in reverse order **)
type ('xs, 'x0) rlistize = 'xs -> 'x0 list

(** takes a generic type 'xs and returns an array of values of type 'x0 in reverse order **)
type ('xs, 'x0) rarrnize = 'xs -> 'x0 array

(** takes a collection 'xs and a function ('x0 -> 'y0) for transforming elements of type 'x0 into elements of type 'y0, 
    and it returns a list of the transformed elements. **)
type ('xs, 'x0, 'y0) map_list = 'xs -> ('x0 -> 'y0) -> 'y0 list

(** takes a collection 'xs and a function ('x0 -> 'y0) for transforming elements of type 'x0 into elements of type 'y0, 
    and it returns a list of the transformed elements in reverse order. **)
type ('xs, 'x0, 'y0) map_rlist = 'xs -> ('x0 -> 'y0) -> 'y0 list


type ('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

type ('xs, 'x0, 'r0) foldright = 'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0
(* ****** ****** *)

(** run a work() fxn n0 times starting from 0 and ending at n0 **)
let int1_foreach (n0:int) (work: int -> unit): unit =
  for i0 = 0 to n0-1 do work(i0) done
;;

(** run a work() fxn n0 times starting from n0 and ending at 0 **)
let int1_rforeach (n0:int) (work: int -> unit): unit = 
  for i0 = 0 to n0-1 do work(n0-1-i0) done
;;

(** init n f is a string of length n with index i holding the character f i (called in increasing index order). **)
let string_init = String.init;;

(** length s is the length (number of bytes/characters) of s **)
let string_length = String.length;;

(** get_at s i is the character at index i in s. This is the same as writing s.[i] **)
let string_get_at(cs:string)(i0:int): char = String.get cs i0;;

(* ****** ****** *)

(** get the first char *)
let string_head(cs:string):char = string_get_at(cs)(0)
(** get the rest of chars *)
let string_tail(cs) =
string_init(string_length(cs)-1)(fun i -> string_get_at(cs)(i+1))

(* ****** ****** *)

(** create a string c0+cs **)
let string_cons(c0: char)(cs: string): string = 
  string_init(string_length(cs) + 1)(
    fun i -> if i <= 0 then c0 else string_get_at cs (i-1)
  )
;;

(** create a string cs+c0 **)
let string_snoc(cs: string)(c0: char): string =
  let len = string_length(cs) in
    string_init(len + 1)(
      fun i -> if i < len then string_get_at (cs) (i) else c0
    )
;;

(** turn cs into an uppercase string **)
let string_toupper(cs: string): string =
  string_init(string_length(cs))(
    fun i0 -> char_toupper(string_get_at(cs)(i0))
  )
;;

(** turn cs into a lowercase string **)
let string_tolower(cs: string): string =
  string_init(string_length(cs))(
    fun i0 -> char_tolower(string_get_at(cs)(i0))
  )
;;

(** for each character of string cs from 0 to length cs, apply the work function **)
let string_foreach(cs: string)(work: char -> unit) =
  int1_foreach(string_length(cs))(
    fun i0 -> work(string_get_at(cs)(i0))
  )
;;

(** for each character of string cs from length cs to 0, apply the work function **)
let string_rforeach(cs: string)(work: char -> unit) =
  int1_rforeach(string_length(cs))(
    fun i0 -> work(string_get_at(cs)(i0))
  )
;;

(** init n f is a string of length n with index i holding the character f i (called in increasing index order) **)
let string_tabulate = String.init;;

(** returns a fresh array containing the elements of xs which is a list of generic a **)
let list_arrnize(xs: 'a list): 'a array = Array.of_list(xs)

(** adds the reverse of generic list xs to generic list ys: xs=[1;2;3] ys=[4;5;6] = [3;2;1;4;5;6] **)
let rec list_revapp(xs: 'a list)(ys: 'a list): 'a list =
  match xs with
  | [] -> ys
  | x1 :: xs -> list_revapp(xs)(x1 :: ys)
;;

(** reverses a generic list **)
let list_reverse(xs: 'a list): 'a list = list_revapp(xs)([]);;

(** iterate through all elements in generic list xs and run the test function on each element. If all pass then return true **)
let rec list_forall(xs: 'a list)(test: 'a -> bool): bool =
  (
    match xs with
    | [] -> true
    | x1 :: xs -> (
        test(x1) && list_forall(xs)(test)
      )
  )
;;

(** iterate through all elements in generic list xs and run the test fxn on eahc element. If one passes then return true **)
let rec list_exists(xs: 'a list)(test: 'a -> bool): bool =
  (
    match xs with
    | [] -> false
    | x1 :: xs -> (
        test(x1) || list_exists(xs)(test)
      )
  )
;;

(** iterate through each element of generic list and apply the work function to that element **)
let rec list_foreach(xs: 'a list) (work: 'a -> unit): unit =
  (
    match xs with
    | [] -> ()
    | x1 :: xs -> (
        work(x1); list_foreach(xs)(work)
      )
  )
;;

(** iterate through each element of generic list and apply the work function to that element in reverse order **)
let rec list_rforeach(xs: 'a list) (work: 'a -> unit): unit =
  list_foreach(list_reverse(xs))(work)
;;

(** the forall_to_foreach function takes a forall function and converts it into a foreach function that applies a 
    given action to each element in the collection while ensuring that all elements are processed **)
let forall_to_foreach(forall: ('xs, 'x0) forall): ('xs, 'x0) foreach =
  fun(xs)(work) -> let _ = forall(xs)(fun(x0) -> (work(x0); true)) in ()
;;

(** **)
let foreach_to_forall(foreach: ('xs, 'x0) foreach): ('xs, 'x0) forall =
  fun(xs)(test) -> 
    try
      let() = foreach(xs)(fun(x0) -> if test(x0) then () else raise False)
    in( true ) with False(*void*) -> (false) 
;;(* end of [foreach_to_forall]: let *)

(** **)
let foreach_to_foldleft(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0 =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
      foreach(xs)(fun(x0) -> res := fopr(!res)(x0));
    !res
;;(* end of [foreach_to_foldleft]: let *)

(** **)
let rec
foreach_to_map_list(foreach: ('xs, 'x0) foreach): ('xs, 'x0, 'y0) map_list =
fun(xs)(fopr) ->
list_reverse(foreach_to_map_rlist(foreach)(xs)(fopr)) 
and 
foreach_to_map_rlist(foreach: ('xs, 'x0) foreach): ('xs, 'x0, 'y0) map_rlist =
fun(xs)(fopr) -> 
let res = ref([]) in
foreach(xs)(fun(x0) -> res := fopr(x0) :: !res); !res
;;(* end of [foreach_to_map_rlist]: let *)

(** **)
let rec foreach_to_listize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) listize =
  fun(xs) -> foreach_to_map_list(foreach)(xs)(fun x -> x)
;;

(** **)
let rec foreach_to_rlistize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) rlistize =
  fun(xs) -> foreach_to_map_rlist(foreach)(xs)(fun x -> x)
;;

(** **)
let rec foreach_to_arrnize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) arrnize =
  (
    fun xs -> list_arrnize(foreach_to_listize(foreach)(xs))
  )
;;

(** **)
let rec foreach_to_rarrnize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) rarrnize =
  (
    fun xs -> list_arrnize(foreach_to_rlistize(foreach)(xs))
  )
;;

(** **)
let rec foreach_to_length(foreach: ('xs, 'x0) foreach): 'xs -> int =
  foldleft_to_length(foreach_to_foldleft(foreach))
  and
  foldleft_to_length(foldleft: ('xs,'x0,'r0) foldleft): 'xs -> int = 
  (
    fun(xs) -> foldleft(xs)(0)(fun(r0)(x0) -> r0+1)
  )
;;

(** **)
let rforeach_to_foldright(rforeach: ('xs, 'x0) rforeach): ('xs, 'x0, 'r0) foldright =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
      rforeach(xs) (fun(x0) -> res := fopr(x0)(!res));
    !res
;;(* end of [rforeach_to_foldright]: let *)

(** **)
let int1_forall(n0) =
  foreach_to_forall(int1_foreach)(n0)
;;

(** **)
let string_forall(cs) =
  foreach_to_forall(string_foreach)(cs)
;;

(** **)
let int1_listize(n0) =
  foreach_to_listize(int1_foreach)(n0)
;;
let int1_rlistize(n0) =
  foreach_to_rlistize(int1_foreach)(n0)
;;

(** **)
let string_listize(cs) =
  foreach_to_listize(string_foreach)(cs)
;;

(** **)
let string_rlistize(cs) =
  foreach_to_rlistize(string_foreach)(cs)
;;

(** **)
let int1_foldleft(n0) =
  foreach_to_foldleft(int1_foreach)(n0)
;;

(** **)
let list_foldleft(xs) =
  foreach_to_foldleft(list_foreach)(xs)
;;

(** **)
let string_foldleft(cs) =
  foreach_to_foldleft(string_foreach)(cs)
;;

(** **)
let int1_foldright(n0) =
  rforeach_to_foldright(int1_rforeach)(n0)
;;

(** **)
let list_foldright(xs) =
  rforeach_to_foldright(list_rforeach)(xs)
;;

(** **)
let string_foldright(cs) =
  rforeach_to_foldright(string_rforeach)(cs)
;;

(* ****** ****** *)

(*
  let foreach_to_foldright(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0 =
    fun xs r0 fopr -> 
      let xs = foreach_to_rlistize(foreach)(xs) in
        list_foldleft(xs)(r0)(fun r0 x0 -> fopr x0 r0)
*)

(* ****** ****** *)

(** transforms the work done by fwork into a list. **)
let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = (res := (x0 :: !res))
    in(*let*)(fwork(work); list_reverse(!res) )
;;

let list_make_filter(test: 'x0 -> bool)(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) =
      if test(x0) then (res := (x0 :: !res))
      in(*let*) (fwork(work); list_reverse(!res))
;;

(* ****** ****** *)

(** transforms the work done by fwork into a list in reverse order. **)
let list_rmake_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = (res := (x0 :: !res)) in (fwork(work); !res)
;;

let list_rmake_filter(test: 'x0 -> bool)(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = if test(x0) then (res := (x0 :: !res)) in (fwork(work); !res)
;;

(* ****** ****** *)

(** The result of the entire expression is a string that represents the characters processed by the fwork function **)
let string_make_fwork(fwork: (char -> unit) -> unit): string =
  let xs =
    Array.of_list(list_make_fwork(fwork)) 
  in String.init (Array.length(xs)) (fun i -> xs.(i))
;;

(** **)
let string_rmake_fwork(fwork: (char -> unit) -> unit): string =
  let xs = 
    Array.of_list(list_rmake_fwork(fwork)) 
  in String.init (Array.length(xs)) (fun i -> xs.(i))
;;

(* ****** ****** *)

(** appends two lists together: [1;2;3] [4;5;6] = [1;2;3;4;5;6]**)
let list_append(xs: 'a list)(ys: 'a list): 'a list =
  list_make_fwork(
    fun work -> (list_foreach xs work; list_foreach ys work)
  )
;;

(** takes in a list of lists and returns one list with all the lists concatenated: [[1;2] ; [3;4]] = [1;2;3;4] **)
let list_concat(xss: 'a list list): 'a list = 
  list_make_fwork(
    fun work -> list_foreach xss (fun xs -> list_foreach xs work)
  )
;;

(* ****** ****** *)

let string_filter
(cs: string)(test: char -> bool) =
string_make_fwork
(fun work -> string_foreach(cs)(fun c -> if test(c) then work(c)))
;;

(* ****** ****** *)

let string_append(xs: string)(ys: string): string =
  string_make_fwork(
    fun work -> (string_foreach xs work; string_foreach ys work)
  )
;;

(** takes a list of strings and gives a string with the strings concatenated **)
let string_concat_list(css: string list): string =
  string_make_fwork(
    fun work -> list_foreach css (fun cs -> string_foreach cs work)
  )
;;

(* ****** ****** *)

(* end of [CS320-2023-Fall-classlib-MyOCaml.ml] *)
