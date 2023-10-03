(* use list make fwork 
  for every letter in the first word, loop through the alphabet and change the letter
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)

(* function to change just one character and make it into a string using string and index *)
(* do that for every character barring original character*)
(* loop through each character in the string using other step function *)
(* stick it all in the list *)

(* takes the char that we're making buddies of and the current index that we're changing char for and the char we're replacing *)
let replace_char(str: string)(i: int)(c: char): string =
  string_tabulate(string_length str) (fun j -> 
    if i = j then c else (string_get_at(str)(i))
  )
(* makes all strings of changing the nth char *)
let generate_strings(str: string)(i: int): 'a list =
  let string_alphabet = "abcdefghijklmnopqrstuvwxyz" in
  list_make_fwork (fun work -> 
      (int1_foreach (26) (fun j -> 
        let curr_char = string_get_at str i in
        let swap_char = string_get_at string_alphabet j in
        if curr_char != swap_char then work(replace_char str i swap_char)
      ))
    )
let
list_of_buddies(word: string): string list = 
  let res = list_make_fwork ( fun work ->
    (int1_foreach (string_length word) (fun i -> work(generate_strings(word)(i))))) 
  in list_concat (res)
;;