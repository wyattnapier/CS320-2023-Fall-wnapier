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
let list_alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
let string_alphabet = "abcdefghijklmnopqrstuvwxyz"
list_make_fwork()