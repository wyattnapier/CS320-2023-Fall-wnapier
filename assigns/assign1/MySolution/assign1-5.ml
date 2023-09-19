#use "./../../../classlib/OCaml/MyOCaml.ml";;

let char_str_concat (c: char) (str: string): string =
  string_init(string_length str + 1) (fun i -> if i = string_length str then c else string_get_at str (i))

let char2int (c: char): int = 
  ord c - ord '0'  (* Convert char to integer *)

let int2char (n: int): char = 
  chr(n + ord '0')

let rec in_out (xs: string) (i: int) (subseq: string): string =
  if i >= string_length xs then subseq
  else if i = 0 || string_length subseq = 0 || ord (string_get_at xs i) >= ord (string_get_at subseq (string_length subseq - 1)) then 
    let add = in_out xs (i + 1) (char_str_concat (string_get_at xs i) subseq) in
    let exclude = in_out xs (i + 1) (subseq) in
    if string_length add >= string_length exclude then add else exclude
  else 
    in_out xs (i + 1) (subseq)

let string_longest_ascend(xs: string): string = 
  in_out xs 0 ""
;;