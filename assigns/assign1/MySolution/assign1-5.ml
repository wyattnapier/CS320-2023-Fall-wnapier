(* if i != 0 && currChar > currChar - 1 then streak++ and if streak > max then max = streak and streakEndIndex = i;
else i++;
return substring of streak
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_longest_ascend(xs: string): string = 
  substring xs 2 2
;;