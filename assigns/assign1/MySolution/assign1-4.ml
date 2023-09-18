#use "./../../../classlib/OCaml/MyOCaml.ml";;

let str_concat (str1: string) (str2: string): string = 
  string_init(string_length str1 + string_length str2) (fun i ->
    if i < string_length str1 then string_get_at str1 i
    else string_get_at str2 (i - string_length str1)
  )

let char_str_concat (c: char) (str: string): string =
  string_init(string_length str + 1) (fun i -> if i = 0 then c else string_get_at str (i - 1))

let char2int (c: char): int = 
  ord c - ord '0'  (* Convert char to integer *)

let int2char (n: int): char = 
  chr(n + ord '0')

let str_no_last (str :string): string = 
  string_init(string_length str - 1) (fun i -> string_get_at str i)

let intrep_add(ds1:string )(ds2: string): string =
  let rec loop (r1: string) (r2: string) (acc: string) (carry: int): string =
    (* if "", "" *)
    if string_length r1 <= 0 && string_length r2 <= 0 then
      if carry > 0 then char_str_concat (int2char carry) acc
      else acc

    (* if "", r2 *)
    else if string_length r1 <= 0 then
      let lastchar2 = string_get_at r2 (string_length r2 - 1) in
      if carry > 0 then
        let sum = char2int lastchar2 + carry in
        let carry = sum / 10 in 
        let digit  = sum mod 10 in
        loop (r1) (str_no_last r2) (char_str_concat (int2char digit) acc) (carry)
      else loop (r1) (str_no_last r2) (char_str_concat (lastchar2 ) acc) (carry)
    (* if r1, "" *)
    else if string_length r2 <= 0 then
      let lastchar1 = string_get_at r1 (string_length r1 - 1) in
      if carry > 0 then
        let sum = char2int lastchar1 + carry in
        let carry = sum / 10 in 
        let digit  = sum mod 10 in
        loop (str_no_last r1) (r2) (char_str_concat (int2char digit) acc) (carry)
      else loop (str_no_last r1) (r2) (char_str_concat (lastchar1 ) acc) (carry)
    (* if r1, r2 *)
    else
      let lastchar1 = string_get_at r1 (string_length r1 - 1) in
      let lastchar2 = string_get_at r2 (string_length r2 -1) in
      let sum = char2int lastchar1 + char2int lastchar2 + carry in
      let carry = sum / 10 in 
      let digit  = sum mod 10 in
      loop (str_no_last r1) (str_no_last r2) (char_str_concat (int2char digit) acc) (carry)
  in loop ds1 ds2 "" 0
;;
    (* 
    cases:
    match r1, r2 with
      | "", "" -> if carry > 0 then str_concatacc
      | "", r2 -> add carry and other thing, same sorta vibe
      | r1, "" -> ^
      | r1, r2 -> do the normal summing thing
    *)
  


  