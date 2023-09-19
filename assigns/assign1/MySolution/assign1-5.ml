(* if i != 0 && currChar > currChar - 1 then streak++ and if streak > max then max = streak and streakEndIndex = i;
else i++;
return substring of streak
*)

#use "./../../../classlib/OCaml/MyOCaml.ml";;

let char_str_concat (c: char) (str: string): string =
  string_init(string_length str + 1) (fun i -> if i = string_length str then c else string_get_at str (i))
let string_longest_ascend(xs: string): string = 
  let rec run_finder (i: int) (max_run: string) (new_run: string):string = 
    if i >= string_length xs then
      if string_length new_run > string_length max_run then new_run else max_run (* error when the two are same length - last one is one short *)
    else 
      let curr = string_get_at xs i in
      let prev = if i = 0 then '0' else string_get_at xs (i - 1) in
        if curr >= prev || i = 0 then 
          let updated_new_run = char_str_concat curr new_run in
          if string_length new_run > string_length max_run then
            run_finder (i+1)(updated_new_run)(updated_new_run)
          else
            run_finder (i+1) (max_run)(updated_new_run)
        else 
          run_finder (i+1) (max_run)("")
  in run_finder 0 "" ""
;;