#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* use string_make_fwork *)
let string_merge(cs1)(cs2): string = 
  if string_length cs1 = 0 then cs2 else if string_length cs2 = 0 then cs1
  else 
    let combolen = string_length cs1 + string_length cs2 in
    let rec building (p1: int) (p2: int) (currString) = 
      if string_length currString = combolen then currString
      else if string_length cs1 <= p1 then building p1 (p2+1) (string_make_fwork (fun work -> (string_foreach(currString)(work); work(string_get_at cs2 p2))))
      else if string_length cs2 <= p2 then building (p1+1) p2 (string_make_fwork (fun work -> (string_foreach(currString)(work); work(string_get_at cs1 p1))))
      else if string_get_at cs1 p1 < string_get_at cs2 p2 then building (p1+1) p2 (string_make_fwork (fun work -> (string_foreach(currString)(work); work(string_get_at cs1 p1))))
      else building p1 (p2+1) (string_make_fwork (fun work -> (string_foreach(currString)(work); work(string_get_at cs2 p2))))
    in building 0 0 "";
;;