(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)

(* I know this is functionally the same as tons of if statements,
   but I have no idea how to do this without recursion/loops so I had to try something *)

let sort5 (a, b, c, d, e): int*int*int*int*int =
  match a, b, c, d, e with
  | (a, b, c, d, e) when a <= b ->
    (match b, c, d, e with
    | (b, c, d, e) when b <= c ->
      (match c, d, e with
      | (c, d, e) when c <= d ->
        (match d, e with
        | (d, e) when d <= e -> (a, b, c, d, e)
        | _ -> (a, b, c, e, d))
      | _ -> (a, b, d, c, e))
    | (b, c, d, e) when b <= d ->
      (match d, e with
      | (d, e) when d <= e -> (a, b, d, c, e)
      | _ -> (a, b, d, e, c))
    | _ -> (a, b, e, c, d))
  | (a, b, c, d, e) ->
    (match a, c, d, e with
    | (a, c, d, e) when a <= c ->
      (match c, d, e with
      | (c, d, e) when c <= d ->
        (match d, e with
        | (d, e) when d <= e -> (a, c, d, e, b)
        | _ -> (a, c, d, b, e))
      | _ -> (a, c, b, d, e))
    | (a, c, d, e) when a <= d ->
      (match d, e with
      | (d, e) when d <= e -> (a, d, c, e, b)
      | _ -> (a, d, c, b, e))
    | _ -> (a, e, c, d, b))



(* ************************************************ *)
