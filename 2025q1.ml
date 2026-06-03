(*(a)*)
(*
- length is not recursive
- merge misses the case where the second list is empty
- split does not decrease n
- l1::h has wrong order
- mergesort has general case before base cases
*)


(*(b)*)
let rec check_sorted = function
  | [] -> true
  | [_] -> true
  | x::y::xs -> 
    x <= y && check_sorted (y::xs)

(*time O(n), space O(n) because it's not tail-recursive*)

(*(c)*)
exception Sort_failed
let rec checksort fs input =
  match fs with
  | [] -> ()
  | f::fs ->
    if check_sorted (f input) then checksort fs input
    else raise Sort_failed