(*(a)*)
(*
A queue is a first-in-first-out data structure. 
Elements are inserted at the rear of the queue and removed from the front. 
A simple list can represent a queue, but it is inefficient if we add elements at the end, because appending to the end of a list takes linear time. 
A better implementation represents a queue as a pair of lists: (front, rear) 
where `front` stores the front part of the queue in the correct order, and `rear` stores the rear part in reverse order. 
The reason this is efficient is that insertion at the rear can be done by `::` onto the reversed rear list. 
This reversal can take linear time, but it does not happen on every operation. 
Each element is put into the rear list once, and later reversed into the front list once. 
So although an individual operation can sometimes cost `O(n)`, the amortised cost of each queue operation is O(1).
*)


(*(b)*)
let head (x::xs) = x
let rec rl_encode = function
  | [] -> []
  | [x] -> [(1, x)]
  | x :: y :: tl ->
      if x = y then
        match rl_encode (y :: tl) with
        | (n, v) :: rest -> (n + 1, v) :: rest
      else
        (1, x) :: rl_encode (y :: tl)

(*type: 'a list -> (int * 'a) list*)

(*solution method: 
taking the first element of the current run and then counting how many times it repeats consecutively.*)
let rec rl_encode = function
  | [] -> []
  | x::xs ->
    let rec code (n, rest) =
      match rest with
      | [] -> [(n, x)]
      | y::ys -> 
        if x = y then code (n+1, ys)
        else (n, x) :: rl_encode (y::ys)
    in
    code (1, xs)


(*(c)*)
let rec genEquals n xs ys =
  match n, xs, ys with
  | n, [], [] -> true
  | n, [], ys -> List.length ys <= n 
  | n, xs, [] -> List.length xs <= n
  | n, x::xs, y::ys ->
    if x = y then genEquals n xs ys
    else n>0 && (genEquals (n-1) xs (y::ys) || genEquals (n-1) (x::xs) ys || genEquals (n-1) xs ys) 