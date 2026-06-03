(*(a)*)
let is_prime = function
  | n when n < 2 -> false
  | n ->
    let rec test_div d = 
      if d * d > n then true
      else if n mod d = 0 then false
      else test_div (d+1)
    in 
    test_div 2

(*(b)*)
let rec fold_range a b f acc =
  if a > b then acc
  else fold_range (a+1) b f (f a acc)

let rec fold f acc l =
  match l with
  | [] -> acc
  | x::xs ->
    fold f (f acc x) xs

(*fold processes list from left to right. At each element it updates acc and then continues with the rest of the list
If the list has length n, then there is one recursive call per list element. 
Assuming f takes constant time, the total time complexity is O(n)
The function itself does not build a new list. It only carries the accumulator and moves through the input list.
Space complexity O(1)
It is tail recursive because the final operation in the recursive case is the recursive call itself.
There is no pending operation after recursive call so it's tail recursive
*)


(*(c)*)
let all_primes a b = 
  fold_range a b (fun x acc -> if is_prime x then x::acc else acc) []