(*(a)*)
(*
- foldl: Start with an accumulator e, scan the list from left to right, and repeatedly update the accumulator using f.
- f: The outer foldl starts with accumulator 1. Each element of x is itself a list, and the inner foldl ( * ) multiplies all elements of that list into the accumulator.
- g: For each element z in zs, if p z is true, put z into the first list x. Otherwise, put z into the second list y. So g is splitting the list into two groups according to predicate p.
*)

(*(b)*)
(*Find the smallest element, remove it from the list, put it at the front of the answer, then sort the remaining list.*)
let rec find_min = function
  | [x] -> x
  | x::xs -> 
    let m = find_min xs in
    if x < m then x else m

let rec remove_min = function
  | [] -> []
  | x::xs -> 
    if x = find_min (x::xs) then xs else x::(remove_min xs)

let rec ssort = function
  | [] -> []
  | xs -> 
    let m = find_min xs in
    m :: ssort (remove_min xs)

(*more efficient way*)
let rec get_min = function
  | ([x], xs) -> (x, xs) (*(current_list, removed_items)*)
  | (x::y::ys, xs) ->
    if y < x then get_min (y::ys, x::xs)
    else get_min (x::ys, y::xs)

let rec ssort = function
  | [] -> []
  | l ->
    let (m, ys) = get_min (l, []) in
    m::ssort ys

(* O(n^2). For a list of length n, getmin scans through the whole list once, then ssort calls getmin once for each element*)


(*(c)*)
let rec range i j =
  if i > j then []
  else i::(range (i+1) j)

let mult_table n = 
  let rn = range 1 n in
  List.map
    (fun i ->
      List.map 
      (fun j -> i*j) 
      rn)
    rn

(*(d)*)
let table3 f n =
  List.map
    (fun i ->
       List.map
         (fun j ->
            List.map
              (fun k -> f i j k)
              (range 1 n))
         (range 1 n))
    (range 1 n)