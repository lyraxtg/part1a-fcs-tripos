(*(a)*)
let rev l =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | x::xs -> aux xs (x::acc)
  in
  aux l []

(*(b)*)
(* insertion sort *)
let rec ins comp x l =
  match l with
  | [] -> [x]
  | y::ys -> 
    if comp x y <= 0 then x::y::ys
    else y::(ins comp x ys)

let rec insort comp l =
  match l with
  | [] -> []
  | x::xs -> ins comp x (insort comp xs)

(* mergesort *)
let rec take i = function
| [] -> []
| x::xs ->
if i > 0 then x :: take (i - 1) xs
else []

let rec drop i = function
| [] -> []
| x::xs ->
if i > 0 then drop (i-1) xs
else x::xs

let rec merge comp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | x::xs, y::ys ->
    if comp x y <= 0 then x::(merge comp xs (y::ys))
    else y::(merge comp (x::xs) ys)

let rec msort comp l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::xs -> 
    let k = List.length l / 2 in
    let left = msort comp (take k l) in
    let right = msort comp (drop k l) in
    merge comp left right

(* quicksort *)
let rec partition comp p l =
  match l with
  | [] -> ([], [])
  | x::xs ->
    let (left, right) = partition comp p xs in
    if comp x p <= 0 then (x::left, right)
    else (left, x::right)

let rec qsort comp l = 
  match l with
  | [] -> []
  | x::xs -> 
    let (left, right) = partition comp x xs in
    (qsort comp left) @ (x::(qsort comp right))


(*(c)*)
let rec rle_encode = function
  | [] -> []
  | x::xs ->
    let rec encode (n, rest) =
      match rest with
      | [] -> [(x, n)]
      | y::ys -> 
        if x = y then encode (n+1, ys)
        else (x, n) :: rle_encode (y::ys)
    in 
    encode (1, xs)

let rle_decode l = 
  let rec decode acc = function
    | [] -> acc
    | (x, n)::xs -> 
      if n = 0 then decode acc xs
      else decode (x::acc) ((x, n-1)::xs)
  in 
  rev (decode [] l)

(*(d)*)
let pair_comp (a, n1) (b, n2) = 
  if n1 < n2 then -1
  else if n1 = n2 then 0
  else 1

let freq_sort cmp l = 
  rle_decode (qsort pair_comp (rle_encode (qsort cmp l)))