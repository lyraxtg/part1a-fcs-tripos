type intset = (int * int) list

(*(a)*)
let rec is_standard = function
  | [] -> true
  | [(a, b)] -> a <= b
  | (a, b)::(c, d)::xs -> a <= b && b+1 < c && is_standard ((c, d) :: xs)

let rec add_interval (a, b) = function
  | [] -> [(a, b)]
  | (c, d)::xs ->
    if b+1 < c then (a, b)::(c, d)::xs
    else if d+1 < a then (c, d)::add_interval (a, b) xs
    else if b > d then add_interval (min a c, b) xs
    else (min a c, d)::xs

let rec standardize = function
  | [] -> []
  | (a, b)::xs -> add_interval (a, b) (standardize xs)

let equal s1 s2 = standardize s1 = standardize s2


(*(b)*)
let rec inter = function
  | s1, [] -> []
  | [], s2 -> []
  | (a, b)::xs, (c, d)::ys ->
    if d < a then inter ((a, b)::xs, ys)
    else if b < c then inter (xs, (c, d)::ys)
    else if d < b then (max a c, d)::inter ((a, b)::xs, ys)
    else if d > b then (max a c, b)::inter (xs, (c, d)::ys)
    else 
      (* d = b *)
      (max a c, b)::inter (xs, ys)