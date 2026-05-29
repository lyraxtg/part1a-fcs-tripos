(*(a)*)
type 'a llists = 'a list list

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

let rec createl w m = 
  if List.length m < w then []
  else 
  (take w m) :: (createl w (drop w m))

let rec getl r c m = List.nth (List.nth m r) c
(* O(W+H)*)


(*(b)*)
type 'a aarrays = 'a array array

let createa w m =
  let make_row r = Array.init w (fun i -> List.nth m (r*w + i)) in
  Array.init (List.length m / w) (fun i -> make_row i)

let geta r c m = Array.get (Array.get m r) c
(*O(1)*)


(*(c)*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
exception Subscript;;
let rec update = function
| Lf, k, w ->
if k = 1 then
Br (w, Lf, Lf)
else
raise Subscript
| Br (v, t1, t2), k, w ->
if k = 1 then
Br (w, t1, t2)
else if k mod 2 = 0 then
Br (v, update (t1, k / 2, w), t2)
else
Br (v, t1, update (t2, k / 2, w));;
let rec sub = function
| Lf, _ -> raise Subscript
| Br (v, t1, t2), 1 -> v
| Br (v, t1, t2), k when k mod 2 = 0 -> sub (t1, k / 2)
| Br (v, t1, t2), k -> sub (t2, k / 2);;


type 'a ttree = 'a tree tree

let rec split = function
  | [] -> [], []
  | [x] -> [x], []
  | x::y::xs -> 
    let lefts, rights = split xs in
    x::lefts, y::rights

let createt w m = 
  (* create_trees takes a list a construct a functional array with list elements *)
  let rec create_trees = function
    | [] -> Lf
    | x::xs -> 
      let lefts, rights = split xs in Br (x, create_trees lefts, create_trees rights)
  in
  create_trees (List.map create_trees (createl w m))

let rec gett r c m = sub (sub (m, r+1), c+1)
(*O(lgH + lgW)*)