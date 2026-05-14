(*(a)*)
type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree
(*A functional array can be represented using a binary tree. 
Each array position is a positive integer, and its path in the tree is determined by repeatedly looking at the binary representation of the index. 
Br (v, left, right) stores value v at the current node, with two subtrees.
The index 1 refers to the root. For an index > 1:
- If k is even, go to the left subtree and continue with k/2
- If k is odd, go to the right subtree and continue with k/2. 
So the path is determined by the bits of k. 
*)
(* lookup *)
exception Subscript
let rec sub = function
  | Lf, _ -> raise Subscript
  | Br (v, t1, t2), k -> 
    if k = 1 then v
    else if k mod 2 = 0 then sub (t1, k/2)
    else sub (t2, k/2)

(* update index k with value w *)
let rec update = function
  | Lf, k, w ->
    if k = 1 then Br (w, Lf, Lf)
    else raise Subscript
  | Br (v, t1, t2), k, w -> 
    if k = 1 then Br (w, t1, t2)
    else if k mod 2 = 0 then Br (v, update (t1, k/2, w), t2)
    else Br (v, t1, update (t2, k/2, w))

(*
lookup: O (log n) where n is the number of elements in the array. 
Each recursive call divides the index by 2, so the number of steps is proportional to the number of bits in the index.

update: O(log n) because it follows one root-to-node path and copies only that path, not the whole tree.
*)


(*(b)*)
let rec split = function
  | [] -> [], []
  | [x] -> [x], []
  | x::y::tl ->
    let odd, even = split tl in
    x::odd, y::even

let rec arrayoflist = function
  | [] -> Lf
  | x::xs -> 
    let l, r = split xs in
    Br (x, arrayoflist l, arrayoflist r)


(*(c)*)
let rec merge l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | x::xs, y::ys -> 
    if x <= y then x::(merge xs (y::ys))
    else y::(merge (x::xs) ys)

let rec filter p = function
  | Lf -> []
  | Br (v, t1, t2) -> 
    let lt1, lt2 = (List.map (fun x -> 2 * x) (filter p t1)), (List.map (fun x -> 2 * x + 1) (filter p t2)) in
    let merged = merge lt1 lt2 in
    if p v then 1::merged
    else merged