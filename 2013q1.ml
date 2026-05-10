(*(a)*)
(*a datatype can be defined by giving a new type name and a set of constructors. 
A constructor is used to build values of that datatype, and later to take them apart by pattern matching.*)
(*Example:*)
type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree

(*Lf and Br are constructors. Lf constructs an empty tree, while Br (x, l, r) constructs a tree node containing value x, left subtree l, and right subtree r. 
The type is polymorphic, so the stored values may have any type 'a.
example:
Br (5,
    Br (2, Lf, Lf),
    Br (8, Lf, Lf))

Pattern matching allows functions to define different behaviour depending on the shape of their argument. 
For example,
*)
let rec size t =
  match t with
  | Lf -> 0
  | Br (x, left, right) ->
      1 + size left + size right
(* matches Lf in the empty case and Br (x, l, r) in the non-empty case. 
This is recursive pattern matching for counting nodes*)

let describe_good t =
  match t with
  | Lf -> "empty tree"
  | Br (x, Lf, Lf) -> "single-node tree"
  | Br (x, Lf, right) -> "node with only right subtree"
  | Br (x, left, Lf) -> "node with only left subtree"
  | Br (x, left, right) -> "node with two subtrees"
(*The order of patterns matters. 
OCaml tries patterns from top to bottom and uses the first one that matches.
So the more specific pattern must come first. If the general pattern came first, the more specific one would never be reached.
*)

(*(b)*)
let rec combine v tl1 tl2 =
  List.concat(
    List.map (fun l -> 
    List.map (fun r -> Br (v, l, r)) tl2) 
      tl1
  )

(*(c)*)
(*For each possible root, recursively generate all possible left subtrees and all possible right subtrees. and combine*)
(*split: Given a list, return all ways to choose one element as the root.
Each output triple contains (left_labels, root_label, right_labels)*)
let rec split = function
  | [] -> []
  | x::xs -> 
    ([], x, xs) :: List.map (fun (left, root, right) -> (x :: left, root, right)) (split xs)

let rec all_trees = function
  | [] -> [Lf]
  | xs -> 
    List.concat
    (List.map 
    (fun (left, root, right) -> combine root (all_trees left) (all_trees right))
    (split xs))
