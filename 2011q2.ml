(*(b)*)
exception Olive
let cannot f x =
  try
    let _ = f x in
    false
  with
    | Olive -> true
    | _ -> false


(*(c)*)
type 'a tree =
  | Leaf of 'a
  | Branch of 'a tree * 'a tree

let rec bun (x, t) =
  match t with
  | Leaf y ->
      if x = y then raise Olive
      else Leaf y
  | Branch (t1, t2) ->
      Branch (bun (x, t1), bun (x, t2))

let cheese (x, t) =
  if cannot bun (x, t) then Leaf x
  else bun (x, t)

(*cheese: 'a * 'a tree -> 'a tree*)

(*bun searches through the leaves of the tree, compare each leaf value with x. 
If it finds a leaf equal to x, it raises Olive.
If it does not find, returns the original tree unchanged.

cheese means if x occurs in the tree, return Leaf x. If x does not occur in tree t, returns original tree t
*)
let rec contains x t =
  match t with
  | Leaf y -> x = y
  | Branch (t1, t2) -> contains x t1 || contains x t2

let cheese2 (x, t) =
  if contains x t then Leaf x else t