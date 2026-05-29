(*(a)*)
type iseq = Nil
| Cons of int * (unit -> iseq)

type itree = Leaf of int
| Branch of itree * itree

let rec merge2 = function
  | Nil, s2 -> s2
  | s1, Nil -> s1
  | Cons (x, xs), Cons (y, ys) ->
      if x <= y then
        Cons (x, fun () -> merge2 (xs (), Cons (y, ys)))
      else
        Cons (y, fun () -> merge2 (Cons (x, xs), ys ()))


(*(b)*)
let rec equal_seq = function
  | Nil, Nil -> true
  | Nil, _ -> false
  | _, Nil -> false
  | Cons (x, xs), Cons (y, ys) ->
      if x = y then
        equal_seq (xs (), ys ())
      else
        false

let rec s1 = Cons (1, fun () -> s1)
let s2 = s1

(*(c)*)
let rec append = function
  | Nil, s2 -> s2
  | s1, Nil -> s1
  | Cons (x, xs), s2 -> Cons (x, fun () -> append (xs (), s2))

let rec fringe = function
  | Leaf x -> Cons (x, fun () -> Nil)
  | Branch (t1, t2) -> append ((fringe t1), (fringe t2))

let equal_fringes t1 t2 = equal_seq (fringe t1, fringe t2)