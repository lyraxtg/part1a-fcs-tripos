(*(a)*)
type 'a seq = 
  | Nil
  | Cons of 'a * (unit -> 'a seq)

(*b*)
let rec interleave = function
  | Nil, yq -> yq
  | Cons (x, xf), yq -> Cons (x, fun () -> interleave (yq, xf()))

(*(c)*)
let rec lmap f = function
  | Nil -> Nil
  | Cons (x, xf) -> Cons (f x, fun () -> lmap f (xf()))

(*(d)*)
let rec iterates f x = Cons (x, fun () -> iterates f (f x))

(*(e)*)
let rec iterates2 f g x y = interleave (lmap (fun m -> (x, m)) (iterates g y), iterates2 f g (f x) y)