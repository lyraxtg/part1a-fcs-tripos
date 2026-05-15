(*(a)*)
(*
Lazy lists are lists whose tail is a function over type unit as delayed computations
This allows us to represent infinite sequences, because we do not compute the whole list at once.
Cons (x, fun () -> E) the expression E is not evaluated immediately. It is only evaluated when the function is called with (). 
*)

type 'a seq =
  | Nil
  | Cons of 'a * (unit -> 'a seq)

let tail (Cons (_, xf)) = xf ()

let pos_int =
  let rec from k =
    Cons (k, fun () -> from (k+1))
  in
  from 1

let rec mapq f (Cons (x, xf)) =
  Cons (f x, fun () -> mapq f (xf ()))


(*(b)*)
let head (Cons (x, _)) = x
let rec diag (Cons (x, xf))= 
  Cons (head x, fun () -> diag (mapq (fun s -> tail s) (xf ())))

(*(c)*)
let rec make f xq yq =
  let hx = head xq in
  let row1 = mapq (fun y -> f hx y) yq in
  Cons (row1, fun () -> make f (tail xq) yq)


(*(d)*)
let rec interleave xq yq = 
  Cons (head xq, fun () -> interleave yq (tail xq))
let rec flatten (Cons (x, xf)) = interleave x (flatten (xf ()))