(*(a)*)
type 'a stream =
  | Cons of 'a * (unit -> 'a stream)

let rec maps f (Cons (x, xf)) =
  Cons (f x, fun () -> maps f (xf ()))

let rec iterates f x =
  Cons (x, fun () -> iterates f (f x))


(*(b)*)
let rec interleave (Cons (x, xf)) ys =
  Cons (x, fun () -> interleave ys (xf ()))

(*(c)*)
let rec powers n =
  Cons
    (n,
     fun () ->
       interleave
         (powers (5 * n))
         (interleave
            (powers (7 * n))
            (powers (9 * n))))

let all_powers =
  powers 1

(*(d)*)
let rec merge (Cons (x, xf)) (Cons (y, yf)) =
  if x < y then
    Cons (x, fun () -> merge (xf ()) (Cons (y, yf)))
  else if y < x then
    Cons (y, fun () -> merge (Cons (x, xf)) (yf ()))
  else
    Cons (x, fun () -> merge (xf ()) (yf ()))

let rec all_powers_sorted =
  Cons
    (1,
     fun () ->
       merge
         (maps (fun x -> 5 * x) all_powers_sorted)
         (merge
            (maps (fun x -> 7 * x) all_powers_sorted)
            (maps (fun x -> 9 * x) all_powers_sorted)))