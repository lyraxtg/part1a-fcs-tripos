(*(a)*)
(*
Functions can be treated like ordinary values. 
They can be passed as arguments, returned as results, stored in data structures and defined anontmously. 
For example, an anonymous function can be written as:
fun n -> n + 1
Functions can also use identifiers from their surrounding context. For example:
let add_k k =
  fun x -> x + k
Here add_k 3 returns a function that adds 3.

Two standard examples of functionals are map and exists. A functional is a function that takes another function as an argument or returns a function as a result.
*)

let rec map f = function
  | [] -> []
  | x::xs -> (f x)::map f xs

let rec exists p = function
  | [] -> false
  | x::xs -> p x || exists p xs


(*(b)*)
let rec zarg f (xs, e) =
  match xs with
  | [] -> e
  | x :: xs -> f (x, zarg f (xs, e))

let sum xs = zarg (fun (x, y) -> x+y) (xs, 0)

(*
zarg takes a function f, a list, and a base value, and recursively combines the list elements from right to left.
*)

(*(c)*)
type 'a vtree =
  | Lf of 'a 
  | Br of ('a vtree) list

(*not linear?*)
let rec flat t = 
  match t with
  | Lf x -> [x]
  | Br xs -> List.concat (List.map (fun s -> flat s) xs)

(*use accumulator*)
(*flatten tree t, then put the resulting labels in front of existing list xs*)
let rec flatten (t, xs) =
  match t with
  | Lf x -> x::xs
  | Br ts -> zarg flatten (ts, xs)

let flat t  = flatten (t, [])

let count x t =
  let rec counting (t, acc) =
    match t with
    | Lf s ->
        if x = s then acc + 1 else acc
    | Br ts ->
        zarg counting (ts, acc)
  in
  counting (t, 0)

(* 'a -> 'a vtree -> int*)