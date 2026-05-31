(*(a)*)
type marks = int list
type results = marks list

let rec fold f acc l =
  match l with
  | [] -> acc
  | x::xs -> fold f (f acc x) xs

let rec map f = function
  | [] -> []
  | x::xs -> (f x)::map f xs

let rec filter p = function
  | [] -> []
  | x::xs -> if p x then x::(filter p xs) else filter p xs


(*(b)*)
exception NotEnoughData
let filtered l = filter (fun x -> x <> 0) l
let length l = fold (fun acc x -> 1 + acc) 0 l
let total l = fold (+) 0 l
let mean m =
  match length (filtered m) with
  | 0 -> raise NotEnoughData
  | n -> (float_of_int (total m)) /. float_of_int n

let sd m =
  match length (filtered m) with 
  | n when n < 2 -> raise NotEnoughData
  | n -> 
    let xbar = mean m in
    let squares = map (fun x -> let d = float_of_int x -. xbar in d *. d) (filtered m) in
    let num = fold (+.) 0.0 squares in
    sqrt (num /. float_of_int (n-1))

type result =
  | MeanOnly of float
  | MeanAndSd of float * float

let analysis l =
  try
    let m = mean l in
    (try
      let s = sd l in
      Some (MeanAndSd (m, s))
    with _ ->
      Some (MeanOnly m)
      )
  with _ ->
    None

  
(*(c)*)
let rec nth i l =
  match i, l with
  | _, [] -> None
  | 0, x::xs -> Some x
  | n, x::xs -> nth (n-1) xs

let rec qmarks q results =
  match results with 
  | [] -> []
  | marks::rest ->
    match nth q marks with
    | None -> qmarks q rest 
    | Some x -> x::qmarks q rest

let qmean q results =
  mean (qmarks q results)

let qstd q results =
  sd (qmarks q results)