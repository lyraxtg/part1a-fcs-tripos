(*(a)*)
let rec range x y =
  if y < x then []
  else x::range (x+1) y

let rec sieve ins out =
  match ins with
  | [] -> out
  | p::rest -> sieve (List.filter (fun n -> n mod p <> 0) rest) (p::out)

let primes n = List.rev (sieve (range 2 n) [])

(*(b)*)
let rec merge xs ys =
  match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | x::xs', y::ys' -> 
    if x < y then x::merge xs' (y::ys')
    else if y < x then y::merge (x::xs') ys'
    else x::merge xs' ys'

let rec distinct xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | _ -> 
    let k = List.length xs/2 in
    merge (distinct (List.take k xs)) (distinct (List.drop k xs))


(*(c)*)
exception Fail

let delPrefix x y =
  if String.starts_with ~prefix:x y then
    String.sub y (String.length x) (String.length y - String.length x)
  else
    raise Fail

let rec splice chunks w =
  match w with
  | "" -> []
  | _ -> 
    let rec scan cs =
      match cs with 
      | [] -> raise Fail
      | c::rest ->
        try
          c::splice chunks (delPrefix c w)
        with Fail ->
          scan rest
    in
    scan chunks