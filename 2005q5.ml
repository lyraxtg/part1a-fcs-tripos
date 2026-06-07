(*(b)*)
let median3 a b c =
  if (a <= b && b <= c) || (c <= b && b <= a) then b
  else if (b <= a && a <= c) || (c <= a && a <= b) then a
  else c


(*(c)*)
let rec nth xs n =
  match xs, n with
  | x::_, 0 -> x
  | _::xs', n -> nth xs' (n-1)
  | [], _ -> failwith "nth"

let choose_pivot xs =
  let n = List.length xs in
  let first = nth xs 0 in
  let middle = nth xs (n/2) in
  let last = nth xs (n-1) in
  median3 first middle last

let rec partition pivot xs =
  match xs with
  | [] -> ([], [], [])
  | x::xs' -> 
    let (less, equal, greater) = partition pivot xs' in
    if x < pivot then (x::less, equal, greater)
    else if x > pivot then (less, equal, x::greater)
    else (less, x::equal, greater)


let rec quicksort xs =
  match xs with
  | [] -> []
  | [_] -> xs
  | _ -> 
    let pivot = choose_pivot xs in
    let (less, equal, greater) = partition pivot xs in
    quicksort less @ equal @ quicksort greater