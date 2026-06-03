(*(a)*)
type expr =
| Add of expr * expr
| Mul of expr * expr
| Number of int

let e = Mul (Add (Number 1, Number 4), Add (Number 10, Number 2))

(*(b)*)
let rec eval = function
  | Number x -> x
  | Add (x, y) -> eval x + eval y
  | Mul (x, y) -> eval x * eval y


(*(c)*)
type t = 
  | Plus
  | Times
  | Num of int


(*(d)*)
let rec reduce = function
  | [] -> []
  | Plus :: Num a :: Num b :: rest -> Num (a+b) :: rest
  | Times :: Num a :: Num b :: rest -> Num (a*b) :: rest
  | x::xs -> x :: reduce xs

(*(e)*)
let rec reduce_all l = 
  let xs = reduce l in
  if xs = l then l else reduce_all xs