(*(a)*)
type 'a tree = 
  | Lf
  | Br of 'a * 'a tree * 'a tree

(*insert all pairs in one tree into another tree*)

let rec insert (k, v) t =
  match t with
  | Lf -> Br ((k, v), Lf, Lf)
  | Br ((ka, va), l, r) -> 
    if k < ka then Br ((ka, va), insert (k, v) l, r)
    else if k > ka then Br ((ka, va), l, insert (k, v) r)
    else Br ((k, v), l, r)

let rec union t1 t2 =
  match t1 with
  | Lf -> t2
  | Br ((k, v), l, r) -> union l (union r (insert (k, v) t2))


(*(b)*)
let rec takeSlice x y = function
  | Lf -> Lf
  | Br ((k, v), l, r) -> 
    if k >= x && k <= y then Br ((k, v), takeSlice x y l, takeSlice x y r)
    else if k < x then takeSlice x y r
    else takeSlice x y l


(*(c)*)
let rec dropSlice x y = function
  | Lf -> Lf
  | Br ((k, v), l, r) -> 
    if k < x then Br ((k, v), l, dropSlice x y r)
    else if k > y then Br ((k, v), dropSlice x y l, r)
    else union (dropSlice x y l) (dropSlice x y r)


(*(d)*)
(*The two trees contain the same key-value pairs, but there are many different binary search trees 
that can represent the same dictionary. The operations may not guarantee giving the same structure.*)