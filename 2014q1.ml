(*(a)*)
(*
Polymorphism means an expression can have a type containing type variables such as 'a and 'b.
The type is still checked at compile time, but the expression can be used generically at many different concrete types. 
For example, lists are polymorphic. A list can be int/string/bool list, but all elements must have the same type. 
Append has type 'a list -> 'a list -> 'a list, meaning it takes two lists whose elements have the same type 'a, and returns another list of that same type. 
Map has type ('a -> 'b) -> 'a list -> 'b list. This means List.map takes a function from type 'a to type 'b, and a list of elements of type 'a, and returns a list of elements of type 'b.
*)

(*(b)*)
type 'a se = 
  | Void
  | Unit of 'a 
  | Join of 'a se * 'a se

(*
This define a new polymorphic datatype 'a se with three constructors. 
Void is a constant constructor with no extra information.
Unit carries one value of type 'a. 
Join combines two existing values of type 'a se. 
It describes a tree-like structure, where Join is an unlabelled binary branch, Unit x is a labelled leaf containing value x, and Void is an unlabelled empty leaf. 
For example:
Join (Unit 1, Join (Void, Unit 3))
      Join
     /    \
 Unit 1   Join
          /   \
       Void  Unit 3

The role of polymorphism is that the labels can have any type.
Functions over this datatype are usually written using pattern matching to distinguish three possible forms of 'a se value. 
*)

(*(c)*)
let rec encode_list = function
  | [] -> Void
  | x::xs -> Join (Unit x, encode_list xs)

let rec decode_list = function
  | Void -> []
  | Unit x -> [x]
  | Join (a, b) -> (decode_list a) @ (decode_list b)


(*(d)*)
let rec cute p s =
  match s with
  | Void -> false
  | Unit x -> p x
  | Join (u, v) -> cute p u || cute p v

(*
type: ('a -> bool) -> 'a se -> bool
It checks whether at least one label inside the structure satisfies the predicate p. 
*)


(*(e)*)
(*fun p -> cute (cute p)*)
(*
cute p: 'a se -> bool, i.e. cute p is itself a predicate on 'a se values. 
In cute (cute p), the outer cute expects a predicate as its first argument. Here the predicate given is cute p. 
Hence cute (cute p): ('a se) se -> bool
So the whole expression fun p -> cute (cute p) has type: ('a -> bool) -> ('a se) se -> bool
It tests whether a nested structure contains some inner structure containing some value satisfying p.
*)