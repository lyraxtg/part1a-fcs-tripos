(*(a)*)
(*
fun x -> E denotes an anonymous function which takes an argument x and returns the value
of expression E when called. 
For example, fun x -> x+1 is a function that adds 1 to its argument. It has type int -> int.

A curried function returns another function as its result.  
For example, fun x -> fun y -> x+y has type int -> (int -> int)
It takes an int and returns a new function that takes another int
*)

let rec replicate n x =
  if n = 0 then []
  else x::replicate (n-1) x


(*(b)*)
(*
References are mutable cells. 
A reference cell is created using ref, read using !, and updated using :=.
*)

let rlist =
  (replicate 4 (ref 0)) @ (List.map ref [1; 2; 3; 4])

let slist =
  List.map (fun r -> ref (!r)) rlist;;

(*
rlist: ref 0 is evaluated once, creating one ref cell containing 0. 
replicate 4 (ref 0) creates one reference cell and puts four pointers to it in the list. 
List.map ref [1; 2; 3; 4] creates four reference cells. 

rlist =
[ r0; r0; r0; r0; r1; r2; r3; r4 ]

where

r0 ───► [0]     shared by first four elements

r1 ───► [1]
r2 ───► [2]
r3 ───► [3]
r4 ───► [4]

slist: for each r, it reads the current content of r and creates a new reference cell containing that value. 
Even if several elements of rlist point to the same cell, slist gets new separate cells. 
slist =
[ s0; s1; s2; s3; s4; s5; s6; s7 ]

s0 ───► [0]
s1 ───► [0]
s2 ───► [0]
s3 ───► [0]
s4 ───► [1]
s5 ───► [2]
s6 ───► [3]
s7 ───► [4]
*)

(*(c)*)
List.map (fun r -> r := !r + 1) rlist;;
(*Each function call updates a reference and returns () because assignment has type unit.
Side effects: the first four elements point to the same cell so it's incremented four times. 
After the first expression, rlist values: [4; 4; 4; 4; 2; 3; 4; 5]*)

List.map (fun r -> r := !r - 1; !r) rlist;;
(*This updates each reference by subtracting 1, then returns its new value due to !r
So the first shared cell changes like this:
4 -> 3    returns 3
3 -> 2    returns 2
2 -> 1    returns 1
1 -> 0    returns 0
The remaining cells change once:
2 -> 1    returns 1
3 -> 2    returns 2
4 -> 3    returns 3
5 -> 4    returns 4
So the returned value is:
[3; 2; 1; 0; 1; 2; 3; 4]
After this expression, rlist is back to:
rlist values: [0; 0; 0; 0; 1; 2; 3; 4]
*)

List.map (fun r -> r := !r + 3; !r) slist;;
(*
slist contains fresh independent references. 
So before this expression, slist values: [0; 0; 0; 0; 1; 2; 3; 4]
Each cell is separate, so each one is simply increased by 3.
Returned value: [3; 3; 3; 3; 4; 5; 6; 7]
*)
