(*(a)*)
let cons x y = x :: y

let rec perms xs =
  match xs with
  | [] -> [[]]
  | _ ->
      let rec perms1 (xs, ys) =
        match xs with
        | [] -> []
        (*x = current candidate for first element
        xs = elements after x*)
        | x :: xs ->
            List.map (cons x) (perms (List.rev ys @ xs)) (*the list begins with x, tail computed using recursion*)
            @ perms1 (xs, x :: ys) (*the list does not begin with x*)
      in
      perms1 (xs, [])

(*
The empty list has one permutation.
Choose each element once as the first element, then permute the remaining elements. 
The list ys contains the elements already skipped, in reverse order, and xs contains the remaining elements. 
Thus List.rev ys @ xs is the original list with x removed. 
The expression List.map (cons x) prefixes x to every permutation of the remaining elements. 
The recursive call to perms1 then considers the next element as the first element. The results are concatenated with @.

perms [1;2;3]
[
  [1; 2; 3];
  [1; 3; 2];
  [2; 1; 3];
  [2; 3; 1];
  [3; 1; 2];
  [3; 2; 1]
]
*)


(*(b)*)
type 'a seq =
  | Nil
  | Cons of 'a * (unit -> 'a seq)

let rec appendq (xs, ys) =
  match xs with
  | Nil -> ys
  | Cons (x, xf) ->
      Cons (x, fun () -> appendq (xf (), ys))

let rec mapq f xs =
  match xs with
  | Nil -> Nil
  | Cons (x, xf) -> Cons (f x, fun () -> mapq f (xf()))

let rec lperms xs =
  match xs with
  | [] ->
      Cons ([], fun () -> Nil)
  | _ ->
      let rec perms1 (xs, ys) =
        match xs with
        | [] -> Nil
        | x :: xs ->
            appendq
              (mapq (cons x) (lperms (List.rev ys @ xs)),
               perms1 (xs, x :: ys))
      in
      perms1 (xs, [])

(*In lazy lists, an expression inside fun () -> E is not evaluated immediately. It is delayed until the function is called
However, in lperms, the recursive calls to lperms and perms1 are not placed inside such functions.
Before calling appendq, both arguments need to be evaluated. 
So the recursive call perms1 and lperms are performed immediately. 
The only delayed expression is Nil in Cons ([], fn () => Nil), which is trivial. 
Hence all n! permutations are computed as soon as lperms is called, so true laziness is not achieved.`
*)


(*(c)*)
(*
f: function to apply to each element
s: lazy list to map over
yf: what to do after s is exhausted
*)
let rec mapq2 f s yf =
  match s with
  | Nil -> yf ()
  | Cons (x, xf) -> Cons (f x, fun () -> mapq2 f (xf()) yf)

let rec lperms xs =
  match xs with
  | [] ->
      Cons ([], fun () -> Nil)
  | _ ->
      let rec perms1 (xs, ys) =
        match xs with
        | [] -> Nil
        | x :: xs ->
          mapq2
            (cons x)
            (lperms (List.rev ys @ xs))
            (fun () -> perms1 (xs, x :: ys))
      in
      perms1 (xs, [])
