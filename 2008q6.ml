(*(a)*)
type 'a puzzle =
  | Puzzle of ('a -> 'a list) * ('a -> bool)

(*(c)*)
exception Fail

let rec depth (Puzzle (next, wins)) x d =
  if wins x then x
  else if d=0 then raise Fail
  else 
    let rec dfs states =
      match states with
      | [] -> raise Fail
      | s::ss -> 
        try depth (Puzzle (next, wins)) s (d-1)
        with Fail -> dfs ss
    in
    dfs (next x)

(*(d)*)
let breadth (Puzzle (next, wins)) x =
  let rec bfs queue =
    match queue with
    | [] -> raise Fail
    | s::ss -> 
      if wins s then s
      else bfs (ss @ next s)
  in
  bfs [x]
