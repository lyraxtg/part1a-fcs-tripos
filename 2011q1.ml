(*(a)*)
type possible_move = Up | Down | Right | Left

type labyrinth = int * int -> possible_move list

let move (x, y) dir = 
  match dir with
  | Up -> (x, y+1)
  | Down -> (x, y-1)
  | Right -> (x+1, y)
  | Left -> (x-1, y)

let next (x, y) lab = List.map (move (x, y)) (lab (x, y))


(*(b)*)
(*DFS, stores only the current path being explored, not all possible paths at the same time.*)
let reachable start target lab =
  let rec search visited stack =
    match stack with
    | [] -> false
    | p::rest -> 
      if p = target then true
      else if List.mem p visited then
        search visited rest
      else 
        search (p::visited) ((next p lab) @ rest)
  in
  search [] [start]


(*(c)*)
(*Iterative deepening, combines DFS space efficiency and BFS shortest path guarantee. Run DFS with a maximum depth limit.
If it fails, increase the limit and try again.
At any moment, iterative deepening is just doing DFS.
DFS only stores the current path being explored.
So if the shortest path length is p, the maximum recursion depth needed when the answer is found is about O(p)
*)

