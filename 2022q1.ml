(*(a)*)
let mapi f l =
  let rec mapfrom i = function
  | [] -> []
  | x::xs -> (f i x)::mapfrom (i+1) xs
  in
  mapfrom 0 l

let rec lookfor y l =
  match l with
  | [] -> None
  | (m, n) :: xs -> 
    if y = n then Some m 
    else lookfor y xs


(*(b)*)
type response = 
  | Green of char
  | Amber of char
  | Black

type responses = response list

type word = char list
type guess = char * int
type guesses = guess list

let rec exists a = function
  | [] -> false
  | x::xs -> if x = a then true else exists a xs

(*'a -> char, 'b -> response. Need fun i c to take a char from target and return a response*)
let respond target gs = 
  mapi 
  (fun i c -> 
    match lookfor i gs with
    | Some c' when c'=c -> Green c
    | Some c' when exists c' target -> Amber c'
    | _ -> Black
  )
  target


(*(c)*)
exception Out_of_turns

let create_game ans =
  let turns = ref 6 in
  let gs = ref [] in
  fun g ->
    if !turns = 0 then raise Out_of_turns;
    turns := !turns - 1;
    gs := prune_guesses (g::!gs);
    respond ans !gs

(*
let game = create_game ["a"; "b"; "c"]
game ("a", 0)
response list = [Green "a", Black, Black]

game ("c", 1)
response list = [Green "a", Amber "c", Black]

game ("x", 2)
response list = [Green "a", Amber "c", Black]
...

Exception: Out_of_turns
*)