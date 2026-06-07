(*(d)*)
type stree = Oak | Birch | Maple

let seq = [ Oak; Birch; Oak; Maple; Maple ]
let cur = ref []

let rec spotter () =
  match !cur with
  | [] -> 
      cur := seq;    (* Refill the sequence *)
      spotter ()     (* Call itself to get the first item *)
  | hd::tl -> 
      cur := tl;     (* Update the remaining list *)
      hd             (* Return the tree *)
