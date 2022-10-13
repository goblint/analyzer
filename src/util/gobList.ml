open Batteries

(** The normal haskell zip that throws no exception *)
let rec combine_short l1 l2 = match l1, l2 with
  | x1 :: l1, x2 :: l2 -> (x1, x2) :: combine_short l1 l2
  | _, _ -> []

let assoc_eq_opt (eq: 'a -> 'a -> bool) (x: 'a) (ys: ('a * 'b) list) : ('b option) =
  Option.map Tuple2.second (List.find_opt (fun (x',_) -> eq x x') ys)

let rec fold_left3 f acc l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> acc
  | x1 :: l1, x2 :: l2, x3 :: l3 -> fold_left3 f (f acc x1 x2 x3) l1 l2 l3
  | _, _, _ -> invalid_arg "GobList.fold_left3"

let rec for_all3 f l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> true
  | x1 :: l1, x2 :: l2, x3 :: l3 -> f x1 x2 x3 && (for_all3 [@tailcall]) f l1 l2 l3
  | _, _, _ -> invalid_arg "GobList.for_all3"

let rec fold_while_some (f : 'a -> 'b -> 'a option) (acc: 'a) (xs: 'b list): 'a option = match xs with
  | [] -> Some acc
  | x::xs ->
    begin match f acc x with
      | Some acc' -> fold_while_some f acc' xs
      | None -> None
    end

let equal = List.eq

(* Split a list by a preciate. Returns a tuple of two lists.
   The elements in the first list satisfy the predicate, the elements in the second do not. *)
let split_by_pred p xs =
  let rec split xs (ys,nos) = match xs with
    | [] -> List.rev ys, List.rev nos
    | h::t -> if p h then split t (h::ys, nos) else split t (ys, h::nos)
  in
  split xs ([], [])
