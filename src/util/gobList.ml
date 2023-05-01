open Batteries

(** The normal haskell zip that throws no exception *)
let rec combine_short l1 l2 = match l1, l2 with
  | x1 :: l1, x2 :: l2 -> (x1, x2) :: combine_short l1 l2
  | _, _ -> []

let assoc_eq_opt (eq: 'a -> 'a -> bool) (x: 'a) (ys: ('a * 'b) list) : ('b option) =
  let open GobOption.Syntax in
  let+ (_, y) = List.find_opt (fun (x', _) -> eq x x') ys in
  y

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

(** [span p xs] is [take_while p xs, drop_while p xs] but may be more efficient *)
let span p =
  let rec span_helper prefix = function
    | x :: xs when p x -> span_helper (x :: prefix) xs
    | suffix -> List.rev prefix, suffix
  in span_helper []

(** Given a predicate and a list, returns two lists [(l1, l2)].
    [l1] contains the prefix of the list until the last element that satisfies the predicate, [l2] contains all subsequent elements. The order of elements is preserved. *)
let until_last_with (pred: 'a -> bool) (xs: 'a list) =
  let rec until_last_helper last seen = function
    | [] -> List.rev last, List.rev seen
    | h::t -> if pred h then until_last_helper (h::seen@last) [] t else until_last_helper last (h::seen) t
  in
  until_last_helper [] [] xs


(** Open this to use applicative functor/monad syntax for {!list}. *)
module Syntax =
struct
  (* Applicative functor. *)
  let (let+) x f = List.map f x
  let (and+) = List.cartesian_product

  (* Monad *)
  let (let*) x f = List.concat_map f x
  let (and*) = (and+)

  let (>>=) x f = List.concat_map f x
end
