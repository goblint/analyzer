(** The normal haskell zip that throws no exception *)
let rec combine_short l1 l2 = match l1, l2 with
  | x1 :: l1, x2 :: l2 -> (x1, x2) :: combine_short l1 l2
  | _, _ -> []

let rec fold_left3 f acc l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> acc
  | x1 :: l1, x2 :: l2, x3 :: l3 -> fold_left3 f (f acc x1 x2 x3) l1 l2 l3
  | _, _, _ -> invalid_arg "GobList.fold_left3"
