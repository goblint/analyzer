type t = Z.t

let to_yojson z =
  `Intlit (Z.to_string z)

let rec for_all_range f (a, b) =
  if Z.compare a b > 0 then
    true
  else
    f a && for_all_range f (Z.succ a, b)
