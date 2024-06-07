type t = Z.t [@@deriving eq, ord, hash]

let to_yojson z =
  `Intlit (Z.to_string z)

let rec for_all_range f (a, b) =
  if Z.compare a b > 0 then
    true
  else
    f a && for_all_range f (Z.succ a, b)

let pretty () x = GoblintCil.Pretty.text (Z.to_string x)

let pp = Z.pp_print
