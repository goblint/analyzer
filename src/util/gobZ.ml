type t = Z.t

let to_yojson z =
  `Intlit (Z.to_string z)
