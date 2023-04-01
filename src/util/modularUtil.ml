open GoblintCil
open GobConfig

let canonical_varinfo x =
  let t = x.vtype in
  TypeVarinfoMap.to_varinfo t

(** If [x] is global and we are in ["modular"] mode, retrieves the canonical_varinfo for [x], else returns [x] itself *)
let varinfo_or_canonical x =
  if get_bool "modular" && x.vglob then canonical_varinfo x else x
