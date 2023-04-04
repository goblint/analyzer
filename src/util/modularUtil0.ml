open GoblintCil
open GobConfig
let type_to_varinfo t =
  TypeVarinfoMap.to_varinfo t

let varinfo_to_canonical x =
  let t = x.vtype in
  type_to_varinfo t

(** If [x] is global and we are in ["modular"] mode, retrieves the varinfo_to_canonical for [x], else returns [x] itself *)
let varinfo_or_canonical x =
  if get_bool "modular" && x.vglob then varinfo_to_canonical x else x
