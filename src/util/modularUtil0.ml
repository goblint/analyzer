open GoblintCil
open GobConfig
let type_to_varinfo t =
  TypeVarinfoMap.to_varinfo t

let varinfo_to_canonical x =
  let t = x.vtype in
  type_to_varinfo t

let modular = ref None

let is_modular () = match !modular with
  | None ->
    let m = get_bool "modular" in
    modular := Some m;
    m
  | Some m -> m

(** If [x] is global and we are in ["modular"] mode, retrieves the varinfo_to_canonical for [x], else returns [x] itself *)
let varinfo_or_canonical x =
  if is_modular () && x.vglob then varinfo_to_canonical x else x
