open GoblintCil
open GobConfig
let type_to_varinfo t =
  TypeVarinfoMap.to_varinfo t

let varinfo_to_canonical x =
  let t = x.vtype in
  type_to_varinfo t

let modular = ref None

module StringSet = BatSet.Make(String)
let modular_funs = ref None

let is_modular () = match !modular with
  | None ->
    let m = get_bool "modular" in
    modular := Some m;
    m
  | Some m -> m

let modular_funs () = match !modular_funs with
  | None ->
    let funs = get_string_list "ana.modular.funs" in
    let funs = StringSet.of_list funs in
    modular_funs := Some funs;
    funs
  | Some funs ->
    funs

let is_modular_fun f =
  let is_modular_fun f =
    StringSet.mem f (modular_funs ())
  in
  is_modular () || is_modular_fun f.vname

let is_any_modular () =
  is_modular () || not (StringSet.is_empty (modular_funs ()))

(** If [x] is global (but not a heap variable) and we are in ["modular"] mode, retrieves the varinfo_to_canonical for [x], else returns [x] itself *)
let varinfo_or_canonical ~is_modular x =
  if is_modular && x.vglob && not (InvariantCil.var_is_heap x) then varinfo_to_canonical x else x
