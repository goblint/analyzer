val validate : Json_repr.yojson -> unit

val convert_schema: Yojson.Safe.t -> (string * (DefaultsCategory.category * (string * string))) list -> unit
