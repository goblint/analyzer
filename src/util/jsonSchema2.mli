val validate : Json_repr.yojson -> unit

val convert_schema: Yojson.Safe.t -> (string * (string * string)) list -> unit
