open Cil

val is_global: Queries.ask -> varinfo -> bool
val is_static: varinfo -> bool
val is_always_unknown: varinfo -> bool
val is_precious_glob: varinfo -> bool
