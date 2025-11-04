(** Basic analysis utilities. *)

open GoblintCil

val is_global: Queries.ask -> varinfo -> bool
val is_static: varinfo -> bool
val is_volatile: varinfo -> bool
val is_always_unknown: varinfo -> bool
val is_excluded_from_earlyglobs: varinfo -> bool
val is_excluded_from_invalidation: varinfo -> bool
