(** Basic analysis utilities. *)

open GoblintCil

val is_global: Queries.ask -> Var.t -> bool
val is_static: varinfo -> bool
val is_volatile: varinfo -> bool
val is_always_unknown: Var.t -> bool
val is_excluded_from_earlyglobs: Var.t -> bool
val is_excluded_from_invalidation: varinfo -> bool
