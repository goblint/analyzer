(** Basic analysis utilities. *)

open GoblintCil

val is_global: Queries.ask -> varinfo -> bool
val is_static: varinfo -> bool
val is_volatile: varinfo -> bool
val is_always_unknown: varinfo -> bool
val is_excluded_from_earlyglobs: varinfo -> bool
val is_excluded_from_invalidation: varinfo -> bool

val mval_is_definite: Queries.ask -> ValueDomain.Mval.t -> bool
(** Whether offset of mvalue has only definite integer indexing (and fields). *)

val addr_is_definite: Queries.ask -> ValueDomain.Addr.t -> bool
(** Whether address is a [NULL] pointer or an mvalue that has only definite integer indexing (and fields). *)

val ad_is_definite: Queries.ask -> ValueDomain.AD.t -> bool
(** Whether address set is a single [NULL] pointer or mvalue that has only definite integer indexing (and fields). *)

val is_globally_unique: Queries.ask -> varinfo -> bool
