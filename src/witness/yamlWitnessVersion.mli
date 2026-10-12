(** YAML witness format version. *)

type t [@@deriving ord]

val v2_0: t
val v2_1: t
val v2_1_goblint: t
val v2_2: t
val v2_2_goblint: t
val v2_3: t
val v2_3_goblint: t

val show : t -> string
val pretty : unit -> t -> GoblintCil.Pretty.doc

val of_string : string -> t
val of_option : unit -> t

val min : t -> t -> t (* TODO: unused *)
val max : t -> t -> t
