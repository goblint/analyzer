(** YAML witness format version. *)

type t =
  | V2_0
  | V2_1
  | V2_1_Goblint
  | V2_2
  | V2_2_Goblint
  | V2_3
  | V2_3_Goblint
[@@deriving ord]

val show : t -> string
val pretty : unit -> t -> GoblintCil.Pretty.doc

val of_string : string -> t
val of_option : unit -> t

val min : t -> t -> t (* TODO: unused *)
val max : t -> t -> t
