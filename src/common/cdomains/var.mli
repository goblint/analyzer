type t = Cil of GoblintCil.varinfo

include Printable.S with type t := t

val typ: t -> GoblintCil.typ

(* include MapDomain.Groupable *)
(* TODO: don't duplicate *)
type group (* use [@@deriving show { with_path = false }] *)
val compare_group: group -> group -> int
val show_group: group -> string
val to_group: t -> group
