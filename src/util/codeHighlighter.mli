(** C code highlighting. *)

type t = Fpath.t -> string BatEnum.t

val none: t
val make_pygments: style_css_file:Fpath.t -> t option
val make: style_css_file:Fpath.t -> t
