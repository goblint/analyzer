(** This allows us to query information about library functions. *)
open GoblintCil

val add_lib_funs : string list -> unit

val use_special : string -> bool
(** This is for when we need to use special transfer function on functions calls that have definitions.
*)

val is_safe_uncalled : string -> bool

(** Find library function descriptor for {e special} function (as per {!is_special}). *)
val find: Cil.varinfo -> LibraryDesc.t

val is_special: Cil.varinfo -> bool
(** Check if function is treated specially. *)


val verifier_atomic_var: Cil.varinfo
