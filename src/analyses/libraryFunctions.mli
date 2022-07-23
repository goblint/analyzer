(** This allows us to query information about library functions. *)
open GoblintCil

val add_lib_funs : string list -> unit
(* can't use Base.Main.store b/c of circular build - this is painful... *)
val add_effects : (string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list option) -> unit
val effects_for : string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list list

val use_special : string -> bool
(** This is for when we need to use special transfer function on functions calls that have definitions.
*)

val is_safe_uncalled : string -> bool

(** Find library function descriptor for function. *)
val find: Cil.varinfo -> LibraryDesc.t
