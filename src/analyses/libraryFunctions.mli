(** This allows us to query information about library functions. *)

open Prelude.Ana

type categories = [
  | `Malloc       of exp
  | `Calloc       of exp * exp
  | `Realloc      of exp * exp
  | `Assert       of exp
  | `Lock         of bool * bool * bool (* try? * write? *)
  | `Unlock
  | `ThreadCreate of exp * exp * exp
  | `ThreadJoin   of exp * exp
  | `Unknown      of string ]

(** Categories of special functions *)

val classify : string -> exp list -> categories

(** *)

val get_invalidate_action : string -> (AccessKind.t -> exp list -> exp list) option
(** Returns None if nothing is known about given function.
  * Otherwise will return function that filters out arguments
  * that may be read or also written to.
*)

val get_threadsafe_inv_ac : string -> (AccessKind.t -> exp list -> exp list) option
(** Same as [get_invalidate_action], but replaces arguments for thread-safe functions.
*)

val add_lib_funs : string list -> unit
(* can't use Base.Main.store b/c of circular build - this is painful... *)
val add_effects : (string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list option) -> unit
val effects_for : string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list list

val use_special : string -> bool
(** This is for when we need to use special transfer function on functions calls that have definitions.
*)

val is_safe_uncalled : string -> bool
