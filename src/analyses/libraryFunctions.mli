(** This allows us to query information about library functions. *)

open Cil

type categories = [
  | `Malloc       
  | `Calloc       
  | `Assert       of exp
  | `Lock         of bool * bool (* try? * write? *)
  | `Unlock       
  | `ThreadCreate of exp * exp
  | `ThreadJoin   of exp * exp
  | `Unknown      of string ]

(** Categories of special functions *)
 
val classify : string -> exp list -> categories

(** *)
  
type action = [ `Write  (** argument may be read or written to *)
              | `Read   (** argument may be read *)
              ]
(** Specifies what is known about an argument. *)

val get_invalidate_action : string -> (action -> Cil.exp list -> Cil.exp list) option
(** Returns None if nothing is known about given function.
  * Otherwise will return function that filters out arguments
  * that may be read or also written to.
  *)

val get_threadsafe_inv_ac : string -> (action -> Cil.exp list -> Cil.exp list) option
(** Same as [get_invalidate_action], but replaces arguments for thread-safe functions.
  *)

val add_lib_funs : string list -> unit

val use_special : string -> bool
(** This is for when we need to use special transfer function on functions calls that have definitions. 
  *)

val osek_renames : bool ref
