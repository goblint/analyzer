(** Invariants for witnesses. *)

type exp
val show_exp: exp -> string
val exp_to_cil: exp -> GoblintCil.exp
(** Breaks abstraction, only for use in {!EvalAssert}! *)

include Lattice.S with type t = [ `Bot | `Lifted of exp | `Top ]

val process_exp: exp -> exp list

val none: t
val of_exp: GoblintCil.exp -> t

val (&&): t -> t -> t
val (||): t -> t -> t


type context = {
  path: int option;
  lvals: Lval.Set.t;
}

val default_context : context
