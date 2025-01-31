(** Invariants for witnesses. *)

include Lattice.S with type t = [ `Bot | `Lifted of GoblintCil.exp | `Top ]

val none: t
val of_exp: GoblintCil.exp -> t

val (&&): t -> t -> t
val (||): t -> t -> t


type context = {
  path: int option;
  lvals: Lval.Set.t;
}

val default_context : context
