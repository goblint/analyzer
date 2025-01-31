(** Invariants for witnesses. *)

include Lattice.S

val none: t
val of_exp: GoblintCil.exp -> t

val to_exp: t -> GoblintCil.exp option

val (&&): t -> t -> t
val (||): t -> t -> t


type context = {
  path: int option;
  lvals: Lval.Set.t;
}

val default_context : context
