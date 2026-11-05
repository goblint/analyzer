(** Invariants for witnesses. *)

module Exp:
sig
  include Printable.S

  val process: t -> t list

  val to_cil: t -> GoblintCil.exp
  (** Breaks abstraction, only for use in {!EvalAssert}! *)
end


include Lattice.S with type t = [ `Bot | `Lifted of Exp.t | `Top ]

val none: t
val of_exp: GoblintCil.exp -> t

val (&&): t -> t -> t
val (||): t -> t -> t


type context = {
  path: int option;
  lvals: Lval.Set.t;
}

val default_context : context
