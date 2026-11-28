(** Linear constraints over integer variables. *)
type comparison = Eq | Ge | Gt | Ne

(** [sum (coefficient * variable) + constant comparison 0].
    Terms have nonzero integer coefficients. *)
type 'var t = {
  terms: (Z.t * 'var) list;
  constant: Z.t;
  comparison: comparison;
}
