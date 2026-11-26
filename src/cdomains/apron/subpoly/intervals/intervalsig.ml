module type IntervalSig = sig
  (** Bound type used by this interval domain. *)
  type bound

  (** Abstract interval value. *)
  type t [@@deriving eq, ord, hash]

  (** The unconstrained interval [-inf, +inf]. *)
  val top : t

  (** Returns [true] iff the interval is unconstrained. *)
  val is_top : t -> bool

  (** Lower and upper bound; [None] means unbounded on that side. *)
  val bounds : t -> bound option * bound option

  (** The singleton interval [c, c]. *)
  val of_const : bound -> t

  (** Pointwise sum of two intervals. *)
  val add : t -> t -> t

  (** Builds an interval from optional bounds.

      [lower = None] means no lower bound. [upper = None] means no upper bound. *)
  val of_bounds : lower:bound option -> upper:bound option -> t

  (** Scales both interval bounds by the given factor.

      If the factor is negative, lower and upper bounds are swapped. *)
  val scale : bound -> t -> t

  (** Add a constant to every finite bound; [None] bounds stay unbounded.

      To pull a linear constant [k] out of the expression into the interval, use
      [add_const (neg k)]: bounds shift by [-k] while the stored linexpr constant becomes zero. *)
  val add_const : bound -> t -> t

  (** Intersects two intervals.

      Returns [None] if the intersection is empty. *)
  val meet : t -> t -> t option

  (** Returns the smallest interval containing both arguments. *)
  val join : t -> t -> t

  (** Inclusion order on intervals.

      [leq x y] holds when [x] is contained in [y]. *)
  val leq : t -> t -> bool

  (** Standard interval widening: [widen old new] keeps bounds of [old] that
      did not grow in [new] and drops the others to infinity. *)
  val widen : t -> t -> t

  (** Widening with thresholds: like [widen], but an unstable bound snaps to
      the nearest threshold still covering the new bound ([lower] must return
      the largest threshold [<=] its argument, [upper] the smallest threshold
      [>=] its argument) instead of jumping to infinity. *)
  val widen_thresholds : lower:(bound -> bound option) -> upper:(bound -> bound option) -> t -> t -> t

  (** Standard interval narrowing: [narrow a b] refines only the infinite
      bounds of [a] with the corresponding bounds of [b]. *)
  val narrow : t -> t -> t

  (** Human-readable representation of an interval. *)
  val show : t -> string
end
