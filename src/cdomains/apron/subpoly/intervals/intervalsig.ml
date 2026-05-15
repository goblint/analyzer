module type IntervalSig = sig
  (** Bound type used by this interval domain. *)
  type bound

  (** Abstract interval value. *)
  type t [@@deriving eq, ord, hash]

  (** The unconstrained interval [-inf, +inf]. *)
  val top : t

  (** Returns [true] iff the interval is unconstrained. *)
  val is_top : t -> bool

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

  (** Human-readable representation of an interval. *)
  val show : t -> string
end
