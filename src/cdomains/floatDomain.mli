(** Abstract Domains for floats. These are domains that support the C
  * operations on double/float values. *)

module type FloatArith = sig
  type t

  val neg : t -> t
  (** Negating an flaot value: [-x] *)
  val add : t -> t -> t
  (** Addition: [x + y] *)
  val sub : t -> t -> t
  (** Subtraction: [x - y] *)
  val mul : t -> t -> t
  (** Multiplication: [x * y] *)
  val div : t -> t -> t
  (** Division: [x / y] *)

  (** {b Comparison operators} *)
  val lt : t -> t -> IntDomain.IntDomTuple.t
  (** Less than: [x < y] *)
  val gt : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than: [x > y] *)
  val le : t -> t -> IntDomain.IntDomTuple.t
  (** Less than or equal: [x <= y] *)
  val ge : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than or equal: [x >= y] *)
  val eq : t -> t -> IntDomain.IntDomTuple.t
  (** Equal to: [x == y] *)
  val ne : t -> t -> IntDomain.IntDomTuple.t
  (** Not equal to: [x != y] *)
end

module FloatInterval : sig (**Currently just for FloatDomainTest *)
  type t = (float * float) option
  include FloatArith with type t := t

  val top : unit -> t

  val is_bot : t -> bool

  val is_top : t -> bool

  val of_const : float -> t
  val of_interval : float*float -> t
  val ending : float -> t
  val starting : float -> t

  val show : t -> string

  val equal : t -> t -> bool

  val leq : t -> t -> bool

  val arbitrary : unit -> t QCheck.arbitrary
end

module type FloatDomainBase = sig
  include Lattice.S
  include FloatArith with type t := t

  val of_const : float -> t
  val of_interval : float*float -> t

  val ending : float -> t
  val starting : float -> t

  val to_float : t -> float option
  val maximal : t -> float option
  val minimal : t -> float option

  val of_int : IntDomain.IntDomTuple.t -> t
  val cast_to : Cil.ikind -> t -> IntDomain.IntDomTuple.t
end

module FloatDomTupleImpl : sig
  include FloatDomainBase
end
