(** Abstract Domains for floats. These are domains that support the C
  * operations on double/float values. *)

  module FloatDomTuple : sig
    include Lattice.S

    val of_const: float -> t

    val neg: t -> t
    (** Negating an flaot value: [-x] *)
  
    val add: t -> t -> t
    (** Addition: [x + y] *)
  
    val sub: t -> t -> t
    (** Subtraction: [x - y] *)
  
    val mul: t -> t -> t
    (** Multiplication: [x * y] *)
  
    val div: t -> t -> t
    (** Division: [x / y] *)  
  
    (** {b Comparison operators} *)
  
    val lt: t -> t -> IntDomain.IntDomTuple.t
    (** Less than: [x < y] *)
  
    val gt: t -> t -> IntDomain.IntDomTuple.t
    (** Greater than: [x > y] *)
  
    val le: t -> t -> IntDomain.IntDomTuple.t
    (** Less than or equal: [x <= y] *)
  
    val ge: t -> t -> IntDomain.IntDomTuple.t
    (** Greater than or equal: [x >= y] *)
  
    val eq: t -> t -> IntDomain.IntDomTuple.t
    (** Equal to: [x == y] *)
  
    val ne: t -> t -> IntDomain.IntDomTuple.t
    (** Not equal to: [x != y] *)
  end

