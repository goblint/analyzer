(** Abstract Domains for floats. These are domains that support the C
  * operations on double/float values. *)

  module FloatDomTuple : sig
    include Lattice.S

    val neg: t -> t
    (** Negating an integer value: [-x] *)
  
    val add: t -> t -> t
    (** Addition: [x + y] *)
  
    val sub: t -> t -> t
    (** Subtraction: [x - y] *)
  
    val mul: t -> t -> t
    (** Multiplication: [x * y] *)
  
    val div: t -> t -> t
    (** Division: [x / y] *)  
  
    (** {b Comparison operators} *)
  
    val lt: t -> t -> t
    (** Less than: [x < y] *)
  
    val gt: t -> t -> t
    (** Greater than: [x > y] *)
  
    val le: t -> t -> t
    (** Less than or equal: [x <= y] *)
  
    val ge: t -> t -> t
    (** Greater than or equal: [x >= y] *)
  
    val eq: t -> t -> t
    (** Equal to: [x == y] *)
  
    val ne: t -> t -> t
    (** Not equal to: [x != y] *)
  end

