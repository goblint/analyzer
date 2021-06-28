(** Abstract Domains for integers. These are domains that support the C
  * operations on integer values. *)

module type Arith =
sig
  type t
  (** {b Arithmetic operators} *)

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

  val rem: t -> t -> t
  (** Integer remainder: [x % y] *)


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


  (** {b Bit operators} *)

  val bitnot: t -> t
  (** Bitwise not (one's complement): [~x] *)

  val bitand: t -> t -> t
  (** Bitwise and: [x & y] *)

  val bitor : t -> t -> t
  (** Bitwise or: [x | y] *)

  val bitxor: t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)

  val shift_left : t -> t -> t
  (** Shifting bits left: [x << y] *)

  val shift_right: t -> t -> t
  (** Shifting bits right: [x >> y] *)


  (** {b Logical operators} *)

  val lognot: t -> t
  (** Logical not: [!x] *)

  val logand: t -> t -> t
  (** Logical and: [x && y] *)

  val logor : t -> t -> t
  (** Logical or: [x || y] *)

end

module type ArithIkind =
sig
  type t
  (** {b Arithmetic operators} *)

  val neg: Cil.ikind -> t -> t
  (** Negating an integer value: [-x] *)

  val add: Cil.ikind -> t -> t -> t
  (** Addition: [x + y] *)

  val sub: Cil.ikind -> t -> t -> t
  (** Subtraction: [x - y] *)

  val mul: Cil.ikind -> t -> t -> t
  (** Multiplication: [x * y] *)

  val div: Cil.ikind -> t -> t -> t
  (** Division: [x / y] *)

  val rem: Cil.ikind -> t -> t -> t
  (** Integer remainder: [x % y] *)


  (** {b Comparison operators} *)

  val lt: Cil.ikind -> t -> t -> t
  (** Less than: [x < y] *)

  val gt: Cil.ikind -> t -> t -> t
  (** Greater than: [x > y] *)

  val le: Cil.ikind -> t -> t -> t
  (** Less than or equal: [x <= y] *)

  val ge: Cil.ikind -> t -> t -> t
  (** Greater than or equal: [x >= y] *)

  val eq: Cil.ikind -> t -> t -> t
  (** Equal to: [x == y] *)

  val ne: Cil.ikind -> t -> t -> t
  (** Not equal to: [x != y] *)


  (** {b Bit operators} *)

  val bitnot: Cil.ikind -> t -> t
  (** Bitwise not (one's complement): [~x] *)

  val bitand: Cil.ikind -> t -> t -> t
  (** Bitwise and: [x & y] *)

  val bitor : Cil.ikind -> t -> t -> t
  (** Bitwise or: [x | y] *)

  val bitxor: Cil.ikind -> t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)

  val shift_left : Cil.ikind -> t -> t -> t
  (** Shifting bits left: [x << y] *)

  val shift_right: Cil.ikind -> t -> t -> t
  (** Shifting bits right: [x >> y] *)


  (** {b Logical operators} *)

  val lognot: Cil.ikind -> t -> t
  (** Logical not: [!x] *)

  val logand: Cil.ikind -> t -> t -> t
  (** Logical and: [x && y] *)

  val logor : Cil.ikind -> t -> t -> t
  (** Logical or: [x || y] *)

end

(* Shared signature of IntDomain implementations and the lifted IntDomains *)
module type B =
sig
  include Lattice.S
  type int_t
  (** {b Accessing values of the ADT} *)

  val bot_of: Cil.ikind -> t
  val top_of: Cil.ikind -> t

  val to_int: t -> int_t option
  (** Return a single integer value if the value is a known constant, otherwise
    * don't return anything. *)

  val is_int: t -> bool
  (** Checks if the element is a definite integer value. If this function
    * returns [true], the above [to_int] should return a real value. *)

  val equal_to: int_t -> t -> [`Eq | `Neq | `Top]

  val to_bool: t -> bool option
  (** Give a boolean interpretation of an abstract value if possible, otherwise
    * don't return anything.*)

  val is_bool: t -> bool
  (** Checks if the element is a definite boolean value. If this function
    * returns [true], the above [to_bool] should return a real value. *)

  val to_excl_list: t -> int_t list option
  (* Gives a list representation of the excluded values if possible. *)

  val of_excl_list: Cil.ikind -> int_t list -> t
  (* Creates a exclusion set from a given list of integers. *)

  val is_excl_list: t -> bool
  (* Checks if the element is an exclusion set. *)

  val to_incl_list: t -> int_t list option
  (* Gives a list representation of the included values if possible. *)

  val maximal    : t -> int_t option
  val minimal    : t -> int_t option

  (** {b Cast} *)

  val cast_to: ?torg:Cil.typ -> Cil.ikind -> t -> t
  (** Cast from original type [torg] to integer type [Cil.ikind]. Currently, [torg] is only present for actual casts. The function is also called to handle overflows/wrap around after operations. In these cases (where the type stays the same) [torg] is None. *)

end

(** The signature of integral value domains. They need to support all integer
  * operations that are allowed in C *)

module type IkindUnawareS =
sig
  include B with type int_t = int64
  include Arith with type t:= t
  val starting   : Cil.ikind -> int_t -> t
  val ending     : Cil.ikind -> int_t -> t
  val of_int: int_t -> t
  (** Transform an integer literal to your internal domain representation. *)

  val of_bool: bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: Cil.ikind -> int_t * int_t -> t
end
(** Interface of IntDomain implementations that do not take ikinds for arithmetic operations yet.
   TODO: Should be ported to S in the future. *)

module type S =
sig
  include B
  include ArithIkind with type t:= t

  val add : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val sub : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val mul : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val div : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val neg : ?no_ov:bool -> Cil.ikind ->  t -> t
  val cast_to : ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t

  val join: Cil.ikind -> t ->  t -> t
  val meet: Cil.ikind -> t -> t -> t
  val narrow: Cil.ikind -> t -> t -> t
  val widen: Cil.ikind -> t -> t -> t
  val starting : Cil.ikind -> int_t -> t
  val ending : Cil.ikind -> int_t -> t
  val of_int: Cil.ikind -> int_t -> t
  (** Transform an integer literal to your internal domain representation. *)

  val of_bool: Cil.ikind -> bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: Cil.ikind -> int_t * int_t -> t
  val is_top_of: Cil.ikind -> t -> bool
  val invariant_ikind : Invariant.context -> Cil.ikind -> t -> Invariant.t

  val refine_with_congruence: t -> (int_t * int_t) option -> t
  val refine_with_interval: t -> (int_t * int_t) option -> t
  val refine_with_excl_list: t -> int_t list option -> t
  val refine_with_incl_list: t -> int_t list option -> t
end
(** Interface of IntDomain implementations taking an ikind for arithmetic operations *)

module OldDomainFacade (Old : IkindUnawareS) : S with type int_t = IntOps.BigIntOps.t and type t = Old.t
(** Facade for IntDomain implementations that do not implement the interface where arithmetic functions take an ikind parameter. *)

module type Y =
sig
  include B
  include Arith with type t:=t

  val of_int: Cil.ikind -> int_t -> t
  (** Transform an integer literal to your internal domain representation with the specified ikind. *)

  val of_bool: Cil.ikind -> bool -> t
  (** Transform a known boolean value to the default internal representation of the specified ikind. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: Cil.ikind -> int_t * int_t -> t

  val starting   : Cil.ikind -> int_t -> t
  val ending     : Cil.ikind -> int_t -> t

  val is_top_of: Cil.ikind -> t -> bool
end
(** The signature of integral value domains keeping track of ikind information *)

module type Z = Y with type int_t = IntOps.BigIntOps.t

module IntDomLifter (I: S): Y with type int_t = I.int_t

module type Ikind =
sig
  val ikind: unit -> Cil.ikind
end

module PtrDiffIkind : Ikind

module IntDomWithDefaultIkind (I: Y) (Ik: Ikind) : Y with type t = I.t and type int_t = I.int_t

(* module ManyInts : S *)
(* module IntDomList : S *)
module IntDomTuple : sig
  include Z
  val no_interval: t -> t
end

val of_const: int64 * Cil.ikind * string option -> IntDomTuple.t


module Size : sig
  (** The biggest type we support for integers. *)
  val top_typ         : Cil.typ
  val range           : Cil.ikind -> int64 * int64
  val range_big_int   : Cil.ikind -> Z.t * Z.t
  val bits            : Cil.ikind -> int * int
end

exception ArithmeticOnIntegerBot of string

exception Unknown
(** An exception that can be raised when the result of a computation is unknown.
  * This is caught by lifted domains and will be replaced by top. *)

exception Error
(** An exception that can be raised when an arithmetic error occurs. This is
  * caught by lifted domains and the evaluation will then be set to bot, which
  * signifies an error in computation *)

exception IncompatibleIKinds of string

(** {b Predefined domains} *)

module Integers : IkindUnawareS with type t = int64
(** The integers with their natural orderings. Calling [top] and [bot] will
  * raise exceptions. *)

module FlatPureIntegers : IkindUnawareS with type t = int64
(** The integers with flattened orderings. Calling [top] and [bot] or [join]ing
    or [meet]ing inequal elements will raise exceptions. *)

module Flattened : IkindUnawareS with type t = [`Top | `Lifted of int64 | `Bot]
(** This is the typical flattened integer domain used in Kildall's constant
  * propagation. *)

module Lifted : IkindUnawareS with type t = [`Top | `Lifted of int64 | `Bot]
(** Artificially bounded integers in their natural ordering. *)

module IntervalFunctor(Ints_t : IntOps.IntOps): S with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option

module Interval32 :Y with (* type t = (IntOps.Int64Ops.t * IntOps.Int64Ops.t) option and *) type int_t = IntOps.Int64Ops.t

module BigInt : Printable.S (* TODO: why doesn't this have a more useful signature like IntOps.BigIntOps? *)

module Interval : S with type int_t = IntOps.BigIntOps.t

module CongruenceFunctor(Ints_t : IntOps.IntOps): S with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option

module Congruence : S with type int_t = IntOps.BigIntOps.t

module DefExc : S with type int_t = IntOps.BigIntOps.t
(** The DefExc domain. The Flattened integer domain is topped by exclusion sets.
  * Good for analysing branches. *)

(** {b Domain constructors} *)

module Flat (Base: IkindUnawareS): IkindUnawareS
(** Creates a flat value domain, where all ordering is lost. Arithmetic
  * operations are lifted such that only lifted values can be evaluated
  * otherwise the top/bot is simply propagated with bot taking precedence over
  * top. *)

module Lift (Base: IkindUnawareS): IkindUnawareS
(** Just like {!Value.Flat} except the order is preserved. *)

module Reverse (Base: IkindUnawareS): IkindUnawareS
(** Reverses bot, top, leq, join, meet *)

(* module Interval : S *)
(** Interval domain with int64-s --- use with caution! *)

(* module IncExcInterval : S with type t = [ | `Excluded of Interval.t| `Included of Interval.t ] *)
(** Inclusive and exclusive intervals. Warning: NOT A LATTICE *)
module Enums : S with type int_t = IntOps.BigIntOps.t

(** {b Boolean domains} *)

module type BooleansNames =
sig
  val truename: string
  (** The name of the [true] abstract value *)

  val falsename: string
  (** The name of the [false] abstract value *)
end
(** Parameter signature for the [MakeBooleans] functor. *)

module MakeBooleans (Names: BooleansNames): IkindUnawareS with type t = bool
(** Creates an abstract domain for integers represented by boolean values. *)

module Booleans: IkindUnawareS with type t = bool
(** Boolean abstract domain, where true is output "True" and false is output
  * "False" *)

(*
module None: S with type t = unit
(** Domain with nothing in it. *)
*)
