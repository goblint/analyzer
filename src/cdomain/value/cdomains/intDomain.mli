(** Abstract domains for C integers. *)

open GoblintCil

val should_wrap: Cil.ikind -> bool
val should_ignore_overflow: Cil.ikind -> bool

val reset_lazy: unit -> unit

type overflow_info = { overflow: bool; underflow: bool;}

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


  (** {b Bitwise logical operators} *)

  val lognot: t -> t
  (** Bitwise logical not (one's complement): [~x] *)

  val logand: t -> t -> t
  (** Bitwise logical and: [x & y] *)

  val logor : t -> t -> t
  (** Bitwise logical or: [x | y] *)

  val logxor: t -> t -> t
  (** Bitwise logical exclusive or: [x ^ y] *)

  val shift_left : t -> t -> t
  (** Shifting bits left: [x << y] *)

  val shift_right: t -> t -> t
  (** Shifting bits right: [x >> y] *)


  (** {b Logical operators} *)

  val c_lognot: t -> t
  (** Logical not: [!x] *)

  val c_logand: t -> t -> t
  (** Logical and: [x && y] *)

  val c_logor : t -> t -> t
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

  val lognot: Cil.ikind -> t -> t
  (** Bitwise not (one's complement): [~x] *)

  val logand: Cil.ikind -> t -> t -> t
  (** Bitwise and: [x & y] *)

  val logor : Cil.ikind -> t -> t -> t
  (** Bitwise or: [x | y] *)

  val logxor: Cil.ikind -> t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)

  val shift_left : Cil.ikind -> t -> t -> t
  (** Shifting bits left: [x << y] *)

  val shift_right: Cil.ikind -> t -> t -> t
  (** Shifting bits right: [x >> y] *)


  (** {b Logical operators} *)

  val c_lognot: Cil.ikind -> t -> t
  (** Logical not: [!x] *)

  val c_logand: Cil.ikind -> t -> t -> t
  (** Logical and: [x && y] *)

  val c_logor : Cil.ikind -> t -> t -> t
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

  val equal_to: int_t -> t -> [`Eq | `Neq | `Top]

  val to_bool: t -> bool option
  (** Give a boolean interpretation of an abstract value if possible, otherwise
    * don't return anything.*)

  val to_excl_list: t -> (int_t list * (int64 * int64)) option
  (** Gives a list representation of the excluded values from included range of bits if possible. *)

  val of_excl_list: Cil.ikind -> int_t list -> t
  (** Creates an exclusion set from a given list of integers. *)

  val is_excl_list: t -> bool
  (** Checks if the element is an exclusion set. *)

  val to_incl_list: t -> int_t list option
  (** Gives a list representation of the included values if possible. *)

  val maximal    : t -> int_t option
  val minimal    : t -> int_t option

  (** {b Cast} *)

  val cast_to: ?suppress_ovwarn:bool -> ?torg:Cil.typ -> Cil.ikind -> t -> t
  (** Cast from original type [torg] to integer type [Cil.ikind]. Currently, [torg] is only present for actual casts. The function is also called to handle overflows/wrap around after operations. In these cases (where the type stays the same) [torg] is None. *)

end

(** The signature of integral value domains. They need to support all integer
  * operations that are allowed in C *)

module type IkindUnawareS =
sig
  include B
  include Arith with type t:= t
  val starting   : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending     : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val of_int: int_t -> t
  (** Transform an integer literal to your internal domain representation. *)

  val of_bool: bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t

  val of_congruence: Cil.ikind -> int_t * int_t -> t
  val arbitrary: unit -> t QCheck.arbitrary
  val invariant: Cil.exp -> t -> Invariant.t
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
  val cast_to : ?suppress_ovwarn:bool -> ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t
  (** @param no_ov If true, assume no overflow can occur. *)

  val join: Cil.ikind -> t ->  t -> t
  val meet: Cil.ikind -> t -> t -> t
  val narrow: Cil.ikind -> t -> t -> t
  val widen: Cil.ikind -> t -> t -> t
  val starting : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val of_int: Cil.ikind -> int_t -> t
  (** Transform an integer literal to your internal domain representation. *)

  val of_bool: Cil.ikind -> bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t
  val of_congruence: Cil.ikind -> int_t * int_t -> t
  val is_top_of: Cil.ikind -> t -> bool
  val invariant_ikind : Cil.exp -> Cil.ikind -> t -> Invariant.t

  val refine_with_congruence: Cil.ikind -> t -> (int_t * int_t) option -> t
  val refine_with_interval: Cil.ikind -> t -> (int_t * int_t) option -> t
  val refine_with_excl_list: Cil.ikind -> t -> (int_t list * (int64 * int64)) option -> t
  val refine_with_incl_list: Cil.ikind -> t -> int_t list option -> t

  val project: Cil.ikind -> PrecisionUtil.int_precision -> t -> t
  val arbitrary: Cil.ikind -> t QCheck.arbitrary
end
(** Interface of IntDomain implementations taking an ikind for arithmetic operations *)

module type SOverflow =
sig

  include S

  val add : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * overflow_info

  val sub : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * overflow_info

  val mul : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * overflow_info

  val div : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * overflow_info

  val neg : ?no_ov:bool -> Cil.ikind ->  t -> t * overflow_info

  val cast_to : ?suppress_ovwarn:bool -> ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t * overflow_info

  val of_int : Cil.ikind -> int_t -> t * overflow_info

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t * overflow_info

  val starting : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t * overflow_info
  val ending : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t * overflow_info

  val shift_left : Cil.ikind -> t -> t -> t * overflow_info

  val shift_right: Cil.ikind -> t -> t -> t * overflow_info


end

module SOverflowUnlifter (D : SOverflow) : S with type int_t = D.int_t and type t = D.t


module type Y =
sig
  include B
  include Arith with type t:=t

  val of_int: Cil.ikind -> int_t -> t
  (** Transform an integer literal to your internal domain representation with the specified ikind. *)

  val of_bool: Cil.ikind -> bool -> t
  (** Transform a known boolean value to the default internal representation of the specified ikind. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t

  val of_congruence: Cil.ikind -> int_t * int_t -> t

  val starting   : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending     : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t

  val is_top_of: Cil.ikind -> t -> bool

  val project: PrecisionUtil.int_precision -> t -> t
  val invariant: Cil.exp -> t -> Invariant.t
end
(** The signature of integral value domains keeping track of ikind information *)

module type Z = Y with type int_t = Z.t

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
  val no_intervalSet: t -> t
  val ikind: t -> ikind
end

val of_const: Z.t * Cil.ikind * string option -> IntDomTuple.t


module Size : sig
  (** The biggest type we support for integers. *)
  val top_typ         : Cil.typ
  val range           : Cil.ikind -> Z.t * Z.t
  val is_cast_injective : from_type:Cil.typ -> to_type:Cil.typ -> bool
  val bits            : Cil.ikind -> int * int
  val cast            : Cil.ikind -> Z.t -> Z.t
end

module BISet: SetDomain.S with type elt = Z.t

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
module Integers(Ints_t : IntOps.IntOps): IkindUnawareS with type t = Ints_t.t and type int_t = Ints_t.t
(** The integers with their natural orderings. Calling [top] and [bot] will
  * raise exceptions. *)

module FlatPureIntegers: IkindUnawareS with type t = IntOps.Int64Ops.t and type int_t = IntOps.Int64Ops.t
(** The integers with flattened orderings. Calling [top] and [bot] or [join]ing
    or [meet]ing inequal elements will raise exceptions. *)

module Flattened : IkindUnawareS with type t = [`Top | `Lifted of IntOps.Int64Ops.t | `Bot] and type int_t = IntOps.Int64Ops.t
(** This is the typical flattened integer domain used in Kildall's constant
  * propagation. *)

module Lifted : IkindUnawareS with type t = [`Top | `Lifted of int64 | `Bot] and type int_t = int64
(** Artificially bounded integers in their natural ordering. *)

module IntervalFunctor(Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option

module BitFieldFunctor(Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t)

module IntervalSetFunctor(Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) list

module Interval32 :Y with (* type t = (IntOps.Int64Ops.t * IntOps.Int64Ops.t) option and *) type int_t = IntOps.Int64Ops.t

module Interval : SOverflow with type int_t = Z.t

module Bitfield : SOverflow with type int_t = Z.t

module IntervalSet : SOverflow with type int_t = Z.t

module Congruence : S with type int_t = Z.t

module DefExc : S with type int_t = Z.t
(** The DefExc domain. The Flattened integer domain is topped by exclusion sets.
  * Good for analysing branches. *)

(** {b Domain constructors} *)

module Flat (Base: IkindUnawareS): IkindUnawareS with type t = [ `Bot | `Lifted of Base.t | `Top ] and type int_t = Base.int_t
(** Creates a flat value domain, where all ordering is lost. Arithmetic
  * operations are lifted such that only lifted values can be evaluated
  * otherwise the top/bot is simply propagated with bot taking precedence over
  * top. *)

module Lift (Base: IkindUnawareS): IkindUnawareS with type t = [ `Bot | `Lifted of Base.t | `Top ] and type int_t = Base.int_t
(** Just like {!Value.Flat} except the order is preserved. *)

module Reverse (Base: IkindUnawareS): IkindUnawareS with type t = Base.t and type int_t = Base.int_t
(** Reverses bot, top, leq, join, meet *)

(* module Interval : S *)
(** Interval domain with int64-s --- use with caution! *)

(* module IncExcInterval : S with type t = [ | `Excluded of Interval.t| `Included of Interval.t ] *)
(** Inclusive and exclusive intervals. Warning: NOT A LATTICE *)
module Enums : S with type int_t = Z.t
