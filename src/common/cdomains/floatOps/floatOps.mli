(** Unified interface for floating-point types. *)

type round_mode =
  | Nearest
  | ToZero
  | Up
  | Down

module type CFloatType = sig
  type t = float

  val name: string
  val zero: t
  val upper_bound: t
  val lower_bound: t
  val smallest : t
  val pi : t

  val of_float: round_mode -> float -> t
  val to_big_int: t -> Z.t

  val is_finite: t -> bool
  val pred: t -> t
  val succ: t -> t

  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int
  val to_yojson: t -> Yojson.Safe.t
  val to_string: t -> string


  val neg: t -> t
  val fabs: t -> t
  val floor: t -> t
  val ceil: t -> t
  val add: round_mode -> t -> t -> t
  val sub: round_mode -> t -> t -> t
  val mul: round_mode -> t -> t -> t
  val div: round_mode -> t -> t -> t
  val sqrt: round_mode -> t -> t
  val acos: round_mode -> t -> t
  val asin: round_mode -> t -> t
  val atan: round_mode -> t -> t
  val cos: round_mode -> t -> t
  val sin: round_mode -> t -> t
  val tan: round_mode -> t -> t
  val atof: round_mode -> string -> t
end

module CDouble : CFloatType
module CFloat : CFloatType
