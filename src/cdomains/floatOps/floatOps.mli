type round_mode =
  | Nearest
  | ToZero
  | Up
  | Down

module type CFloatType = sig
  type t

  val name: string
  val zero: t
  val upper_bound: t
  val lower_bound: t
  val smallest : t

  val of_float: round_mode -> float -> t
  val to_float: t -> float option
  val to_big_int: t -> Big_int_Z.big_int

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
  val atof: round_mode -> string -> t
end

module CDouble : CFloatType
module CFloat : CFloatType
