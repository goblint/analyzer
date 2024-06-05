(* Order must match with round_mode in stubs.c *)
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
  val to_float: t -> float
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

let big_int_of_float f =
  let x, n = Float.frexp f in
  let shift = min 52 n in
  let x' = x *. Float.pow 2. (Float.of_int shift) in
  Z.mul
    (Z.of_int64 (Int64.of_float x'))
    (Z.pow (Z.of_int 2) (n - shift))

module CDouble = struct
  type t = float [@@deriving eq, ord, hash, to_yojson]

  let name = "double"
  let zero = Float.zero
  let upper_bound = Float.max_float
  let lower_bound = -. Float.max_float
  let smallest = Float.min_float
  let pi = Float.pi

  let of_float _ x = x
  let to_float x = x
  let to_big_int = big_int_of_float

  let is_finite = Float.is_finite
  let pred = Float.pred
  let succ = Float.succ

  let to_string = Float.to_string

  let neg = Float.neg
  let fabs = Float.abs
  let floor  = Float.floor
  let ceil = Float.ceil
  external add: round_mode -> t -> t -> t = "add_double"
  external sub: round_mode -> t -> t -> t = "sub_double"
  external mul: round_mode -> t -> t -> t = "mul_double"
  external div: round_mode -> t -> t -> t = "div_double"
  external sqrt: round_mode -> t -> t = "sqrt_double"

  external acos: round_mode -> t -> t = "acos_double"
  external asin: round_mode -> t -> t = "asin_double"
  external atan: round_mode -> t -> t = "atan_double"
  external cos: round_mode -> t -> t = "cos_double"
  external sin: round_mode -> t -> t = "sin_double"
  external tan: round_mode -> t -> t = "tan_double"

  external atof: round_mode -> string -> t = "atof_double"
end

module CFloat : CFloatType = struct
  type t = float [@@deriving eq, ord, hash, to_yojson]

  let name = "float"
  let zero = Float.zero

  external upper': unit -> float = "max_float"
  external smallest': unit -> float = "smallest_float"
  external pi': unit -> float = "pi_float"

  let upper_bound = upper' ()
  let lower_bound = -. upper_bound
  let smallest = smallest' ()
  let pi = pi' ()

  let to_float x = x
  let to_big_int = big_int_of_float

  let is_finite x = Float.is_finite x && x >= lower_bound && x <= upper_bound

  let to_string = Float.to_string

  let neg = Float.neg
  let fabs = Float.abs
  let floor  = Float.floor
  let ceil = Float.ceil
  external add: round_mode -> t -> t -> t = "add_float"
  external sub: round_mode -> t -> t -> t = "sub_float"
  external mul: round_mode -> t -> t -> t = "mul_float"
  external div: round_mode -> t -> t -> t = "div_float"
  external sqrt: round_mode -> t -> t = "sqrt_float"

  external acos: round_mode -> t -> t = "acos_float"
  external asin: round_mode -> t -> t = "asin_float"
  external atan: round_mode -> t -> t = "atan_float"
  external cos: round_mode -> t -> t = "cos_float"
  external sin: round_mode -> t -> t = "sin_float"
  external tan: round_mode -> t -> t = "tan_float"

  external atof: round_mode -> string -> t = "atof_float"

  let of_float mode x = add mode zero x
  let pred x = of_float Down (Float.pred x)
  let succ x = of_float Up (Float.succ x)
end
