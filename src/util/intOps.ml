(* --------------------------------------------------------------
 * IntOps Basics
 * -------------------------------------------------------------- *)

open Batteries

(* IntOps
 * Wrapper around integer types providing unified interface for
 * arithmetic and logical operations. *)
module type IntOpsBase =
sig
  type t

  (* Constants *)
  val zero : t
  val one : t
  val lower_bound : t option (* TODO: unused *)
  val upper_bound : t option (* TODO: unused *)

  (* Arithmetic *)
  val neg : t -> t
  val abs : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  (* This should be the remainder, not the Euclidian Modulus *)
  (* -1 rem 5 == -1, whereas -1 Euclid-Mod 5 == 4 *)
  val rem : t -> t -> t
  val gcd : t-> t -> t

  (* Bitwise *)
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val bitand : t -> t -> t
  val bitor : t -> t -> t
  val bitxor : t -> t -> t
  val bitnot : t -> t

  (* Comparison *)
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val top_range : t -> t -> bool (* TODO: unused *)
  val max : t -> t -> t
  val min : t -> t -> t

  (* Conversions *)
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t (* TODO: unused *)
  val to_string : t -> string
  val of_bigint : Big_int_Z.big_int -> t
  val to_bigint : t -> Big_int_Z.big_int
end

module type IntOps =
sig
  include IntOpsBase
  (* Logical: These are intended to be the logical operations in the C sense!   *)
  (* Int64 calls its bit-wise operations e.g. logand, we call those e.g. bitand *)
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val to_bool : t -> bool
  val of_bool : bool -> t
end

(* --------------------------------------------------------------
 * IntOps Implementations
 * -------------------------------------------------------------- *)
module NIntOpsBase : IntOpsBase with type t = int =
struct
  type t = int [@@deriving hash]
  let zero = 0
  let one = 1
  let lower_bound = Some min_int
  let upper_bound = Some max_int

  let neg x = (- x)
  let abs = abs
  let add = (+)
  let sub = (-)
  let mul a b = a * b
  let div = (/)
  let rem = (mod)
  let gcd x y =
    let rec gcd' x y = if y = 0 then x else gcd' y (rem x y) in
    abs @@ gcd' x y

  let shift_left = (lsl)
  let shift_right = (lsr)
  let bitand = (land)
  let bitor = (lor)
  let bitxor = (lxor)
  let bitnot = (lnot)


  let compare = compare
  let equal = Int.equal
  let top_range a b = (a = min_int) && (b = max_int)
  let max = Int.max
  let min = Int.min

  let of_int x = x
  let to_int x = x
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let of_string = int_of_string
  let to_string = string_of_int
  let of_bigint = Z.to_int
  let to_bigint = Z.of_int
end

module Int32OpsBase : IntOpsBase with type t = int32 =
struct
  type t = int32 [@@deriving hash]
  let zero = 0l
  let one = 1l
  let lower_bound = Some Int32.min_int
  let upper_bound = Some Int32.max_int

  let neg = Int32.neg
  let abs = Int32.abs
  let add = Int32.add
  let sub = Int32.sub
  let mul = Int32.mul
  let div = Int32.div
  let rem = Int32.rem
  let gcd x y =
    let rec gcd' x y = if y = zero then x else gcd' y (rem x y) in
    abs @@ gcd' x y

  let shift_left = Int32.shift_left
  let shift_right = Int32.shift_right_logical
  let bitand = Int32.logand (* Int32 calls bitwise operations 'log' *)
  let bitor = Int32.logor (* Int32 calls bitwise operations 'log' *)
  let bitxor = Int32.logxor (* Int32 calls bitwise operations 'log' *)

  let bitnot = Int32.lognot (* Int32 calls bitwise operations 'log' *)

  let compare = Int32.compare
  let equal = Int32.equal

  let top_range a b =
    (0 = compare a Int32.min_int) && (0 = compare b Int32.max_int)
  let max = Int32.max
  let min = Int32.min

  let of_int = Int32.of_int
  let to_int = Int32.to_int
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let of_string = Int32.of_string
  let to_string = Int32.to_string
  let of_bigint = Z.to_int32
  let to_bigint = Z.of_int32
end

module Int64OpsBase : IntOpsBase with type t = int64 =
struct
  type t = int64 [@@deriving hash]
  let zero = 0L
  let one = 1L
  let lower_bound = Some Int64.min_int
  let upper_bound = Some Int64.max_int

  let neg = Int64.neg
  let abs = Int64.abs
  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul
  let div = Int64.div
  let rem = Int64.rem
  let gcd x y =
    let rec gcd' x y = if y = zero then x else gcd' y (rem x y) in
    abs @@ gcd' x y

  let shift_left = Int64.shift_left
  let shift_right = Int64.shift_right_logical
  let bitand = Int64.logand (* Int64 calls bitwise operations 'log' *)
  let bitor = Int64.logor (* Int64 calls bitwise operations 'log' *)
  let bitxor = Int64.logxor (* Int64 calls bitwise operations 'log' *)

  let bitnot = Int64.lognot (* Int64 calls bitwise operations 'log' *)

  let compare = Int64.compare
  let equal = Int64.equal

  let top_range a b =
    (0 = compare a Int64.min_int) && (0 = compare b Int64.max_int)
  let max = Int64.max
  let min = Int64.min

  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let of_int64 x = x
  let to_int64 x = x
  let of_string = Int64.of_string
  let to_string = Int64.to_string
  let of_bigint = Z.to_int64
  let to_bigint = Z.of_int64
end

module BigIntOpsBase : IntOpsBase with type t = Big_int_Z.big_int =
struct
  type t = Big_int_Z.big_int
  let zero = Z.zero
  let one = Z.one
  let upper_bound = None
  let lower_bound = None

  let neg = Z.neg
  let abs = Z.abs
  let add = Z.add
  let sub = Z.sub
  let mul = Z.mul

  (* If the first operand of a div is negative, Zarith rounds the result away from zero.
     We thus always transform this into a division with a non-negative first operand.
  *)
  let div a b = if Z.compare a zero < 0 then Z.neg (Z.ediv (Z.neg a) b) else Z.ediv a b

  (* Big_int_Z.mod_big_int computes the Euclidian Modulus, but what we want here is the remainder, as returned by mod on ints
     -1 rem 5 == -1, whereas -1 Euclid-Mod 5 == 4
  *)
  let rem a b = Z.sub a (mul b (div a b))

  let gcd x y = abs @@ Z.gcd x y
  let compare = Z.compare
  let equal = Z.equal
  let hash = Z.hash

  let top_range _ _ = false

  let max = Z.max
  let min = Z.min

  let of_int = Z.of_int
  let to_int = Z.to_int
  let of_int64 x = Z.of_int64 x
  let to_int64 x = Z.to_int64 x
  let of_string = Z.of_string
  let to_string = Z.to_string
  let of_bigint x = x
  let to_bigint x = x

  let shift_left = Z.shift_left
  let shift_right = Z.shift_right
  let bitnot x = sub (neg x) one
  let bitand = Z.logand
  let bitor = Z.logor
  let bitxor = Z.logxor

end


module IntOpsDecorator(B: IntOpsBase) =
struct
  include B
  let pred x = sub x one
  let of_bool x = if x then one else zero
  let to_bool x = x <> zero

  (* These are logical operations in the C sense! *)
  let log_op op a b = of_bool @@ op (to_bool a) (to_bool b)
  let lognot x = of_bool (x = zero)
  let logand = log_op (&&)
  let logor = log_op (||)
  let logxor = log_op (<>)

  let lt x y = of_bool (compare x y < 0)
  let gt x y = of_bool (compare x y > 0)
  let le x y = of_bool (compare x y <= 0)
  let ge x y = of_bool (compare x y >= 0)
end

module BigIntOps = struct
  include IntOpsDecorator(BigIntOpsBase)
  let trailing_zeros x = Z.trailing_zeros x
end
module NIntOps = IntOpsDecorator(NIntOpsBase)
module Int32Ops = IntOpsDecorator(Int32OpsBase)
module Int64Ops = IntOpsDecorator(Int64OpsBase)
