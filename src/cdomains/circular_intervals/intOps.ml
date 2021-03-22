(* --------------------------------------------------------------
 * IntOps Basics
 * -------------------------------------------------------------- *)

(* IntOps
 * Wrapper around integer types providing unified interface for
 * arithmetic and logical operations. *)
module type IntOpsBase =
sig
  type t

  (* Constants *)
  val zero : t
  val one : t
  val lower_bound : t option
  val upper_bound : t option

  (* Arithmetic *)
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t

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
  val top_range : t -> t -> bool

  (* Conversions *)
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val of_bigint : Big_int_Z.big_int -> t
  val to_bigint : t -> Big_int_Z.big_int
end

module type IntOps =
sig
  include IntOpsBase
  (* Logical *)
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
  type t = int
  let zero = 0
  let one = 1
  let lower_bound = Some min_int
  let upper_bound = Some max_int

  let neg x = (- x)
  let add = (+)
  let sub = (-)
  let mul a b = a * b
  let div = (/)
  let rem = (mod)

  let shift_left = (lsl)
  let shift_right = (lsr)
  let bitand = (land)
  let bitor = (lor)
  let bitxor = Int.logxor
  let bitnot = (lnot)


  let compare = compare
  let equal = Int.equal
  let top_range a b = (a = min_int) && (b = max_int)

  let of_int x = x
  let to_int x = x
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let of_string = int_of_string
  let to_string = string_of_int
  let of_bigint = Big_int_Z.int_of_big_int
  let to_bigint = Big_int_Z.big_int_of_int
end

module Int32OpsBase : IntOpsBase with type t = int32 =
struct
  type t = int32
  let zero = 0l
  let one = 1l
  let lower_bound = Some Int32.min_int
  let upper_bound = Some Int32.max_int

  let neg = Int32.neg
  let add = Int32.add
  let sub = Int32.sub
  let mul = Int32.mul
  let div = Int32.div
  let rem = Int32.rem

  let shift_left = Int32.shift_left
  let shift_right = Int32.shift_right_logical
  let bitand = Int32.logand
  let bitor = Int32.logor
  let bitxor = Int32.logxor

  let bitnot = Int32.lognot

  let compare = Int32.compare
  let equal = Int32.equal

  let top_range a b =
    (0 = compare a Int32.min_int) && (0 = compare b Int32.max_int)

  let of_int = Int32.of_int
  let to_int = Int32.to_int
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let of_string = Int32.of_string
  let to_string = Int32.to_string
  let of_bigint = Big_int_Z.int32_of_big_int
  let to_bigint = Big_int_Z.big_int_of_int32
end

module Int64OpsBase : IntOpsBase with type t = int64 =
struct
  type t = int64
  let zero = 0L
  let one = 1L
  let lower_bound = Some Int64.min_int
  let upper_bound = Some Int64.max_int

  let neg = Int64.neg
  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul
  let div = Int64.div
  let rem = Int64.rem

  let shift_left = Int64.shift_left
  let shift_right = Int64.shift_right_logical
  let bitand = Int64.logand
  let bitor = Int64.logor
  let bitxor = Int64.logxor

  let bitnot = Int64.lognot

  let compare = Int64.compare
  let equal = Int64.equal

  let top_range a b =
    (0 = compare a Int64.min_int) && (0 = compare b Int64.max_int)

  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let of_int64 x = x
  let to_int64 x = x
  let of_string = Int64.of_string
  let to_string = Int64.to_string
  let of_bigint = Big_int_Z.int64_of_big_int
  let to_bigint = Big_int_Z.big_int_of_int64
end

module BigIntOpsBase =
struct
  type t = Big_int_Z.big_int
  let zero = Big_int_Z.zero_big_int
  let one = Big_int_Z.unit_big_int
  let upper_bound = None
  let lower_bound = None

  let neg = Big_int_Z.minus_big_int
  let add = Big_int_Z.add_big_int
  let sub = Big_int_Z.sub_big_int
  let mul = Big_int_Z.mult_big_int
  let div = Big_int_Z.div_big_int
  let rem = Big_int_Z.mod_big_int

  let compare = Big_int_Z.compare_big_int
  let equal = Big_int_Z.eq_big_int

  let top_range _ _ = false

  let of_int = Big_int_Z.big_int_of_int
  let to_int = Big_int_Z.int_of_big_int
  let of_int64 x = Big_int_Z.big_int_of_int64 x
  let to_int64 x = Big_int_Z.int64_of_big_int x
  let of_string = Big_int_Z.big_int_of_string
  let to_string = Big_int_Z.string_of_big_int
  let of_bigint x = x
  let to_bigint x = x

  let of_bool = function
    | true -> one
    | false -> zero

  let to_bool x = if equal x zero then false else true

  let shift_left = Big_int_Z.shift_left_big_int
  let shift_right = Big_int_Z.shift_right_big_int
  let bitnot x = sub (neg x) one
  let bitand = Big_int_Z.and_big_int
  let bitor = Big_int_Z.or_big_int
  let bitxor = Big_int_Z.xor_big_int

end


module IntOpsDecorator(B: IntOpsBase) =
struct
  include B
  let pred x = sub x one
  let of_bool x = if x then one else zero
  let to_bool x = x = zero
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

module BigIntOps = IntOpsDecorator(BigIntOpsBase)
module NIntOps = IntOpsDecorator(NIntOpsBase)
module Int32Ops = IntOpsDecorator(Int32OpsBase)
module Int64Ops = IntOpsDecorator(Int64OpsBase)
