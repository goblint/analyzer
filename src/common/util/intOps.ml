(** Unified interface for integer types. *)

open Batteries

(* IntOps
 * Wrapper around integer types providing unified interface for
 * arithmetic and logical operations. *)
module type IntOpsBase =
sig
  type t

  val name : unit -> string

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
  (* shift_left x y shifts x to the left by y bits. *)
  val shift_right : t -> int -> t
  (* shift_right x y shifts x to the right by y bits. *)
  val logand : t -> t -> t
  (* Bitwise logical and. *)
  val logor : t -> t -> t
  (* Bitwise logical or. *)
  val logxor : t -> t -> t
  (* Bitwise logical exclusive or. *)
  val lognot : t -> t
  (* Bitwise logical negation. *)

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
  val of_bigint : Z.t -> t
  val to_bigint : t -> Z.t

  val arbitrary : unit -> t QCheck.arbitrary
end

module type IntOps =
sig
  include IntOpsBase
  (* Logical: These are intended to be the logical operations in the C sense! *)
  (* Int64 calls its bit-wise operations e.g. logand, without the c_ prefix *)
  val c_logand : t -> t -> t
  val c_logor : t -> t -> t
  val c_logxor : t -> t -> t
  val c_lognot : t -> t
  val to_bool : t -> bool
  val of_bool : bool -> t
end

(* --------------------------------------------------------------
 * IntOps Implementations
 * -------------------------------------------------------------- *)
module NIntOpsBase : IntOpsBase with type t = int =
struct
  type t = int [@@deriving eq, ord, hash]
  let name () = "int"
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
  let logand = (land)
  let logor = (lor)
  let logxor = (lxor)
  let lognot = (lnot)

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

  let arbitrary () = QCheck.int
end

module Int32OpsBase : IntOpsBase with type t = int32 =
struct
  type t = int32 [@@deriving eq, ord, hash]
  let name () = "int32"
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
  let logand = Int32.logand (* Int32 calls bitwise operations 'log' *)
  let logor = Int32.logor (* Int32 calls bitwise operations 'log' *)
  let logxor = Int32.logxor (* Int32 calls bitwise operations 'log' *)

  let lognot = Int32.lognot (* Int32 calls bitwise operations 'log' *)

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

  let arbitrary () = QCheck.int32
end

module Int64OpsBase : IntOpsBase with type t = int64 =
struct
  type t = int64 [@@deriving eq, ord, hash]
  let name () = "int64"
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
  let logand = Int64.logand (* Int64 calls bitwise operations 'log' *)
  let logor = Int64.logor (* Int64 calls bitwise operations 'log' *)
  let logxor = Int64.logxor (* Int64 calls bitwise operations 'log' *)

  let lognot = Int64.lognot (* Int64 calls bitwise operations 'log' *)

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

  let arbitrary () = QCheck.int64
end

module BigIntOpsBase : IntOpsBase with type t = Z.t =
struct
  type t = Z.t [@@deriving eq, ord, hash]
  let name () = "Z"
  let zero = Z.zero
  let one = Z.one
  let upper_bound = None
  let lower_bound = None

  let neg = Z.neg
  let abs = Z.abs
  let add = Z.add
  let sub = Z.sub
  let mul = Z.mul
  let div = Z.div
  let rem = Z.rem

  let gcd = Z.gcd

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
  let lognot = Z.lognot
  let logand = Z.logand
  let logor = Z.logor
  let logxor = Z.logxor

  let arbitrary () = QCheck.map ~rev:Z.to_int64 Z.of_int64 QCheck.int64
end


module IntOpsDecorator(B: IntOpsBase) =
struct
  include Printable.StdLeaf
  include B
  let show = to_string
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = to_string
    end
    )
  let pred x = sub x one
  let of_bool x = if x then one else zero
  let to_bool x = x <> zero

  (* These are logical operations in the C sense! *)
  let log_op op a b = of_bool @@ op (to_bool a) (to_bool b)
  let c_lognot x = of_bool (x = zero)
  let c_logand = log_op (&&)
  let c_logor = log_op (||)
  let c_logxor = log_op (<>)

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
