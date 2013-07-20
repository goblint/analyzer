(* --------------------------------------------------------------
 * IntOps Basics
 * -------------------------------------------------------------- *)

(* IntOps
 * Wrapper around integer types providing unified interface for
 * arithmetic and logical operations. *)
module type IntOps =
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
    val logand : t -> t -> t
    val logor : t -> t -> t
    val lognot : t -> t

    (* Comparison *)
    val compare : t -> t -> int
    val top_range : t -> t -> bool

    (* Conversions *)
    val of_int : int -> t
    val to_int : t -> int
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val of_string : string -> t
    val to_string : t -> string
  end

(* --------------------------------------------------------------
 * IntOps Implementations
 * -------------------------------------------------------------- *)
module NIntOps : IntOps with type t = int =
  struct
    type t = int;;
    let zero = 0;;
    let one = 1;;
    let lower_bound = Some min_int;;
    let upper_bound = Some max_int;;

    let neg x = (- x);;
    let add = (+);;
    let sub = (-);;
    let mul a b = a * b;;
    let div = (/);;
    let rem = (mod);;

    let shift_left = (lsl);;
    let shift_right = (lsr);;
    let logand = (land);;
    let logor = (lor);;
    let lognot = (lnot);;

    let compare = (compare);;    
    let top_range a b = (a = min_int) && (b = max_int);;

    let of_int x = x;;
    let to_int x = x;;
    let of_int64 = Int64.to_int;;
    let to_int64 = Int64.of_int;;
    let of_string = int_of_string;;
    let to_string = string_of_int;;
  end

module Int32Ops : IntOps with type t = int32 =
  struct
    type t = int32;;
    let zero = 0l;;
    let one = 1l;;
    let lower_bound = Some Int32.min_int;;
    let upper_bound = Some Int32.max_int;;

    let neg = Int32.neg;;
    let add = Int32.add;;
    let sub = Int32.sub;;
    let mul = Int32.mul;;
    let div = Int32.div;;
    let rem = Int32.rem;;

    let shift_left = Int32.shift_left;;
    let shift_right = Int32.shift_right_logical;;
    let logand = Int32.logand;;
    let logor = Int32.logor;;
    let lognot = Int32.lognot;;

    let compare = Int32.compare;;
    let top_range a b =
      (0 = compare a Int32.min_int) && (0 = compare b Int32.max_int);;

    let of_int = Int32.of_int;;
    let to_int = Int32.to_int;;
    let of_int64 = Int64.to_int32;;
    let to_int64 = Int64.of_int32;;
    let of_string = Int32.of_string;;
    let to_string = Int32.to_string;;
  end

module Int64Ops : IntOps with type t = int64 =
  struct
    type t = int64;;
    let zero = 0L;;
    let one = 1L;;
    let lower_bound = Some Int64.min_int;;
    let upper_bound = Some Int64.max_int;;

    let neg = Int64.neg;;
    let add = Int64.add;;
    let sub = Int64.sub;;
    let mul = Int64.mul;;
    let div = Int64.div;;
    let rem = Int64.rem;;

    let shift_left = Int64.shift_left;;
    let shift_right = Int64.shift_right_logical;;
    let logand = Int64.logand;;
    let logor = Int64.logor;;
    let lognot = Int64.lognot;;

    let compare = Int64.compare;;
    let top_range a b =
      (0 = compare a Int64.min_int) && (0 = compare b Int64.max_int);;

    let of_int = Int64.of_int;;
    let to_int = Int64.to_int;;
    let of_int64 x = x;;
    let to_int64 x = x;;
    let of_string = Int64.of_string;;
    let to_string = Int64.to_string;;
  end

module BigIntOps : IntOps with type t = Big_int.big_int =
  struct
    type t = Big_int.big_int;;
    let zero = Big_int.zero_big_int;;
    let one = Big_int.unit_big_int;; 
    let upper_bound = None;;
    let lower_bound = None;;

    let neg = Big_int.minus_big_int;;
    let add = Big_int.add_big_int;;
    let sub = Big_int.sub_big_int;;
    let mul = Big_int.mult_big_int;;
    let div = Big_int.div_big_int;;
    let rem = Big_int.mod_big_int;;

    let shift_left = Big_int.shift_left_big_int;;
    let shift_right = Big_int.shift_right_big_int;;
    let logand = Big_int.and_big_int;;
    let logor = Big_int.or_big_int;;
    let lognot x = sub (neg x) one;;

    let compare = Big_int.compare_big_int;;
    let top_range _ _ = false;;

    let of_int = Big_int.big_int_of_int;;
    let to_int = Big_int.int_of_big_int;;
    let of_int64 x = Big_int.big_int_of_int64 x;;
    let to_int64 x = Big_int.int64_of_big_int x;;
    let of_string = Big_int.big_int_of_string;;
    let to_string = Big_int.string_of_big_int;;
  end
