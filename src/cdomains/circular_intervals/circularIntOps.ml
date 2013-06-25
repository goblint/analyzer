open IntOps

(* CircularIntOps
 * Interface for Fixed Width Integer implementation with
 * well-defined Overflow behaviour. *)
module type CircularIntOps =
  sig
    type t

    (* Constants *)
    val zero : t
    val one : t
    val lower_bound : int -> t option
    val upper_bound : int -> t option

    (* Arithmetic *)
    val neg : int -> t -> t
    val add : int -> t -> t -> t
    val sub : int -> t -> t -> t
    val mul : int -> t -> t -> t
    val div : int -> t -> t -> t
    val rem : int -> t -> t -> t
    val inc : int -> t -> t
    val dec : int -> t -> t

    val min : t list -> t
    val max : t list -> t

    (* Bitwise *)
    val shift_left : t -> int -> t
    val shift_right : t -> int -> t
    val logand : t -> t -> t
    val logor : t -> t -> t
    val lognot : int -> t -> t

    (* Comparison *)
    val compare : t -> t -> int
    val lt : t -> t -> bool;;
    val gt : t -> t -> bool;;
    val eq : t -> t -> bool;;
    val leq : t -> t -> bool;;
    val geq : t -> t -> bool;;
    val top_range : int -> t -> t -> bool
    val in_range : int -> t -> bool

    (* Conversions *)
    val of_int : int -> int -> t
    val to_int : int -> t -> int
    val of_int64 : int -> int64 -> t
    val to_int64 : int -> t -> int64
    val of_string : int -> string -> t
    val to_string : int -> t -> string
    
    (* Circle Constants *)
    val max_value : int -> t
    val top_value : int -> t

    (* Additional *)
    val wrap : int -> t -> t
    val arith_shift_right : int -> t -> int -> t

    (* Raw Operations *)
    val add' : t -> t -> t
    val sub' : t -> t -> t
    val mul' : t -> t -> t
    val div' : t -> t -> t
    val rem' : t -> t -> t

  end

(* CircularInt
 * Wrapper for any Integer Type to be restricted to a fixed width.
 * Integers will be represented as unsigned values, meaning that an
 * IntOps module designed for n-bit types can only be used with
 * a width < n. *)
module CircularInt (I : IntOps) : CircularIntOps with type t = I.t =
  struct
    type t = I.t;;

    (* Circle Constants *)
    let top_value w = I.shift_left I.one w;;
    let max_value w = I.sub (top_value w) I.one;;
    let upper_bound w = Some (max_value w);;
    let lower_bound w = Some I.zero;;
    let zero = I.zero;;
    let one = I.one;;

    (* Wrap Function *)
    let wrap w x = I.rem x (top_value w);;
    let wrap_fn1 f = fun w a -> wrap w (f a);;
    let wrap_fn2 f = fun w a b -> wrap w (f a b);;

    (* Arithmetic *)
    let neg = wrap_fn1 I.neg;;
    let add = wrap_fn2 I.add;;
    let sub = wrap_fn2 I.sub;;
    let mul = wrap_fn2 I.mul;;
    let div = wrap_fn2 I.div;;
    let rem = wrap_fn2 I.rem;;
    let inc w x = add w x I.one;;
    let dec w x = sub w x I.one;;

    (* Compare *)
    let compare = I.compare;;
    let lt a b = (compare a b) < 0;;
    let gt a b = (compare a b) > 0;;
    let eq a b = (compare a b) = 0;;
    let leq a b = (compare a b) <= 0;;
    let geq a b = (compare a b) >= 0;;
    let top_range w a b = eq (inc w b) a;;
    
    (* Bitwise *)
    let shift_left a b = I.shift_left a b;;
    let shift_right a b = I.shift_right a b;; 
    let logand a b = I.logand a b;;
    let logor a b = I.logor a b;;
    let lognot w x = wrap w (I.lognot x);;
    
    (* Min/Max *)
    let min xs =
      List.fold_left 
        (fun m x -> if lt x m then x else m)
        (List.hd xs) xs;;

    let max xs =
      List.fold_left 
        (fun m x -> if gt x m then x else m)
        (List.hd xs) xs;;

    (* Specific Comparisons *)
    let in_range w x =
      match lower_bound w,upper_bound w with
      | None, None ->  true
      | None, Some max -> leq x max
      | Some min, None -> geq x min
      | Some min, Some max -> (leq x max) && (geq x min);;

    (* Arithmetic Shifting *)
    let arith_shift_right w x k = 
      let n = shift_right (max_value w) 1 in
      if leq x n
      then shift_right x k
      else if k >= w then max_value w
      else
        let msk = wrap w (shift_left (max_value w) (w - k)) in
        logor (shift_right x k) msk;;

    (* Conversion *)
    let to_int w x = I.to_int (wrap w x);;
    let of_int w x = wrap w (I.of_int x);;
    let to_int64 w x = I.to_int64 (wrap w x);;
    let of_int64 w x = wrap w (I.of_int64 x);;
    let to_string w x = I.to_string (wrap w x);;
    let of_string w x = wrap w (I.of_string x);;

    (* Raw Operations *)
    let add' = I.add;;
    let sub' = I.sub;;
    let mul' = I.mul;;
    let div' = I.div;;
    let rem' = I.rem;;
  end

(* Shorthands *)
module CircularNInt = CircularInt (NIntOps);;
module CircularInt32 = CircularInt (Int32Ops);;
module CircularInt64 = CircularInt (Int64Ops);;
module CircularBigInt = CircularInt (BigIntOps);;
