open CircularIntOps

(* Circular Interval Type
 * - either the full interval
 * - or the empty interval
 * - or an interval with lower and upper boundaries (included) *)
type 'a interval = 
  Top of int 
| Int of int * 'a * 'a 
| Bot of int;;

(* IntervalOps
 * Operations to be implemented by Intervals. *)
module type IntervalBaseOps =
  sig
    (* Create/Convert *)
    type t
    val of_t : int -> t -> t -> t interval
    val of_int : int -> int -> int -> t interval
    val of_int64 : int -> int64 -> int64 -> t interval
    val of_string : int -> string -> string -> t interval
    val to_string : t interval -> string
    val width : t interval -> int
    val bounds : t interval -> (t * t) option
    val eql : t interval -> t interval -> bool
  end

module type IntervalOps =
  sig
    include IntervalBaseOps

    (* Inclusion *)
    val contains : t interval -> t interval -> bool
    val contains_element : t interval -> t -> bool

    (* Arithmetic *)
    val neg : t interval -> t interval
    val add : t interval -> t interval -> t interval
    val sub : t interval -> t interval -> t interval
    val mul : t interval -> t interval -> t interval
    val div_s : t interval -> t interval -> t interval
    val div_u : t interval -> t interval -> t interval
    val rem : t interval -> t interval -> t interval

    (* Bitwise *)
    val shift_left_k : t interval -> int -> t interval
    val shift_right_k : t interval -> int -> t interval
    val arith_shift_right_k : t interval -> int -> t interval
    val shift_left : t interval -> t interval -> t interval
    val shift_right : t interval -> t interval -> t interval
    val arith_shift_right : t interval -> t interval -> t interval
    val logor : t interval -> t interval -> t interval
    val logand : t interval -> t interval -> t interval
    val logxor : t interval -> t interval -> t interval

    (* Others *)
    val meet : t interval -> t interval -> t interval
    val join : t interval -> t interval -> t interval
    val complement : t interval -> t interval
  end

(* IntervalOutOfBounds Exception
 * thrown when of_t gets values that do not match the underlying
 * IntOps' type's boundaries. *)
exception IntervalOutOfBounds

(* IntervalBase
 * - creation of intervals
 * - conversion to string *)
module IntervalBase (I : CircularIntOps) : IntervalBaseOps with type t = I.t =
  struct
    type t = I.t;;
    
    (* Bounds *)
    let bounds x = 
      match x with
      | Bot _ -> None
      | Top w -> Some (I.zero, I.max_value w)
      | Int(w,a,b) -> Some (a,b);;
    
    
    (* Width *)
    let width x =
      match x with
      | Bot w -> w
      | Top w -> w
      | Int(w,_,_) -> w;;

    (* Creation (ensure Top is returned if the interval spans everything) *)
    let of_t w a b =
      let a = I.wrap w a and b = I.wrap w b in
      if (I.in_range w a) && (I.in_range w b)
      then
        if I.top_range w a b
        then Top w
        else Int (w,a,b)
      else Top w;;

    let of_int64 w a b = of_t w (I.of_int64 w a) (I.of_int64 w b);;
    let of_string w a b = of_t w (I.of_string w a) (I.of_string w b);; 
    let of_int w a b = of_int64 w (Int64.of_int a) (Int64.of_int b);;

    (* Conversion *)
    let to_string =
      let max_value_string w =
        match I.upper_bound w with
        | Some x -> I.to_string w x
        | None -> "+inf"
      and min_value_string w =
        match I.lower_bound w with
        | Some x -> I.to_string w x
        | None -> "-inf"
      and as_string x y = "[" ^ x ^ ";" ^ y ^ "]" in
      let max_interval_string w = 
        as_string (min_value_string w) (max_value_string w)
      and min_interval_string = "[]" in
      fun i ->
        match i with
        | Top w -> max_interval_string w
        | Bot _ -> min_interval_string
        | Int(w,a,b) -> as_string (I.to_string w a) (I.to_string w b);;

    (* Output *)
    let prn x = print_endline (to_string x);;

    (* Equality *)
    let eql s t =
      match s,t with
      | Int(w0,a,b), Int(w1,c,d) -> (w0 = w1) && (I.eq a c) && (I.eq b d)
      | _ -> (width s) = (width t)

  end
