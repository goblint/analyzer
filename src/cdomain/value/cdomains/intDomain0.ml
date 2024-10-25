open GobConfig
open GoblintCil
open Pretty
open PrecisionUtil

module M = Messages

let (%) = Batteries.(%)
let (|?) = Batteries.(|?)

exception IncompatibleIKinds of string
exception Unknown
exception Error
exception ArithmeticOnIntegerBot of string




(** Define records that hold mutable variables representing different Configuration values.
  * These values are used to keep track of whether or not the corresponding Config values are en-/disabled  *)
type ana_int_config_values = {
  mutable interval_threshold_widening : bool option;
  mutable interval_narrow_by_meet : bool option;
  mutable def_exc_widen_by_join : bool option;
  mutable interval_threshold_widening_constants : string option;
  mutable refinement : string option;
}

let ana_int_config: ana_int_config_values = {
  interval_threshold_widening = None;
  interval_narrow_by_meet = None;
  def_exc_widen_by_join = None;
  interval_threshold_widening_constants = None;
  refinement = None;
}

let get_interval_threshold_widening () =
  if ana_int_config.interval_threshold_widening = None then
    ana_int_config.interval_threshold_widening <- Some (get_bool "ana.int.interval_threshold_widening");
  Option.get ana_int_config.interval_threshold_widening

let get_interval_narrow_by_meet () =
  if ana_int_config.interval_narrow_by_meet = None then
    ana_int_config.interval_narrow_by_meet <- Some (get_bool "ana.int.interval_narrow_by_meet");
  Option.get ana_int_config.interval_narrow_by_meet

let get_def_exc_widen_by_join () =
  if ana_int_config.def_exc_widen_by_join = None then
    ana_int_config.def_exc_widen_by_join <- Some (get_bool "ana.int.def_exc_widen_by_join");
  Option.get ana_int_config.def_exc_widen_by_join

let get_interval_threshold_widening_constants () =
  if ana_int_config.interval_threshold_widening_constants = None then
    ana_int_config.interval_threshold_widening_constants <- Some (get_string "ana.int.interval_threshold_widening_constants");
  Option.get ana_int_config.interval_threshold_widening_constants

let get_refinement () =
  if ana_int_config.refinement = None then
    ana_int_config.refinement <- Some (get_string "ana.int.refinement");
  Option.get ana_int_config.refinement



(** Whether for a given ikind, we should compute with wrap-around arithmetic.
  *  Always for unsigned types, for signed types if 'sem.int.signed_overflow' is 'assume_wraparound'  *)
let should_wrap ik = not (Cil.isSigned ik) || get_string "sem.int.signed_overflow" = "assume_wraparound"

(** Whether for a given ikind, we should assume there are no overflows.
  * Always false for unsigned types, true for signed types if 'sem.int.signed_overflow' is 'assume_none'  *)
let should_ignore_overflow ik = Cil.isSigned ik && get_string "sem.int.signed_overflow" = "assume_none"

let widening_thresholds = ResettableLazy.from_fun WideningThresholds.thresholds
let widening_thresholds_desc = ResettableLazy.from_fun (List.rev % WideningThresholds.thresholds)

type overflow_info = { overflow: bool; underflow: bool;}

let set_overflow_flag ~cast ~underflow ~overflow ik =
  if !AnalysisState.executing_speculative_computations then
    (* Do not produce warnings when the operations are not actually happening in code *)
    ()
  else
    let signed = Cil.isSigned ik in
    if !AnalysisState.postsolving && signed && not cast then
      AnalysisState.svcomp_may_overflow := true;
    let sign = if signed then "Signed" else "Unsigned" in
    match underflow, overflow with
    | true, true ->
      M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 190; CWE 191] "%s integer overflow and underflow" sign
    | true, false ->
      M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 191] "%s integer underflow" sign
    | false, true ->
      M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 190] "%s integer overflow" sign
    | false, false -> assert false

let reset_lazy () =
  ResettableLazy.reset widening_thresholds;
  ResettableLazy.reset widening_thresholds_desc;
  ana_int_config.interval_threshold_widening <- None;
  ana_int_config.interval_narrow_by_meet <- None;
  ana_int_config.def_exc_widen_by_join <- None;
  ana_int_config.interval_threshold_widening_constants <- None;
  ana_int_config.refinement <- None

module type Arith =
sig
  type t
  val neg: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t

  val lt: t -> t -> t
  val gt: t -> t -> t
  val le: t -> t -> t
  val ge: t -> t -> t
  val eq: t -> t -> t
  val ne: t -> t -> t

  val lognot: t -> t
  val logand: t -> t -> t
  val logor : t -> t -> t
  val logxor: t -> t -> t

  val shift_left : t -> t -> t
  val shift_right: t -> t -> t

  val c_lognot: t -> t
  val c_logand: t -> t -> t
  val c_logor : t -> t -> t

end

module type ArithIkind =
sig
  type t
  val neg: Cil.ikind -> t -> t
  val add: Cil.ikind -> t -> t -> t
  val sub: Cil.ikind -> t -> t -> t
  val mul: Cil.ikind -> t -> t -> t
  val div: Cil.ikind -> t -> t -> t
  val rem: Cil.ikind -> t -> t -> t

  val lt: Cil.ikind -> t -> t -> t
  val gt: Cil.ikind -> t -> t -> t
  val le: Cil.ikind -> t -> t -> t
  val ge: Cil.ikind -> t -> t -> t
  val eq: Cil.ikind -> t -> t -> t
  val ne: Cil.ikind -> t -> t -> t

  val lognot: Cil.ikind -> t -> t
  val logand: Cil.ikind -> t -> t -> t
  val logor : Cil.ikind -> t -> t -> t
  val logxor: Cil.ikind -> t -> t -> t

  val shift_left : Cil.ikind -> t -> t -> t
  val shift_right: Cil.ikind -> t -> t -> t

  val c_lognot: Cil.ikind -> t -> t
  val c_logand: Cil.ikind -> t -> t -> t
  val c_logor : Cil.ikind -> t -> t -> t

end

(* Shared functions between S and Z *)
module type B =
sig
  include Lattice.S
  type int_t
  val bot_of: Cil.ikind -> t
  val top_of: Cil.ikind -> t
  val to_int: t -> int_t option
  val equal_to: int_t -> t -> [`Eq | `Neq | `Top]

  val to_bool: t -> bool option
  val to_excl_list: t -> (int_t list * (int64 * int64)) option
  val of_excl_list: Cil.ikind -> int_t list -> t
  val is_excl_list: t -> bool

  val to_incl_list: t -> int_t list option

  val maximal    : t -> int_t option
  val minimal    : t -> int_t option

  val cast_to: ?suppress_ovwarn:bool -> ?torg:Cil.typ -> Cil.ikind -> t -> t
end

(** Interface of IntDomain implementations that do not take ikinds for arithmetic operations yet. TODO: Should be ported to S in the future. *)
module type IkindUnawareS =
sig
  include B
  include Arith with type t := t
  val starting   : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending     : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val of_int: int_t -> t
  val of_bool: bool -> t
  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t
  val of_congruence: Cil.ikind -> int_t * int_t -> t
  val arbitrary: unit -> t QCheck.arbitrary
  val invariant: Cil.exp -> t -> Invariant.t
end

(** Interface of IntDomain implementations taking an ikind for arithmetic operations *)
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

  val join: Cil.ikind -> t -> t -> t
  val meet: Cil.ikind -> t -> t -> t
  val narrow: Cil.ikind -> t -> t -> t
  val widen: Cil.ikind -> t -> t -> t
  val starting : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val of_int: Cil.ikind -> int_t -> t
  val of_bool: Cil.ikind -> bool -> t
  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t
  val of_congruence: Cil.ikind -> int_t * int_t -> t
  val is_top_of: Cil.ikind -> t -> bool
  val invariant_ikind : Cil.exp -> Cil.ikind -> t -> Invariant.t

  val refine_with_congruence: Cil.ikind -> t -> (int_t * int_t) option -> t
  val refine_with_interval: Cil.ikind -> t -> (int_t * int_t) option -> t
  val refine_with_excl_list: Cil.ikind -> t -> (int_t list * (int64 * int64)) option -> t
  val refine_with_incl_list: Cil.ikind -> t -> int_t list option -> t

  val project: Cil.ikind -> int_precision -> t -> t
  val arbitrary: Cil.ikind -> t QCheck.arbitrary
end

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

  val shift_right : Cil.ikind -> t -> t -> t * overflow_info
end

module type Y =
sig
  (* include B *)
  include B
  include Arith with type t:= t
  val of_int: Cil.ikind -> int_t -> t
  val of_bool: Cil.ikind -> bool -> t
  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t
  val of_congruence: Cil.ikind -> int_t * int_t -> t

  val starting   : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending     : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val is_top_of: Cil.ikind -> t -> bool

  val project: int_precision -> t -> t
  val invariant: Cil.exp -> t -> Invariant.t
end

module type Z = Y with type int_t = Z.t


module IntDomLifter (I : S) =
struct
  open Cil
  type int_t = I.int_t
  type t = { v : I.t; ikind : CilType.Ikind.t } [@@deriving eq, ord, hash]

  let ikind {ikind; _} = ikind

  (* Helper functions *)
  let check_ikinds x y = if x.ikind <> y.ikind then raise (IncompatibleIKinds (GobPretty.sprintf "ikinds %a and %a are incompatible. Values: %a and %a" CilType.Ikind.pretty x.ikind CilType.Ikind.pretty y.ikind I.pretty x.v I.pretty y.v))
  let lift op x = {x with v = op x.ikind x.v }
  (* For logical operations the result is of type int *)
  let lift_logical op x = {v = op x.ikind x.v; ikind = Cil.IInt}
  let lift2 op x y = check_ikinds x y; {x with v = op x.ikind x.v y.v }
  let lift2_cmp op x y = check_ikinds x y; {v = op x.ikind x.v y.v; ikind = Cil.IInt}

  let bot_of ikind = { v = I.bot_of ikind; ikind}
  let bot () = failwith "bot () is not implemented for IntDomLifter."
  let is_bot x = I.is_bot x.v
  let top_of ikind = { v = I.top_of ikind; ikind}
  let top () = failwith "top () is not implemented for IntDomLifter."
  let is_top x = I.is_top x.v

  (* Leq does not check for ikind, because it is used in invariant with arguments of different type.
     TODO: check ikinds here and fix invariant to work with right ikinds *)
  let leq x y = I.leq x.v y.v
  let join = lift2 I.join
  let meet = lift2 I.meet
  let widen = lift2 I.widen
  let narrow = lift2 I.narrow

  let show x =
    if not (GobConfig.get_bool "dbg.full-output") && I.is_top_of x.ikind x.v then
      "⊤"
    else
      I.show x.v  (* TODO add ikind to output *)
  let pretty () x =
    if not (GobConfig.get_bool "dbg.full-output") && I.is_top_of x.ikind x.v then
      Pretty.text "⊤"
    else
      I.pretty () x.v (* TODO add ikind to output *)
  let pretty_diff () (x, y) = I.pretty_diff () (x.v, y.v) (* TODO check ikinds, add them to output *)
  let printXml o x =
    if not (GobConfig.get_bool "dbg.full-output") && I.is_top_of x.ikind x.v then
      BatPrintf.fprintf o "<value>\n<data>\n⊤\n</data>\n</value>\n"
    else
      I.printXml o x.v (* TODO add ikind to output *)
  (* This is for debugging *)
  let name () = "IntDomLifter(" ^ (I.name ()) ^ ")"
  let to_yojson x = I.to_yojson x.v
  let invariant e x =
    let e' = Cilfacade.mkCast ~e ~newt:(TInt (x.ikind, [])) in
    I.invariant_ikind e' x.ikind x.v
  let tag x = I.tag x.v
  let arbitrary ik = failwith @@ "Arbitrary not implement for " ^ (name ()) ^ "."
  let to_int x = I.to_int x.v
  let of_int ikind x = { v = I.of_int ikind x; ikind}
  let equal_to i x = I.equal_to i x.v
  let to_bool x = I.to_bool x.v
  let of_bool ikind b = { v = I.of_bool ikind b; ikind}
  let to_excl_list x = I.to_excl_list x.v
  let of_excl_list ikind is = {v = I.of_excl_list ikind is; ikind}
  let is_excl_list x = I.is_excl_list x.v
  let to_incl_list x = I.to_incl_list x.v
  let of_interval ?(suppress_ovwarn=false) ikind (lb,ub) = {v = I.of_interval ~suppress_ovwarn ikind (lb,ub); ikind}
  let of_congruence ikind (c,m) = {v = I.of_congruence ikind (c,m); ikind}
  let starting ?(suppress_ovwarn=false) ikind i = {v = I.starting ~suppress_ovwarn  ikind i; ikind}
  let ending ?(suppress_ovwarn=false) ikind i = {v = I.ending ~suppress_ovwarn ikind i; ikind}
  let maximal x = I.maximal x.v
  let minimal x = I.minimal x.v

  let neg = lift I.neg
  let add = lift2 I.add
  let sub = lift2 I.sub
  let mul = lift2 I.mul
  let div = lift2 I.div
  let rem = lift2 I.rem
  let lt = lift2_cmp I.lt
  let gt = lift2_cmp I.gt
  let le = lift2_cmp I.le
  let ge = lift2_cmp I.ge
  let eq = lift2_cmp I.eq
  let ne = lift2_cmp I.ne
  let lognot = lift I.lognot
  let logand = lift2 I.logand
  let logor = lift2 I.logor
  let logxor = lift2 I.logxor
  let shift_left x y = {x with v = I.shift_left x.ikind x.v y.v } (* TODO check ikinds*)
  let shift_right x y = {x with v = I.shift_right x.ikind x.v y.v } (* TODO check ikinds*)
  let c_lognot = lift_logical I.c_lognot
  let c_logand = lift2 I.c_logand
  let c_logor = lift2 I.c_logor

  let cast_to ?(suppress_ovwarn=false) ?torg ikind x = {v = I.cast_to  ~suppress_ovwarn ~torg:(TInt(x.ikind,[])) ikind x.v; ikind}

  let is_top_of ik x = ik = x.ikind && I.is_top_of ik x.v

  let relift x = { v = I.relift x.v; ikind = x.ikind }

  let project p v =  { v = I.project v.ikind p v.v; ikind = v.ikind }
end

module type Ikind =
sig
  val ikind: unit -> Cil.ikind
end

module PtrDiffIkind : Ikind =
struct
  let ikind = Cilfacade.ptrdiff_ikind
end

module IntDomWithDefaultIkind (I: Y) (Ik: Ikind) : Y with type t = I.t and type int_t = I.int_t =
struct
  include I
  let top () = I.top_of (Ik.ikind ())
  let bot () = I.bot_of (Ik.ikind ())
end

module Size = struct (* size in bits as int, range as int64 *)
  open Cil
  let sign x = if Z.compare x Z.zero < 0 then `Signed else `Unsigned

  let top_typ = TInt (ILongLong, [])
  let min_for x = intKindForValue x (sign x = `Unsigned)
  let bit = function (* bits needed for representation *)
    | IBool -> 1
    | ik -> bytesSizeOfInt ik * 8
  let is_int64_big_int x = Z.fits_int64 x
  let card ik = (* cardinality *)
    let b = bit ik in
    Z.shift_left Z.one b
  let bits ik = (* highest bits for neg/pos values *)
    let s = bit ik in
    if isSigned ik then s-1, s-1 else 0, s
  let bits_i64 ik = BatTuple.Tuple2.mapn Int64.of_int (bits ik)
  let range ik =
    let a,b = bits ik in
    let x = if isSigned ik then Z.neg (Z.shift_left Z.one a) (* -2^a *) else Z.zero in
    let y = Z.pred (Z.shift_left Z.one b) in (* 2^b - 1 *)
    x,y

  let is_cast_injective ~from_type ~to_type =
    let (from_min, from_max) = range (Cilfacade.get_ikind from_type) in
    let (to_min, to_max) = range (Cilfacade.get_ikind to_type) in
    if M.tracing then M.trace "int" "is_cast_injective %a (%a, %a) -> %a (%a, %a)" CilType.Typ.pretty from_type GobZ.pretty from_min GobZ.pretty from_max CilType.Typ.pretty to_type GobZ.pretty to_min GobZ.pretty to_max;
    Z.compare to_min from_min <= 0 && Z.compare from_max to_max <= 0

  let cast t x = (* TODO: overflow is implementation-dependent! *)
    if t = IBool then
      (* C11 6.3.1.2 Boolean type *)
      if Z.equal x Z.zero then Z.zero else Z.one
    else
      let a,b = range t in
      let c = card t in
      let y = Z.erem x c in
      let y = if Z.gt y b then Z.sub y c
        else if Z.lt y a then Z.add y c
        else y
      in
      if M.tracing then M.tracel "cast" "Cast %a to range [%a, %a] (%a) = %a (%s in int64)" GobZ.pretty x GobZ.pretty a GobZ.pretty b GobZ.pretty c GobZ.pretty y (if is_int64_big_int y then "fits" else "does not fit");
      y

  let min_range_sign_agnostic x =
    let size ik =
      let a,b = bits_i64 ik in
      Int64.neg a,b
    in
    if sign x = `Signed then
      size (min_for x)
    else
      let a, b = size (min_for x) in
      if b <= 64L then
        let upper_bound_less = Int64.sub b 1L in
        let max_one_less = Z.(pred @@ shift_left Z.one (Int64.to_int upper_bound_less)) in
        if x <= max_one_less then
          a, upper_bound_less
        else
          a,b
      else
        a, b

  (* From the number of bits used to represent a positive value, determines the maximal representable value *)
  let max_from_bit_range pos_bits = Z.(pred @@ shift_left Z.one (to_int (Z.of_int64 pos_bits)))

  (* From the number of bits used to represent a non-positive value, determines the minimal representable value *)
  let min_from_bit_range neg_bits = Z.(if neg_bits = 0L then Z.zero else neg @@ shift_left Z.one (to_int (neg (Z.of_int64 neg_bits))))

end


module StdTop (B: sig type t val top_of: Cil.ikind -> t end) = struct
  open B
  (* these should be overwritten for better precision if possible: *)
  let to_excl_list    x = None
  let of_excl_list ik x = top_of ik
  let is_excl_list    x = false
  let to_incl_list    x = None
  let of_interval ?(suppress_ovwarn=false) ik x = top_of ik
  let of_congruence ik x = top_of ik
  let starting ?(suppress_ovwarn=false) ik x = top_of ik
  let ending ?(suppress_ovwarn=false)   ik x = top_of ik
  let maximal         x = None
  let minimal         x = None
end

module Std (B: sig
    type t
    val name: unit -> string
    val top_of: Cil.ikind -> t
    val bot_of: Cil.ikind -> t
    val show: t -> string
    val equal: t -> t -> bool
  end) = struct
  include Printable.StdLeaf
  let name = B.name (* overwrite the one from Printable.Std *)
  open B
  let is_top x = failwith "is_top not implemented for IntDomain.Std"
  let is_bot x = B.equal x (bot_of Cil.IInt) (* Here we assume that the representation of bottom is independent of the ikind
                                                This may be true for intdomain implementations, but not e.g. for IntDomLifter. *)
  let is_top_of ik x = B.equal x (top_of ik)

  (* all output is based on B.show *)
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y

  include StdTop (B)
end

(* Textbook interval arithmetic, without any overflow handling etc. *)
module IntervalArith (Ints_t : IntOps.IntOps) = struct
  let min4 a b c d = Ints_t.min (Ints_t.min a b) (Ints_t.min c d)
  let max4 a b c d = Ints_t.max (Ints_t.max a b) (Ints_t.max c d)

  let mul (x1, x2) (y1, y2) =
    let x1y1 = (Ints_t.mul x1 y1) in
    let x1y2 = (Ints_t.mul x1 y2) in
    let x2y1 = (Ints_t.mul x2 y1) in
    let x2y2 = (Ints_t.mul x2 y2) in
    (min4 x1y1 x1y2 x2y1 x2y2, max4 x1y1 x1y2 x2y1 x2y2)

  let shift_left (x1,x2) (y1,y2) =
    let y1p = Ints_t.shift_left Ints_t.one y1 in
    let y2p = Ints_t.shift_left Ints_t.one y2 in
    mul (x1, x2) (y1p, y2p)

  let div (x1, x2) (y1, y2) =
    let x1y1n = (Ints_t.div x1 y1) in
    let x1y2n = (Ints_t.div x1 y2) in
    let x2y1n = (Ints_t.div x2 y1) in
    let x2y2n = (Ints_t.div x2 y2) in
    let x1y1p = (Ints_t.div x1 y1) in
    let x1y2p = (Ints_t.div x1 y2) in
    let x2y1p = (Ints_t.div x2 y1) in
    let x2y2p = (Ints_t.div x2 y2) in
    (min4 x1y1n x1y2n x2y1n x2y2n, max4 x1y1p x1y2p x2y1p x2y2p)

  let add (x1, x2) (y1, y2) = (Ints_t.add x1 y1, Ints_t.add x2 y2)
  let sub (x1, x2) (y1, y2) = (Ints_t.sub x1 y2, Ints_t.sub x2 y1)

  let neg (x1, x2) = (Ints_t.neg x2, Ints_t.neg x1)

  let one = (Ints_t.one, Ints_t.one)
  let zero = (Ints_t.zero, Ints_t.zero)
  let top_bool = (Ints_t.zero, Ints_t.one)

  let to_int (x1, x2) =
    if Ints_t.equal x1 x2 then Some x1 else None

  let upper_threshold u max_ik =
    let ts = if get_interval_threshold_widening_constants () = "comparisons" then WideningThresholds.upper_thresholds () else ResettableLazy.force widening_thresholds in
    let u = Ints_t.to_bigint u in
    let max_ik' = Ints_t.to_bigint max_ik in
    let t = List.find_opt (fun x -> Z.compare u x <= 0 && Z.compare x max_ik' <= 0) ts in
    BatOption.map_default Ints_t.of_bigint max_ik t
  let lower_threshold l min_ik =
    let ts = if get_interval_threshold_widening_constants () = "comparisons" then WideningThresholds.lower_thresholds () else ResettableLazy.force widening_thresholds_desc in
    let l = Ints_t.to_bigint l in
    let min_ik' = Ints_t.to_bigint min_ik in
    let t = List.find_opt (fun x -> Z.compare l x >= 0 && Z.compare x min_ik' >= 0) ts in
    BatOption.map_default Ints_t.of_bigint min_ik t
  let is_upper_threshold u =
    let ts = if get_interval_threshold_widening_constants () = "comparisons" then WideningThresholds.upper_thresholds () else ResettableLazy.force widening_thresholds in
    let u = Ints_t.to_bigint u in
    List.exists (Z.equal u) ts
  let is_lower_threshold l =
    let ts = if get_interval_threshold_widening_constants () = "comparisons" then WideningThresholds.lower_thresholds () else ResettableLazy.force widening_thresholds_desc in
    let l = Ints_t.to_bigint l in
    List.exists (Z.equal l) ts
end

module IntInvariant =
struct
  let of_int e ik x =
    if get_bool "witness.invariant.exact" then
      Invariant.of_exp Cil.(BinOp (Eq, e, kintegerCilint ik x, intType))
    else
      Invariant.none

  let of_incl_list e ik ps =
    match ps with
    | [_; _] when ik = IBool && not (get_bool "witness.invariant.inexact-type-bounds") ->
      assert (List.mem Z.zero ps);
      assert (List.mem Z.one ps);
      Invariant.none
    | [_] when get_bool "witness.invariant.exact" ->
      Invariant.none
    | _ :: _ :: _
    | [_] | [] ->
      List.fold_left (fun a x ->
          let i = Invariant.of_exp Cil.(BinOp (Eq, e, kintegerCilint ik x, intType)) in
          Invariant.(a || i) [@coverage off] (* bisect_ppx cannot handle redefined (||) *)
        ) (Invariant.bot ()) ps

  let of_interval_opt e ik = function
    | (Some x1, Some x2) when Z.equal x1 x2 ->
      of_int e ik x1
    | x1_opt, x2_opt ->
      let (min_ik, max_ik) = Size.range ik in
      let inexact_type_bounds = get_bool "witness.invariant.inexact-type-bounds" in
      let i1 =
        match x1_opt, inexact_type_bounds with
        | Some x1, false when Z.equal min_ik x1 -> Invariant.none
        | Some x1, _ -> Invariant.of_exp Cil.(BinOp (Le, kintegerCilint ik x1, e, intType))
        | None, _ -> Invariant.none
      in
      let i2 =
        match x2_opt, inexact_type_bounds with
        | Some x2, false when Z.equal x2 max_ik -> Invariant.none
        | Some x2, _ -> Invariant.of_exp Cil.(BinOp (Le, e, kintegerCilint ik x2, intType))
        | None, _ -> Invariant.none
      in
      Invariant.(i1 && i2)

  let of_interval e ik (x1, x2) =
    of_interval_opt e ik (Some x1, Some x2)

  let of_excl_list e ik ns =
    List.fold_left (fun a x ->
        let i = Invariant.of_exp Cil.(BinOp (Ne, e, kintegerCilint ik x, intType)) in
        Invariant.(a && i)
      ) (Invariant.top ()) ns
end

module IntervalFunctor (Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option =
struct
  let name () = "intervals"
  type int_t = Ints_t.t
  type t = (Ints_t.t * Ints_t.t) option [@@deriving eq, ord, hash]
  module IArith = IntervalArith (Ints_t)

  let range ik = BatTuple.Tuple2.mapn Ints_t.of_bigint (Size.range ik)

  let top () = failwith @@ "top () not implemented for " ^ (name ())
  let top_of ik = Some (range ik)
  let bot () = None
  let bot_of ik = bot () (* TODO: improve *)

  let show = function None -> "bottom" | Some (x,y) -> "["^Ints_t.to_string x^","^Ints_t.to_string y^"]"

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) ->
      if a = b && b = i then `Eq else if Ints_t.compare a i <= 0 && Ints_t.compare i b <=0 then `Top else `Neq

  let norm ?(suppress_ovwarn=false) ?(cast=false) ik : (t -> t * overflow_info) = function None -> (None, {underflow=false; overflow=false}) | Some (x,y) ->
    if Ints_t.compare x y > 0 then
      (None,{underflow=false; overflow=false})
    else (
      let (min_ik, max_ik) = range ik in
      let underflow = Ints_t.compare min_ik x > 0 in
      let overflow = Ints_t.compare max_ik y < 0 in
      let ov_info = { underflow = underflow && not suppress_ovwarn; overflow = overflow && not suppress_ovwarn } in
      let v =
        if underflow || overflow then
          if should_wrap ik then (* could add [|| cast], but that's GCC implementation-defined behavior: https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html#Integers-implementation *)
            (* We can only soundly wrap if at most one overflow occurred, otherwise the minimal and maximal values of the interval *)
            (* on Z will not safely contain the minimal and maximal elements after the cast *)
            let diff = Ints_t.abs (Ints_t.sub max_ik min_ik) in
            let resdiff = Ints_t.abs (Ints_t.sub y x) in
            if Ints_t.compare resdiff diff > 0 then
              top_of ik
            else
              let l = Ints_t.of_bigint @@ Size.cast ik (Ints_t.to_bigint x) in
              let u = Ints_t.of_bigint @@ Size.cast ik (Ints_t.to_bigint y) in
              if Ints_t.compare l u <= 0 then
                Some (l, u)
              else
                (* Interval that wraps around (begins to the right of its end). We can not represent such intervals *)
                top_of ik
          else if not cast && should_ignore_overflow ik then
            let tl, tu = BatOption.get @@ top_of ik in
            Some (Ints_t.max tl x, Ints_t.min tu y)
          else
            top_of ik
        else
          Some (x,y)
      in
      (v, ov_info)
    )

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (x1,x2), Some (y1,y2) -> Ints_t.compare x1 y1 >= 0 && Ints_t.compare x2 y2 <= 0

  let join ik (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (Ints_t.min x1 y1, Ints_t.max x2 y2) |> fst

  let meet ik (x:t) y =
    match x, y with
    | None, z | z, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (Ints_t.max x1 y1, Ints_t.min x2 y2) |> fst

  (* TODO: change to_int signature so it returns a big_int *)
  let to_int x = Option.bind x (IArith.to_int)
  let of_interval ?(suppress_ovwarn=false) ik (x,y) = norm ~suppress_ovwarn ik @@ Some (x,y)
  let of_int ik (x: int_t) = of_interval ik (x,x)
  let zero = Some IArith.zero
  let one  = Some IArith.one
  let top_bool = Some IArith.top_bool

  let of_bool _ik = function true -> one | false -> zero
  let to_bool (a: t) = match a with
    | None -> None
    | Some (l, u) when Ints_t.compare l Ints_t.zero = 0 && Ints_t.compare u Ints_t.zero = 0 -> Some false
    | x -> if leq zero x then None else Some true

  let starting ?(suppress_ovwarn=false) ik n =
    norm ~suppress_ovwarn ik @@ Some (n, snd (range ik))

  let ending ?(suppress_ovwarn=false) ik n =
    norm ~suppress_ovwarn ik @@ Some (fst (range ik), n)

  (* TODO: change signature of maximal, minimal to return big_int*)
  let maximal = function None -> None | Some (x,y) -> Some y
  let minimal = function None -> None | Some (x,y) -> Some x

  let cast_to ?(suppress_ovwarn=false) ?torg ?no_ov t = norm ~cast:true t (* norm does all overflow handling *)

  let widen ik x y =
    match x, y with
    | None, z | z, None -> z
    | Some (l0,u0), Some (l1,u1) ->
      let (min_ik, max_ik) = range ik in
      let threshold = get_interval_threshold_widening () in
      let l2 =
        if Ints_t.compare l0 l1 = 0 then l0
        else if threshold then IArith.lower_threshold l1 min_ik
        else min_ik
      in
      let u2 =
        if Ints_t.compare u0 u1 = 0 then u0
        else if threshold then IArith.upper_threshold u1 max_ik
        else max_ik
      in
      norm ik @@ Some (l2,u2) |> fst
  let widen ik x y =
    let r = widen ik x y in
    if M.tracing && not (equal x y) then M.tracel "int" "interval widen %a %a -> %a" pretty x pretty y pretty r;
    assert (leq x y); (* TODO: remove for performance reasons? *)
    r

  let narrow ik x y =
    match x, y with
    | _,None | None, _ -> None
    | Some (x1,x2), Some (y1,y2) ->
      let threshold = get_interval_threshold_widening () in
      let (min_ik, max_ik) = range ik in
      let lr = if Ints_t.compare min_ik x1 = 0 || threshold && Ints_t.compare y1 x1 > 0 && IArith.is_lower_threshold x1 then y1 else x1 in
      let ur = if Ints_t.compare max_ik x2 = 0 || threshold && Ints_t.compare y2 x2 < 0 && IArith.is_upper_threshold x2 then y2 else x2 in
      norm ik @@ Some (lr,ur) |> fst


  let narrow ik x y =
    if get_interval_narrow_by_meet () then
      meet ik x y
    else
      narrow ik x y

  let log f ~annihilator ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_bool i1, to_bool i2 with
      | Some x, _ when x = annihilator -> of_bool ik annihilator
      | _, Some y when y = annihilator -> of_bool ik annihilator
      | Some x, Some y -> of_bool ik (f x y)
      | _              -> top_of ik

  let c_logor = log (||) ~annihilator:true
  let c_logand = log (&&) ~annihilator:false

  let log1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_bool i1 with
      | Some x -> of_bool ik (f ik x)
      | _      -> top_of ik

  let c_lognot = log1 (fun _ik -> not)

  let bit f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try of_int ik (f ik x y) |> fst with Division_by_zero -> top_of ik)
      | _              -> top_of ik

  let bitcomp f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> (bot_of ik,{underflow=false; overflow=false})
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try of_int ik (f ik x y) with Division_by_zero | Invalid_argument _ -> (top_of ik,{underflow=false; overflow=false}))
      | _              -> (top_of ik,{underflow=true; overflow=true})

  let logxor = bit (fun _ik -> Ints_t.logxor)

  let logand ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try of_int ik (Ints_t.logand x y) |> fst with Division_by_zero -> top_of ik)
      | _, Some y when Ints_t.equal y Ints_t.zero -> of_int ik Ints_t.zero |> fst
      | _, Some y when Ints_t.equal y Ints_t.one -> of_interval ik (Ints_t.zero, Ints_t.one) |> fst
      | _ -> top_of ik

  let logor  = bit (fun _ik -> Ints_t.logor)

  let bit1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_int i1 with
      | Some x -> of_int ik (f ik x) |> fst
      | _      -> top_of ik

  let lognot = bit1 (fun _ik -> Ints_t.lognot)
  let shift_right = bitcomp (fun _ik x y -> Ints_t.shift_right x (Ints_t.to_int y))

  let neg ?no_ov ik = function None -> (None,{underflow=false; overflow=false}) | Some x -> norm ik @@ Some (IArith.neg x)

  let binary_op_with_norm ?no_ov op ik x y = match x, y with
    | None, None -> (None, {overflow=false; underflow= false})
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some x, Some y -> norm ik @@ Some (op x y)

  let add ?no_ov = binary_op_with_norm IArith.add
  let mul ?no_ov = binary_op_with_norm IArith.mul
  let sub ?no_ov = binary_op_with_norm IArith.sub

  let shift_left ik a b =
    match is_bot a, is_bot b with
    | true, true -> (bot_of ik,{underflow=false; overflow=false})
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show a) (show b)))
    | _ ->
      match a, minimal b, maximal b with
      | Some a, Some bl, Some bu when (Ints_t.compare bl Ints_t.zero >= 0) ->
        (try
           let r = IArith.shift_left a (Ints_t.to_int bl, Ints_t.to_int bu) in
           norm ik @@ Some r
         with Z.Overflow -> (top_of ik,{underflow=false; overflow=true}))
      | _              -> (top_of ik,{underflow=true; overflow=true})

  let rem ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (xl, xu), Some (yl, yu) ->
      if is_top_of ik x && is_top_of ik y then
        (* This is needed to preserve soundness also on things bigger than int32 e.g.  *)
        (* x:     3803957176L -> T in Interval32 *)
        (* y:     4209861404L -> T in Interval32 *)
        (* x % y: 3803957176L -> T in Interval32 *)
        (* T in Interval32 is [-2147483648,2147483647] *)
        (* the code below computes [-2147483647,2147483647] for this though which is unsound *)
        top_of ik
      else
        (* If we have definite values, Ints_t.rem will give a definite result.
         * Otherwise we meet with a [range] the result can be in.
         * This range is [0, min xu b] if x is positive, and [max xl -b, min xu b] if x can be negative.
         * The precise bound b is one smaller than the maximum bound. Negative y give the same result as positive. *)
        let pos x = if Ints_t.compare x Ints_t.zero < 0 then Ints_t.neg x else x in
        let b = Ints_t.sub (Ints_t.max (pos yl) (pos yu)) Ints_t.one in
        let range = if Ints_t.compare xl Ints_t.zero>= 0 then Some (Ints_t.zero, Ints_t.min xu b) else Some (Ints_t.max xl (Ints_t.neg b), Ints_t.min (Ints_t.max (pos xl) (pos xu)) b) in
        meet ik (bit (fun _ik -> Ints_t.rem) ik x y) range

  let rec div ?no_ov ik x y =
    match x, y with
    | None, None -> (bot (),{underflow=false; overflow=false})
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | (Some (x1,x2) as x), (Some (y1,y2) as y) ->
      begin
        let is_zero v = Ints_t.compare v Ints_t.zero = 0 in
        match y1, y2 with
        | l, u when is_zero l && is_zero u -> (top_of ik,{underflow=false; overflow=false}) (* TODO warn about undefined behavior *)
        | l, _ when is_zero l              -> div ik (Some (x1,x2)) (Some (Ints_t.one,y2))
        | _, u when is_zero u              -> div ik (Some (x1,x2)) (Some (y1, Ints_t.(neg one)))
        | _ when leq (of_int ik (Ints_t.zero) |> fst) (Some (y1,y2)) -> (top_of ik,{underflow=false; overflow=false})
        | _ -> binary_op_with_norm IArith.div ik x y
      end

  let ne ik x y =
    match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      if Ints_t.compare y2 x1 < 0 || Ints_t.compare x2 y1 < 0 then
        of_bool ik true
      else if Ints_t.compare x2 y1 <= 0 && Ints_t.compare y2 x1 <= 0 then
        of_bool ik false
      else top_bool

  let eq ik x y =
    match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      if Ints_t.compare y2 x1 <= 0 && Ints_t.compare x2 y1 <= 0 then
        of_bool ik true
      else if Ints_t.compare y2 x1 < 0 || Ints_t.compare x2 y1 < 0 then
        of_bool ik false
      else top_bool

  let ge ik x y =
    match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      if Ints_t.compare y2 x1 <= 0 then of_bool ik true
      else if Ints_t.compare x2 y1 < 0 then of_bool ik false
      else top_bool

  let le ik x y =
    match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      if Ints_t.compare x2 y1 <= 0 then of_bool ik true
      else if Ints_t.compare  y2 x1 < 0 then of_bool ik false
      else top_bool

  let gt ik x y =
    match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      if Ints_t.compare y2 x1 < 0 then of_bool ik true
      else if Ints_t.compare x2 y1 <= 0 then of_bool ik false
      else top_bool

  let lt ik x y =
    match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      if Ints_t.compare x2 y1 < 0 then of_bool ik true
      else if Ints_t.compare y2 x1 <= 0 then of_bool ik false
      else top_bool

  let invariant_ikind e ik = function
    | Some (x1, x2) ->
      let (x1', x2') = BatTuple.Tuple2.mapn Ints_t.to_bigint (x1, x2) in
      IntInvariant.of_interval e ik (x1', x2')
    | None -> Invariant.none

  let arbitrary ik =
    let open QCheck.Iter in
    (* let int_arb = QCheck.map ~rev:Ints_t.to_bigint Ints_t.of_bigint GobQCheck.Arbitrary.big_int in *)
    (* TODO: apparently bigints are really slow compared to int64 for domaintest *)
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 GobQCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let shrink = function
      | Some (l, u) -> (return None) <+> (GobQCheck.shrink pair_arb (l, u) >|= of_interval ik >|= fst)
      | None -> empty
    in
    QCheck.(set_shrink shrink @@ set_print show @@ map (*~rev:BatOption.get*) (fun x -> of_interval ik x |> fst ) pair_arb)

  let modulo n k =
    let result = Ints_t.rem n k in
    if Ints_t.compare result Ints_t.zero >= 0 then result
    else Ints_t.add result  k

  let refine_with_congruence ik (intv : t) (cong : (int_t * int_t ) option) : t =
    match intv, cong with
    | Some (x, y), Some (c, m) ->
      if Ints_t.equal m Ints_t.zero && (Ints_t.compare c x < 0 || Ints_t.compare c y > 0) then None
      else if Ints_t.equal m Ints_t.zero then
        Some (c, c)
      else
        let (min_ik, max_ik) = range ik in
        let rcx =
          if Ints_t.equal x min_ik then x else
            Ints_t.add x (modulo (Ints_t.sub c x) (Ints_t.abs m)) in
        let lcy =
          if Ints_t.equal y max_ik then y else
            Ints_t.sub y (modulo (Ints_t.sub y c) (Ints_t.abs m)) in
        if Ints_t.compare rcx lcy > 0 then None
        else if Ints_t.equal rcx lcy then norm ik @@ Some (rcx, rcx) |> fst
        else norm ik @@ Some (rcx, lcy) |> fst
    | _ -> None

  let refine_with_congruence ik x y =
    let refn = refine_with_congruence ik x y in
    if M.tracing then M.trace "refine" "int_refine_with_congruence %a %a -> %a" pretty x pretty y pretty refn;
    refn

  let refine_with_interval ik a b = meet ik a b

  let refine_with_excl_list ik (intv : t) (excl : (int_t list * (int64 * int64)) option) : t =
    match intv, excl with
    | None, _ | _, None -> intv
    | Some(l, u), Some(ls, (rl, rh)) ->
      let rec shrink op b =
        let new_b = (op b (Ints_t.of_int(Bool.to_int(BatList.mem_cmp Ints_t.compare b ls)))) in
        if not (Ints_t.equal b new_b) then shrink op new_b else new_b
      in
      let (min_ik, max_ik) = range ik in
      let l' = if Ints_t.equal l min_ik then l else shrink Ints_t.add l in
      let u' = if Ints_t.equal u max_ik then u else shrink Ints_t.sub u in
      let intv' = norm ik @@ Some (l', u') |> fst in
      let range = norm ~suppress_ovwarn:true ik (Some (Ints_t.of_bigint (Size.min_from_bit_range rl), Ints_t.of_bigint (Size.max_from_bit_range rh))) |> fst in
      meet ik intv' range

  let refine_with_incl_list ik (intv: t) (incl : (int_t list) option) : t =
    match intv, incl with
    | None, _ | _, None -> intv
    | Some(l, u), Some(ls) ->
      let rec min m1 ms = match ms with | [] -> m1 | x::xs -> match m1 with
        | None -> min (Some x) xs | Some m -> if Ints_t.compare m x < 0 then min (Some m) xs else min (Some x) xs in
      let rec max m1 ms = match ms with | [] -> m1 | x::xs -> match m1 with
        | None -> max (Some x) xs | Some m -> if Ints_t.compare m x > 0 then max (Some m) xs else max (Some x) xs in
      match min None ls, max None ls with
      | Some m1, Some m2 -> refine_with_interval ik (Some(l, u)) (Some (m1, m2))
      | _, _-> intv

  let project ik p t = t
end

module SOverflowUnlifter (D : SOverflow) : S with type int_t = D.int_t and type t = D.t = struct
  include D

  let add ?no_ov ik x y = fst @@ D.add ?no_ov ik x y

  let sub ?no_ov ik x y = fst @@ D.sub ?no_ov ik x y

  let mul ?no_ov ik x y = fst @@ D.mul ?no_ov ik x y

  let div ?no_ov ik x y = fst @@ D.div ?no_ov ik x y

  let neg ?no_ov ik x = fst @@ D.neg ?no_ov ik x

  let cast_to ?suppress_ovwarn ?torg ?no_ov ik x = fst @@ D.cast_to ?suppress_ovwarn ?torg ?no_ov ik x

  let of_int ik x = fst @@ D.of_int ik x

  let of_interval ?suppress_ovwarn ik x = fst @@ D.of_interval ?suppress_ovwarn ik x

  let starting ?suppress_ovwarn ik x = fst @@ D.starting ?suppress_ovwarn ik x

  let ending ?suppress_ovwarn ik x = fst @@ D.ending ?suppress_ovwarn ik x

  let shift_left ik x y = fst @@ D.shift_left ik x y

  let shift_right ik x y = fst @@ D.shift_right ik x y
end

module IntIkind = struct let ikind () = Cil.IInt end
module Interval = IntervalFunctor (IntOps.BigIntOps)
module Interval32 = IntDomWithDefaultIkind (IntDomLifter (SOverflowUnlifter (IntervalFunctor (IntOps.Int64Ops)))) (IntIkind)
module IntervalSet = IntervalSetFunctor (IntOps.BigIntOps)
module Integers (Ints_t : IntOps.IntOps): IkindUnawareS with type t = Ints_t.t and type int_t = Ints_t.t = (* no top/bot, order is <= *)
struct
  include Printable.Std
  let name () = "integers"
  type t = Ints_t.t [@@deriving eq, ord, hash]
  type int_t = Ints_t.t
  let top () = raise Unknown
  let bot () = raise Error
  let top_of ik = top ()
  let bot_of ik = bot ()
  let show (x: Ints_t.t) = Ints_t.to_string x

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)
  (* is_top and is_bot are never called, but if they were, the Std impl would raise their exception, so we overwrite them: *)
  let is_top _ = false
  let is_bot _ = false

  let equal_to i x = if i > x then `Neq else `Top
  let leq x y = x <= y
  let join x y = if Ints_t.compare x y > 0 then x else y
  let widen = join
  let meet x y = if Ints_t.compare x y > 0 then y else x
  let narrow = meet

  let of_bool x = if x then Ints_t.one else Ints_t.zero
  let to_bool' x = x <> Ints_t.zero
  let to_bool x = Some (to_bool' x)
  let of_int  x = x
  let to_int  x = Some x

  let neg  = Ints_t.neg
  let add  = Ints_t.add (* TODO: signed overflow is undefined behavior! *)
  let sub  = Ints_t.sub
  let mul  = Ints_t.mul
  let div  = Ints_t.div
  let rem  = Ints_t.rem
  let lt n1 n2 = of_bool (n1 <  n2)
  let gt n1 n2 = of_bool (n1 >  n2)
  let le n1 n2 = of_bool (n1 <= n2)
  let ge n1 n2 = of_bool (n1 >= n2)
  let eq n1 n2 = of_bool (n1 =  n2)
  let ne n1 n2 = of_bool (n1 <> n2)
  let lognot = Ints_t.lognot
  let logand = Ints_t.logand
  let logor  = Ints_t.logor
  let logxor = Ints_t.logxor
  let shift_left  n1 n2 = Ints_t.shift_left n1 (Ints_t.to_int n2)
  let shift_right n1 n2 = Ints_t.shift_right n1 (Ints_t.to_int n2)
  let c_lognot n1    = of_bool (not (to_bool' n1))
  let c_logand n1 n2 = of_bool ((to_bool' n1) && (to_bool' n2))
  let c_logor  n1 n2 = of_bool ((to_bool' n1) || (to_bool' n2))
  let cast_to ?(suppress_ovwarn=false) ?torg t x =  failwith @@ "Cast_to not implemented for " ^ (name ()) ^ "."
  let arbitrary ik = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 GobQCheck.Arbitrary.int64 (* TODO: use ikind *)
  let invariant _ _ = Invariant.none (* TODO *)
end

module FlatPureIntegers: IkindUnawareS with type t = int64 and type int_t = int64 = (* Integers, but raises Unknown/Error on join/meet *)
struct
  include Integers(IntOps.Int64Ops)
  let top () = raise Unknown
  let bot () = raise Error
  let leq = equal
  let pretty_diff () (x,y) = Pretty.dprintf "Integer %a instead of %a" pretty x pretty y
  let join x y = if equal x y then x else top ()
  let meet x y = if equal x y then x else bot ()
end

module Flat (Base: IkindUnawareS): IkindUnawareS with type t = [ `Bot | `Lifted of Base.t | `Top ] and type int_t = Base.int_t = (* identical to Lift, but goes to `Top/`Bot if Base raises Unknown/Error *)
struct
  type int_t = Base.int_t
  include Lattice.FlatConf (struct
      include Printable.DefaultConf
      let top_name = "Unknown int"
      let bot_name = "Error int"
    end) (Base)

  let top_of ik = top ()
  let bot_of ik = bot ()


  let name () = "flat integers"
  let cast_to ?(suppress_ovwarn=false) ?torg t = function
    | `Lifted x -> `Lifted (Base.cast_to t x)
    | x -> x

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Top -> `Top
    | `Lifted x -> Base.equal_to i x

  let of_int  x = `Lifted (Base.of_int x)
  let to_int  x = match x with
    | `Lifted x -> Base.to_int x
    | _ -> None

  let of_bool x = `Lifted (Base.of_bool x)
  let to_bool x = match x with
    | `Lifted x -> Base.to_bool x
    | _ -> None

  let to_excl_list x = None
  let of_excl_list ik x = top_of ik
  let is_excl_list x = false
  let to_incl_list x = None
  let of_interval ?(suppress_ovwarn=false) ik x = top_of ik
  let of_congruence ik x = top_of ik
  let starting ?(suppress_ovwarn=false) ikind x = top_of ikind
  let ending ?(suppress_ovwarn=false)   ikind x = top_of ikind
  let maximal      x = None
  let minimal      x = None

  let lift1 f x = match x with
    | `Lifted x ->
      (try `Lifted (f x) with Unknown -> `Top | Error -> `Bot)
    | x -> x
  let lift2 f x y = match x,y with
    | `Lifted x, `Lifted y ->
      (try `Lifted (f x y) with Unknown -> `Top | Error -> `Bot)
    | `Bot, `Bot -> `Bot
    | _ -> `Top

  let neg  = lift1 Base.neg
  let add  = lift2 Base.add
  let sub  = lift2 Base.sub
  let mul  = lift2 Base.mul
  let div  = lift2 Base.div
  let rem  = lift2 Base.rem
  let lt = lift2 Base.lt
  let gt = lift2 Base.gt
  let le = lift2 Base.le
  let ge = lift2 Base.ge
  let eq = lift2 Base.eq
  let ne = lift2 Base.ne
  let lognot = lift1 Base.lognot
  let logand = lift2 Base.logand
  let logor  = lift2 Base.logor
  let logxor = lift2 Base.logxor
  let shift_left  = lift2 Base.shift_left
  let shift_right = lift2 Base.shift_right
  let c_lognot = lift1 Base.c_lognot
  let c_logand = lift2 Base.c_logand
  let c_logor  = lift2 Base.c_logor

  let invariant e = function
    | `Lifted x -> Base.invariant e x
    | `Top | `Bot -> Invariant.none
end

module Lift (Base: IkindUnawareS): IkindUnawareS with type t = [ `Bot | `Lifted of Base.t | `Top ] and type int_t = Base.int_t = (* identical to Flat, but does not go to `Top/Bot` if Base raises Unknown/Error *)
struct
  include Lattice.LiftPO (struct
      include Printable.DefaultConf
      let top_name = "MaxInt"
      let bot_name = "MinInt"
    end) (Base)
  type int_t = Base.int_t
  let top_of ik = top ()
  let bot_of ik = bot ()
  include StdTop (struct type nonrec t = t let top_of = top_of end)

  let name () = "lifted integers"
  let cast_to ?(suppress_ovwarn=false) ?torg t = function
    | `Lifted x -> `Lifted (Base.cast_to t x)
    | x -> x

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Top -> `Top
    | `Lifted x -> Base.equal_to i x

  let of_int  x = `Lifted (Base.of_int x)
  let to_int  x = match x with
    | `Lifted x -> Base.to_int x
    | _ -> None

  let of_bool x = `Lifted (Base.of_bool x)
  let to_bool x = match x with
    | `Lifted x -> Base.to_bool x
    | _ -> None

  let lift1 f x = match x with
    | `Lifted x -> `Lifted (f x)
    | x -> x
  let lift2 f x y = match x,y with
    | `Lifted x, `Lifted y -> `Lifted (f x y)
    | `Bot, `Bot -> `Bot
    | _ -> `Top

  let neg  = lift1 Base.neg
  let add  = lift2 Base.add
  let sub  = lift2 Base.sub
  let mul  = lift2 Base.mul
  let div  = lift2 Base.div
  let rem  = lift2 Base.rem
  let lt = lift2 Base.lt
  let gt = lift2 Base.gt
  let le = lift2 Base.le
  let ge = lift2 Base.ge
  let eq = lift2 Base.eq
  let ne = lift2 Base.ne
  let lognot = lift1 Base.lognot
  let logand = lift2 Base.logand
  let logor  = lift2 Base.logor
  let logxor = lift2 Base.logxor
  let shift_left  = lift2 Base.shift_left
  let shift_right = lift2 Base.shift_right
  let c_lognot = lift1 Base.c_lognot
  let c_logand = lift2 Base.c_logand
  let c_logor  = lift2 Base.c_logor

  let invariant e = function
    | `Lifted x -> Base.invariant e x
    | `Top | `Bot -> Invariant.none
end

module Flattened = Flat (Integers (IntOps.Int64Ops))
module Lifted = Lift (Integers (IntOps.Int64Ops))

module Reverse (Base: IkindUnawareS) =
struct
  include Base
  include (Lattice.Reverse (Base) : Lattice.S with type t := Base.t)
end

module BISet = struct
  include SetDomain.Make (IntOps.BigIntOps)
  let is_singleton s = cardinal s = 1
end

(* The module [Exclusion] constains common functionality about handling of exclusion sets between [DefExc] and [Enums] *)
module Exclusion =
struct
  module R = Interval32
  (* We use these types for the functions in this module to make the intended meaning more explicit *)
  type t = Exc of BISet.t * Interval32.t
  type inc = Inc of BISet.t [@@unboxed]
  let max_of_range r = Size.max_from_bit_range (Option.get (R.maximal r))
  let min_of_range r = Size.min_from_bit_range (Option.get (R.minimal r))
  let cardinality_of_range r = Z.succ (Z.add (Z.neg (min_of_range r)) (max_of_range r))

  let cardinality_BISet s =
    Z.of_int (BISet.cardinal s)

  let leq_excl_incl (Exc (xs, r)) (Inc ys) =
    (* For a <= b to hold, the cardinalities must fit, i.e. |a| <= |b|, which implies |min_r, max_r| - |xs| <= |ys|. We check this first. *)
    let lower_bound_cardinality_a = Z.sub (cardinality_of_range r) (cardinality_BISet xs) in
    let card_b = cardinality_BISet ys in
    if Z.compare lower_bound_cardinality_a card_b > 0 then
      false
    else (* The cardinality did fit, so we check for all elements that are represented by range r, whether they are in (xs union ys) *)
      let min_a = min_of_range r in
      let max_a = max_of_range r in
      GobZ.for_all_range (fun el -> BISet.mem el xs || BISet.mem el ys) (min_a, max_a)

  let leq (Exc (xs, r)) (Exc (ys, s)) =
    let min_a, max_a = min_of_range r, max_of_range r in
    let excluded_check = BISet.for_all (fun y -> BISet.mem y xs || Z.compare y min_a < 0 || Z.compare y max_a > 0) ys in (* if true, then the values ys, that are not in b, also do not occur in a *)
    if not excluded_check
    then false
    else begin (* Check whether all elements that are in the range r, but not in s, are in xs, i.e. excluded. *)
      if R.leq r s then true
      else begin if Z.compare (cardinality_BISet xs) (Z.sub (cardinality_of_range r) (cardinality_of_range s)) >= 0 (* Check whether the number of excluded elements in a is as least as big as |min_r, max_r| - |min_s, max_s| *)
        then
          let min_b, max_b = min_of_range s, max_of_range s in
          let leq1 = (* check whether the elements in [r_l; s_l-1] are all in xs, i.e. excluded *)
            if Z.compare min_a min_b < 0 then
              GobZ.for_all_range (fun x -> BISet.mem x xs) (min_a, Z.pred min_b)
            else
              true
          in
          let leq2 () = (* check whether the elements in [s_u+1; r_u] are all in xs, i.e. excluded *)
            if Z.compare max_b max_a < 0 then
              GobZ.for_all_range (fun x -> BISet.mem x xs) (Z.succ max_b, max_a)
            else
              true
          in
          leq1 && (leq2 ())
        else
          false
      end
    end
end

module SOverflowLifter (D : S) : SOverflow with type int_t = D.int_t and type t = D.t = struct

  include D

  let lift v = (v, {overflow=false; underflow=false})

  let add ?no_ov ik x y = lift @@ D.add ?no_ov ik x y

  let sub ?no_ov ik x y = lift @@ D.sub ?no_ov ik x y

  let mul ?no_ov ik x y = lift @@ D.mul ?no_ov ik x y

  let div ?no_ov ik x y = lift @@ D.div ?no_ov ik x y

  let neg ?no_ov ik x = lift @@ D.neg ?no_ov ik x

  let cast_to ?suppress_ovwarn ?torg ?no_ov ik x = lift @@ D.cast_to ?suppress_ovwarn ?torg ?no_ov ik x

  let of_int ik x = lift @@ D.of_int ik x

  let of_interval ?suppress_ovwarn ik x = lift @@ D.of_interval ?suppress_ovwarn ik x

  let starting ?suppress_ovwarn ik x = lift @@ D.starting ?suppress_ovwarn ik x

  let ending ?suppress_ovwarn ik x = lift @@ D.ending ?suppress_ovwarn ik x

  let shift_left ik x y = lift @@ D.shift_left ik x y

  let shift_right ik x y = lift @@ D.shift_right ik x y

end
