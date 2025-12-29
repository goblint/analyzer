open GobConfig
open GoblintCil
open Pretty

include IntDomain_intf

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

type overflow_info = IntDomain_intf.overflow_info = { overflow: bool; underflow: bool;}
type overflow_op =
  | Binop of binop
  | Unop of unop
  | Cast
  | Internal

let add_overflow_check ~(op:overflow_op) ~underflow ~overflow ik =
  if !AnalysisState.executing_speculative_computations then
    (* Do not produce warnings when the operations are not actually happening in code *)
    ()
  else
    let signed = Cil.isSigned ik in
    if !AnalysisState.postsolving && signed && op <> Cast && (underflow || overflow) then
      AnalysisState.svcomp_may_overflow := true;
    let sign = if signed then "Signed" else "Unsigned" in
    let op =
      match op with
      (* explicitly distinguish binary and unary - *)
      | Binop (MinusA | MinusPP | MinusPI) -> "binary -" (* only MinusA should probably be possible here *)
      | Unop Neg -> "unary -"
      | Binop bop -> CilType.Binop.show bop
      | Unop uop -> CilType.Unop.show uop
      | Cast -> "cast"
      | Internal -> "internal operation"
    in
    match underflow, overflow with
    | true, true ->
      M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 190; CWE 191] "%s integer overflow and underflow in %s" sign op;
      Checks.warn Checks.Category.IntegerOverflow "%s integer overflow and underflow in %s" sign op
    | true, false ->
      M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 191] "%s integer underflow in %s" sign op;
      Checks.warn Checks.Category.IntegerOverflow "%s integer underflow in %s" sign op
    | false, true ->
      M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 190] "%s integer overflow in %s" sign op;
      Checks.warn Checks.Category.IntegerOverflow "%s integer overflow in %s" sign op
    | false, false ->
      let sign = if signed then "signed" else "unsigned" in (* lowercase constants *)
      Checks.safe_msg Checks.Category.IntegerOverflow "No %s integer overflow or underflow in %s" sign op


let reset_lazy () =
  ana_int_config.interval_threshold_widening <- None;
  ana_int_config.interval_narrow_by_meet <- None;
  ana_int_config.def_exc_widen_by_join <- None;
  ana_int_config.interval_threshold_widening_constants <- None;
  ana_int_config.refinement <- None


module type Bitfield_SOverflow =
sig

  include SOverflow

  (* necessary for baseInvariant *)
  val refine_bor : t -> t -> t -> t * t
  val refine_band : t -> t -> t -> t * t

end


module IntDomLifter (I : S2) =
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
  let top_of ?bitfield ikind = { v = I.top_of ?bitfield ikind; ikind}
  let top () = failwith "top () is not implemented for IntDomLifter."
  let is_top _ = failwith "is_top is not implemented for IntDomLifter."

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
  let of_int ?(suppress_ovwarn=false) ikind x = { v = I.of_int ~suppress_ovwarn ikind x; ikind}
  let equal_to i x = I.equal_to i x.v
  let to_bool x = I.to_bool x.v
  let of_bool ikind b = { v = I.of_bool ikind b; ikind}
  let to_excl_list x = I.to_excl_list x.v
  let of_excl_list ikind is = {v = I.of_excl_list ikind is; ikind}
  let is_excl_list x = I.is_excl_list x.v
  let to_incl_list x = I.to_incl_list x.v
  let of_interval ?(suppress_ovwarn=false) ikind (lb,ub) = {v = I.of_interval ~suppress_ovwarn ikind (lb,ub); ikind}
  let of_congruence ikind (c,m) = {v = I.of_congruence ikind (c,m); ikind}
  let of_bitfield ikind (z,o) = {v = I.of_bitfield ikind (z,o); ikind}
  let to_bitfield ikind x = I.to_bitfield ikind x.v

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


module PtrDiffIkind : Ikind =
struct
  let ikind = Cilfacade.ptrdiff_ikind
end

module IntDomWithDefaultIkind (I: Y) (Ik: Ikind) : Y with type t = I.t and type int_t = I.int_t =
struct
  include I
  let top () = I.top_of (Ik.ikind ())
  let is_top x = I.is_top_of (Ik.ikind ()) x
  let bot () = I.bot_of (Ik.ikind ())
end

module Size = struct (* size in bits as int, range as int64 *)
  open Cil
  let sign x = if Z.compare x Z.zero < 0 then `Signed else `Unsigned

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
  (* let bits_i64 ik = BatTuple.Tuple2.mapn Int64.of_int (bits ik) *)
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

  (** @return Bit range always includes 0. *)
  let min_range_sign_agnostic x =
    let size ik =
      let a,b = bits ik in
      -a,b
    in
    if sign x = `Signed then
      size (min_for x)
    else
      let a, b = size (min_for x) in
      if b <= 64 then
        let upper_bound_less = b - 1 in
        let max_one_less = Z.(pred @@ shift_left Z.one upper_bound_less) in
        if x <= max_one_less then
          a, upper_bound_less
        else
          a,b
      else
        a, b

  (* From the number of bits used to represent a positive value, determines the maximal representable value *)
  let max_from_bit_range pos_bits = Z.(pred @@ shift_left Z.one (to_int (Z.of_int pos_bits)))

  (* From the number of bits used to represent a non-positive value, determines the minimal representable value *)
  let min_from_bit_range neg_bits = Z.(if neg_bits = 0 then Z.zero else neg @@ shift_left Z.one (to_int (neg (Z.of_int neg_bits))))

end


module StdTop (B: sig type t val top_of: ?bitfield:int -> Cil.ikind -> t end) = struct
  open B
  (* these should be overwritten for better precision if possible: *)
  let to_excl_list    x = None
  let of_excl_list ik x = top_of ik
  let is_excl_list    x = false
  let to_incl_list    x = None
  let of_interval ?(suppress_ovwarn=false) ik x = top_of ik
  let of_congruence ik x = top_of ik
  let of_bitfield ik x = top_of ik
  let starting ?(suppress_ovwarn=false) ik x = top_of ik
  let ending ?(suppress_ovwarn=false)   ik x = top_of ik
  let maximal         x = None
  let minimal         x = None
end

module Std (B: sig
    type t
    val name: unit -> string
    val top_of: ?bitfield:int -> Cil.ikind -> t
    val bot_of: Cil.ikind -> t
    val show: t -> string
    val equal: t -> t -> bool
  end) = struct
  include Printable.StdLeaf
  let name = B.name (* overwrite the one from Printable.Std *)
  open B
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
  type t = Ints_t.t * Ints_t.t [@@deriving eq, ord, hash]

  (* TODO: use this for Interval and IntervalSet *)
  let show (x,y) = "["^Ints_t.to_string x^","^Ints_t.to_string y^"]"

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

  (** Divide mathematical intervals.
      Excludes 0 from denominator - must be handled as desired by caller.

      @return negative and positive denominator cases separately, if they exist.

      @see <https://mine.perso.lip6.fr/publi/article-mine-FTiPL17.pdf> Miné, A. Tutorial on Static Inference of Numeric Invariants by Abstract Interpretation. Figure 4.6. *)
  let div (a, b) (c, d) =
    let pos =
      if Ints_t.(compare one d) <= 0 then
        let c = Ints_t.(max one c) in
        Some (Ints_t.(min (div a c) (div a d), max (div b c) (div b d)))
      else
        None
    in
    let neg =
      if Ints_t.(compare c zero) < 0 then
        let d = Ints_t.(min d (neg one)) in
        Some (Ints_t.(min (div b c) (div b d), max (div a c) (div a d)))
      else
        None
    in
    (neg, pos)

  let add (x1, x2) (y1, y2) = (Ints_t.add x1 y1, Ints_t.add x2 y2)
  let sub (x1, x2) (y1, y2) = (Ints_t.sub x1 y2, Ints_t.sub x2 y1)

  let neg (x1, x2) = (Ints_t.neg x2, Ints_t.neg x1)

  let one = (Ints_t.one, Ints_t.one)
  let zero = (Ints_t.zero, Ints_t.zero)
  let top_bool = (Ints_t.zero, Ints_t.one)

  (* TODO: use these for Interval and IntervalSet *)
  let maximal = snd
  let minimal = fst

  (* TODO: use these for Interval and IntervalSet *)
  let leq (x1,x2) (y1,y2) = Ints_t.compare x1 y1 >= 0 && Ints_t.compare x2 y2 <= 0
  let join (x1,x2) (y1,y2) = (Ints_t.min x1 y1, Ints_t.max x2 y2)
  let meet (x1,x2) (y1,y2) =
    let (r1, r2) as r = (Ints_t.max x1 y1, Ints_t.min x2 y2) in
    if Ints_t.compare r1 r2 > 0 then
      None
    else
      Some r

  let to_int (x1, x2) =
    if Ints_t.equal x1 x2 then Some x1 else None

  let find_thresholds lower_or_upper =
    let ts = if get_interval_threshold_widening_constants () = "comparisons" then lower_or_upper else WideningThresholds.thresholds in
    ResettableLazy.force ts

  let upper_threshold u max_ik =
    let u = Ints_t.to_bigint u in
    let max_ik' = Ints_t.to_bigint max_ik in
    find_thresholds WideningThresholds.upper_thresholds
    |> WideningThresholds.Thresholds.find_first_opt (fun x -> Z.compare u x <= 0)
    |> BatOption.filter (fun x -> Z.compare x max_ik' <= 0)
    |> BatOption.map_default Ints_t.of_bigint max_ik
  let lower_threshold l min_ik =
    let l = Ints_t.to_bigint l in
    let min_ik' = Ints_t.to_bigint min_ik in
    find_thresholds WideningThresholds.lower_thresholds
    |> WideningThresholds.Thresholds.find_last_opt (fun x -> Z.compare l x >= 0)
    |> BatOption.filter (fun x -> Z.compare x min_ik' >= 0)
    |> BatOption.map_default Ints_t.of_bigint min_ik

  let is_threshold t ts =
    let ts = find_thresholds ts in
    let t = Ints_t.to_bigint t in
    WideningThresholds.Thresholds.mem t ts

  let is_upper_threshold u = is_threshold u WideningThresholds.upper_thresholds
  let is_lower_threshold l = is_threshold l WideningThresholds.lower_thresholds
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

module SOverflowUnlifter (D : SOverflow) : S2 with type int_t = D.int_t and type t = D.t = struct
  include D

  let add ?no_ov ik x y = fst @@ D.add ?no_ov ik x y

  let sub ?no_ov ik x y = fst @@ D.sub ?no_ov ik x y

  let mul ?no_ov ik x y = fst @@ D.mul ?no_ov ik x y

  let div ?no_ov ik x y = fst @@ D.div ?no_ov ik x y

  let neg ?no_ov ik x = fst @@ D.neg ?no_ov ik x

  let cast_to ?suppress_ovwarn ?torg ?no_ov ik x = fst @@ D.cast_to ?torg ?no_ov ik x

  let of_int ?suppress_ovwarn ik x = fst @@ D.of_int ik x

  let of_interval ?suppress_ovwarn ik x = fst @@ D.of_interval ik x

  let starting ?suppress_ovwarn ik x = fst @@ D.starting ik x

  let ending ?suppress_ovwarn ik x = fst @@ D.ending ik x

  let shift_left ik x y = fst @@ D.shift_left ik x y

  let shift_right ik x y = fst @@ D.shift_right ik x y
end

module IntIkind = struct let ikind () = Cil.IInt end

module Integers (Ints_t : IntOps.IntOps): IkindUnawareS with type t = Ints_t.t and type int_t = Ints_t.t = (* no top/bot, order is <= *)
struct
  include Printable.Std
  let name () = "integers"
  type t = Ints_t.t [@@deriving eq, ord, hash]
  type int_t = Ints_t.t
  let top () = raise Unknown
  let bot () = raise Error
  let top_of ?bitfield ik = top ()
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

  let top_of ?bitfield ik = top ()
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
  let of_bitfield ik x = top_of ik
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
  include Lattice.LiftConf (struct
      include Printable.DefaultConf
      let top_name = "MaxInt"
      let bot_name = "MinInt"
    end) (Base)
  type int_t = Base.int_t
  let top_of ?bitfield ik = top ()
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

module SOverflowLifter (D : S) : SOverflow with type int_t = D.int_t and type t = D.t = struct

  include D

  let lift v = (v, {overflow=false; underflow=false})

  let add ?no_ov ik x y = lift @@ D.add ?no_ov ik x y

  let sub ?no_ov ik x y = lift @@ D.sub ?no_ov ik x y

  let mul ?no_ov ik x y = lift @@ D.mul ?no_ov ik x y

  let div ?no_ov ik x y = lift @@ D.div ?no_ov ik x y

  let neg ?no_ov ik x = lift @@ D.neg ?no_ov ik x

  let cast_to ?torg ?no_ov ik x = lift @@ D.cast_to ?torg ?no_ov ik x

  let of_int ik x = lift @@ D.of_int ik x

  let of_interval ik x = lift @@ D.of_interval ik x

  let starting ik x = lift @@ D.starting ik x

  let ending ik x = lift @@ D.ending ik x

  let shift_left ik x y = lift @@ D.shift_left ik x y

  let shift_right ik x y = lift @@ D.shift_right ik x y

end
