open GobConfig
open GoblintCil
open Pretty
open PrecisionUtil

module GU = Goblintutil
module M = Messages
module BI = IntOps.BigIntOps

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
  let signed = Cil.isSigned ik in
  if !GU.postsolving && signed && not cast then
    Goblintutil.svcomp_may_overflow := true;
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

  val bitnot: t -> t
  val bitand: t -> t -> t
  val bitor : t -> t -> t
  val bitxor: t -> t -> t

  val shift_left : t -> t -> t
  val shift_right: t -> t -> t

  val lognot: t -> t
  val logand: t -> t -> t
  val logor : t -> t -> t

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

  val bitnot: Cil.ikind -> t -> t
  val bitand: Cil.ikind -> t -> t -> t
  val bitor : Cil.ikind -> t -> t -> t
  val bitxor: Cil.ikind -> t -> t -> t

  val shift_left : Cil.ikind -> t -> t -> t
  val shift_right: Cil.ikind -> t -> t -> t

  val lognot: Cil.ikind -> t -> t
  val logand: Cil.ikind -> t -> t -> t
  val logor : Cil.ikind -> t -> t -> t

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

  val cast_to: ?torg:Cil.typ -> Cil.ikind -> t -> t
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
  val cast_to : ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t

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

  val cast_to : ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t * overflow_info

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

module type Z = Y with type int_t = BI.t

module OldDomainFacade (Old : IkindUnawareS with type int_t = int64) : S with type int_t = BI.t and type t = Old.t =
struct
  include Old
  type int_t = BI.t
  let neg ?no_ov _ik = Old.neg
  let add ?no_ov _ik = Old.add
  let sub ?no_ov _ik = Old.sub
  let mul ?no_ov _ik = Old.mul
  let div ?no_ov _ik = Old.div
  let rem _ik = Old.rem

  let lt _ik = Old.lt
  let gt _ik = Old.gt
  let le _ik = Old.le
  let ge _ik = Old.ge
  let eq _ik = Old.eq
  let ne _ik = Old.ne

  let bitnot _ik = bitnot
  let bitand _ik = bitand
  let bitor  _ik = bitor
  let bitxor _ik = bitxor

  let shift_left  _ik = shift_left
  let shift_right _ik = shift_right

  let lognot _ik = lognot
  let logand _ik = logand
  let logor  _ik = logor


  let to_int a = Option.map BI.of_int64 (Old.to_int a)

  let equal_to (x: int_t) (a: t)=
    try
      Old.equal_to (BI.to_int64 x) a
    with Z.Overflow | Failure _ -> `Top

  let to_excl_list a = Option.map (BatTuple.Tuple2.map1 (List.map BI.of_int64)) (Old.to_excl_list a)
  let of_excl_list ik xs =
    let xs' = List.map BI.to_int64 xs in
    Old.of_excl_list ik xs'

  let to_incl_list a = Option.map (List.map BI.of_int64) (Old.to_incl_list a)

  let maximal a = Option.map BI.of_int64 (Old.maximal a)
  let minimal a = Option.map BI.of_int64 (Old.minimal a)

  let of_int ik x =
    (* If we cannot convert x to int64, we have to represent it with top in the underlying domain*)
    try
      Old.of_int (BI.to_int64 x)
    with
      Failure _ -> top_of ik

  let of_bool ik b = Old.of_bool b
  let of_interval ?(suppress_ovwarn=false) ik (l, u) =
    try
      Old.of_interval ~suppress_ovwarn ik (BI.to_int64 l, BI.to_int64 u)
    with
      Failure _ -> top_of ik
  let of_congruence ik (c, m) =
    try
      Old.of_congruence ik (BI.to_int64 c, BI.to_int64 m)
    with
      Failure _ -> top_of ik

  let starting ?(suppress_ovwarn=false) ik x =
    try Old.starting ~suppress_ovwarn ik (BI.to_int64 x) with Failure _ -> top_of ik
  let ending ?(suppress_ovwarn=false) ik x =
    try Old.ending ~suppress_ovwarn ik (BI.to_int64 x) with Failure _ -> top_of ik

  let join _ik = Old.join
  let meet _ik = Old.meet
  let narrow _ik = Old.narrow
  let widen _ik = Old.widen

  let is_top_of _ik = Old.is_top

  let invariant_ikind e ik t = Old.invariant e t

  let cast_to ?torg ?no_ov = Old.cast_to ?torg

  let refine_with_congruence ik a b = a
  let refine_with_interval ik a b = a
  let refine_with_excl_list ik a b = a
  let refine_with_incl_list ik a b = a

  let project ik p t = t

  let arbitrary _ik = Old.arbitrary ()
end


module IntDomLifter (I : S) =
struct
  open Cil
  type int_t = I.int_t
  type t = { v : I.t; ikind : CilType.Ikind.t } [@@deriving eq, ord, hash]

  let ikind {ikind; _} = ikind

  (* Helper functions *)
  let check_ikinds x y = if x.ikind <> y.ikind then raise (IncompatibleIKinds ("ikinds " ^ GobPretty.sprint Cil.d_ikind x.ikind ^ " and " ^ GobPretty.sprint Cil.d_ikind y.ikind ^ " are incompatible. Values: " ^ GobPretty.sprint I.pretty x.v ^ " and " ^ GobPretty.sprint I.pretty y.v)) else ()
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

  let show x = I.show x.v  (* TODO add ikind to output *)
  let pretty () x = I.pretty () x.v (* TODO add ikind to output *)
  let pretty_diff () (x, y) = I.pretty_diff () (x.v, y.v) (* TODO check ikinds, add them to output *)
  let printXml o x = I.printXml o x.v (* TODO add ikind to output *)
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
  let bitnot = lift I.bitnot
  let bitand = lift2 I.bitand
  let bitor = lift2 I.bitor
  let bitxor = lift2 I.bitxor
  let shift_left x y = {x with v = I.shift_left x.ikind x.v y.v } (* TODO check ikinds*)
  let shift_right x y = {x with v = I.shift_right x.ikind x.v y.v } (* TODO check ikinds*)
  let lognot = lift_logical I.lognot
  let logand = lift2 I.logand
  let logor = lift2 I.logor

  let cast_to ?torg ikind x = {v = I.cast_to ~torg:(TInt(x.ikind,[])) ikind x.v; ikind}

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
  let sign x = if BI.compare x BI.zero < 0 then `Signed else `Unsigned

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
    if M.tracing then M.trace "int" "is_cast_injective %a (%s, %s) -> %a (%s, %s)\n" CilType.Typ.pretty from_type (BI.to_string from_min) (BI.to_string from_max) CilType.Typ.pretty to_type (BI.to_string to_min) (BI.to_string to_max);
    BI.compare to_min from_min <= 0 && BI.compare from_max to_max <= 0

  let cast t x = (* TODO: overflow is implementation-dependent! *)
    let a,b = range t in
    let c = card t in
    (* let z = add (rem (sub x a) c) a in (* might lead to overflows itself... *)*)
    let y = Z.erem x c in
    let y = if Z.gt y b then Z.sub y c
      else if Z.lt y a then Z.add y c
      else y
    in
    if M.tracing then M.tracel "cast_int" "Cast %s to range [%s, %s] (%s) = %s (%s in int64)\n" (Z.to_string x) (Z.to_string a) (Z.to_string b) (Z.to_string c) (Z.to_string y) (if is_int64_big_int y then "fits" else "does not fit");
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
        let max_one_less = BI.(pred @@ shift_left BI.one (Int64.to_int upper_bound_less)) in
        if x <= max_one_less then
          a, upper_bound_less
        else
          a,b
      else
        a, b

  (* From the number of bits used to represent a positive value, determines the maximal representable value *)
  let max_from_bit_range pos_bits = BI.(pred @@ shift_left BI.one (to_int (BI.of_int64 pos_bits)))

  (* From the number of bits used to represent a non-positive value, determines the minimal representable value *)
  let min_from_bit_range neg_bits = BI.(if neg_bits = 0L then BI.zero else neg @@ shift_left BI.one (to_int (neg (BI.of_int64 neg_bits))))

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
module IntervalArith(Ints_t : IntOps.IntOps) = struct
  let min4 a b c d = Ints_t.min (Ints_t.min a b) (Ints_t.min c d)
  let max4 a b c d = Ints_t.max (Ints_t.max a b) (Ints_t.max c d)

  let mul (x1, x2) (y1, y2) =
    let x1y1 = (Ints_t.mul x1 y1) in
    let x1y2 = (Ints_t.mul x1 y2) in
    let x2y1 = (Ints_t.mul x2 y1) in
    let x2y2 = (Ints_t.mul x2 y2) in
    (min4 x1y1 x1y2 x2y1 x2y2, max4 x1y1 x1y2 x2y1 x2y2)

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
end

module IntervalFunctor(Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option =
struct
  let name () = "intervals"
  type int_t = Ints_t.t
  type t = (Ints_t.t * Ints_t.t) option [@@deriving eq, ord, hash]
  module IArith = IntervalArith(Ints_t)

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

  let cast_to ?torg ?no_ov t = norm ~cast:true t (* norm does all overflow handling *)

  let widen ik x y =
    match x, y with
    | None, z | z, None -> z
    | Some (l0,u0), Some (l1,u1) ->
      let (min_ik, max_ik) = range ik in
      let threshold = get_interval_threshold_widening () in
      let upper_threshold u =
        let ts = if get_interval_threshold_widening_constants () = "comparisons" then WideningThresholds.upper_thresholds () else ResettableLazy.force widening_thresholds in
        let u = Ints_t.to_bigint u in
        let t = List.find_opt (fun x -> Z.compare u x <= 0) ts in
        BatOption.map_default Ints_t.of_bigint max_ik t
      in
      let lower_threshold l =
        let ts = if get_interval_threshold_widening_constants () = "comparisons" then WideningThresholds.lower_thresholds () else ResettableLazy.force widening_thresholds_desc in
        let l = Ints_t.to_bigint l in
        let t = List.find_opt (fun x -> Z.compare l x >= 0) ts in
        BatOption.map_default Ints_t.of_bigint min_ik t
      in
      let lt = if threshold then lower_threshold l1 else min_ik in
      let l2 = if Ints_t.compare l0 l1 = 0 then l0 else Ints_t.min l1 (Ints_t.max lt min_ik) in
      let ut = if threshold then upper_threshold u1 else max_ik in
      let u2 = if Ints_t.compare u0 u1 = 0 then u0 else Ints_t.max u1 (Ints_t.min ut max_ik) in
      norm ik @@ Some (l2,u2) |> fst
  let widen ik x y =
    let r = widen ik x y in
    if M.tracing then M.tracel "int" "interval widen %a %a -> %a\n" pretty x pretty y pretty r;
    assert (leq x y); (* TODO: remove for performance reasons? *)
    r

  let narrow ik x y =
    match x, y with
    | _,None | None, _ -> None
    | Some (x1,x2), Some (y1,y2) ->
      let (min_ik, max_ik) = range ik in
      let lr = if Ints_t.compare min_ik x1 = 0 then y1 else x1 in
      let ur = if Ints_t.compare max_ik x2 = 0 then y2 else x2 in
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

  let logor = log (||) ~annihilator:true
  let logand = log (&&) ~annihilator:false

  let log1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_bool i1 with
      | Some x -> of_bool ik (f ik x)
      | _      -> top_of ik

  let lognot = log1 (fun _ik -> not)

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

  let bitxor = bit (fun _ik -> Ints_t.bitxor)
  let bitand = bit (fun _ik -> Ints_t.bitand)
  let bitor  = bit (fun _ik -> Ints_t.bitor)

  let bit1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_int i1 with
      | Some x -> of_int ik (f ik x) |> fst
      | _      -> top_of ik

  let bitnot = bit1 (fun _ik -> Ints_t.bitnot)
  let shift_right = bitcomp (fun _ik x y -> Ints_t.shift_right x (Ints_t.to_int y))
  let shift_left  = bitcomp (fun _ik x y -> Ints_t.shift_left  x (Ints_t.to_int y))

  let neg ?no_ov ik = function None -> (None,{underflow=false; overflow=false}) | Some x -> norm ik @@ Some (IArith.neg x)

  let binary_op_with_norm ?no_ov op ik x y = match x, y with
    | None, None -> (None, {overflow=false; underflow= false})
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some x, Some y -> norm ik @@ Some (op x y)

  let add ?no_ov = binary_op_with_norm IArith.add
  let mul ?no_ov = binary_op_with_norm IArith.mul
  let sub ?no_ov = binary_op_with_norm IArith.sub

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

  let invariant_ikind e ik x =
    match x with
    | Some (x1, x2) when Ints_t.compare x1 x2 = 0 ->
      if get_bool "witness.invariant.exact" then
        let x1 = Ints_t.to_bigint x1 in
        Invariant.of_exp Cil.(BinOp (Eq, e, kintegerCilint ik x1, intType))
      else
        Invariant.top ()
    | Some (x1, x2) ->
      let (min_ik, max_ik) = range ik in
      let (x1', x2') = BatTuple.Tuple2.mapn (Ints_t.to_bigint) (x1, x2) in
      let inexact_type_bounds = get_bool "witness.invariant.inexact-type-bounds" in
      let i1 = if inexact_type_bounds || Ints_t.compare min_ik x1 <> 0 then Invariant.of_exp Cil.(BinOp (Le, kintegerCilint ik x1', e, intType)) else Invariant.none in
      let i2 = if inexact_type_bounds || Ints_t.compare x2 max_ik <> 0 then Invariant.of_exp Cil.(BinOp (Le, e, kintegerCilint ik x2', intType)) else Invariant.none in
      Invariant.(i1 && i2)
    | None -> Invariant.none

  let arbitrary ik =
    let open QCheck.Iter in
    (* let int_arb = QCheck.map ~rev:Ints_t.to_bigint Ints_t.of_bigint MyCheck.Arbitrary.big_int in *)
    (* TODO: apparently bigints are really slow compared to int64 for domaintest *)
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let shrink = function
      | Some (l, u) -> (return None) <+> (MyCheck.shrink pair_arb (l, u) >|= of_interval ik >|= fst)
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
    if M.tracing then M.trace "refine" "int_refine_with_congruence %a %a -> %a\n" pretty x pretty y pretty refn;
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

(** IntervalSetFunctor that is not just disjunctive completion of intervals, but attempts to be precise for wraparound arithmetic for unsigned types *)
module IntervalSetFunctor(Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) list =
struct

  module Interval = IntervalFunctor(Ints_t)
  module IArith = IntervalArith(Ints_t)


  let name () = "interval_sets"

  type int_t = Ints_t.t

  let (>.) a b = Ints_t.compare a b > 0
  let (=.) a b = Ints_t.compare a b = 0
  let (<.) a b = Ints_t.compare a b < 0
  let (>=.) a b = Ints_t.compare a b >= 0
  let (<=.) a b = Ints_t.compare a b <= 0
  let (+.) a b = Ints_t.add a b
  let (-.) a b = Ints_t.sub a b

  (*
    Each domain's element is guaranteed to be in canonical form. That is, each interval contained
    inside the set does not overlap with each other and they are not adjacent.
  *)
  type t = (Ints_t.t * Ints_t.t) list [@@deriving eq, hash, ord]

  let range ik = BatTuple.Tuple2.mapn Ints_t.of_bigint (Size.range ik)

  let top () = failwith @@ "top () not implemented for " ^ (name ())

  let top_of ik = [range ik]

  let bot () = []

  let bot_of ik = bot ()

  let show (x: t) =
    let show_interval i = Printf.sprintf "[%s, %s]" (Ints_t.to_string (fst i)) (Ints_t.to_string (snd i)) in
    List.fold_left (fun acc i -> (show_interval i) :: acc) [] x |> List.rev |> String.concat ", " |> Printf.sprintf "[%s]"

  (* New type definition for the sweeping line algorithm used for implementing join/meet functions. *)
  type event = Enter of Ints_t.t | Exit of Ints_t.t

  let unbox_event = function Enter x -> x | Exit x -> x

  let cmp_events x y =
    (* Deliberately comparing ints first => Cannot be derived *)
    let res = Ints_t.compare (unbox_event x) (unbox_event y) in
    if res <> 0 then res
    else
      begin
        match (x, y) with
        | (Enter _, Exit _) -> -1
        | (Exit _, Enter _) -> 1
        | (_, _) -> 0
      end

  let interval_set_to_events (xs: t) =
    List.concat_map (fun (a, b) -> [Enter a; Exit b]) xs

  let two_interval_sets_to_events (xs: t) (ys: t) =
    let xs = interval_set_to_events xs in
    let ys = interval_set_to_events ys in
    List.merge cmp_events xs ys

  (* Using the sweeping line algorithm, combined_event_list returns a new event list representing the intervals in which at least n intervals in xs overlap
     This function is used for both join and meet operations with different parameter n: 1 for join, 2 for meet *)
  let combined_event_list lattice_op (xs:event list)  =
    let l = match lattice_op with `Join -> 1 | `Meet -> 2 in
    let aux (interval_count, acc) = function
      | Enter x -> (interval_count + 1, if (interval_count + 1) >= l && interval_count < l then (Enter x)::acc else acc)
      | Exit x -> (interval_count - 1, if interval_count >= l && (interval_count - 1) < l then (Exit x)::acc else acc)
    in
    List.fold_left aux (0, []) xs |> snd |> List.rev

  let rec events_to_intervals = function
    | [] -> []
    | (Enter x)::(Exit y)::xs  -> (x, y)::(events_to_intervals xs)
    | _ -> failwith "Invalid events list"

  let remove_empty_gaps (xs: t) =
    let aux acc (l, r) = match acc with
      | ((a, b)::acc') when (b +. Ints_t.one) >=. l -> (a, r)::acc'
      | _ -> (l, r)::acc
    in
    List.fold_left aux [] xs |> List.rev

  let canonize (xs: t) =
    interval_set_to_events xs |>
    List.sort cmp_events |>
    combined_event_list `Join |>
    events_to_intervals |>
    remove_empty_gaps

  let unop (x: t) op = match x with
    | [] -> []
    | _ -> canonize @@ List.concat_map op x

  let binop (x: t) (y: t) op : t = match x, y with
    | [], _ -> []
    | _, [] -> []
    | _, _ -> canonize @@ List.concat_map op (BatList.cartesian_product x y)


  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let minimal = function
    | [] -> None
    | (x, _)::_ -> Some x

  let maximal = function
    | [] -> None
    | xs -> Some (BatList.last xs |> snd)

  let equal_to_interval i (a, b) =
    if a =. b && b =. i then
      `Eq
    else if a <=. i && i <=. b then
      `Top
    else
      `Neq

  let equal_to i xs = match List.map (equal_to_interval i) xs with
    | [] -> failwith "unsupported: equal_to with bottom"
    | [`Eq] ->  `Eq
    | ys when List.for_all ((=) `Neq) ys -> `Neq
    | _ -> `Top

  let norm_interval ?(suppress_ovwarn=false) ?(cast=false) ik (x,y) : t*overflow_info =
    if x >. y then
      ([],{underflow=false; overflow=false})
    else
      let (min_ik, max_ik) = range ik in
      let underflow = min_ik >. x in
      let overflow = max_ik <. y in
      let v = if underflow || overflow then
          begin
            if should_wrap ik then (* could add [|| cast], but that's GCC implementation-defined behavior: https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html#Integers-implementation *)
              (* We can only soundly wrap if at most one overflow occurred, otherwise the minimal and maximal values of the interval *)
              (* on Z will not safely contain the minimal and maximal elements after the cast *)
              let diff = Ints_t.abs (max_ik -. min_ik) in
              let resdiff = Ints_t.abs (y -. x) in
              if resdiff >. diff then
                [range ik]
              else
                let l = Ints_t.of_bigint @@ Size.cast ik (Ints_t.to_bigint x) in
                let u = Ints_t.of_bigint @@ Size.cast ik (Ints_t.to_bigint y) in
                if l <=. u then
                  [(l, u)]
                else
                  (* Interval that wraps around (begins to the right of its end). We CAN represent such intervals *)
                  [(min_ik, u); (l, max_ik)]
            else if not cast && should_ignore_overflow ik then
              [Ints_t.max min_ik x, Ints_t.min max_ik y]
            else
              [range ik]
          end
        else
          [(x,y)]
      in
      if suppress_ovwarn then (v, {underflow=false; overflow=false}) else (v, {underflow; overflow})

  let norm_intvs ?(suppress_ovwarn=false) ?(cast=false) (ik:ikind) (xs: t) : t*overflow_info =
    let res = List.map (norm_interval ~suppress_ovwarn ~cast ik) xs in
    let intvs = List.concat_map fst res in
    let underflow = List.exists (fun (_,{underflow; _}) -> underflow) res in
    let overflow = List.exists (fun (_,{overflow; _}) -> underflow) res in
    (canonize intvs,{underflow; overflow})

  let binary_op_with_norm op (ik:ikind) (x: t) (y: t) : t*overflow_info = match x, y with
    | [], _ -> ([],{overflow=false; underflow=false})
    | _, [] -> ([],{overflow=false; underflow=false})
    | _, _ -> norm_intvs ik @@ List.concat_map (fun (x,y) -> [op x y]) (BatList.cartesian_product x y)

  let binary_op_with_ovc (x: t) (y: t) op : t*overflow_info = match x, y with
    | [], _ -> ([],{overflow=false; underflow=false})
    | _, [] -> ([],{overflow=false; underflow=false})
    | _, _ ->
      let res = List.map op (BatList.cartesian_product x y) in
      let intvs = List.concat_map fst res in
      let underflow = List.exists (fun (_,{underflow; _}) -> underflow) res in
      let overflow = List.exists (fun (_,{overflow; _}) -> underflow) res in
      (canonize intvs,{underflow; overflow})

  let unary_op_with_norm op (ik:ikind) (x: t) = match x with
    | [] -> ([],{overflow=false; underflow=false})
    | _ -> norm_intvs ik @@ List.concat_map (fun x -> [op x]) x

  let rec leq (xs: t) (ys: t) =
    let leq_interval (al, au) (bl, bu) = al >=. bl && au <=. bu in
    match xs, ys with
    | [], _ -> true
    | _, [] -> false
    | (xl,xr)::xs', (yl,yr)::ys' ->
      if leq_interval (xl,xr) (yl,yr) then
        leq xs' ys
      else if xr <. yl then
        false
      else
        leq xs ys'

  let join ik (x: t) (y: t): t =
    two_interval_sets_to_events x y |>
    combined_event_list `Join |>
    events_to_intervals |>
    remove_empty_gaps

  let meet ik (x: t) (y: t): t =
    two_interval_sets_to_events x y |>
    combined_event_list  `Meet |>
    events_to_intervals

  let to_int = function
    | [x] -> IArith.to_int x
    | _ -> None

  let zero = [IArith.zero]
  let one = [IArith.one]
  let top_bool = [IArith.top_bool]

  let not_bool (x:t) =
    let is_false x = equal x zero in
    let is_true x = equal x one in
    if is_true x then zero else if is_false x then one else top_bool

  let to_bool = function
    | [(l,u)] when l =. Ints_t.zero && u =. Ints_t.zero -> Some false
    | x -> if leq zero x then None else Some true

  let of_bool _ = function true -> one | false -> zero

  let of_interval ?(suppress_ovwarn=false) ik (x,y) =  norm_interval  ~suppress_ovwarn ~cast:false ik (x,y)

  let of_int ik (x: int_t) = of_interval ik (x, x)

  let lt ik x y =
    match x, y with
    | [], [] -> bot_of ik
    | [], _ | _, [] -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, _ ->
      let (max_x, min_y) = (maximal x |> Option.get, minimal y |> Option.get) in
      let (min_x, max_y) = (minimal x |> Option.get, maximal y |> Option.get) in
      if max_x <. min_y then
        of_bool ik true
      else if min_x >=. max_y then
        of_bool ik false
      else
        top_bool

  let le ik x y =
    match x, y with
    | [], [] -> bot_of ik
    | [], _ | _, [] -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, _ ->
      let (max_x, min_y) = (maximal x |> Option.get, minimal y |> Option.get) in
      let (min_x, max_y) = (minimal x |> Option.get, maximal y |> Option.get) in
      if max_x <=. min_y then
        of_bool ik true
      else if min_x >. max_y then
        of_bool ik false
      else
        top_bool

  let gt ik x y = not_bool @@ le ik x y

  let ge ik x y = not_bool @@ lt ik x y

  let eq ik x y = match x, y with
    | (a, b)::[], (c, d)::[] when a =. b && c =. d && a =. c ->
      one
    | _ ->
      if is_bot (meet ik x y) then
        zero
      else
        top_bool

  let ne ik x y = not_bool @@ eq ik x y
  let interval_to_int i = Interval.to_int (Some i)
  let interval_to_bool i = Interval.to_bool (Some i)

  let log f ik (i1, i2) =
    match (interval_to_bool i1, interval_to_bool i2) with
    | Some x, Some y -> of_bool ik (f x y)
    | _ -> top_of ik


  let bit f ik (i1, i2) =
    match (interval_to_int i1), (interval_to_int i2) with
    | Some x, Some y -> (try of_int ik (f x y) |> fst with Division_by_zero -> top_of ik)
    | _ -> top_of ik


  let bitcomp f ik (i1, i2) =
    match (interval_to_int i1, interval_to_int i2) with
    | Some x, Some y -> (try of_int ik (f x y) with Division_by_zero | Invalid_argument _ -> (top_of ik,{overflow=false; underflow=false}))
    | _, _ -> (top_of ik,{overflow=false; underflow=false})

  let bitand ik x y =
    let interval_bitand = bit Ints_t.bitand ik in
    binop x y interval_bitand

  let bitor ik x y =
    let interval_bitor = bit Ints_t.bitor ik in
    binop x y interval_bitor

  let bitxor ik x y =
    let interval_bitxor = bit Ints_t.bitxor ik in
    binop x y interval_bitxor

  let bitnot ik x =
    let interval_bitnot i =
      match interval_to_int i with
      | Some x -> of_int ik (Ints_t.bitnot x) |> fst
      | _ -> top_of ik
    in
    unop x interval_bitnot

  let shift_left ik x y =
    let interval_shiftleft = bitcomp (fun x y -> Ints_t.shift_left x (Ints_t.to_int y)) ik in
    binary_op_with_ovc x y interval_shiftleft

  let shift_right ik x y =
    let interval_shiftright = bitcomp (fun x y -> Ints_t.shift_right x (Ints_t.to_int y)) ik in
    binary_op_with_ovc x y interval_shiftright

  let lognot ik x =
    let log1 f ik i1 =
      match interval_to_bool i1 with
      | Some x -> of_bool ik (f x)
      | _ -> top_of ik
    in
    let interval_lognot = log1 not ik in
    unop x interval_lognot

  let logand ik x y =
    let interval_logand = log (&&) ik in
    binop x y interval_logand

  let logor ik x y =
    let interval_logor = log (||) ik in
    binop x y interval_logor

  let add ?no_ov = binary_op_with_norm IArith.add
  let sub ?no_ov = binary_op_with_norm IArith.sub
  let mul ?no_ov = binary_op_with_norm IArith.mul
  let neg ?no_ov = unary_op_with_norm IArith.neg

  let div ?no_ov ik x y =
    let rec interval_div x (y1, y2) = begin
      let top_of ik = top_of ik |> List.hd in
      let is_zero v = v =. Ints_t.zero in
      match y1, y2 with
      | l, u when is_zero l && is_zero u -> top_of ik (* TODO warn about undefined behavior *)
      | l, _ when is_zero l              -> interval_div x (Ints_t.one,y2)
      | _, u when is_zero u              -> interval_div x (y1, Ints_t.(neg one))
      | _ when leq (of_int ik (Ints_t.zero) |> fst) ([(y1,y2)]) -> top_of ik
      | _ -> IArith.div x (y1, y2)
    end
    in binary_op_with_norm interval_div ik x y

  let rem ik x y =
    let interval_rem (x, y) =
      if Interval.is_top_of ik (Some x) && Interval.is_top_of ik (Some y) then
        top_of ik
      else
        let (xl, xu) = x in let (yl, yu) = y in
        let pos x = if x <. Ints_t.zero then Ints_t.neg x else x in
        let b = (Ints_t.max (pos yl) (pos yu)) -. Ints_t.one in
        let range = if xl >=. Ints_t.zero then (Ints_t.zero, Ints_t.min xu b) else (Ints_t.max xl (Ints_t.neg b), Ints_t.min (Ints_t.max (pos xl) (pos xu)) b) in
        meet ik (bit Ints_t.rem ik (x, y)) [range]
    in
    binop x y interval_rem

  let cast_to ?torg ?no_ov ik x = norm_intvs ~cast:true ik x

  (*
      narrows down the extremeties of xs if they are equal to boundary values of the ikind with (possibly) narrower values from ys
  *)
  let narrow ik xs ys = match xs ,ys with
    | [], _ -> [] | _ ,[] -> xs
    | _, _ ->
      let min_xs = minimal xs |> Option.get in
      let max_xs = maximal xs |> Option.get in
      let min_ys = minimal ys |> Option.get in
      let max_ys = maximal ys |> Option.get in
      let min_range,max_range = range ik in
      let min = if min_xs =. min_range then min_ys else min_xs in
      let max = if max_xs =. max_range then max_ys else max_xs in
      xs
      |> (function (_, y)::z -> (min, y)::z | _ -> [])
      |> List.rev
      |> (function (x, _)::z -> (x, max)::z | _ -> [])
      |> List.rev

  (*
    1. partitions the intervals of xs by assigning each of them to the an interval in ys that includes it.
     and joins all intervals in xs assigned to the same interval in ys as one interval.
    2. checks for every pair of adjacent pairs whether the pairs did approach (if you compare the intervals from xs and ys) and merges them if it is the case.
    3. checks whether partitions at the extremeties are approaching infinity (and expands them to infinity. in that case)

    The expansion (between a pair of adjacent partitions or at extremeties ) stops at a threshold.
  *)
  let widen ik xs ys =
    let (min_ik,max_ik) = range ik in
    let threshold = get_bool "ana.int.interval_threshold_widening" in
    let upper_threshold (_,u) =
      let ts = if GobConfig.get_string "ana.int.interval_threshold_widening_constants" = "comparisons" then WideningThresholds.upper_thresholds () else ResettableLazy.force widening_thresholds in
      let u = Ints_t.to_bigint u in
      let t = List.find_opt (fun x -> Z.compare u x <= 0) ts in
      BatOption.map_default Ints_t.of_bigint max_ik t
    in
    let lower_threshold (l,_) =
      let ts = if GobConfig.get_string "ana.int.interval_threshold_widening_constants" = "comparisons" then WideningThresholds.lower_thresholds () else ResettableLazy.force widening_thresholds_desc in
      let l = Ints_t.to_bigint l in
      let t = List.find_opt (fun x -> Z.compare l x >= 0) ts in
      BatOption.map_default Ints_t.of_bigint min_ik t
    in
    (*obtain partitioning of xs intervals according to the ys interval that includes them*)
    let rec interval_sets_to_partitions (ik: ikind) (acc : (int_t * int_t) option) (xs: t) (ys: t)=
      match xs,ys with
      | _, [] -> []
      | [], (y::ys) -> (acc,y):: interval_sets_to_partitions ik None [] ys
      | (x::xs), (y::ys) when Interval.leq (Some x) (Some y) -> interval_sets_to_partitions ik (Interval.join ik acc (Some x)) xs (y::ys)
      | (x::xs), (y::ys) -> (acc,y) :: interval_sets_to_partitions ik None  (x::xs) ys
    in
    let interval_sets_to_partitions ik xs ys = interval_sets_to_partitions ik None xs ys in
    (*merge a pair of adjacent partitions*)
    let merge_pair ik (a,b) (c,d) =
      let new_a = function
        | None -> Some (upper_threshold b, upper_threshold b)
        | Some (ax,ay) -> Some (ax, upper_threshold b)
      in
      let new_c = function
        | None -> Some (lower_threshold d, lower_threshold d)
        | Some (cx,cy) -> Some (lower_threshold d, cy)
      in
      if threshold && (lower_threshold d +. Ints_t.one) >. (upper_threshold b) then
        [(new_a a,(fst b, upper_threshold b)); (new_c c, (lower_threshold d, snd d))]
      else
        [(Interval.join ik a c, (Interval.join ik (Some b) (Some d) |> Option.get))]
    in
    let partitions_are_approaching part_left part_right = match part_left, part_right with
      | (Some (_, left_x), (_, left_y)), (Some (right_x, _), (right_y, _)) -> (right_x -. left_x) >. (right_y -. left_y)
      | _,_ -> false
    in
    (*merge all approaching pairs of adjacent partitions*)
    let rec merge_list ik = function
      | [] -> []
      | x::y::xs  when partitions_are_approaching x y -> merge_list ik ((merge_pair ik x y) @ xs)
      | x::xs -> x :: merge_list ik xs
    in
    (*expands left extremity*)
    let widen_left = function
      | [] -> []
      | (None,(lb,rb))::ts -> let lt = if threshold then lower_threshold (lb,lb) else min_ik in (None, (lt,rb))::ts
      | (Some (la,ra), (lb,rb))::ts  when lb <. la ->  let lt = if threshold then lower_threshold (lb,lb) else min_ik in (Some (la,ra),(lt,rb))::ts
      | x  -> x
    in
    (*expands right extremity*)
    let widen_right x =
      let map_rightmost = function
        | [] -> []
        | (None,(lb,rb))::ts -> let ut = if threshold then upper_threshold (rb,rb) else max_ik in (None, (lb,ut))::ts
        | (Some (la,ra), (lb,rb))::ts  when ra <. rb -> let ut = if threshold then upper_threshold (rb,rb) else max_ik in (Some (la,ra),(lb,ut))::ts
        | x  -> x
      in
      List.rev x |> map_rightmost |> List.rev
    in
    interval_sets_to_partitions ik xs ys |> merge_list ik |> widen_left |> widen_right |> List.map snd

  let starting ?(suppress_ovwarn=false) ik n = norm_interval ik ~suppress_ovwarn (n, snd (range ik))

  let ending ?(suppress_ovwarn=false) ik n = norm_interval ik ~suppress_ovwarn (fst (range ik), n)

  let invariant_ikind e ik xs =
    List.map (fun x -> Interval.invariant_ikind e ik (Some x)) xs |>
    let open Invariant in List.fold_left (||) (bot ())

  let modulo n k =
    let result = Ints_t.rem n k in
    if result >=. Ints_t.zero then result
    else result +. k

  let refine_with_congruence ik (intvs: t) (cong: (int_t * int_t ) option): t =
    let refine_with_congruence_interval ik (cong : (int_t * int_t ) option) (intv : (int_t * int_t ) option): t =
      match intv, cong with
      | Some (x, y), Some (c, m) ->
        if m =. Ints_t.zero && (c <. x || c >. y) then []
        else if m =. Ints_t.zero then
          [(c, c)]
        else
          let (min_ik, max_ik) = range ik in
          let rcx =
            if x =. min_ik then x else
              x +. (modulo (c -. x) (Ints_t.abs m)) in
          let lcy =
            if y =. max_ik then y else
              y -. (modulo (y -. c) (Ints_t.abs m)) in
          if rcx >. lcy then []
          else if rcx =. lcy then norm_interval ik (rcx, rcx) |> fst
          else norm_interval ik (rcx, lcy) |> fst
      | _ -> []
    in
    List.map (fun x -> Some x) intvs |> List.map (refine_with_congruence_interval ik cong) |> List.flatten

  let refine_with_interval ik xs = function None -> [] | Some (a,b) -> meet ik xs [(a,b)]

  let refine_with_incl_list ik intvs  = function
    | None -> intvs
    | Some xs -> meet ik intvs (List.map (fun x -> (x,x)) xs)

  let excl_range_to_intervalset (ik: ikind) ((min, max): int_t * int_t) (excl: int_t): t =
    let intv1 = (min, excl -. Ints_t.one) in
    let intv2 = (excl +. Ints_t.one, max) in
    norm_intvs ik ~suppress_ovwarn:true [intv1 ; intv2] |> fst

  let of_excl_list ik (excls: int_t list) =
    let excl_list = List.map (excl_range_to_intervalset ik (range ik)) excls in
    let res = List.fold_left (meet ik) (top_of ik) excl_list in
    res

  let refine_with_excl_list ik (intv : t) = function
    | None -> intv
    | Some (xs, range) ->
      let excl_to_intervalset (ik: ikind) ((rl, rh): (int64 * int64)) (excl: int_t): t =
        excl_range_to_intervalset ik (Ints_t.of_bigint (Size.min_from_bit_range rl),Ints_t.of_bigint (Size.max_from_bit_range rh)) excl
      in
      let excl_list = List.map (excl_to_intervalset ik range) xs in
      List.fold_left (meet ik) intv excl_list

  let project ik p t = t

  let arbitrary ik =
    let open QCheck.Iter in
    (* let int_arb = QCheck.map ~rev:Ints_t.to_bigint Ints_t.of_bigint MyCheck.Arbitrary.big_int in *)
    (* TODO: apparently bigints are really slow compared to int64 for domaintest *)
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let list_pair_arb = QCheck.small_list pair_arb in
    let canonize_randomly_generated_list = (fun x -> norm_intvs ik  x |> fst) in
    let shrink xs = MyCheck.shrink list_pair_arb xs >|= canonize_randomly_generated_list
    in QCheck.(set_shrink shrink @@ set_print show @@ map (*~rev:BatOption.get*) canonize_randomly_generated_list list_pair_arb)
end

module SOverflowUnlifter (D : SOverflow) : S with type int_t = D.int_t and type t = D.t = struct
  include D

  let add ?no_ov ik x y = fst @@ D.add ?no_ov ik x y

  let sub ?no_ov ik x y = fst @@ D.sub ?no_ov ik x y

  let mul ?no_ov ik x y = fst @@ D.mul ?no_ov ik x y

  let div ?no_ov ik x y = fst @@ D.div ?no_ov ik x y

  let neg ?no_ov ik x = fst @@ D.neg ?no_ov ik x

  let cast_to ?torg ?no_ov ik x = fst @@ D.cast_to ?torg ?no_ov ik x

  let of_int ik x = fst @@ D.of_int ik x

  let of_interval ?suppress_ovwarn ik x = fst @@ D.of_interval ?suppress_ovwarn ik x

  let starting ?suppress_ovwarn ik x = fst @@ D.starting ?suppress_ovwarn ik x

  let ending ?suppress_ovwarn ik x = fst @@ D.ending ?suppress_ovwarn ik x

  let shift_left ik x y = fst @@ D.shift_left ik x y

  let shift_right ik x y = fst @@ D.shift_right ik x y
end

module IntIkind = struct let ikind () = Cil.IInt end
module Interval =  IntervalFunctor (BI)
module Interval32 = IntDomWithDefaultIkind (IntDomLifter ( SOverflowUnlifter (IntervalFunctor (IntOps.Int64Ops)) ) ) (IntIkind)
module IntervalSet = IntervalSetFunctor(BI)
module Integers(Ints_t : IntOps.IntOps): IkindUnawareS with type t = Ints_t.t and type int_t = Ints_t.t = (* no top/bot, order is <= *)
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
  let bitnot = Ints_t.bitnot
  let bitand = Ints_t.bitand
  let bitor  = Ints_t.bitor
  let bitxor = Ints_t.bitxor
  let shift_left  n1 n2 = Ints_t.shift_left n1 (Ints_t.to_int n2)
  let shift_right n1 n2 = Ints_t.shift_right n1 (Ints_t.to_int n2)
  let lognot n1    = of_bool (not (to_bool' n1))
  let logand n1 n2 = of_bool ((to_bool' n1) && (to_bool' n2))
  let logor  n1 n2 = of_bool ((to_bool' n1) || (to_bool' n2))
  let cast_to ?torg t x =  failwith @@ "Cast_to not implemented for " ^ (name ()) ^ "."
  let arbitrary ik = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 (* TODO: use ikind *)
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

module Flat (Base: IkindUnawareS) = (* identical to Lift, but goes to `Top/`Bot if Base raises Unknown/Error *)
struct
  type int_t = Base.int_t
  include Lattice.Flat (Base) (struct
      let top_name = "Unknown int"
      let bot_name = "Error int"
    end)

  let top_of ik = top ()
  let bot_of ik = bot ()


  let name () = "flat integers"
  let cast_to ?torg t = function
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
  let bitnot = lift1 Base.bitnot
  let bitand = lift2 Base.bitand
  let bitor  = lift2 Base.bitor
  let bitxor = lift2 Base.bitxor
  let shift_left  = lift2 Base.shift_left
  let shift_right = lift2 Base.shift_right
  let lognot = lift1 Base.lognot
  let logand = lift2 Base.logand
  let logor  = lift2 Base.logor

  let invariant e = function
    | `Lifted x -> Base.invariant e x
    | `Top | `Bot -> Invariant.none
end

module Lift (Base: IkindUnawareS) = (* identical to Flat, but does not go to `Top/Bot` if Base raises Unknown/Error *)
struct
  include Lattice.LiftPO (Base) (struct
      let top_name = "MaxInt"
      let bot_name = "MinInt"
    end)
  type int_t = Base.int_t
  let top_of ik = top ()
  let bot_of ik = bot ()
  include StdTop (struct type nonrec t = t let top_of = top_of end)

  let name () = "lifted integers"
  let cast_to ?torg t = function
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
  let bitnot = lift1 Base.bitnot
  let bitand = lift2 Base.bitand
  let bitor  = lift2 Base.bitor
  let bitxor = lift2 Base.bitxor
  let shift_left  = lift2 Base.shift_left
  let shift_right = lift2 Base.shift_right
  let lognot = lift1 Base.lognot
  let logand = lift2 Base.logand
  let logor  = lift2 Base.logor

  let invariant e = function
    | `Lifted x -> Base.invariant e x
    | `Top | `Bot -> Invariant.none
end

module Flattened = Flat (Integers(IntOps.Int64Ops))
module FlattenedBI = Flat (Integers(IntOps.BigIntOps))
module Lifted    = Lift (Integers(IntOps.Int64Ops))

module Reverse (Base: IkindUnawareS) =
struct
  include Base
  include (Lattice.Reverse (Base) : Lattice.S with type t := Base.t)
end

module BigInt = struct
  include  BI
  let name () = "BigIntPrintable"
  let top () = raise Unknown
  let bot () = raise Error
  let top_of ik = top ()
  let bot_of ik = bot ()
  let cast_to ik x = Size.cast ik x
  let to_bool x = Some (not (BI.equal (BI.zero) x))

  let show x = BI.to_string x
  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)
  let arbitrary () = QCheck.map ~rev:to_int64 of_int64 QCheck.int64
end

module BISet = struct
  include SetDomain.Make(BigInt)
  let is_singleton s = cardinal s = 1
end

(* The module [Exclusion] constains common functionality about handling of exclusion sets between [DefExc] and [Enums] *)
module Exclusion =
struct
  module R = Interval32
  module I = BI
  (* We use these types for the functions in this module to make the intended meaning more explicit *)
  type t = Exc of BISet.t * Interval32.t
  type inc = Inc of BISet.t
  let max_of_range r = Size.max_from_bit_range (Option.get (R.maximal r))
  let min_of_range r = Size.min_from_bit_range (Option.get (R.minimal r))
  let cardinality_of_range r = BI.add BI.one (BI.add (BI.neg (min_of_range r)) (max_of_range r))

  let cardinality_BISet s =
    BI.of_int (BISet.cardinal s)

  let leq_excl_incl (Exc (xs, r)) (Inc ys) =
    (* For a <= b to hold, the cardinalities must fit, i.e. |a| <= |b|, which implies |min_r, max_r| - |xs| <= |ys|. We check this first. *)
    let lower_bound_cardinality_a = BI.sub (cardinality_of_range r) (cardinality_BISet xs) in
    let card_b = cardinality_BISet ys in
    if I.compare lower_bound_cardinality_a card_b > 0 then
      false
    else (* The cardinality did fit, so we check for all elements that are represented by range r, whether they are in (xs union ys) *)
      let min_a = min_of_range r in
      let max_a = max_of_range r in
      GobZ.for_all_range (fun el -> BISet.mem el xs || BISet.mem el ys) (min_a, max_a)

  let leq (Exc (xs, r)) (Exc (ys, s)) =
    let min_a, max_a = min_of_range r, max_of_range r in
    let excluded_check = BISet.for_all (fun y -> BISet.mem y xs || I.compare y min_a < 0 || I.compare y max_a > 0) ys in (* if true, then the values ys, that are not in b, also do not occur in a *)
    if not excluded_check
    then false
    else begin (* Check whether all elements that are in the range r, but not in s, are in xs, i.e. excluded. *)
      if R.leq r s then true
      else begin if I.compare (cardinality_BISet xs) (I.sub (cardinality_of_range r) (cardinality_of_range s)) >= 0 (* Check whether the number of excluded elements in a is as least as big as |min_r, max_r| - |min_s, max_s| *)
        then
          let min_b, max_b = min_of_range s, max_of_range s in
          let leq1 = (* check whether the elements in [r_l; s_l-1] are all in xs, i.e. excluded *)
            if I.compare min_a min_b < 0 then
              GobZ.for_all_range (fun x -> BISet.mem x xs) (min_a, BI.sub min_b BI.one)
            else
              true
          in
          let leq2 () = (* check whether the elements in [s_u+1; r_u] are all in xs, i.e. excluded *)
            if I.compare max_b max_a < 0 then
              GobZ.for_all_range (fun x -> BISet.mem x xs) (BI.add max_b BI.one, max_a)
            else
              true
          in
          leq1 && (leq2 ())
        else
          false
      end
    end
end

module DefExc : S with type int_t = BigInt.t = (* definite or set of excluded values *)
struct
  module S = BISet
  module R = Interval32 (* range for exclusion *)

  (* Ikind used for intervals representing the domain *)
  let range_ikind = Cil.IInt
  let size t = R.of_interval range_ikind (let a,b = Size.bits_i64 t in Int64.neg a,b)


  type t = [
    | `Excluded of S.t * R.t
    | `Definite of BigInt.t
    | `Bot
  ] [@@deriving eq, ord, hash]
  type int_t = BigInt.t
  let name () = "def_exc"


  let top_range = R.of_interval range_ikind (-99L, 99L) (* Since there is no top ikind we use a range that includes both ILongLong [-63,63] and IULongLong [0,64]. Only needed for intermediate range computation on longs. Correct range is set by cast. *)
  let top () = `Excluded (S.empty (), top_range)
  let bot () = `Bot
  let top_of ik = `Excluded (S.empty (), size ik)
  let bot_of ik = bot ()



  let show x =
    let short_size x = "("^R.show x^")" in
    match x with
    | `Bot -> "Error int"
    | `Definite x -> BigInt.show x
    (* Print the empty exclusion as if it was a distinct top element: *)
    | `Excluded (s,l) when S.is_empty s -> "Unknown int" ^ short_size l
    (* Prepend the exclusion sets with something: *)
    | `Excluded (s,l) -> "Not " ^ S.show s ^ short_size l

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let maximal = function
    | `Definite x -> Some x
    | `Excluded (s,r) -> Some (Exclusion.max_of_range r)
    | `Bot -> None

  let minimal = function
    | `Definite x -> Some x
    | `Excluded (s,r) -> Some (Exclusion.min_of_range r)
    | `Bot -> None

  let in_range r i =
    let lowerb = Exclusion.min_of_range r in
    if BI.compare i BI.zero < 0  then BI.compare lowerb i <= 0
    else (
      let upperb = Exclusion.max_of_range r in
      BI.compare i upperb <= 0
    )

  let is_top x = x = top ()

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Definite x -> if i = x then `Eq else `Neq
    | `Excluded (s,r) -> if S.mem i s then `Neq else `Top

  let top_of ik = `Excluded (S.empty (), size ik)
  let cast_to ?torg ?no_ov ik = function
    | `Excluded (s,r) ->
      let r' = size ik in
      `Excluded (
        if R.leq r r' then (* upcast -> no change *)
          s, r
        else (* downcast: may overflow *)
          (* let s' = S.map (BigInt.cast_to ik) s in *)
          (* We want to filter out all i in s' where (t)x with x in r could be i. *)
          (* Since this is hard to compute, we just keep all i in s' which overflowed, since those are safe - all i which did not overflow may now be possible due to overflow of r. *)
          (* S.diff s' s, r' *)
          (* The above is needed for test 21/03, but not sound! See example https://github.com/goblint/analyzer/pull/95#discussion_r483023140 *)
          S.empty (), r'
      )
    | `Definite x -> `Definite (BigInt.cast_to ik x)
    | `Bot -> `Bot

  (* Wraps definite values and excluded values according to the ikind.
   * For an `Excluded s,r , assumes that r is already an overapproximation of the range of possible values.
   * r might be larger than the possible range of this type; the range of the returned `Excluded set will be within the bounds of the ikind.
  *)
  let norm ik v =
    match v with
    | `Excluded (s, r) ->
      let possibly_overflowed = not (R.leq r (size ik)) || not (S.for_all (in_range (size ik)) s) in
      (* If no overflow occurred, just return x *)
      if not possibly_overflowed then (
        v
      )
      (* Else, if an overflow might have occurred but we should just ignore it *)
      else if should_ignore_overflow ik then (
        let r = size ik in
        (* filter out excluded elements that are not in the range *)
        let mapped_excl = S.filter (in_range r) s in
        `Excluded (mapped_excl, r)
      )
      (* Else, if an overflow occurred that we should not treat with wrap-around, go to top *)
      else if not (should_wrap ik) then (
        top_of ik
      ) else (
        (* Else an overflow occurred that we should treat with wrap-around *)
        let r = size ik in
        (* Perform a wrap-around for unsigned values and for signed values (if configured). *)
        let mapped_excl = S.map (fun excl -> BigInt.cast_to ik excl) s in
        match ik with
        | IBool ->
          begin match S.mem BigInt.zero mapped_excl, S.mem BigInt.one mapped_excl with
            | false, false -> `Excluded (mapped_excl, r) (* Not {} -> Not {} *)
            | true, false -> `Definite BigInt.one (* Not {0} -> 1 *)
            | false, true -> `Definite BigInt.zero (* Not {1} -> 0 *)
            | true, true -> `Bot (* Not {0, 1} -> bot *)
          end
        | ik ->
          `Excluded (mapped_excl, r)
      )
    | `Definite x ->
      let min, max = Size.range ik in
      (* Perform a wrap-around for unsigned values and for signed values (if configured). *)
      if should_wrap ik then (
        cast_to ik v
      )
      else if BigInt.compare min x <= 0 && BigInt.compare x max <= 0 then (
        v
      )
      else if should_ignore_overflow ik then (
        M.warn ~category:M.Category.Integer.overflow "DefExc: Value was outside of range, indicating overflow, but 'sem.int.signed_overflow' is 'assume_none' -> Returned Bot";
        `Bot
      )
      else (
        top_of ik
      )
    | `Bot -> `Bot

  let leq x y = match (x,y) with
    (* `Bot <= x is always true *)
    | `Bot, _ -> true
    (* Anything except bot <= bot is always false *)
    | _, `Bot -> false
    (* Two known values are leq whenever equal *)
    | `Definite (x: int_t), `Definite y -> x = y
    (* A definite value is leq all exclusion sets that don't contain it *)
    | `Definite x, `Excluded (s,r) -> in_range r x && not (S.mem x s)
    (* No finite exclusion set can be leq than a definite value *)
    | `Excluded (xs, xr), `Definite d ->
      Exclusion.(leq_excl_incl (Exc (xs, xr)) (Inc (S.singleton d)))
    | `Excluded (xs,xr), `Excluded (ys,yr) ->
      Exclusion.(leq (Exc (xs,xr)) (Exc (ys, yr)))

  let join' ?range ik x y =
    match (x,y) with
    (* The least upper bound with the bottom element: *)
    | `Bot, x -> x
    | x, `Bot -> x
    (* The case for two known values: *)
    | `Definite (x: int_t), `Definite y ->
      (* If they're equal, it's just THAT value *)
      if x = y then `Definite x
      (* Unless one of them is zero, we can exclude it: *)
      else
        let a,b = Size.min_range_sign_agnostic x, Size.min_range_sign_agnostic y in
        let r = R.join (R.of_interval range_ikind a) (R.of_interval range_ikind b) in
        `Excluded ((if BI.equal x BI.zero || BI.equal y BI.zero then S.empty () else S.singleton BI.zero), r)
    (* A known value and an exclusion set... the definite value should no
     * longer be excluded: *)
    | `Excluded (s,r), `Definite x
    | `Definite x, `Excluded (s,r) ->
      if not (in_range r x) then
        let a = R.of_interval range_ikind (Size.min_range_sign_agnostic x) in
        `Excluded (S.remove x s, R.join a r)
      else
        `Excluded (S.remove x s, r)
    (* For two exclusion sets, only their intersection can be excluded: *)
    | `Excluded (x,wx), `Excluded (y,wy) -> `Excluded (S.inter x y, range |? R.join wx wy)

  let join ik = join' ik


  let widen ik x y =
    if get_def_exc_widen_by_join () then
      join' ik x y
    else if equal x y then
      x
    else
      join' ~range:(size ik) ik x y


  let meet ik x y =
    match (x,y) with
    (* Greatest LOWER bound with the least element is trivial: *)
    | `Bot, _ -> `Bot
    | _, `Bot -> `Bot
    (* Definite elements are either equal or the glb is bottom *)
    | `Definite x, `Definite y -> if x = y then `Definite x else `Bot
    (* The glb of a definite element and an exclusion set is either bottom or
     * just the element itself, if it isn't in the exclusion set *)
    | `Excluded (s,r), `Definite x
    | `Definite x, `Excluded (s,r) -> if S.mem x s || not (in_range r x) then `Bot else `Definite x
    (* The greatest lower bound of two exclusion sets is their union, this is
     * just DeMorgans Law *)
    | `Excluded (x,r1), `Excluded (y,r2) ->
      let r' = R.meet r1 r2 in
      let s' = S.union x y |> S.filter (in_range r') in
      `Excluded (s', r')

  let narrow ik x y = x

  let of_int ik x = norm ik @@ `Definite x
  let to_int x = match x with
    | `Definite x -> Some x
    | _ -> None

  let from_excl ikind (s: S.t) = norm ikind @@ `Excluded (s, size ikind)
  let not_zero ikind = from_excl ikind (S.singleton BI.zero)

  let of_bool_cmp ik x = of_int ik (if x then BI.one else BI.zero)
  let of_bool = of_bool_cmp
  let to_bool x =
    match x with
    | `Definite x -> BigInt.to_bool x
    | `Excluded (s,r) when S.mem BI.zero s -> Some true
    | _ -> None
  let top_bool = `Excluded (S.empty (), R.of_interval range_ikind (0L, 1L))

  let of_interval ?(suppress_ovwarn=false) ik (x,y) = if BigInt.compare x y = 0 then of_int ik x else top_of ik

  let starting ?(suppress_ovwarn=false) ikind x = if BigInt.compare x BigInt.zero > 0 then not_zero ikind else top_of ikind
  let ending ?(suppress_ovwarn=false) ikind x = if BigInt.compare x BigInt.zero < 0 then not_zero ikind else top_of ikind

  let of_excl_list t l =
    let r = size t in (* elements in l are excluded from the full range of t! *)
    `Excluded (List.fold_right S.add l (S.empty ()), r)
  let is_excl_list l = match l with `Excluded _ -> true | _ -> false
  let to_excl_list (x:t) = match x with
    | `Definite _ -> None
    | `Excluded (s,r) -> Some (S.elements s, (Option.get (R.minimal r), Option.get (R.maximal r)))
    | `Bot -> None

  let to_incl_list x = match x with
    | `Definite x -> Some [x]
    | `Excluded _ -> None
    | `Bot -> None

  let apply_range f r = (* apply f to the min/max of the old range r to get a new range *)
    (* If the Int64 might overflow on us during computation, we instead go to top_range *)
    match R.minimal r, R.maximal r with
    | _ ->
      let rf m = (size % Size.min_for % f) (m r) in
      let r1, r2 = rf Exclusion.min_of_range, rf Exclusion.max_of_range in
      R.join r1 r2

  (* Default behaviour for unary operators, simply maps the function to the
   * DefExc data structure. *)
  let lift1 f ik x = norm ik @@ match x with
    | `Excluded (s,r) ->
      let s' = S.map f s in
      `Excluded (s', apply_range f r)
    | `Definite x -> `Definite (f x)
    | `Bot -> `Bot

  let lift2 f ik x y = norm ik (match x,y with
      (* We don't bother with exclusion sets: *)
      | `Excluded _, `Definite _
      | `Definite _, `Excluded _
      | `Excluded _, `Excluded _ -> top ()
      (* The good case: *)
      | `Definite x, `Definite y ->
        (try `Definite (f x y) with | Division_by_zero -> top ())
      | `Bot, `Bot -> `Bot
      | _ ->
        (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
        raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y))))

  (* Default behaviour for binary operators that are injective in either
   * argument, so that Exclusion Sets can be used: *)
  let lift2_inj f ik x y =
    let def_exc f x s r = `Excluded (S.map (f x) s, apply_range (f x) r) in
    norm ik @@
    match x,y with
    (* If both are exclusion sets, there isn't anything we can do: *)
    | `Excluded _, `Excluded _ -> top ()
    (* A definite value should be applied to all members of the exclusion set *)
    | `Definite x, `Excluded (s,r) -> def_exc f x s r
    (* Same thing here, but we should flip the operator to map it properly *)
    | `Excluded (s,r), `Definite x -> def_exc (Batteries.flip f) x s r
    (* The good case: *)
    | `Definite x, `Definite y -> `Definite (f x y)
    | `Bot, `Bot -> `Bot
    | _ ->
      (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))

  (* The equality check: *)
  let eq ik x y = match x,y with
    (* Not much to do with two exclusion sets: *)
    | `Excluded _, `Excluded _ -> top ()
    (* Is x equal to an exclusion set, if it is a member then NO otherwise we
     * don't know: *)
    | `Definite x, `Excluded (s,r) -> if S.mem x s then of_bool IInt false else top ()
    | `Excluded (s,r), `Definite x -> if S.mem x s then of_bool IInt false else top ()
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool IInt (x = y)
    | `Bot, `Bot -> `Bot
    | _ ->
      (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))

  (* The inequality check: *)
  let ne ik x y = match x,y with
    (* Not much to do with two exclusion sets: *)
    | `Excluded _, `Excluded _ -> top ()
    (* Is x unequal to an exclusion set, if it is a member then Yes otherwise we
     * don't know: *)
    | `Definite x, `Excluded (s,r) -> if S.mem x s then of_bool IInt true else top ()
    | `Excluded (s,r), `Definite x -> if S.mem x s then of_bool IInt true else top ()
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool IInt (x <> y)
    | `Bot, `Bot -> `Bot
    | _ ->
      (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))

  let neg ?no_ov ik (x :t) = norm ik @@ lift1 BigInt.neg ik x
  let add ?no_ov ik x y = norm ik @@ lift2_inj BigInt.add ik x y

  let sub ?no_ov ik x y = norm ik @@ lift2_inj BigInt.sub ik x y
  let mul ?no_ov ik x y = norm ik @@ match x, y with
    | `Definite z, (`Excluded _ | `Definite _) when BigInt.equal z BigInt.zero -> x
    | (`Excluded _ | `Definite _), `Definite z when BigInt.equal z BigInt.zero -> y
    | `Definite a, `Excluded (s,r)
    (* Integer multiplication with even numbers is not injective. *)
    (* Thus we cannot exclude the values to which the exclusion set would be mapped to. *)
    | `Excluded (s,r),`Definite a when BigInt.equal (BigInt.rem a (BigInt.of_int 2)) BigInt.zero -> `Excluded (S.empty (), apply_range (BigInt.mul a) r)
    | _ -> lift2_inj BigInt.mul ik x y
  let div ?no_ov ik x y = lift2 BigInt.div ik x y
  let rem ik x y = lift2 BigInt.rem ik x y

  (* Comparison handling copied from Enums. *)
  let handle_bot x y f = match x, y with
    | `Bot, `Bot -> `Bot
    | `Bot, _
    | _, `Bot -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, _ -> f ()

  let lt ik x y =
    handle_bot x y (fun () ->
        match minimal x, maximal x, minimal y, maximal y with
        | _, Some x2, Some y1, _ when BigInt.compare x2 y1 < 0 -> of_bool ik true
        | Some x1, _, _, Some y2 when BigInt.compare x1 y2 >= 0 -> of_bool ik false
        | _, _, _, _ -> top_bool)

  let gt ik x y = lt ik y x

  let le ik x y =
    handle_bot x y (fun () ->
        match minimal x, maximal x, minimal y, maximal y with
        | _, Some x2, Some y1, _ when BigInt.compare x2 y1 <= 0 -> of_bool ik true
        | Some x1, _, _, Some y2 when BigInt.compare x1 y2 > 0 -> of_bool ik false
        | _, _, _, _ -> top_bool)

  let ge ik x y = le ik y x

  let bitnot = lift1 BigInt.bitnot
  let bitand = lift2 BigInt.bitand
  let bitor  = lift2 BigInt.bitor
  let bitxor = lift2 BigInt.bitxor

  let shift (shift_op: int_t -> int -> int_t) (ik: Cil.ikind) (x: t) (y: t) =
    (* BigInt only accepts int as second argument for shifts; perform conversion here *)
    let shift_op_big_int a (b: int_t) =
      let (b : int) = BI.to_int b in
      shift_op a b
    in
    (* If one of the parameters of the shift is negative, the result is undefined *)
    let x_min = minimal x in
    let y_min = minimal y in
    if x_min = None || y_min = None || BI.compare (Option.get x_min) BI.zero < 0 || BI.compare (Option.get y_min) BI.zero < 0 then
      top_of ik
    else
      norm ik @@ lift2 shift_op_big_int ik x y

  let shift_left =
    shift BigInt.shift_left

  let shift_right =
    shift BigInt.shift_right
  (* TODO: lift does not treat Not {0} as true. *)
  let logand ik x y =
    match to_bool x, to_bool y with
    | Some false, _
    | _, Some false ->
      of_bool ik false
    | _, _ ->
      lift2 BigInt.logand ik x y
  let logor ik x y =
    match to_bool x, to_bool y with
    | Some true, _
    | _, Some true ->
      of_bool ik true
    | _, _ ->
      lift2 BigInt.logor ik x y
  let lognot ik = eq ik (of_int ik BigInt.zero)

  let invariant_ikind e ik (x:t) =
    match x with
    | `Definite x ->
      if get_bool "witness.invariant.exact" then
        Invariant.of_exp Cil.(BinOp (Eq, e, kintegerCilint ik x, intType))
      else
        Invariant.top ()
    | `Excluded (s, r) ->
      (* Emit range invariant if tighter than ikind bounds.
         This can be more precise than interval, which has been widened. *)
      let (rmin, rmax) = (Exclusion.min_of_range r, Exclusion.max_of_range r) in
      let (ikmin, ikmax) =
        let ikr = size ik in
        (Exclusion.min_of_range ikr, Exclusion.max_of_range ikr)
      in
      let inexact_type_bounds = get_bool "witness.invariant.inexact-type-bounds" in
      let imin = if inexact_type_bounds || BI.compare ikmin rmin <> 0 then Invariant.of_exp Cil.(BinOp (Le, kintegerCilint ik rmin, e, intType)) else Invariant.none in
      let imax = if inexact_type_bounds || BI.compare rmax ikmax <> 0 then Invariant.of_exp Cil.(BinOp (Le, e, kintegerCilint ik rmax, intType)) else Invariant.none in
      S.fold (fun x a ->
          let i = Invariant.of_exp Cil.(BinOp (Ne, e, kintegerCilint ik x, intType)) in
          Invariant.(a && i)
        ) s Invariant.(imin && imax)
    | `Bot -> Invariant.none

  let arbitrary ik =
    let open QCheck.Iter in
    let excluded s = from_excl ik s in
    let definite x = of_int ik x in
    let shrink = function
      | `Excluded (s, _) -> MyCheck.shrink (S.arbitrary ()) s >|= excluded (* S TODO: possibly shrink excluded to definite *)
      | `Definite x -> (return `Bot) <+> (MyCheck.shrink (BigInt.arbitrary ()) x >|= definite)
      | `Bot -> empty
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map excluded (S.arbitrary ());
      10, QCheck.map definite (BigInt.arbitrary ());
      1, QCheck.always `Bot
    ] (* S TODO: decide frequencies *)

  let refine_with_congruence ik a b = a
  let refine_with_interval ik a b = match a, b with
    | x, Some(i) -> meet ik x (of_interval ik i)
    | _ -> a
  let refine_with_excl_list ik a b = match a, b with
    | `Excluded (s, r), Some(ls, _) -> meet ik (`Excluded (s, r)) (of_excl_list ik ls) (* TODO: refine with excl range? *)
    | _ -> a
  let refine_with_incl_list ik a b = a

  let project ik p t = t
end

(* BOOLEAN DOMAINS *)

module type BooleansNames =
sig
  val truename: string
  val falsename: string
end

module MakeBooleans (N: BooleansNames) =
struct
  type int_t = IntOps.Int64Ops.t
  type t = bool [@@deriving eq, ord, hash, to_yojson]
  let name () = "booleans"
  let top () = true
  let bot () = false
  let top_of ik = top ()
  let bot_of ik = bot ()
  let show x = if x then N.truename else N.falsename
  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)
  let is_top x = x (* override Std *)

  let equal_to i x = if x then `Top else failwith "unsupported: equal_to with bottom"
  let cast_to ?torg _ x = x (* ok since there's no smaller ikind to cast to *)

  let leq x y = not x || y
  let join = (||)
  let widen = join
  let meet = (&&)
  let narrow = meet

  let of_bool x = x
  let to_bool x = Some x
  let of_int x  = x = Int64.zero
  let to_int x  = if x then None else Some Int64.zero

  let neg x = x
  let add x y = x || y
  let sub x y = x || y
  let mul x y = x && y
  let div x y = true
  let rem x y = true
  let lt n1 n2 = true
  let gt n1 n2 = true
  let le n1 n2 = true
  let ge n1 n2 = true
  let eq n1 n2 = true
  let ne n1 n2 = true
  let bitnot x = true
  let bitand x y = x && y
  let bitor  x y = x || y
  let bitxor x y = x && not y || not x && y
  let shift_left  n1 n2 = n1
  let shift_right n1 n2 = n1
  let lognot = (not)
  let logand = (&&)
  let logor  = (||)
  let arbitrary () = QCheck.bool
  let invariant _ _ = Invariant.none (* TODO *)
end

module Booleans = MakeBooleans (
  struct
    let truename = "True"
    let falsename = "False"
  end)

(* Inclusion/Exclusion sets. Go to top on arithmetic operations (except for some easy cases, e.g. multiplication with 0). Joins on widen, i.e. precise integers as long as not derived from arithmetic expressions. *)
module Enums : S with type int_t = BigInt.t = struct
  open Batteries
  module I = BigInt
  module R = Interval32 (* range for exclusion *)

  let range_ikind = Cil.IInt
  let size t = R.of_interval range_ikind (let a,b = Size.bits_i64 t in Int64.neg a,b)

  type t = Inc of BISet.t | Exc of BISet.t * R.t [@@deriving eq, ord, hash] (* inclusion/exclusion set *)

  type int_t = BI.t
  let name () = "enums"
  let bot () = failwith "bot () not implemented for Enums"
  let top_of ik = Exc (BISet.empty (), size ik)
  let top () = failwith "top () not implemented for Enums"
  let bot_of ik = Inc (BISet.empty ())
  let top_bool = Inc (BISet.of_list [I.zero; I.one])

  let range ik = BatTuple.Tuple2.mapn I.of_bigint (Size.range ik)

(*
  let max_of_range r = Size.max_from_bit_range (Option.get (R.maximal r))
  let min_of_range r = Size.min_from_bit_range (Option.get (R.minimal r))
  let cardinality_of_range r = I.add (I.neg (min_of_range r)) (max_of_range r) *)
  let value_in_range (min, max) v = I.compare min v <= 0 && I.compare v max <= 0

  let show = function
    | Inc xs when BISet.is_empty xs -> "bot"
    | Inc xs -> "{" ^ (String.concat ", " (List.map I.show (BISet.elements  xs))) ^ "}"
    | Exc (xs,r) -> "not {" ^ (String.concat ", " (List.map I.show (BISet.elements xs))) ^ "} " ^ "("^R.show r^")"

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  (* Normalization function for enums, that handles overflows for Inc.
     As we do not compute on Excl, we do not have to perform any overflow handling for it. *)
  let norm ikind v =
    let min, max = range ikind in
    (* Whether the value v lies within the values of the specified ikind. *)
    let value_in_ikind v =
      I.compare min v <= 0 && I.compare v max <= 0
    in
    match v with
    | Inc xs when BISet.for_all value_in_ikind xs -> v
    | Inc xs ->
      if should_wrap ikind then
        Inc (BISet.map (BigInt.cast_to ikind) xs)
      else if should_ignore_overflow ikind then
        Inc (BISet.filter value_in_ikind xs)
      else
        top_of ikind
    | Exc (xs, r) ->
      (* The following assert should hold for Exc, therefore we do not have to overflow handling / normalization for it:
         let range_in_ikind r =
         R.leq r (size ikind)
         in
         let r_min, r_max = min_of_range r, max_of_range r in
         assert (range_in_ikind r && BISet.for_all (value_in_range (r_min, r_max)) xs); *)
      begin match ikind with
        | IBool ->
          begin match BISet.mem BigInt.zero xs, BISet.mem BigInt.one xs with
            | false, false -> top_bool  (* Not {} -> {0, 1} *)
            | true, false -> Inc (BISet.singleton BigInt.one) (* Not {0} -> {1} *)
            | false, true -> Inc (BISet.singleton BigInt.zero) (* Not {1} -> {0} *)
            | true, true -> bot_of ikind (* Not {0, 1} -> bot *)
          end
        | _ ->
          v
      end


  let equal_to i = function
    | Inc x ->
      if BISet.mem i x then
        if BISet.is_singleton x then `Eq
        else `Top
      else `Neq
    | Exc (x, r) ->
      if BISet.mem i x then `Neq
      else `Top

  let cast_to ?torg ?no_ov ik v = norm ik @@ match v with
    | Exc (s,r) ->
      let r' = size ik in
      if R.leq r r' then (* upcast -> no change *)
        Exc (s, r)
      else (* downcast: may overflow *)
        Exc ((BISet.empty ()), r')
    | Inc xs ->
      let casted_xs = BISet.map (BigInt.cast_to ik) xs in
      if Cil.isSigned ik && not (BISet.equal xs casted_xs)
      then top_of ik (* When casting into a signed type and the result does not fit, the behavior is implementation-defined *)
      else Inc casted_xs

  let of_int ikind x = cast_to ikind (Inc (BISet.singleton x))

  let of_interval ?(suppress_ovwarn=false) ik (x,y) = if x = y then of_int ik x else top_of ik

  let join ik = curry @@ function
    | Inc x, Inc y -> Inc (BISet.union x y)
    | Exc (x,r1), Exc (y,r2) -> Exc (BISet.inter x y, R.join r1 r2)
    | Exc (x,r), Inc y
    | Inc y, Exc (x,r) ->
      let r = if BISet.is_empty y
        then r
        else
          let (min_el_range, max_el_range) = Tuple2.mapn (fun x -> R.of_interval range_ikind (Size.min_range_sign_agnostic x)) (BISet.min_elt y, BISet.max_elt y) in
          let range = R.join min_el_range max_el_range in
          R.join r range
      in
      Exc (BISet.diff x y, r)

  let meet ikind = curry @@ function
    | Inc x, Inc y -> Inc (BISet.inter x y)
    | Exc (x,r1), Exc (y,r2) ->
      let r = R.meet r1 r2 in
      let r_min, r_max = Exclusion.min_of_range r, Exclusion.max_of_range r in
      let filter_by_range = BISet.filter (value_in_range (r_min, r_max)) in
      (* We remove those elements from the exclusion set that do not fit in the range anyway *)
      let excl = BISet.union (filter_by_range x) (filter_by_range y) in
      Exc (excl, r)
    | Inc x, Exc (y,r)
    | Exc (y,r), Inc x -> Inc (BISet.diff x y)

  let widen = join
  let narrow = meet
  let leq a b =
    match a, b with
    | Inc xs, Exc (ys, r) ->
      if BISet.is_empty xs
      then true
      else
        let min_b, max_b = Exclusion.min_of_range r, Exclusion.max_of_range r in
        let min_a, max_a = BISet.min_elt xs, BISet.max_elt xs in
        (* Check that the xs fit into the range r  *)
        I.compare min_b min_a <= 0 && I.compare max_a max_b <= 0 &&
        (* && check that none of the values contained in xs is excluded, i.e. contained in ys. *)
        BISet.for_all (fun x -> not (BISet.mem x ys)) xs
    | Inc xs, Inc ys ->
      BISet.subset xs ys
    | Exc (xs, r), Exc (ys, s) ->
      Exclusion.(leq (Exc (xs, r)) (Exc (ys, s)))
    | Exc (xs, r), Inc ys ->
      Exclusion.(leq_excl_incl (Exc (xs, r)) (Inc ys))

  let handle_bot x y f = match is_bot x, is_bot y with
    | false, false -> f ()
    | true, false
    | false, true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | true, true -> Inc (BISet.empty ())

  let lift1 f ikind v = norm ikind @@ match v with
    | Inc x when BISet.is_empty x -> v (* Return bottom when value is bottom *)
    | Inc x when BISet.is_singleton x -> Inc (BISet.singleton (f (BISet.choose x)))
    | _ -> top_of ikind

  let lift2 f (ikind: Cil.ikind) u v =
    handle_bot u v (fun () ->
        norm ikind @@ match u, v with
        | Inc x,Inc y when BISet.is_singleton x && BISet.is_singleton y -> Inc (BISet.singleton (f (BISet.choose x) (BISet.choose y)))
        | _,_ -> top_of ikind)

  let lift2 f ikind a b =
    try lift2 f ikind a b with Division_by_zero -> top_of ikind

  let neg ?no_ov = lift1 I.neg
  let add ?no_ov ikind = curry @@ function
    | Inc z,x when BISet.is_singleton z && BISet.choose z = BI.zero -> x
    | x,Inc z when BISet.is_singleton z && BISet.choose z = BI.zero -> x
    | x,y -> lift2 I.add ikind x y
  let sub ?no_ov = lift2 I.sub
  let mul ?no_ov ikind a b =
    match a, b with
    | Inc one,x when BISet.is_singleton one && BISet.choose one = BI.one -> x
    | x,Inc one when BISet.is_singleton one && BISet.choose one = BI.one -> x
    | Inc zero,_ when BISet.is_singleton zero && BISet.choose zero = BI.zero -> a
    | _,Inc zero when BISet.is_singleton zero && BISet.choose zero = BI.zero -> b
    | x,y -> lift2 I.mul ikind x y

  let div ?no_ov ikind a b = match a, b with
    | x,Inc one when BISet.is_singleton one && BISet.choose one = BI.one -> x
    | _,Inc zero when BISet.is_singleton zero && BISet.choose zero = BI.zero -> top_of ikind
    | Inc zero,_ when BISet.is_singleton zero && BISet.choose zero = BI.zero -> a
    | x,y -> lift2 I.div ikind x y

  let rem = lift2 I.rem

  let bitnot = lift1 BigInt.bitnot
  let bitand = lift2 BigInt.bitand
  let bitor  = lift2 BigInt.bitor
  let bitxor = lift2 BigInt.bitxor

  let shift (shift_op: int_t -> int -> int_t) (ik: Cil.ikind) (x: t) (y: t) =
    handle_bot x y (fun () ->
        (* BigInt only accepts int as second argument for shifts; perform conversion here *)
        let shift_op_big_int a (b: int_t) =
          let (b : int) = BI.to_int b in
          shift_op a b
        in
        (* If one of the parameters of the shift is negative, the result is undefined *)
        let x_min = minimal x in
        let y_min = minimal y in
        if x_min = None || y_min = None || BI.compare (Option.get x_min) BI.zero < 0 || BI.compare (Option.get y_min) BI.zero < 0 then
          top_of ik
        else
          lift2 shift_op_big_int ik x y)

  let shift_left =
    shift BigInt.shift_left

  let shift_right =
    shift BigInt.shift_right

  let of_bool ikind x = Inc (BISet.singleton (if x then BI.one else BI.zero))
  let to_bool  = function
    | Inc e when BISet.is_empty e -> None
    | Exc (e,_) when BISet.is_empty e -> None
    | Inc zero when BISet.is_singleton zero && BISet.choose zero = BI.zero -> Some false
    | Inc xs when BISet.for_all ((<>) BI.zero) xs -> Some true
    | Exc (xs,_) when BISet.exists ((=) BI.zero) xs -> Some true
    | _ -> None
  let to_int = function Inc x when BISet.is_singleton x -> Some (BISet.choose x) | _ -> None

  let to_excl_list = function Exc (x,r) when not (BISet.is_empty x) -> Some (BISet.elements x, (Option.get (R.minimal r), Option.get (R.maximal r))) | _ -> None
  let of_excl_list ik xs =
    let min_ik, max_ik = Size.range ik in
    let exc = BISet.of_list @@ List.filter (value_in_range (min_ik, max_ik)) xs in
    norm ik @@ Exc (exc, size ik)
  let is_excl_list = BatOption.is_some % to_excl_list
  let to_incl_list = function Inc s when not (BISet.is_empty s) -> Some (BISet.elements s) | _ -> None

  let starting ?(suppress_ovwarn=false) ikind x = top_of ikind
  let ending ?(suppress_ovwarn=false) ikind x = top_of ikind

  let lognot ik x =
    if is_bot x
    then x
    else
      match to_bool x with
      | Some b -> of_bool ik (not b)
      | None -> top_bool

  let logand = lift2 I.logand
  let logor  = lift2 I.logor
  let maximal = function
    | Inc xs when not (BISet.is_empty xs) -> Some (BISet.max_elt xs)
    | Exc (excl,r) ->
      let rec decrement_while_contained v =
        if BISet.mem v excl
        then decrement_while_contained (BI.sub v (BI.one))
        else v
      in
      let range_max = Exclusion.max_of_range r in
      Some (decrement_while_contained range_max)
    | _ (* bottom case *) -> None

  let minimal = function
    | Inc xs when not (BISet.is_empty xs) -> Some (BISet.min_elt xs)
    | Exc (excl,r) ->
      let rec increment_while_contained v =
        if BISet.mem v excl
        then increment_while_contained (BI.add v (BI.one))
        else v
      in
      let range_min = Exclusion.min_of_range r in
      Some (increment_while_contained range_min)
    | _ (* bottom case *) -> None

  let lt ik x y =
    handle_bot x y (fun () ->
        match minimal x, maximal x, minimal y, maximal y with
        | _, Some x2, Some y1, _ when I.compare x2 y1 < 0 -> of_bool ik true
        | Some x1, _, _, Some y2 when I.compare x1 y2 >= 0 -> of_bool ik false
        | _, _, _, _ -> top_bool)

  let gt ik x y = lt ik y x

  let le ik x y =
    handle_bot x y (fun () ->
        match minimal x, maximal x, minimal y, maximal y with
        | _, Some x2, Some y1, _ when I.compare x2 y1 <= 0 -> of_bool ik true
        | Some x1, _, _, Some y2 when I.compare x1 y2 > 0 -> of_bool ik false
        | _, _, _, _ -> top_bool)

  let ge ik x y = le ik y x

  let eq ik x y =
    handle_bot x y (fun () ->
        match x, y with
        | Inc xs, Inc ys when BISet.is_singleton xs && BISet.is_singleton ys -> of_bool ik (I.equal (BISet.choose xs) (BISet.choose ys))
        | _, _ ->
          if is_bot (meet ik x y) then
            (* If the meet is empty, there is no chance that concrete values are equal *)
            of_bool ik false
          else
            top_bool)

  let ne ik x y = lognot ik (eq ik x y)

  let invariant_ikind e ik x =
    match x with
    | Inc ps ->
      if BISet.cardinal ps > 1 || get_bool "witness.invariant.exact" then
        List.fold_left (fun a x ->
            let i = Invariant.of_exp Cil.(BinOp (Eq, e, kintegerCilint ik x, intType)) in
            Invariant.(a || i)
          ) (Invariant.bot ()) (BISet.elements ps)
      else
        Invariant.top ()
    | Exc (ns, _) ->
      List.fold_left (fun a x ->
          let i = Invariant.of_exp Cil.(BinOp (Ne, e, kintegerCilint ik x, intType)) in
          Invariant.(a && i)
        ) (Invariant.top ()) (BISet.elements ns)


  let arbitrary ik =
    let open QCheck.Iter in
    let neg s = of_excl_list ik (BISet.elements s) in
    let pos s = norm ik (Inc s) in
    let shrink = function
      | Exc (s, _) -> MyCheck.shrink (BISet.arbitrary ()) s >|= neg (* S TODO: possibly shrink neg to pos *)
      | Inc s -> MyCheck.shrink (BISet.arbitrary ()) s >|= pos
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map neg (BISet.arbitrary ());
      10, QCheck.map pos (BISet.arbitrary ());
    ] (* S TODO: decide frequencies *)

  let refine_with_congruence ik a b =
    let contains c m x = if BI.equal m BI.zero then BI.equal c x else BI.equal (BI.rem (BI.sub x c) m) BI.zero in
    match a, b with
    | Inc e, None -> bot_of ik
    | Inc e, Some (c, m) -> Inc (BISet.filter (contains c m) e)
    | _ -> a

  let refine_with_interval ik a b = a

  let refine_with_excl_list ik a b =
    match b with
    | Some (ls, _) -> meet ik a (of_excl_list ik ls) (* TODO: refine with excl range? *)
    | _ -> a

  let refine_with_incl_list ik a b =
    match a, b with
    | Inc x, Some (ls) -> meet ik (Inc x) (Inc (BISet.of_list ls))
    | _ -> a

  let project ik p t = t
end

module Congruence : S with type int_t = BI.t and type t = (BI.t * BI.t) option =
struct
  let name () = "congruences"
  module Ints_t = BI
  type int_t = Ints_t.t

  (* represents congruence class of c mod m, None is bot *)
  type t = (Ints_t.t * Ints_t.t) option [@@deriving eq, ord, hash]

  let ( *: ) = Ints_t.mul
  let (+:) = Ints_t.add
  let (-:) = Ints_t.sub
  let (%:) = Ints_t.rem
  let (/:) = Ints_t.div
  let (=:) = Ints_t.equal
  let (<:) x y = Ints_t.compare x y < 0
  let (>:) x y = Ints_t.compare x y > 0
  let (<=:) x y = Ints_t.compare x y <= 0
  let (>=:) x y = Ints_t.compare x y >= 0
  (* a divides b *)
  let ( |: ) a b =
    if a =: Ints_t.zero then false else (b %: a) =: Ints_t.zero

  let normalize ik x =
    match x with
    | None -> None
    | Some (c, m) ->
      if m =: Ints_t.zero then
        if should_wrap ik then
          Some (BigInt.cast_to ik c, m)
        else
          Some (c, m)
      else
        let m' = Ints_t.abs m in
        let c' = c %: m' in
        if c' <: Ints_t.zero then
          Some (c' +: m', m')
        else
          Some (c' %: m', m')

  let range ik = BatTuple.Tuple2.mapn Ints_t.of_bigint (Size.range ik)

  let top () = Some (Ints_t.zero, Ints_t.one)
  let top_of ik = Some (Ints_t.zero, Ints_t.one)
  let bot () = None
  let bot_of ik = bot ()

  let show = function ik -> match ik with
    | None -> "⟂"
    | Some (c, m) when (c, m) = (Ints_t.zero, Ints_t.zero) -> Ints_t.to_string c
    | Some (c, m) ->
      let a = if c =: Ints_t.zero then "" else Ints_t.to_string c in
      let b = if m =: Ints_t.zero then "" else if m = Ints_t.one then "ℤ" else Ints_t.to_string m^"ℤ" in
      let c = if a = "" || b = "" then "" else "+" in
      a^c^b

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let is_top x = x = top ()

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) when b =: Ints_t.zero -> if a =: i then `Eq else `Neq
    | Some (a, b) ->  if i %: b =: a then `Top else `Neq

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (c1,m1), Some (c2,m2) when m2 =: Ints_t.zero && m1 =: Ints_t.zero -> c1 =: c2
    | Some (c1,m1), Some (c2,m2) when m2 =: Ints_t.zero -> c1 =: c2 && m1 =: Ints_t.zero
    | Some (c1,m1), Some (c2,m2) -> m2 |: (Ints_t.gcd (c1 -: c2) m1)
  (* Typo in original equation of P. Granger (m2 instead of m1): gcd (c1 -: c2) m2
     Reference: https://doi.org/10.1080/00207168908803778 Page 171 corollary 3.3*)

  let leq x y =
    let res = leq x y in
    if M.tracing then M.trace "congruence" "leq %a %a -> %a \n" pretty x pretty y pretty (Some(Ints_t.of_int (Bool.to_int res), Ints_t.zero)) ;
    res

  let join ik (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (c1,m1), Some (c2,m2) ->
      let m3 = Ints_t.gcd m1 (Ints_t.gcd m2 (c1 -: c2)) in
      normalize ik (Some (c1, m3))

  let join ik (x:t) y =
    let res = join ik x y in
    if M.tracing then M.trace "congruence" "join %a %a -> %a\n" pretty x pretty y pretty res;
    res


  let meet ik x y =
    (* if it exists, c2/a2 is solution to a*x ≡ c (mod m) *)
    let congruence_series a c m =
      let rec next a1 c1 a2 c2 =
        if a2 |: a1 then (a2, c2)
        else next a2 c2 (a1 %: a2) (c1 -: (c2 *: (a1 /: a2)))
      in next m Ints_t.zero a c
    in
    let simple_case i c m =
      if m |: (i -: c)
      then Some (i, Ints_t.zero) else None
    in
    match x, y with
    | Some (c1, m1), Some (c2, m2) when m1 =: Ints_t.zero && m2 =: Ints_t.zero -> if c1 =: c2 then Some (c1, Ints_t.zero) else None
    | Some (c1, m1), Some (c2, m2) when m1 =: Ints_t.zero -> simple_case c1 c2 m2
    | Some (c1, m1), Some (c2, m2) when m2 =: Ints_t.zero -> simple_case c2 c1 m1
    | Some (c1, m1), Some (c2, m2) when (Ints_t.gcd m1 m2) |: (c1 -: c2) ->
      let (c, m) = congruence_series m1 (c2 -: c1 ) m2 in
      normalize ik (Some(c1 +: (m1 *: (m /: c)), m1 *: (m2 /: c)))
    | _  -> None

  let meet ik x y =
    let res = meet ik x y in
    if M.tracing then M.trace "congruence" "meet %a %a -> %a\n" pretty x pretty y pretty res;
    res

  let to_int = function Some (c, m) when m =: Ints_t.zero -> Some c | _ -> None
  let of_int ik (x: int_t) = normalize ik @@ Some (x, Ints_t.zero)
  let zero = Some (Ints_t.zero, Ints_t.zero)
  let one  = Some (Ints_t.one, Ints_t.zero)
  let top_bool = top()

  let of_bool _ik = function true -> one | false -> zero

  let to_bool (a: t) = match a with
    | None -> None
    | x when equal zero x -> Some false
    | x -> if leq zero x then None else Some true

  let starting ?(suppress_ovwarn=false) ik n = top()

  let ending = starting

  let of_congruence ik (c,m) = normalize ik @@ Some(c,m)

  let maximal t = match t with
    | Some (x, y) when y =: Ints_t.zero -> Some x
    | _ -> None

  let minimal t = match t with
    | Some (x,y) when y =: Ints_t.zero -> Some x
    | _ -> None

  (* cast from original type to ikind, set to top if the value doesn't fit into the new type *)
  let cast_to ?torg ?(no_ov=false) t x =
    match x with
    | None -> None
    | Some (c, m) when m =: Ints_t.zero ->
      let c' = Ints_t.of_bigint @@ BigInt.cast_to t (Ints_t.to_bigint c) in
      (* When casting into a signed type and the result does not fit, the behavior is implementation-defined. (C90 6.2.1.2, C99 and C11 6.3.1.3) *)
      (* We go with GCC behavior here: *)
      (*  For conversion to a type of width N, the value is reduced modulo 2^N to be within range of the type; no signal is raised. *)
      (*   (https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html)   *)
      (* Clang behaves the same but they never document that anywhere *)
      Some (c', m)
    | _ ->
      let (min_t, max_t) = range t in
      let p ikorg =
        let (min_ikorg, max_ikorg) = range ikorg in
        ikorg = t || (max_t >=: max_ikorg && min_t <=: min_ikorg)
      in
      match torg with
      | Some (Cil.TInt (ikorg, _)) when p ikorg ->
        if M.tracing then M.trace "cong-cast" "some case";
        x
      | _ -> top ()


  let cast_to ?torg ?no_ov (t : Cil.ikind) x =
    let pretty_bool _ x = Pretty.text (string_of_bool x) in
    let res = cast_to ?torg ?no_ov t x in
    if M.tracing then M.trace "cong-cast" "Cast %a to %a (no_ov: %a) = %a\n" pretty x Cil.d_ikind t (Pretty.docOpt (pretty_bool ())) no_ov pretty res;
    res

  let widen = join

  let widen ik x y =
    let res = widen ik x y in
    if M.tracing then M.trace "congruence" "widen %a %a -> %a\n" pretty x pretty y pretty res;
    res

  let narrow = meet

  let log f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_bool i1, to_bool i2 with
      | Some x, Some y -> of_bool ik (f x y)
      | _              -> top_of ik

  let logor = log (||)
  let logand = log (&&)

  let log1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_bool i1 with
      | Some x -> of_bool ik (f ik x)
      | _      -> top_of ik

  let lognot = log1 (fun _ik -> not)

  let shift_right _ _ _ = top()

  let shift_right ik x y =
    let res = shift_right ik x y in
    if M.tracing then  M.trace "congruence" "shift_right : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let shift_left ik x y =
    (* Naive primality test *)
    (* let is_prime n =
         let n = Ints_t.abs n in
         let rec is_prime' d =
           (d *: d >: n) || ((not ((n %: d) =: Ints_t.zero)) && (is_prime' [@tailcall]) (d +: Ints_t.one))
         in
         not (n =: Ints_t.one) && is_prime' (Ints_t.of_int 2)
       in *)
    match x, y with
    | None, None -> None
    | None, _
    | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') when (Cil.isSigned ik) || c <: Ints_t.zero || c' <: Ints_t.zero -> top_of ik
    | Some (c, m), Some (c', m') ->
      let (_, max_ik) = range ik in
      if (m =: Ints_t.zero && m' =: Ints_t.zero) then
        normalize ik @@ Some (Ints_t.bitand max_ik (Ints_t.shift_left c (Ints_t.to_int c')), Ints_t.zero)
      else
        let x = (Ints_t.bitand max_ik (Ints_t.shift_left Ints_t.one (Ints_t.to_int c'))) in   (* 2^c' *)
        (* TODO: commented out because fails test with _Bool *)
        (* if is_prime (m' +: Ints_t.one) then
             normalize ik @@ Some (x *: c, Ints_t.gcd (x *: m) ((c *: x) *: (m' +: Ints_t.one)))
           else *)
        normalize ik @@ Some (x *: c, Ints_t.gcd (x *: m) (c *: x))

  let shift_left ik x y =
    let res = shift_left ik x y in
    if M.tracing then  M.trace "congruence" "shift_left : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  (* Handle unsigned overflows.
     From n === k mod (2^a * b), we conclude n === k mod 2^a, for a <= bitwidth.
     The congruence modulo b may not persist on an overflow. *)
  let handle_overflow ik (c, m) =
    if m =: Ints_t.zero then
      normalize ik (Some (c, m))
    else
      (* Find largest m'=2^k (for some k) such that m is divisible by m' *)
      let tz = Ints_t.trailing_zeros m in
      let m' = Ints_t.shift_left (Ints_t.of_int 1) tz in

      let max = (snd (Size.range ik)) +: Ints_t.one in
      if m' >=: max then
        (* if m' >= 2 ^ {bitlength}, there is only one value in range *)
        let c' = c %: max in
        Some (c', Ints_t.zero)
      else
        normalize ik (Some (c, m'))

  let mul ?(no_ov=false) ik x y =
    let no_ov_case (c1, m1) (c2, m2) =
      (c1 *: c2, Ints_t.gcd (c1 *: m2) (Ints_t.gcd (m1 *: c2) (m1 *: m2)))
    in
    match x, y with
    | None, None -> bot ()
    | None, _ | _, None ->
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some (c2, m2) when no_ov ->
      Some (no_ov_case (c1, m1) (c2, m2))
    | Some (c1, m1), Some (c2, m2) when m1 =: Ints_t.zero && m2 =: Ints_t.zero && not (Cil.isSigned ik) ->
      let (_, max_ik) = range ik in
      Some((c1 *: c2) %: (max_ik +: Ints_t.one), Ints_t.zero)
    | Some a, Some b when not (Cil.isSigned ik) ->
      handle_overflow ik (no_ov_case a b )
    | _ -> top ()

  let mul ?no_ov ik x y =
    let res = mul ?no_ov ik x y in
    if M.tracing then  M.trace "congruence" "mul : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let neg ?(no_ov=false) ik x =
    match x with
    | None -> bot()
    | Some _ ->  mul ~no_ov ik (of_int ik (Ints_t.of_int (-1))) x

  let add ?(no_ov=false) ik x y =
    let no_ov_case (c1, m1) (c2, m2) =
      c1 +: c2, Ints_t.gcd m1 m2
    in
    match (x, y) with
    | None, None -> bot ()
    | None, _ | _, None ->
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some a, Some b when no_ov ->
      normalize ik (Some (no_ov_case a b))
    | Some (c1, m1), Some (c2, m2) when m1 =: Ints_t.zero && m2 =: Ints_t.zero && not (Cil.isSigned ik) ->
      let (_, max_ik) = range ik in
      Some((c1 +: c2) %: (max_ik +: Ints_t.one), Ints_t.zero)
    | Some a, Some b when not (Cil.isSigned ik) ->
      handle_overflow ik (no_ov_case a b)
    | _ -> top ()


  let add ?no_ov ik x y =
    let res = add ?no_ov ik x y in
    if M.tracing then
      M.trace "congruence" "add : %a %a -> %a \n" pretty x pretty y
        pretty res ;
    res

  let sub ?(no_ov=false) ik x y = add ~no_ov ik x (neg ~no_ov ik y)


  let sub ?no_ov ik x y =
    let res = sub ?no_ov ik x y in
    if M.tracing then
      M.trace "congruence" "sub : %a %a -> %a \n" pretty x pretty y
        pretty res ;
    res

  let bitnot ik x = match x with
    | None -> None
    | Some (c, m) ->
      if (Cil.isSigned ik) then
        sub ik (neg ik x) one
      else
        let (_, max_ik) = range ik in
        Some (Ints_t.sub max_ik c, m)

  (** The implementation of the bit operations could be improved based on the master’s thesis
      'Abstract Interpretation and Abstract Domains' written by Stefan Bygde.
      see: https://www.dsi.unive.it/~avp/domains.pdf *)
  let bit2 f ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') ->
      if (m =: Ints_t.zero && m' =: Ints_t.zero) then Some (f c c', Ints_t.zero)
      else top ()

  let bitor ik x y = bit2 Ints_t.bitor ik x y

  let bitand ik x y = bit2 Ints_t.bitand ik x y

  let bitxor ik x y = bit2 Ints_t.bitxor ik x y

  let rem ik x y =
    match x, y with
    | None, None -> bot()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some(c2, m2) ->
      if m2 =: Ints_t.zero then
        if (c2 |: m1) then
          Some(c1 %: c2,Ints_t.zero)
        else
          normalize ik (Some(c1, (Ints_t.gcd m1 c2)))
      else
        normalize ik (Some(c1, Ints_t.gcd m1 (Ints_t.gcd c2 m2)))

  let rem ik x y = let res = rem ik x y in
    if M.tracing then  M.trace "congruence" "rem : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let div ?(no_ov=false) ik x y =
    match x,y with
    | None, None -> bot ()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, x when leq zero x -> top ()
    | Some(c1, m1), Some(c2, m2) when not no_ov && m2 =: Ints_t.zero && c2 =: Ints_t.neg Ints_t.one -> top ()
    | Some(c1, m1), Some(c2, m2) when m1 =: Ints_t.zero && m2 =: Ints_t.zero -> Some(c1 /: c2, Ints_t.zero)
    | Some(c1, m1), Some(c2, m2) when m2 =: Ints_t.zero ->  if (c2 |: m1) && (c2 |: c1) then Some(c1 /: c2, m1 /: c2) else top ()
    | _, _ -> top ()


  let div ?no_ov ik x y =
    let res = div ?no_ov ik x y in
    if M.tracing then
      M.trace "congruence" "div : %a %a -> %a \n" pretty x pretty y pretty
        res ;
    res

  let ne ik (x: t) (y: t) = match x, y with
    | Some (c1, m1), Some (c2, m2) when (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) -> of_bool ik (not (c1 =: c2 ))
    | x, y -> if meet ik x y = None then of_bool ik true else top_bool

  let eq ik (x: t) (y: t) = match x, y with
    | Some (c1, m1), Some (c2, m2) when (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) -> of_bool ik (c1 =: c2)
    | x, y -> if meet ik x y <> None then top_bool else of_bool ik false

  let comparison ik op x y = match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some(c2, m2) -> if (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) then
        if op c1 c2 then of_bool ik true else of_bool ik false
      else top_bool

  let ge ik x y = comparison ik (>=:) x y

  let ge ik x y =
    let res = ge ik x y in
    if M.tracing then  M.trace "congruence" "greater or equal : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let le ik x y = comparison ik (<=:) x y

  let le ik x y =
    let res = le ik x y in
    if M.tracing then  M.trace "congruence" "less or equal : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let gt ik x y = comparison ik (>:) x y


  let gt ik x y =
    let res = gt ik x y in
    if M.tracing then  M.trace "congruence" "greater than : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let lt ik x y = comparison ik (<:) x y

  let lt ik x y =
    let res = lt ik x y in
    if M.tracing then  M.trace "congruence" "less than : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let invariant_ikind e ik x =
    match x with
    | Some (c, m) when m =: Ints_t.zero ->
      if get_bool "witness.invariant.exact" then
        let c = Ints_t.to_bigint c in
        Invariant.of_exp Cil.(BinOp (Eq, e, Cil.kintegerCilint ik c, intType))
      else
        Invariant.top ()
    | Some (c, m) ->
      let open Cil in
      let (c, m) = BatTuple.Tuple2.mapn (fun a -> kintegerCilint ik @@ Ints_t.to_bigint a) (c, m) in
      Invariant.of_exp (BinOp (Eq, (BinOp (Mod, e, m, TInt(ik,[]))), c, intType))
    | None -> Invariant.none

  let arbitrary ik =
    let open QCheck in
    let int_arb = map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 in
    let cong_arb = pair int_arb int_arb in
    let of_pair ik p = normalize ik (Some p) in
    let to_pair = Option.get in
    set_print show (map ~rev:to_pair (of_pair ik) cong_arb)

  let refine_with_interval ik (cong : t) (intv : (int_t * int_t ) option) : t =
    match intv, cong with
    | Some (x, y), Some (c, m) ->
      if m =: Ints_t.zero then
        if (c <: x || c >: y) then None else Some (c, Ints_t.zero)
      else
        let rcx = x +: ((c -: x) %: Ints_t.abs m) in
        let lcy = y -: ((y -: c) %: Ints_t.abs m) in
        if rcx >: lcy then None
        else if rcx =: lcy then Some (rcx, Ints_t.zero)
        else cong
    | _ -> None

  let refine_with_interval ik (cong : t) (intv : (int_t * int_t) option) : t =
    let pretty_intv _ i = (match i with
        | Some(l, u) -> let s = "["^Ints_t.to_string l^","^Ints_t.to_string u^"]" in Pretty.text s
        | _ -> Pretty.text ("Display Error")) in
    let refn = refine_with_interval ik cong intv in
    if M.tracing then M.trace "refine" "cong_refine_with_interval %a %a -> %a\n" pretty cong pretty_intv intv pretty refn;
    refn

  let refine_with_congruence ik a b = meet ik a b
  let refine_with_excl_list ik a b = a
  let refine_with_incl_list ik a b = a

  let project ik p t = t
end

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

  let of_interval ?suppress_ovwarn ik x = lift @@ D.of_interval ?suppress_ovwarn ik x

  let starting ?suppress_ovwarn ik x = lift @@ D.starting ?suppress_ovwarn ik x

  let ending ?suppress_ovwarn ik x = lift @@ D.ending ?suppress_ovwarn ik x

  let shift_left ik x y = lift @@ D.shift_left ik x y

  let shift_right ik x y = lift @@ D.shift_right ik x y

end



(* The old IntDomList had too much boilerplate since we had to edit every function in S when adding a new domain. With the following, we only have to edit the places where fn are applied, i.e., create, mapp, map, map2. You can search for I3 below to see where you need to extend. *)
(* discussion: https://github.com/goblint/analyzer/pull/188#issuecomment-818928540 *)
module IntDomTupleImpl = struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Batteries
  type int_t = BI.t
  module I1 = SOverflowLifter(DefExc)
  module I2 = Interval
  module I3 = SOverflowLifter(Enums)
  module I4 = SOverflowLifter(Congruence)
  module I5 = IntervalSetFunctor (BI)

  type t = I1.t option * I2.t option * I3.t option * I4.t option * I5.t option
  [@@deriving to_yojson, eq, ord]

  let name () = "intdomtuple"

  (* The Interval domain can lead to too many contexts for recursive functions (top is [min,max]), but we don't want to drop all ints as with `ana.base.context.int`. TODO better solution? *)
  let no_interval = Tuple5.map2 (const None)
  let no_intervalSet = Tuple5.map5 (const None)

  type 'a m = (module SOverflow with type t = 'a)
  type 'a m2 = (module SOverflow with type t = 'a and type int_t = int_t )

  (* only first-order polymorphism on functions -> use records to get around monomorphism restriction on arguments *)
  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } (* inject *)
  type 'b poly2_in  = { fi2  : 'a. 'a m2 -> 'b -> 'a } (* inject for functions that depend on int_t *)
  type 'b poly2_in_ovc  = { fi2_ovc  : 'a. 'a m2 -> 'b -> 'a * overflow_info} (* inject for functions that depend on int_t *)

  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } (* project *)
  type 'b poly_pr2  = { fp2  : 'a. 'a m2 -> 'a -> 'b } (* project for functions that depend on int_t *)
  type 'b poly2_pr = {f2p: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'b}
  type poly1 = {f1: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a} (* needed b/c above 'b must be different from 'a *)
  type poly1_ovc = {f1_ovc: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a * overflow_info } (* needed b/c above 'b must be different from 'a *)
  type poly2 = {f2: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'a}
  type poly2_ovc = {f2_ovc: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'a * overflow_info }
  type 'b poly3 = { f3: 'a. 'a m -> 'a option } (* used for projection to given precision *)
  let create r x ((p1, p2, p3, p4, p5): int_precision) =
    let f b g = if b then Some (g x) else None in
    f p1 @@ r.fi (module I1), f p2 @@ r.fi (module I2), f p3 @@ r.fi (module I3), f p4 @@ r.fi (module I4), f p5 @@ r.fi (module I5)
  let create r x = (* use where values are introduced *)
    create r x (int_precision_from_node_or_config ())
  let create2 r x ((p1, p2, p3, p4, p5): int_precision) =
    let f b g = if b then Some (g x) else None in
    f p1 @@ r.fi2 (module I1), f p2 @@ r.fi2 (module I2), f p3 @@ r.fi2 (module I3), f p4 @@ r.fi2 (module I4), f p5 @@ r.fi2 (module I5)
  let create2 r x = (* use where values are introduced *)
    create2 r x (int_precision_from_node_or_config ())

  let no_overflow ik = function
    | Some(_, {underflow; overflow}) -> not (underflow || overflow)
    | _ -> false

  let check_ov ik intv intv_set =
    let no_ov = (no_overflow ik intv) || (no_overflow ik intv_set) in
    if not no_ov && ( BatOption.is_some intv || BatOption.is_some intv_set) then (
      let (_,{underflow=underflow_intv; overflow=overflow_intv}) = match intv with None -> (I2.bot (), {underflow= true; overflow = true}) | Some x -> x in
      let (_,{underflow=underflow_intv_set; overflow=overflow_intv_set}) = match intv_set with None -> (I5.bot (), {underflow= true; overflow = true}) | Some x -> x in
      let underflow = underflow_intv && underflow_intv_set in
      let overflow = overflow_intv && overflow_intv_set in
      set_overflow_flag ~cast:false ~underflow ~overflow ik;
    );
    no_ov

  let create2_ovc ik r x ((p1, p2, p3, p4, p5): int_precision) =
    let f b g = if b then Some (g x) else None in
    let map x = Option.map fst x in
    let intv =  f p2 @@ r.fi2_ovc (module I2) in
    let intv_set = f p5 @@ r.fi2_ovc (module I5) in
    ignore (check_ov ik intv intv_set);
    map @@ f p1 @@ r.fi2_ovc (module I1), map @@ f p2 @@ r.fi2_ovc (module I2), map @@ f p3 @@ r.fi2_ovc (module I3), map @@ f p4 @@ r.fi2_ovc (module I4), map @@ f p5 @@ r.fi2_ovc (module I5)

  let create2_ovc ik r x = (* use where values are introduced *)
    create2_ovc ik r x (int_precision_from_node_or_config ())


  let opt_map2 f ?no_ov =
    curry @@ function Some x, Some y -> Some (f ?no_ov x y) | _ -> None

  let to_list x = Tuple5.enum x |> List.of_enum |> List.filter_map identity (* contains only the values of activated domains *)
  let to_list_some x = List.filter_map identity @@ to_list x (* contains only the Some-values of activated domains *)

  let exists = function
    | (Some true, _, _, _, _)
    | (_, Some true, _, _, _)
    | (_, _, Some true, _, _)
    | (_, _, _, Some true, _)
    | (_, _, _, _, Some true) ->
      true
    | _ ->
      false

  let for_all = function
    | (Some false, _, _, _, _)
    | (_, Some false, _, _, _)
    | (_, _, Some false, _, _)
    | (_, _, _, Some false, _)
    | (_, _, _, _, Some false) ->
      false
    | _ ->
      true

  (* f0: constructors *)
  let top () = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.top } ()
  let bot () = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.bot } ()
  let top_of = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.top_of }
  let bot_of = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.bot_of }
  let of_bool ik = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.of_bool ik }
  let of_excl_list ik = create2 { fi2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_excl_list ik}
  let of_int ik = create2_ovc ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_int ik }
  let starting ?(suppress_ovwarn=false) ik = create2_ovc ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.starting ~suppress_ovwarn ik }
  let ending ?(suppress_ovwarn=false) ik = create2_ovc ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.ending ~suppress_ovwarn ik }
  let of_interval ?(suppress_ovwarn=false) ik = create2_ovc ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_interval ~suppress_ovwarn ik }
  let of_congruence ik = create2 { fi2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_congruence ik }

  let refine_with_congruence ik ((a, b, c, d, e) : t) (cong : (int_t * int_t) option) : t=
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_congruence ik a cong
    , opt I2.refine_with_congruence ik b cong
    , opt I3.refine_with_congruence ik c cong
    , opt I4.refine_with_congruence ik d cong
    , opt I5.refine_with_congruence ik e cong)

  let refine_with_interval ik (a, b, c, d, e) intv =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_interval ik a intv
    , opt I2.refine_with_interval ik b intv
    , opt I3.refine_with_interval ik c intv
    , opt I4.refine_with_interval ik d intv
    , opt I5.refine_with_interval ik e intv )

  let refine_with_excl_list ik (a, b, c, d, e) excl =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_excl_list ik a excl
    , opt I2.refine_with_excl_list ik b excl
    , opt I3.refine_with_excl_list ik c excl
    , opt I4.refine_with_excl_list ik d excl
    , opt I5.refine_with_excl_list ik e excl )

  let refine_with_incl_list ik (a, b, c, d, e) incl =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_incl_list ik a incl
    , opt I2.refine_with_incl_list ik b incl
    , opt I3.refine_with_incl_list ik c incl
    , opt I4.refine_with_incl_list ik d incl
    , opt I5.refine_with_incl_list ik e incl )


  let mapp r (a, b, c, d, e) =
    let map = BatOption.map in
    ( map (r.fp (module I1)) a
    , map (r.fp (module I2)) b
    , map (r.fp (module I3)) c
    , map (r.fp (module I4)) d
    , map (r.fp (module I5)) e)


  let mapp2 r (a, b, c, d, e) =
    BatOption.
      ( map (r.fp2 (module I1)) a
      , map (r.fp2 (module I2)) b
      , map (r.fp2 (module I3)) c
      , map (r.fp2 (module I4)) d
      , map (r.fp2 (module I5)) e)


  (* exists/for_all *)
  let is_bot = exists % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_bot }
  let is_top = for_all % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_top }
  let is_top_of ik = for_all % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_top_of ik }
  let is_excl_list = exists % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_excl_list }

  let map2p r (xa, xb, xc, xd, xe) (ya, yb, yc, yd, ye) =
    ( opt_map2 (r.f2p (module I1)) xa ya
    , opt_map2 (r.f2p (module I2)) xb yb
    , opt_map2 (r.f2p (module I3)) xc yc
    , opt_map2 (r.f2p (module I4)) xd yd
    , opt_map2 (r.f2p (module I5)) xe ye)

  (* f2p: binary projections *)
  let (%%) f g x = f % (g x) (* composition for binary function g *)

  let leq =
    for_all
    %% map2p {f2p= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.leq)}

  let flat f x = match to_list_some x with [] -> None | xs -> Some (f xs)

  let to_excl_list x =
    let merge ps =
      let (vs, rs) = List.split ps in
      let (mins, maxs) = List.split rs in
      (List.concat vs, (List.min mins, List.max maxs))
    in
    mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.to_excl_list } x |> flat merge

  let to_incl_list x =
    let hd l = match l with h::t -> h | _ -> [] in
    let tl l = match l with h::t -> t | _ -> [] in
    let a y = BatSet.of_list (hd y) in
    let b y = BatList.map BatSet.of_list (tl y) in
    let merge y = BatSet.elements @@ BatList.fold BatSet.intersect (a y) (b y)
    in
    mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.to_incl_list } x |> flat merge


  let pretty () = (fun xs -> text "(" ++ (try List.reduce (fun a b -> a ++ text "," ++ b) xs with Invalid_argument _ -> nil) ++ text ")") % to_list % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> (* assert sf==I.short; *) I.pretty () } (* NOTE: the version above does something else. also, we ignore the sf-argument here. *)


  let refine_functions ik : (t -> t) list =
    let maybe reffun ik domtup dom =
      match dom with Some y -> reffun ik domtup y | _ -> domtup
    in
    [(fun (a, b, c, d, e) -> refine_with_excl_list ik (a, b, c, d, e) (to_excl_list (a, b, c, d, e)));
     (fun (a, b, c, d, e) -> refine_with_incl_list ik (a, b, c, d, e) (to_incl_list (a, b, c, d, e)));
     (fun (a, b, c, d, e) -> maybe refine_with_interval ik (a, b, c, d, e) b);
     (fun (a, b, c, d, e) -> maybe refine_with_congruence ik (a, b, c, d, e) d)]

  let refine ik ((a, b, c, d, e) : t ) : t =
    let dt = ref (a, b, c, d, e) in
    (match get_refinement () with
     | "never" -> ()
     | "once" ->
       List.iter (fun f -> dt := f !dt) (refine_functions ik);
     | "fixpoint" ->
       let quit_loop = ref false in
       while not !quit_loop do
         let old_dt = !dt in
         List.iter (fun f -> dt := f !dt) (refine_functions ik);
         quit_loop := equal old_dt !dt;
         if is_bot !dt then dt := bot_of ik; quit_loop := true;
         if M.tracing then M.trace "cong-refine-loop" "old: %a, new: %a\n" pretty old_dt pretty !dt;
       done;
     | _ -> ()
    ); !dt


  (* map with overflow check *)
  let mapovc ?(cast=false) ik r (a, b, c, d, e) =
    let map f ?no_ov = function Some x -> Some (f ?no_ov x) | _ -> None  in
    let intv = map (r.f1_ovc (module I2)) b in
    let intv_set = map (r.f1_ovc (module I5)) e in
    let no_ov = check_ov ik intv intv_set in
    let no_ov = no_ov || should_ignore_overflow ik in
    refine ik
      ( map (fun ?no_ov x -> r.f1_ovc ?no_ov (module I1) x |> fst) a
      , BatOption.map fst intv
      , map (fun ?no_ov x -> r.f1_ovc ?no_ov (module I3) x |> fst) c
      , map (fun ?no_ov x -> r.f1_ovc ?no_ov (module I4) x |> fst) ~no_ov d
      , BatOption.map fst intv_set )

  (* map2 with overflow check *)
  let map2ovc ik r (xa, xb, xc, xd, xe) (ya, yb, yc, yd, ye) =
    let intv = opt_map2 (r.f2_ovc (module I2)) xb yb in
    let intv_set = opt_map2 (r.f2_ovc (module I5)) xe ye in
    let no_ov = check_ov ik intv intv_set in
    let no_ov = no_ov || should_ignore_overflow ik in
    refine ik
      ( opt_map2 (fun ?no_ov x y -> r.f2_ovc ?no_ov (module I1) x y |> fst) xa ya
      , BatOption.map fst intv
      , opt_map2 (fun ?no_ov x y -> r.f2_ovc ?no_ov (module I3) x y |> fst) xc yc
      , opt_map2 (fun ?no_ov x y -> r.f2_ovc ?no_ov (module I4) x y |> fst) ~no_ov:no_ov xd yd
      , BatOption.map fst intv_set )

  let map ik r (a, b, c, d, e) =
    refine ik
      BatOption.
        ( map (r.f1 (module I1)) a
        , map (r.f1 (module I2)) b
        , map (r.f1 (module I3)) c
        , map (r.f1 (module I4)) d
        , map (r.f1 (module I5)) e)

  let map2 ?(norefine=false) ik r (xa, xb, xc, xd, xe) (ya, yb, yc, yd, ye) =
    let r =
      ( opt_map2 (r.f2 (module I1)) xa ya
      , opt_map2 (r.f2 (module I2)) xb yb
      , opt_map2 (r.f2 (module I3)) xc yc
      , opt_map2 (r.f2 (module I4)) xd yd
      , opt_map2 (r.f2 (module I5)) xe ye)
    in
    if norefine then r else refine ik r


  (* f1: unary ops *)
  let neg ?no_ov ik =
    mapovc ik {f1_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.neg ?no_ov ik)}

  let bitnot ik =
    map ik {f1 = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.bitnot ik)}

  let lognot ik =
    map ik {f1 = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.lognot ik)}

  let cast_to ?torg ?no_ov t =
    mapovc ~cast:true t {f1_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.cast_to ?torg ?no_ov t)}

  (* fp: projections *)
  let equal_to i x =
    let xs = mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.equal_to i } x |> Tuple5.enum |> List.of_enum |> List.filter_map identity in
    if List.mem `Eq xs then `Eq else
    if List.mem `Neq xs then `Neq else
      `Top
  let same show x = let xs = to_list_some x in let us = List.unique xs in let n = List.length us in
    if n = 1 then Some (List.hd xs)
    else (
      if n>1 then Messages.info ~category:Unsound "Inconsistent state! %a" (Pretty.docList ~sep:(Pretty.text ",") (Pretty.text % show)) us; (* do not want to abort *)
      None
    )
  let to_int = same BI.to_string % mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.to_int }
  let to_bool = same string_of_bool % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.to_bool }
  let minimal = flat (List.max ~cmp:BI.compare) % mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.minimal }
  let maximal = flat (List.min ~cmp:BI.compare) % mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.maximal }
  (* others *)
  let show = String.concat "; " % to_list % mapp { fp = fun (type a) (module I:SOverflow with type t = a) x -> I.name () ^ ":" ^ (I.show x) }
  let to_yojson = [%to_yojson: Yojson.Safe.t list] % to_list % mapp { fp = fun (type a) (module I:SOverflow with type t = a) x -> I.to_yojson x }
  let hash = List.fold_left (lxor) 0 % to_list % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.hash }

  (* `map/opt_map` are used by `project` *)
  let opt_map b f =
    curry @@ function None, true -> f | x, y when y || b -> x | _ -> None
  let map ~keep r (i1, i2, i3, i4, i5) (b1, b2, b3, b4, b5) =
    ( opt_map keep (r.f3 (module I1)) i1 b1
    , opt_map keep (r.f3 (module I2)) i2 b2
    , opt_map keep (r.f3 (module I3)) i3 b3
    , opt_map keep (r.f3 (module I4)) i4 b4
    , opt_map keep (r.f3 (module I5)) i5 b5 )

  (** Project tuple t to precision p
   * We have to deactivate IntDomains after the refinement, since we might
   * lose information if we do it before. E.g. only "Interval" is active
   * and shall be projected to only "Def_Exc". By seting "Interval" to None
   * before refinement we have no information for "Def_Exc".
   *
   * Thus we have 3 Steps:
   * 1. Add padding to t by setting `None` to `I.top_of ik` if p is true for this element
   * 2. Refine the padded t
   * 3. Set elements of t to `None` if p is false for this element
   *
   * Side Note:
   * ~keep is used to reuse `map/opt_map` for Step 1 and 3.
   * ~keep:true will keep elements that are `Some x` but should be set to `None` by p.
   *  This way we won't loose any information for the refinement.
   * ~keep:false will set the elements to `None` as defined by p *)
  let project ik (p: int_precision) t =
    let t_padded = map ~keep:true { f3 = fun (type a) (module I:SOverflow with type t = a) -> Some (I.top_of ik) } t p in
    let t_refined = refine ik t_padded in
    map ~keep:false { f3 = fun (type a) (module I:SOverflow with type t = a) -> None } t_refined p


  (* f2: binary ops *)
  let join ik =
    map2 ~norefine:true ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.join ik)}

  let meet ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.meet ik)}

  let widen ik =
    map2 ~norefine:true ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.widen ik)}

  let narrow ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.narrow ik)}

  let add ?no_ov ik =
    map2ovc ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.add ?no_ov ik)}

  let sub ?no_ov ik =
    map2ovc ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.sub ?no_ov ik)}

  let mul ?no_ov ik =
    map2ovc ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.mul ?no_ov ik)}

  let div ?no_ov ik =
    map2ovc ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.div ?no_ov ik)}

  let rem ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.rem ik)}

  let lt ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.lt ik)}

  let gt ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.gt ik)}

  let le ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.le ik)}

  let ge ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.ge ik)}

  let eq ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.eq ik)}

  let ne ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.ne ik)}

  let bitand ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.bitand ik)}

  let bitor ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.bitor ik)}

  let bitxor ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.bitxor ik)}

  let shift_left ik =
    map2ovc ik {f2_ovc= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.shift_left ik)}

  let shift_right ik =
    map2ovc ik {f2_ovc= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.shift_right ik)}

  let logand ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.logand ik)}

  let logor ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.logor ik)}


  (* printing boilerplate *)
  let pretty_diff () (x,y) = dprintf "%a instead of %a" pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  let invariant_ikind e ik x =
    match to_int x with
    | Some v ->
      if get_bool "witness.invariant.exact" then
        (* If definite, output single equality instead of every subdomain repeating same equality *)
        Invariant.of_exp Cil.(BinOp (Eq, e, kintegerCilint ik v, intType))
      else
        Invariant.top ()
    | None ->
      let is = to_list (mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.invariant_ikind e ik } x)
      in List.fold_left (fun a i ->
          Invariant.(a && i)
        ) (Invariant.top ()) is

  let arbitrary ik = QCheck.(set_print show @@ tup5 (option (I1.arbitrary ik)) (option (I2.arbitrary ik)) (option (I3.arbitrary ik)) (option (I4.arbitrary ik)) (option (I5.arbitrary ik)))

  let relift (a, b, c, d, e) =
    (Option.map I1.relift a, Option.map I2.relift b, Option.map I3.relift c, Option.map I4.relift d, Option.map I5.relift e)
end

module IntDomTuple =
struct
  module I = IntDomLifter (IntDomTupleImpl)
  include I

  let top () = failwith "top in IntDomTuple not supported. Use top_of instead."
  let no_interval (x: I.t) = {x with v = IntDomTupleImpl.no_interval x.v}

  let no_intervalSet (x: I.t) = {x with v = IntDomTupleImpl.no_intervalSet x.v}
end

let of_const (i, ik, str) = IntDomTuple.of_int ik i
