open GobConfig
open Pretty

module GU = Goblintutil
module JB = Json
module M = Messages
module BI = IntOps.BigIntOps

let (%) = Batteries.(%)
let (|?) = Batteries.(|?)

exception IncompatibleIKinds of string
exception Unknown
exception Error
exception ArithmeticOnIntegerBot of string

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
  val is_int: t -> bool
  val equal_to: int_t -> t -> [`Eq | `Neq | `Top]

  val to_bool: t -> bool option
  val is_bool: t -> bool
  val to_excl_list: t -> int_t list option
  val of_excl_list: Cil.ikind -> int_t list -> t
  val is_excl_list: t -> bool

  val maximal    : t -> int_t option
  val minimal    : t -> int_t option

  val cast_to: ?torg:Cil.typ -> Cil.ikind -> t -> t
end


module type IkindUnawareS =
sig
  include B with type int_t = int64
  include Arith with type t:= t
  val starting   : Cil.ikind -> int_t -> t
  val ending     : Cil.ikind -> int_t -> t
  val of_int: int_t -> t
  val of_bool: bool -> t
  val of_interval: Cil.ikind -> int_t * int_t -> t
end
(** Interface of IntDomain implementations that do not take ikinds for arithmetic operations yet.
   TODO: Should be ported to S in the future. *)

module type S =
sig
  include B
  include ArithIkind with type t:= t

  val add : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val sub : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val mul : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val div : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val cast_to : ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t

  val join: Cil.ikind -> t -> t -> t
  val meet: Cil.ikind -> t -> t -> t
  val narrow: Cil.ikind -> t -> t -> t
  val widen: Cil.ikind -> t -> t -> t
  val starting : Cil.ikind -> int_t -> t
  val ending : Cil.ikind -> int_t -> t
  val of_int: Cil.ikind -> int_t -> t
  val of_bool: Cil.ikind -> bool -> t
  val of_interval: Cil.ikind -> int_t * int_t -> t
  val is_top_of: Cil.ikind -> t -> bool
  val invariant_ikind : Invariant.context -> Cil.ikind -> t -> Invariant.t

  val refine_with_congruence: t -> (int_t * int_t) option -> t
  val refine_with_interval: t -> (int_t * int_t) option -> t
  val refine_with_excl_list: t -> int_t list option -> t
  val refine_with_incl_list: t -> int_t list option -> t
end
(** Interface of IntDomain implementations taking an ikind for arithmetic operations *)

module type Y =
sig
  (* include B *)
  include B
  include Arith with type t:= t
  val of_int: Cil.ikind -> int_t -> t
  val of_bool: Cil.ikind -> bool -> t
  val of_interval: Cil.ikind -> int_t * int_t -> t

  val starting   : Cil.ikind -> int_t -> t
  val ending     : Cil.ikind -> int_t -> t
  val is_top_of: Cil.ikind -> t -> bool
end

module type Z = Y with type int_t = BI.t

module OldDomainFacade (Old : IkindUnawareS) : S with type int_t = BI.t and type t = Old.t =
struct
  include Old
  type int_t = BI.t
  let neg _ik = Old.neg
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
    with e -> `Top

  let to_excl_list a = Option.map (List.map BI.of_int64) (Old.to_excl_list a)
  let of_excl_list ik xs =
    let xs' = List.map BI.to_int64 xs in
    Old.of_excl_list ik xs'

  let maximal a = Option.map BI.of_int64 (Old.maximal a)
  let minimal a = Option.map BI.of_int64 (Old.minimal a)

  let of_int ik x =
    (* If we cannot convert x to int64, we have to represent it with top in the underlying domain*)
    try
      Old.of_int (BI.to_int64 x)
    with
      Failure _ -> top_of ik

  let of_bool ik b = Old.of_bool b
  let of_interval ik (l, u) =
    try
      Old.of_interval ik (BI.to_int64 l, BI.to_int64 u)
    with
      Failure _ -> top_of ik

  let starting ik x =
    try Old.starting ik (BI.to_int64 x) with Failure _ -> top_of ik
  let ending ik x =
    try Old.ending ik (BI.to_int64 x) with Failure _ -> top_of ik

  let join _ik = Old.join
  let meet _ik = Old.meet
  let narrow _ik = Old.narrow
  let widen _ik = Old.widen

  let is_top_of _ik = Old.is_top

  let invariant_ikind c ik t = Old.invariant c t

  let cast_to ?torg ?no_ov = Old.cast_to ?torg

  let refine_with_congruence a b = a
  let refine_with_interval a b = a
  let refine_with_excl_list a b = a
  let refine_with_incl_list a b = a
end


module IntDomLifter (I : S) =
struct
  open Cil
  type int_t = I.int_t
  type t = { v : I.t; ikind : ikind }

  (* Helper functions *)
  let check_ikinds x y = if x.ikind <> y.ikind then raise (IncompatibleIKinds ("ikinds " ^ Prelude.Ana.sprint Cil.d_ikind x.ikind ^ " and " ^ Prelude.Ana.sprint Cil.d_ikind y.ikind ^ " are incompatible. Values: " ^ Prelude.Ana.sprint I.pretty x.v ^ " and " ^ Prelude.Ana.sprint I.pretty y.v)) else ()
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
  let equal x y = if x.ikind <> y.ikind then false else I.equal x.v y.v

  let hash x =
    let ikind_to_int (ikind: ikind) = match ikind with
    | IChar 	-> 0
    | ISChar 	-> 1
    | IUChar 	-> 2
    | IBool 	-> 3
    | IInt 	  -> 4
    | IUInt 	-> 5
    | IShort 	-> 6
    | IUShort -> 7
    | ILong 	-> 8
    | IULong 	-> 9
    | ILongLong -> 10
    | IULongLong -> 11
    in
    3 * (I.hash x.v) + 5 * (ikind_to_int x.ikind)
  let compare x y = let ik_c = compare x.ikind y.ikind in
    if ik_c <> 0
      then ik_c
      else I.compare x.v y.v
  let show x = I.show x.v  (* TODO add ikind to output *)
  let pretty () x = I.pretty () x.v (* TODO add ikind to output *)
  let pretty_diff () (x, y) = I.pretty_diff () (x.v, y.v) (* TODO check ikinds, add them to output *)
  let printXml o x = I.printXml o x.v (* TODO add ikind to output *)
  (* This is for debugging *)
  let name () = "IntDomLifter(" ^ (I.name ()) ^ ")"
  let to_yojson x = I.to_yojson x.v
  let invariant c x = I.invariant_ikind c x.ikind x.v
  let tag x = I.tag x.v
  let arbitrary () = failwith @@ "Arbitrary not implement for " ^ (name ()) ^ "."
  let to_int x = I.to_int x.v
  let of_int ikind x = { v = I.of_int ikind x; ikind}
  let is_int x = I.is_int x.v
  let equal_to i x = I.equal_to i x.v
  let to_bool x = I.to_bool x.v
  let of_bool ikind b = { v = I.of_bool ikind b; ikind}
  let is_bool x = I.is_bool x.v
  let to_excl_list x = I.to_excl_list x.v
  let of_excl_list ikind is = {v = I.of_excl_list ikind is; ikind}
  let is_excl_list x = I.is_excl_list x.v
  let of_interval ikind (lb,ub) = {v = I.of_interval ikind (lb,ub); ikind}
  let starting ikind i = {v = I.starting ikind i; ikind}
  let ending ikind i = {v = I.ending ikind i; ikind}
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

  let cast_to ?torg ikind x = {v = I.cast_to ?torg ikind x.v; ikind}

  let is_top_of ik x = ik = x.ikind && I.is_top_of ik x.v

  let relift x = { v = I.relift x.v; ikind = x.ikind }
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
  exception Not_in_int64
  open Cil open Int64 open Big_int_Z
  let sign x = if x<0L then `Signed else `Unsigned
  let sign_big_int x = if BI.compare x BI.zero < 0 then `Signed else `Unsigned

  let max = function
    | `Signed -> ILongLong
    | `Unsigned -> IULongLong
  let top_typ = TInt (ILongLong, [])
  let min_for x = intKindForValue (fst (truncateCilint (max (sign_big_int x)) (Big x))) (sign_big_int x = `Unsigned)
  let bit = function (* bits needed for representation *)
    | IBool -> 1
    | ik -> bytesSizeOfInt ik * 8
  let is_int64_big_int x = try let _ = int64_of_big_int x in true with _ -> false
  let card ik = (* cardinality *)
    let b = bit ik in
    shift_left_big_int unit_big_int b
  let bits ik = (* highest bits for neg/pos values *)
    let s = bit ik in
    if isSigned ik then s-1, s-1 else 0, s
  let bits_i64 ik = BatTuple.Tuple2.mapn of_int (bits ik)
  let range ik = (* min/max values as int64 (signed), anything bigger is cropped! *)
    let a,b = bits ik in
    if a>63 || b>63 then raise Not_in_int64 else
      let x = if isSigned ik then neg (shift_left 1L a) (* -2^a *) else 0L in
      let y = sub (shift_left 1L b) 1L in (* 2^b - 1 *)
      x,y
  let range_big_int ik =
    let a,b = bits ik in
    let x = if isSigned ik then minus_big_int (shift_left_big_int unit_big_int a) (* -2^a *) else zero_big_int in
    let y = sub_big_int (shift_left_big_int unit_big_int b) unit_big_int in (* 2^b - 1 *)
    x,y
  let cast_big_int t x = (* TODO: overflow is implementation-dependent! *)
    let a,b = range_big_int t in
    let c = card t in
    (* let z = add (rem (sub x a) c) a in (* might lead to overflows itself... *)*)
    let y = mod_big_int x c in
    let y = if gt_big_int y b then sub_big_int y c
      else if lt_big_int y a then add_big_int y c
      else y
    in
    M.tracel "cast_int" "Cast %s to range [%s, %s] (%s) = %s (%s in int64)\n" (string_of_big_int x) (string_of_big_int a) (string_of_big_int b) (string_of_big_int c) (string_of_big_int y) (if is_int64_big_int y then "fits" else "does not fit");
    y
  let cast t x =
    let x' = big_int_of_int64 x in
    try int64_of_big_int (cast_big_int t x') with _ -> raise Not_in_int64

  let min_range_sign_agnostic x =
    let size ik =
      let a,b = bits_i64 ik in
      Int64.neg a,b
    in
    if sign_big_int x = `Signed then
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
  let max_from_bit_range pos_bits =
    match pos_bits with
    | Some i when i < 64L -> Some(BI.(pred @@ shift_left BI.one (to_int (BI.of_int64 i)))) (* things that are bigger than (2^63)-1 can not be represented as int64 *)
    | _ -> None

  (* From the number of bits used to represent a non-positive value, determines the minimal representable value *)
  let min_from_bit_range neg_bits =
    match neg_bits with
    | Some i when i > -64L -> Some(BI.(if i = 0L then BI.zero else neg @@ shift_left BI.one (to_int (neg (BI.of_int64 i))))) (* things that are smaller than (-2^63)can not be represented as int64 *)
    | _ -> None

end


module StdTop (B: sig type t val top_of: Cil.ikind -> t end) = struct
  open B
  (* these should be overwritten for better precision if possible: *)
  let to_excl_list    x = None
  let of_excl_list ik x = top_of ik
  let is_excl_list    x = false
  let of_interval  ik x = top_of ik
  let starting     ik x = top_of ik
  let ending       ik x = top_of ik
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
  include Printable.StdPolyCompare
  let name = B.name (* overwrite the one from Printable.Std *)
  open B
  let hash = Hashtbl.hash
  let is_top x = failwith "is_top not implemented for IntDomain.Std"
  let is_bot x = B.equal x (bot_of Cil.IInt) (* Here we assume that the representation of bottom is independent of the ikind
                                                This may be true for intdomain implementations, but not e.g. for IntDomLifter. *)
  let is_top_of ik x = B.equal x (top_of ik)

  (* all output is based on B.show *)
  let pretty () x = text (show x)
  let pretty_diff () (x,y) = dprintf "%s: %a instead of %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  include StdTop (B)
end

module IntervalFunctor(Ints_t : IntOps.IntOps): S with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option =
struct
  let name () = "intervals"
  type int_t = Ints_t.t
  type t = (Ints_t.t * Ints_t.t) option [@@deriving eq]
  let to_yojson t = failwith "to yojson unimplemented"

  let min_int ik = Ints_t.of_bigint @@ fst @@ Size.range_big_int ik
  let max_int ik = Ints_t.of_bigint @@ snd @@ Size.range_big_int ik
  let top () = failwith @@ "top () not implemented for " ^ (name ())
  let top_of ik = Some (min_int ik, max_int ik)
  let bot () = None
  let bot_of ik = bot () (* TODO: improve *)

  let is_top x = failwith "is_top not implemented for intervals"

  let is_bot x  = failwith "is_bot not implemented for intervals"

  let show = function None -> "bottom" | Some (x,y) -> "["^Ints_t.to_string x^","^Ints_t.to_string y^"]"

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) ->
      if a = b && b = i then `Eq else if Ints_t.compare a i <= 0 && Ints_t.compare i b <=0 then `Top else `Neq

  let set_overflow_flag ik =
    if Cil.isSigned ik && !GU.in_verifying_stage then
      Goblintutil.did_overflow := true

  let norm ik = function None -> None | Some (x,y) ->
    if Ints_t.compare x y > 0 then None
    else if Ints_t.compare (min_int ik) x > 0 || Ints_t.compare (max_int ik) y < 0 then (set_overflow_flag ik; top_of ik)
    else Some (x,y)

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (x1,x2), Some (y1,y2) -> Ints_t.compare x1 y1 >= 0 && Ints_t.compare x2 y2 <= 0

  let join ik (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (min x1 y1, max x2 y2)

  let meet ik (x:t) y =
    match x, y with
    | None, z | z, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (max x1 y1, min x2 y2)

  let is_int = function Some (x,y) when Ints_t.compare x y = 0 -> true | _ -> false

  (* TODO: change to_int signature so it returns a big_int *)
  let to_int = function Some (x,y) when Ints_t.compare x y = 0 -> Some x | _ -> None
  let of_interval ik (x,y) = norm ik @@ Some (x,y)
  let of_int ik (x: int_t) = of_interval ik (x,x)
  let zero = Some (Ints_t.zero, Ints_t.zero)
  let one  = Some (Ints_t.one, Ints_t.one)
  let top_bool = Some (Ints_t.zero, Ints_t.one)

  let of_bool _ik = function true -> one | false -> zero
  let is_bool x = x <> None && not (leq zero x) || equal x zero
  let to_bool (a: t) = match a with
    | None -> None
    | Some (l, u) when Ints_t.compare l Ints_t.zero = 0 && Ints_t.compare u Ints_t.zero = 0 -> Some false
    | x -> if leq zero x then None else Some true
  let to_bool_interval x = match x with
    | None -> x
    | Some (l, u) when Ints_t.compare l Ints_t.zero = 0 && Ints_t.compare u Ints_t.zero = 0 -> x
    | _ -> if leq zero x then top_bool else one

  let range_opt f = function
    | None -> None
    | Some ik -> Some (Ints_t.of_bigint @@ f @@ Size.range_big_int ik)

  let starting ik n =
    (norm ik) @@ Some (n, range_opt snd (Some ik) |? (max_int ik))

  let ending ik n =
    norm ik @@ Some (range_opt fst (Some ik) |? min_int ik, n)

  (* TODO: change signature of maximal, minimal to return big_int*)
  let maximal = function None -> None | Some (x,y) -> Some y
  let minimal = function None -> None | Some (x,y) -> Some x

  let cast_to ?torg ?no_ov t = function
    | None -> None
    | Some (x,y) ->
      try
        let a = Ints_t.of_bigint @@ Size.cast_big_int t (Ints_t.to_bigint x) in
        let b = Ints_t.of_bigint @@ Size.cast_big_int t (Ints_t.to_bigint y) in
        let a,b = if Ints_t.compare x a <> 0 || Ints_t.compare y b <> 0 then Size.range_big_int t |> (fun (a, b) -> (Ints_t.of_bigint a, Ints_t.of_bigint b)) else a,b in
        norm t @@ Some (a, b)
      with Size.Not_in_int64 -> top_of t

  let widen ik x y =
    match x, y with
    | None, z | z, None -> z
    | Some (l0,u0), Some (l1,u1) ->
      let l2 = if Ints_t.compare l0 l1 = 0 then l0 else min l1 (min_int ik) in
      let u2 = if Ints_t.compare u0 u1 = 0 then u0 else max u1 (max_int ik) in
      norm ik @@ Some (l2,u2)
  let widen ik x y =
    let r = widen ik x y in
    if M.tracing then M.trace "int" "interval widen %a %a -> %a\n" pretty x pretty y pretty r;
    r

  let narrow ik x y =
    match x, y with
    | _,None | None, _ -> None
    | Some (x1,x2), Some (y1,y2) ->
      let lr = if Ints_t.compare (min_int ik) x1 = 0 then y1 else x1 in
      let ur = if Ints_t.compare (max_int ik) x2 = 0 then y2 else x2 in
      norm ik @@ Some (lr,ur)

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

  let bit f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try norm ik (of_int ik (f ik x y)) with Division_by_zero -> top_of ik)
      | _              -> top_of ik

  let bitcomp f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try norm ik (of_int ik (f ik x y)) with Division_by_zero | Invalid_argument _ -> top_of ik)
      | _              -> (set_overflow_flag ik;  top_of ik)

  let bitxor = bit (fun _ik -> Ints_t.bitxor)
  let bitand = bit (fun _ik -> Ints_t.bitand)
  let bitor  = bit (fun _ik -> Ints_t.bitor)

  let bit1 f ik i1 =
    if is_bot i1 then
      bot_of ik
    else
      match to_int i1 with
      | Some x -> of_int ik (f ik x)
      | _      -> top_of ik

  let bitnot = bit1 (fun _ik -> Ints_t.bitnot)
  let shift_right = bitcomp (fun _ik x y -> Ints_t.shift_right x (Ints_t.to_int y))
  let shift_left  = bitcomp (fun _ik x y -> Ints_t.shift_left  x (Ints_t.to_int y))

  let neg ik = function None -> None | Some (x,y) -> norm ik @@ Some (Ints_t.neg y, Ints_t.neg x)

  let add ?no_ov ik x y = match x, y with
  | None, None -> None
  | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
  | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (Ints_t.add x1 y1, Ints_t.add x2 y2)

  let sub ?no_ov ik x y = match x, y with
  | None, None -> None
  | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
  | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (Ints_t.sub x1 y2, Ints_t.sub x2 y1) (* y1, y2 are in different order here than in add *)

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
        let b = Ints_t.sub (max (pos yl) (pos yu)) Ints_t.one in
        let range = if Ints_t.compare xl Ints_t.zero>= 0 then Some (Ints_t.zero, min xu b) else Some (max xl (Ints_t.neg b), min xu b) in
        meet ik (bit (fun _ik -> Ints_t.rem) ik x y) range

  let mul ?no_ov ik x y =
    match x, y with
    | None, None -> bot ()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      let x1y1 = (Ints_t.mul x1 y1) in let x1y2 = (Ints_t.mul x1 y2) in
      let x2y1 = (Ints_t.mul x2 y1) in let x2y2 = (Ints_t.mul x2 y2) in
      norm ik @@ Some ((min (min x1y1 x1y2) (min x2y1 x2y2)),
                      (max (max x1y1 x1y2) (max x2y1 x2y2)))

  let rec div ?no_ov ik x y =
    match x, y with
    | None, None -> bot ()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (x1,x2), Some (y1,y2) ->
      begin
        let is_zero v = Ints_t.compare v Ints_t.zero = 0 in
        match y1, y2 with
        | l, u when is_zero l && is_zero u -> top_of ik (* TODO warn about undefined behavior *)
        | l, _ when is_zero l              -> div ik (Some (x1,x2)) (Some (Ints_t.one,y2))
        | _, u when is_zero u              -> div ik (Some (x1,x2)) (Some (y1, Ints_t.(neg one)))
        | _ when leq (of_int ik (Ints_t.zero)) (Some (y1,y2)) -> top_of ik
        | _ ->
          let x1y1n = (Ints_t.div x1 y1) in let x1y2n = (Ints_t.div x1 y2) in
          let x2y1n = (Ints_t.div x2 y1) in let x2y2n = (Ints_t.div x2 y2) in
          let x1y1p = (Ints_t.div x1 y1) in let x1y2p = (Ints_t.div x1 y2) in
          let x2y1p = (Ints_t.div x2 y1) in let x2y2p = (Ints_t.div x2 y2) in
          norm ik @@ Some ((min (min x1y1n x1y2n) (min x2y1n x2y2n)),
                        (max (max x1y1p x1y2p) (max x2y1p x2y2p)))
      end
  let ne ik i1 i2 = to_bool_interval (sub ik i1 i2)

  let eq ik (i1: t) (i2: t) = to_bool_interval (lognot ik (sub ik i1 i2))

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

  let invariant c x = failwith "unimplemented"

  let invariant_ikind c ik x =
    let c = Cil.(Lval (BatOption.get c.Invariant.lval)) in
    match x with
    | Some (x1, x2) when Ints_t.compare x1 x2 = 0 ->
      let x1 = Ints_t.to_int64 x1 in
      Invariant.of_exp Cil.(BinOp (Eq, c, kinteger64 IInt x1, intType))
    | Some (x1, x2) ->
      let open Invariant in
      let (x1', x2') = BatTuple.Tuple2.mapn (fun a -> Cilint.Big (Ints_t.to_bigint a)) (x1, x2) in
      (try
        (* typeOf will fail if c is heap allocated *)
        let i1 = if Ints_t.compare (min_int ik) x1 <> 0 then of_exp Cil.(BinOp (Le, kintegerCilint ik x1', c, intType)) else none in
        let i2 = if Ints_t.compare x2 (max_int ik) <> 0 then of_exp Cil.(BinOp (Le, c, kintegerCilint ik x2', intType)) else none in
        i1 && i2
      with e -> None)
    | None -> None

  let arbitrary () =
    (* TODO: use arbitrary ikind? *)
    let ik = Cil.ILongLong in
    let open QCheck.Iter in
    (* let int_arb = QCheck.map ~rev:Ints_t.to_bigint Ints_t.of_bigint MyCheck.Arbitrary.big_int in *)
    (* TODO: apparently bigints are really slow compared to int64 for domaintest *)
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let shrink = function
      | Some (l, u) -> (return None) <+> (MyCheck.shrink pair_arb (l, u) >|= of_interval ik)
      | None -> empty
    in
    QCheck.(set_shrink shrink @@ set_print show @@ map (*~rev:BatOption.get*) (of_interval ik) pair_arb)
  let relift x = x

  let refine_with_congruence (intv : t) (cong : (int_t * int_t ) option) : t =
    match intv, cong with
    | Some (x, y), Some (c, m) -> None (* TODO: implement *)
    | _ -> cong

  let refine_with_interval a b = a
  let refine_with_excl_list a b = a
  let refine_with_incl_list a b = a

end


module IntIkind = struct let ikind () = Cil.IInt end
module Interval =  IntervalFunctor (BI)
module Interval32 = IntDomWithDefaultIkind (IntDomLifter (IntervalFunctor (IntOps.Int64Ops))) (IntIkind)

module Integers : IkindUnawareS with type t = int64 and type int_t = int64 = (* no top/bot, order is <= *)
struct
  include Printable.Std
  let name () = "integers"
  type t = int64 [@@deriving eq, to_yojson]
  type int_t = int64
  let top () = raise Unknown
  let bot () = raise Error
  let top_of ik = top ()
  let bot_of ik = bot ()
  let show x = if x = GU.inthack then "*" else Int64.to_string x

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)
  (* FIXME: poly compare *)
  let hash (x:t) = ((Int64.to_int x) - 787) * 17
  (* is_top and is_bot are never called, but if they were, the Std impl would raise their exception, so we overwrite them: *)
  let is_top _ = false
  let is_bot _ = false

  let equal_to i x = if i > x then `Neq else `Top
  let leq x y = x <= y
  let join x y = if Int64.compare x y > 0 then x else y
  let widen = join
  let meet x y = if Int64.compare x y > 0 then y else x
  let narrow = meet

  let of_bool x = if x then Int64.one else Int64.zero
  let to_bool' x = x <> Int64.zero
  let to_bool x = Some (to_bool' x)
  let is_bool _ = true
  let of_int  x = x
  let to_int  x = Some x
  let is_int  _ = true

  let neg  = Int64.neg
  let add  = Int64.add (* TODO: signed overflow is undefined behavior! *)
  let sub  = Int64.sub
  let mul  = Int64.mul
  let div  = Int64.div
  let rem  = Int64.rem
  let lt n1 n2 = of_bool (n1 <  n2)
  let gt n1 n2 = of_bool (n1 >  n2)
  let le n1 n2 = of_bool (n1 <= n2)
  let ge n1 n2 = of_bool (n1 >= n2)
  let eq n1 n2 = of_bool (n1 =  n2)
  let ne n1 n2 = of_bool (n1 <> n2)
  let bitnot = Int64.lognot
  let bitand = Int64.logand
  let bitor  = Int64.logor
  let bitxor = Int64.logxor
  let shift_left  n1 n2 = Int64.shift_left n1 (Int64.to_int n2)
  let shift_right n1 n2 = Int64.shift_right n1 (Int64.to_int n2)
  let lognot n1    = of_bool (not (to_bool' n1))
  let logand n1 n2 = of_bool ((to_bool' n1) && (to_bool' n2))
  let logor  n1 n2 = of_bool ((to_bool' n1) || (to_bool' n2))
  let cast_to ?torg t x = Size.cast t x
  let arbitrary () = MyCheck.Arbitrary.int64
end

module FlatPureIntegers = (* Integers, but raises Unknown/Error on join/meet *)
struct
  include Integers

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
  let is_int  x = match x with
    | `Lifted x -> true
    | _ -> false

  let of_bool x = `Lifted (Base.of_bool x)
  let to_bool x = match x with
    | `Lifted x -> Base.to_bool x
    | _ -> None
  let is_bool = is_int

  let to_excl_list x = None
  let of_excl_list ik x = top_of ik
  let is_excl_list x = false
  let of_interval ik x = top_of ik
  let starting     ikind x = top_of ikind
  let ending       ikind x = top_of ikind
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
  let is_int  x = match x with
    | `Lifted x -> true
    | _ -> false

  let of_bool x = `Lifted (Base.of_bool x)
  let to_bool x = match x with
    | `Lifted x -> Base.to_bool x
    | _ -> None
  let is_bool = is_int

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
end

module Flattened = Flat (Integers)
module Lifted    = Lift (Integers)

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
  let cast_to ik x = Size.cast_big_int ik x
  let to_bool x = Some (not (BI.equal (BI.zero) x))

  let hash x = (BI.to_int x) * 2147483647
  let show x = BI.to_string x
  let pretty _ x = Pretty.text (BI.to_string x)
  let to_yojson x = failwith "to_yojson not implemented for BigIntPrintable"
  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let arbitrary () = QCheck.map ~rev:to_int64 of_int64 QCheck.int64
end

module DefExc : S with type int_t = BigInt.t = (* definite or set of excluded values *)
struct
  module S = SetDomain.Make (BigInt)
  module R = Interval32 (* range for exclusion *)

  (* Ikind used for intervals representing the domain *)
  let range_ikind = Cil.IInt
  let size t = R.of_interval range_ikind (let a,b = Size.bits_i64 t in Int64.neg a,b)

  (* Returns min and max values for a range given by the number of possibly used bits *)
  let range_min_max (r : R.t) =
    match R.minimal r, R.maximal r with
    | None, _ | _, None -> BigInt.zero, BigInt.zero
    | Some l, Some u ->
      let l = Int64.to_int l in
      let u = Int64.to_int u in
      let min = if l = 0 then BigInt.zero else BigInt.neg @@ BigInt.shift_left BigInt.one (-l) in
      let max = BigInt.sub (BigInt.shift_left BigInt.one u) BigInt.one in
      min, max

  type t = [
    | `Excluded of S.t * R.t
    | `Definite of BigInt.t
    | `Bot
  ] [@@deriving eq, to_yojson]
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
  let hash (x:t) =
    match x with
    | `Excluded (s,r) -> S.hash s + R.hash r
    | `Definite i -> 83*BigInt.hash i
    | `Bot -> 61426164

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)
  (* FIXME: poly compare? *)

  let is_top x = x = top ()

  let equal_to i = function
  | `Bot -> failwith "unsupported: equal_to with bottom"
  | `Definite x -> if i = x then `Eq else `Neq
  | `Excluded (s,r) -> if S.mem i s then `Top else `Neq

  let top_of ik = `Excluded (S.empty (), size ik)
  let top_if_not_in_int64 ik f x = try f x with Size.Not_in_int64 -> top_of ik
  let cast_to ?torg ?no_ov ik = top_if_not_in_int64 ik @@ function
    | `Excluded (s,r) ->
      let r' = size ik in
      `Excluded (
        if R.leq r r' then (* upcast -> no change *)
          s, r
        else if torg = None then (* same static type -> no overflows for r, but we need to cast s since it may be out of range after lift2_inj *)
          let s' = S.map (BigInt.cast_to ik) s in
          s', r'
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
      let should_wrap ik = not (Cil.isSigned ik) || GobConfig.get_bool "ana.int.wrap_on_signed_overflow" in
      match v with
      | `Excluded (s, r) ->
        let possibly_overflowed = not (R.leq r (size ik)) in
        (* If no overflow occurred, just return x *)
        if not possibly_overflowed then (
          v
        )
        (* Else, if an overflow occurred that we should not treat with wrap-around, go to top *)
        else if not (should_wrap ik) then(
          top_of ik
        ) else (
          (* Else an overflow occurred that we should treat with wrap-around *)
          let r = size ik in
          (* Perform a wrap-around for unsigned values and for signed values (if configured). *)
          let mapped_excl = S.map (fun excl -> BigInt.cast_to ik excl) s in
          `Excluded (mapped_excl, r)
        )
      | `Definite x ->
        let min, max = Size.range_big_int ik in
        (* Perform a wrap-around for unsigned values and for signed values (if configured). *)
        if should_wrap ik then (
          cast_to ik v
        ) else if BigInt.compare min x <= 0 && BigInt.compare x max <= 0 then (
          v
        ) else (
          top_of ik
        )
      | `Bot -> `Bot

  let max_of_range r = Size.max_from_bit_range (R.maximal r)
  let min_of_range r = Size.min_from_bit_range (R.minimal r)

  let maximal = function
    | `Definite x -> Some x
    | `Excluded (s,r) -> max_of_range r
    | `Bot -> None

  let minimal = function
    | `Definite x -> Some x
    | `Excluded (s,r) -> min_of_range r
    | `Bot -> None

  let in_range r i =
    match min_of_range r with
    | None when BI.compare i BI.zero < 0 -> true
    | Some l when BI.compare i BI.zero < 0  -> BI.compare l i <= 0
    | _ ->
      match max_of_range r with
      | None -> true
      | Some u -> i <= u

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
    | `Excluded _, `Definite _ -> false
    (* Excluding X <= Excluding Y whenever Y <= X *)
    | `Excluded (x,xw), `Excluded (y,yw) -> S.subset y x && R.leq xw yw

  let join ik x y =
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
    | `Excluded (x,wx), `Excluded (y,wy) -> `Excluded (S.inter x y, R.join wx wy)

  let widen = join

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
  let is_int  x = match x with
    | `Definite x -> true
    | _ -> false

  let zero ik = of_int ik BI.zero
  let from_excl ikind (s: S.t) = norm ikind @@ `Excluded (s, size ikind)
  let not_zero ikind = from_excl ikind (S.singleton BI.zero)

  let of_bool_cmp ik x = of_int ik (if x then BI.one else BI.zero)
  let of_bool = of_bool_cmp
  let to_bool x =
    match x with
    | `Definite x -> BigInt.to_bool x
    | `Excluded (s,r) when S.mem BI.zero s -> Some true
    | _ -> None
  let is_bool x =
    match x with
    | `Definite x -> true
    | `Excluded (s,r) -> S.mem BI.zero s
    | _ -> false

  let of_interval ik (x,y) = if BigInt.compare x y = 0 then of_int ik x else top_of ik

  let starting ikind x = if BigInt.compare x BigInt.zero > 0 then not_zero ikind else top_of ikind
  let ending ikind x = if BigInt.compare x BigInt.zero < 0 then not_zero ikind else top_of ikind

  let of_excl_list t l =
    let r = size t in (* elements in l are excluded from the full range of t! *)
    `Excluded (List.fold_right S.add l (S.empty ()), r)
  let is_excl_list l = match l with `Excluded _ -> true | _ -> false
  let to_excl_list x = match x with
    | `Definite _ -> None
    | `Excluded (s,r) -> Some (S.elements s)
    | `Bot -> None

  let apply_range f r = (* apply f to the min/max of the old range r to get a new range *)
    (* If the Int64 might overflow on us during computation, we instead go to top_range *)
    match R.minimal r, R.maximal r with
    | _ ->
      let rf m = BatOption.map (size % Size.min_for % f) (m r) in
      match rf min_of_range, rf max_of_range with
        | Some r1, Some r2 -> R.join r1 r2
        | _ , _ -> top_range

  (* Default behaviour for unary operators, simply maps the function to the
   * DefExc data structure. *)
  let lift1 f ik x = match x with
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

  let lift_comp f ik x y = norm ik (match x,y with
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

  let neg ik (x :t) = norm ik @@ lift1 BigInt.neg ik x
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
  let lt ik = lift2 BigInt.lt ik
  let gt ik = lift2 BigInt.gt ik
  let le ik = lift2 BigInt.le ik
  let ge ik = lift2 BigInt.ge ik
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
    (* If one of the parameters of the shift is negative, the result is undedined *)
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
  let logand = lift2 BigInt.logand
  let logor  = lift2 BigInt.logor
  let lognot ik = eq ik (of_int ik BigInt.zero)

  let invariant_ikind c ik (x:t) =
    let c = Cil.(Lval (BatOption.get c.Invariant.lval)) in
    match x with
    | `Definite x -> Invariant.of_exp Cil.(BinOp (Eq, c, kintegerCilint ik (Big x), intType))
    | `Excluded (s, _) ->
      S.fold (fun x a ->
          let i = Invariant.of_exp Cil.(BinOp (Ne, c, kintegerCilint ik (Big x), intType)) in
          Invariant.(a && i)
        ) s Invariant.none
    | `Bot -> Invariant.none

  let arbitrary () =
    let open QCheck.Iter in
    let excluded s = `Excluded (s, size Cil.ILongLong) in (* S TODO: non-fixed range *)
    let definite x = `Definite x in
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

  let refine_with_congruence a b = a
  let refine_with_interval a b = a
  let refine_with_excl_list a b = a
  let refine_with_incl_list a b = a
end

module OverflowInt64 = (* throws Overflow for add, sub, mul *)
struct
  exception Overflow of string

  include Int64

  let add (a:int64) (b:int64) =
    if logor (logxor a b) (logxor a (lognot (add a b))) < 0L  (* no kidding! *)
    then add a b
    else raise (Overflow (Printf.sprintf "%Ld + %Ld" a b))

  let sub (a:int64) (b:int64) =
    if b = min_int
    then
      if a >= 0L
      then raise (Overflow (Printf.sprintf "%Ld - %Ld" a b))
      else sub a b
    else
      let oppb = neg b in
      add a oppb

  let mul (a:int64) (b:int64) =
    if a = 0L then 0L
    else
      let x = mul a b in
      if b = div x a
      then x
      else raise (Overflow (Printf.sprintf "%Ld * %Ld" a b))

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
  type t = bool [@@deriving eq, to_yojson]
  let name () = "booleans"
  let top () = true
  let bot () = false
  let top_of ik = top ()
  let bot_of ik = bot ()
  let show x = if x then N.truename else N.falsename
  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)
  let hash = function true -> 51534333 | _ -> 561123444
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
  let is_bool x = not x
  let of_int x  = x = Int64.zero
  let to_int x  = if x then None else Some Int64.zero
  let is_int x  = not x

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
  module ISet = struct
    include SetDomain.Make(I)
    let is_singleton s = cardinal s = 1
  end
  type t = Inc of ISet.t | Exc of ISet.t * R.t [@@deriving eq, ord, to_yojson] (* inclusion/exclusion set *)

  type int_t = BI.t
  let name () = "enums"
  let bot () = failwith "bot () not implemented for Enums"
  let top_of ik = Exc (ISet.empty (), size ik)
  let top () = failwith "top () not implemented for Enums"
  let bot_of ik = Inc (ISet.empty ())
  let top_bool = Inc (ISet.of_list [I.zero; I.one])

  let min_int ik = I.of_bigint @@ fst @@ Size.range_big_int ik
  let max_int ik = I.of_bigint @@ snd @@ Size.range_big_int ik

  let show = function
    | Inc xs when ISet.is_empty xs -> "bot"
    | Inc xs -> "{" ^ (String.concat ", " (List.map I.show (ISet.elements  xs))) ^ "}"
    | Exc (xs,r) -> "not {" ^ (String.concat ", " (List.map I.show (ISet.elements xs))) ^ "} " ^ "("^R.show r^")"

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let hash = function
    | Inc x -> ISet.hash x
    | Exc (x, r) -> 31 * R.hash r + 37  * ISet.hash x

  let norm ikind v =
    let min, max = min_int ikind, max_int ikind in
    (* Whether the value v lies within the values of the specified ikind. *)
    let value_in_ikind v =
      I.compare min v <= 0 && I.compare v max <= 0
    in
    (* Whether the range r lies within the range of the ikind. *)
    let range_in_ikind r =
      R.leq r (size ikind)
    in
    match v with
    | Inc xs when ISet.for_all value_in_ikind xs -> v
    | Exc (xs, r) when ISet.for_all value_in_ikind xs && range_in_ikind r -> v
    | _ -> top_of ikind

  let equal_to i = function
    | Inc x ->
      if ISet.mem i x then
        if ISet.is_singleton x then `Eq
        else `Top
      else `Neq
    | Exc (x, r) ->
      if ISet.mem i x then `Neq
      else `Top

  let cast_to ?torg ?no_ov ik v = norm ik @@ match v with
    | Exc (s,r) ->
      let r' = size ik in
      if R.leq r r' then (* upcast -> no change *)
        Exc (s, r)
      else if torg = None then (* same static type -> no overflows for r, but we need to cast s since it may be out of range after lift2_inj *)
        let s' = ISet.map (I.cast_to ik) s in
        Exc (s', r')
      else (* downcast: may overflow *)
        Exc ((ISet.empty ()), r')
    | Inc xs ->
      let casted_xs = ISet.map (BigInt.cast_to ik) xs in
      if Cil.isSigned ik && not (ISet.equal xs casted_xs)
        then top_of ik (* When casting into a signed type and the result does not fit, the behavior is implementation-defined *)
        else Inc casted_xs

  let of_int ikind x = cast_to ikind (Inc (ISet.singleton x))

  let of_interval ik (x,y) = if x = y then of_int ik x else top_of ik

  let join_ignore_ikind = curry @@ function
  | Inc x, Inc y -> Inc (ISet.union x y)
  | Exc (x,r1), Exc (y,r2) -> Exc (ISet.inter x y, R.join r1 r2)
  | Exc (x,r), Inc y
  | Inc y, Exc (x,r) ->
    let r = if ISet.is_empty y
      then r
      else
        let (min_el_range, max_el_range) = Tuple2.mapn  (fun x -> R.of_interval range_ikind (Size.min_range_sign_agnostic x)) (ISet.min_elt y, ISet.max_elt y) in
        let range = R.join min_el_range max_el_range in
        R.join r range
    in
    Exc (ISet.diff x y, r)

  let join ikind = join_ignore_ikind

  let meet ikind = curry @@ function
    | Inc x, Inc y -> Inc (ISet.inter x y)
    | Exc (x,r1), Exc (y,r2) -> Exc (ISet.union x y, R.meet r1 r2)
    | Inc x, Exc (y,r)
    | Exc (y,r), Inc x -> Inc (ISet.diff x y)

  let widen = join
  let narrow = meet

  let leq x y = equal (join_ignore_ikind x y) y
  let handle_bot x y f = match is_bot x, is_bot y with
    | false, false -> f ()
    | true, false
    | false, true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | true, true -> Inc (ISet.empty ())

  let lift1 f ikind v = norm ikind @@ match v with
    | Inc x when ISet.is_empty x -> v (* Return bottom when value is bottom *)
    | Inc x when ISet.is_singleton x -> Inc (ISet.singleton (f (ISet.choose x)))
    | _ -> top_of ikind

  let lift2 f (ikind: Cil.ikind) u v =
    handle_bot u v (fun () ->
      norm ikind @@ match u, v with
      | Inc x,Inc y when ISet.is_singleton x && ISet.is_singleton y -> Inc (ISet.singleton (f (ISet.choose x) (ISet.choose y)))
      | _,_ -> top_of ikind)

  let lift2 f ikind a b =
    try lift2 f ikind a b with Division_by_zero -> top_of ikind

  let neg = lift1 I.neg
  let add ?no_ov ikind = curry @@ function
    | Inc z,x when ISet.is_singleton z && ISet.choose z = BI.zero -> x
    | x,Inc z when ISet.is_singleton z && ISet.choose z = BI.zero -> x
    | x,y -> lift2 I.add ikind x y
  let sub ?no_ov = lift2 I.sub
  let mul ?no_ov ikind a b =
    match a, b with
    | Inc one,x when ISet.is_singleton one && ISet.choose one = BI.one -> x
    | x,Inc one when ISet.is_singleton one && ISet.choose one = BI.one -> x
    | Inc zero,_ when ISet.is_singleton zero && ISet.choose zero = BI.zero -> a
    | _,Inc zero when ISet.is_singleton zero && ISet.choose zero = BI.zero -> b
    | x,y -> lift2 I.mul ikind x y

  let div ?no_ov ikind a b = match a, b with
    | x,Inc one when ISet.is_singleton one && ISet.choose one = BI.one -> x
    | _,Inc zero when ISet.is_singleton zero && ISet.choose zero = BI.zero -> top_of ikind
    | Inc zero,_ when ISet.is_singleton zero && ISet.choose zero = BI.zero -> a
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

  let of_bool ikind x = Inc (ISet.singleton (if x then BI.one else BI.zero))
  let to_bool  = function
    | Inc e when ISet.is_empty e -> None
    | Exc (e,_) when ISet.is_empty e -> None
    | Inc zero when ISet.is_singleton zero && ISet.choose zero = BI.zero -> Some false
    | Inc xs when ISet.for_all ((<>) BI.zero) xs -> Some true
    | Exc (xs,_) when ISet.exists ((=) BI.zero) xs -> Some true
    | _ -> None
  let is_bool = BatOption.is_some % to_bool
  let to_int = function Inc x when ISet.is_singleton x -> Some (ISet.choose x) | _ -> None
  let is_int = BatOption.is_some % to_int

  let to_excl_list = function Exc (x,r) when not (ISet.is_empty x) -> Some (ISet.elements x) | _ -> None
  let of_excl_list t x = Exc (ISet.of_list x, size t)
  let is_excl_list = BatOption.is_some % to_excl_list
  let starting     ikind x = top_of ikind
  let ending       ikind x = top_of ikind

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
    | Inc xs when not (ISet.is_empty xs) -> Some (ISet.max_elt xs)
    | Exc (excl,r) ->
      (match Size.max_from_bit_range (R.maximal r) with
       | None -> None
       | Some range_max ->
         let rec decrement_while_contained v s =
           if ISet.mem range_max s
           then decrement_while_contained (BI.sub v (BI.one)) s
           else v
         in
         Some (decrement_while_contained range_max excl))
    | _ (* bottom case *) -> None

  let minimal = function
    | Inc xs when not (ISet.is_empty xs) -> Some (ISet.min_elt xs)
    | Exc (excl,r) ->
      (match Size.min_from_bit_range (R.minimal r) with
       | None -> None
       | Some range_min ->
         let rec increment_while_contained v s =
           if ISet.mem range_min s
           then increment_while_contained (BI.add v (BI.one)) s
           else v
         in
         Some (increment_while_contained range_min excl))
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
      | Inc xs, Inc ys when ISet.is_singleton xs && ISet.is_singleton ys -> of_bool ik (I.equal (ISet.choose xs) (ISet.choose ys))
      | _, _ ->
        if is_bot (meet ik x y) then
          (* If the meet is empty, there is no chance that concrete values are equal *)
          of_bool ik false
        else
          top_bool)

  let ne ik x y = lognot ik (eq ik x y)

  let invariant_ikind c ik x =
    let c = Cil.(Lval (Option.get c.Invariant.lval)) in
    match x with
    | Inc ps ->
      List.fold_left (fun a x ->
          let i = Invariant.of_exp Cil.(BinOp (Eq, c, kintegerCilint ik (Big x), intType)) in
          Invariant.(a || i)
        ) Invariant.none (ISet.elements ps)
    | Exc (ns, _) ->
      List.fold_left (fun a x ->
          let i = Invariant.of_exp Cil.(BinOp (Ne, c, kintegerCilint ik (Big x), intType)) in
          Invariant.(a && i)
        ) Invariant.none (ISet.elements ns)


  let arbitrary () =
    let open QCheck.Iter in
    let neg s = Exc (s, size Cil.ILong) in (* S TODO: non-fixed range *)
    let pos s = Inc s in
    let shrink = function
      | Exc (s, _) -> MyCheck.shrink (ISet.arbitrary ()) s >|= neg (* S TODO: possibly shrink neg to pos *)
      | Inc s -> MyCheck.shrink (ISet.arbitrary ()) s >|= pos
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map neg (ISet.arbitrary ());
      10, QCheck.map pos (ISet.arbitrary ());
    ] (* S TODO: decide frequencies *)

    let refine_with_congruence a b =
      let contains c m x = if BI.equal m BI.zero then BI.equal c x else (BI.rem (BI.sub x c) m) == BI.zero in
      match a with
      | Inc e -> ( match b with
                | None -> Inc e
                | Some (c, m) -> Inc (ISet.filter (contains c m) e))
      | a -> a
  let refine_with_interval a b = a
  let refine_with_excl_list a b = a
  let refine_with_incl_list a b = a
end

module CongruenceFunctor(Ints_t : IntOps.IntOps): S with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option =
struct
  let name () = "congruences"
  type int_t = Ints_t.t

  (* represents congruence class of c mod m *)
  type t = (Ints_t.t * Ints_t.t) option

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
  let ( |: ) a b = (b %: a) =: Ints_t.zero

  let to_yojson t = failwith "to yojson unimplemented"

  let rec gcd x y = (*TODO Double Check*)
    if y =: Ints_t.zero then x else gcd y (x %: y)

  let abs x = let r = Ints_t.neg x
    in if x <: r then r else x

  let normalize x =
    match x with
    | None -> None
    | Some (c, m) -> if m =: Ints_t.zero then Some (c, m) else Some (c %: (abs m), (abs m))

  let min_int ik = Ints_t.of_bigint @@ fst @@ Size.range_big_int ik

  let max_int ik = Ints_t.of_bigint @@ snd @@ Size.range_big_int ik
  let top () = Some (Ints_t.zero, Ints_t.one)
  let top_of ik = Some (Ints_t.zero, Ints_t.one)
  let bot () = None
  let bot_of ik = bot () (* TODO: improve  *)

  let is_top x = x = top ()

  let show = function ik -> match ik with
    | None -> "∅"
    | Some (c, m) when (c, m) = (Ints_t.zero, Ints_t.zero) -> Ints_t.to_string c
    | Some (c, m) ->
      let a = if c =: Ints_t.zero then "" else Ints_t.to_string c in
      let b = if m =: Ints_t.zero then "" else if m = Ints_t.one then "ℤ" else Ints_t.to_string m^"ℤ" in
      let c = if a = "" || b = "" then "" else "+" in
      a^c^b

  let equal a b = match (normalize a), (normalize b) with
    | None, None -> true
    | Some (a, b), Some (c, d) -> a =: c && b =: d
    | _, _ -> false

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) when b =: Ints_t.zero -> if a = i then `Eq else `Neq
    | Some (a, b) -> if i %: b = a then `Top else `Neq

  let set_overflow_flag ik =
    if Cil.isSigned ik && !GU.in_verifying_stage then
      Goblintutil.did_overflow := true

  let norm ik = function None -> None | Some (c,m) ->
    if c <: min_int ik   || c >: max_int ik || m <: min_int ik || m >: max_int ik then (set_overflow_flag ik; top_of ik)
    else Some (c,m)

  let leq (x:t) (y:t) =
    match x, y with
    | None, _ -> true
    | Some _, None -> false
    | Some (c1,m1), Some (c2,m2) when m2 =: Ints_t.zero && m1 =: Ints_t.zero -> c1 =: c2
    | Some (c1,m1), Some (c2,m2) when m2 =: Ints_t.zero -> c1 =: c2 && m1 =: Ints_t.zero
    | Some (c1,m1), Some (c2,m2) -> m2 |: (gcd (c1 -: c2) m2)

  let leq x y =
    let res = leq x y in
    if M.tracing then M.trace "mything" "Cong. leq %a %a -> %a \n" pretty x pretty y pretty (Some(Ints_t.of_int (Bool.to_int res), Ints_t.zero)) ;
    res

  let join ik (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (c1,m1), Some (c2,m2) ->
      let m3 = gcd m1 (gcd m2 (c1 -: c2)) in
      norm ik @@ normalize (Some (c1, m3))

  let join ik (x:t) y =
    let res = join ik x y in
    if M.tracing then M.trace "mything" "Cong. join %a %a -> %a\n" pretty x pretty y pretty res;
    res

  (* if it exists, c2/a2 is solution to a*x ≡ c (mod m) *)
  let congruence_series a c m =
    let rec next a1 c1 a2 c2 =
      if a2 |: a1 then (a2, c2)
      else next a2 c2 (a1 %: a2) ((c1 -: c2) *: (a1 /: a2))
    in next m Ints_t.zero a c

  let meet ik x y =
    let simple_case i c m =
      if m |: (i -: c)
      then Some (i, Ints_t.zero) else None
    in
    match x, y with
    | Some (c1, m1), Some (c2, m2) when m1 =: Ints_t.zero && m2 =: Ints_t.zero -> if c1 =: c2 then Some (c1, Ints_t.zero) else None
    | Some (c1, m1), Some (c2, m2) when m1 =: Ints_t.zero -> simple_case c1 c2 m2
    | Some (c1, m1), Some (c2, m2) when m2 =: Ints_t.zero -> simple_case c2 c1 m1
    | Some (c1, m1), Some (c2, m2) when (gcd m1 m2) |: (c1 -: c2) ->
      let (c, m) = congruence_series m1 (c2 -: c1 ) m2 in
      normalize (Some(c1 +: (m1 *: (m /: c)), m1 *: (m2 /: c)))
    | _  -> None

  let meet ik x y =
    let res = meet ik x y in
    if M.tracing then M.trace "mything" "Cong. meet %a %a -> %a\n" pretty x pretty y pretty res;
    res

  let is_int = function Some (c, m) when m =: Ints_t.zero -> true | _ -> false

  (* TODO: change to_int signature so it returns a big_int *)
  let to_int = function Some (c, m) when m =: Ints_t.zero -> Some c | _ -> None
  let of_int ik (x: int_t) = Some (x, Ints_t.zero)
  let of_pair ik p = normalize (Some p)
  let zero = Some (Ints_t.zero, Ints_t.zero)
  let one  = Some (Ints_t.one, Ints_t.zero)
  let top_bool = top()

  let of_bool _ik = function true -> one | false -> zero
  let is_bool x = x <> None && not (leq zero x) || equal x zero

  let to_bool (a: t) = match a with
    | None -> None
    | x when equal zero x -> Some false
    | x -> if leq zero x then None else Some true

  let to_bool (a: t) = let res = to_bool a in  if M.tracing then M.trace "mything" "Cong. to_bool %a -> ?\n" pretty a ;
    res

  let starting ik n = top()

  let ending = starting

  let maximal t = match t with
    | Some (x, y) when y =: Ints_t.zero -> Some x
    | _ -> None

  let minimal t = match t with
    | Some (x,y) when y =: Ints_t.zero -> Some x
    | _ -> None

  (* cast from original type to ikind, set to top if the value doesn't fit into the new type *)
  let cast_to ?torg ?no_ov t = function
    | None -> None
    | Some (c,m) -> try
        let a = Ints_t.of_bigint @@ Size.cast_big_int t (Ints_t.to_bigint c) in
        let b = Ints_t.of_bigint @@ Size.cast_big_int t (Ints_t.to_bigint m) in
        let a,b = if Ints_t.compare c a <> 0 || Ints_t.compare m b <> 0 then Size.range_big_int t |> (fun (a, b) -> (Ints_t.of_bigint a, Ints_t.of_bigint b)) else a,b in
            norm t @@ Some (a, b)
        with Size.Not_in_int64 -> top_of t

  let widen = join

  let widen ik x y =
    let res = widen ik x y in
    if M.tracing then M.trace "mything" "Cong. widen %a %a -> %a\n" pretty x pretty y pretty res;
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

  let bitcomp f ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> bot_of ik
    | true, _
    | _   , true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try norm ik (of_int ik (f ik x y)) with Division_by_zero | Invalid_argument _ -> top_of ik)
      | _              -> (set_overflow_flag ik;  top_of ik)

  let is_power_of_two x = x >: Ints_t.zero && Ints_t.of_int 2 |: x

  (* Not very pretty. A proper log operation might be better*)
  let rec log2 c k = if float_of_int 2 **  float_of_int k = float_of_int c then k else log2 c (k + 1)

  (*let shift_right ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (short 80 x) (short 80 y)))
    | Some (c, m), Some (c', m') when m =: Ints_t.zero && m' =: Ints_t.zero -> if c' <: Ints_t.zero then top() else norm ik @@ Some (Ints_t.shift_right c (Ints_t.to_int c'), Ints_t.zero)
    | Some (c, m), Some (c', m') when m' =: Ints_t.zero && is_power_of_two m -> let n = log2 (Ints_t.to_int m) 0 in
          if c' <: Ints_t.of_int n then norm ik @@ Some (Ints_t.shift_right c (Ints_t.to_int c'), Ints_t.of_int (int_of_float ((float_of_int 2) ** float_of_int (n - (Ints_t.to_int c'))))) else top()
    | _, _ -> top()*)

  let shift_right _ _ _ = top()

  let shift_right ik x y =
    let res = shift_right ik x y in
     if M.tracing then  M.trace "shifting" "Cong. shift_right : %a %a becomes %a \n" pretty x pretty y pretty res;
     res

  (* Naive primility test *)
  let is_prime n =
      let n = Ints_t.to_int (abs n) in
      let rec is_not_divisor d =
        d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
      n <> 1 && is_not_divisor 2

(* ToDo: Check for negative y? *)
let shift_left ik x y = match x, y with
  | None, None -> None
  | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
  | Some (c, m), Some (c', m') when c <: Ints_t.zero || c' <: Ints_t.zero -> top()
  | Some (c, m), Some (c', m') when c >: (max_int ik) || c' >: (max_int ik) -> top()
  | Some (c, m), Some (c', m') -> if (m =: Ints_t.zero && m' =: Ints_t.zero) then Some (Ints_t.shift_left c (Ints_t.to_int c'), Ints_t.zero)
    else let t = m' +: Ints_t.one in let x = Ints_t.shift_left Ints_t.one (Ints_t.to_int c') in   (* 2^c' *)
      if is_prime t then Some (x *: c, gcd (x *: m) ((c *: x) *: t)) else Some (x *: c, gcd (x *: m) (c *: x))

(*  let shift_left _ _ _ = top()*)

  let shift_left ik x y =
    let res = shift_left ik x y in
    if M.tracing then  M.trace "shifting" "Cong. shift_left : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let mul ?(no_ov=false) ik x y =
    match x, y with
    | None, None -> bot ()
    | None, _ | _, None ->
       raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some (c2, m2) when no_ov ->
       Some (c1 *: c2, gcd (c1 *: m2) (gcd (m1 *: c2) (m1 *: m2)))
    | Some (c1, m1), Some (c2, m2)
         when m1 =: Ints_t.zero && m2 =: Ints_t.zero && not (Cil.isSigned ik) ->
       Some((c1 *: c2) %: max_int ik, Ints_t.zero)
    | _ -> top ()

  let mul ?no_ov ik x y =
    let res = mul ik x y in
    if M.tracing then  M.trace "mything" "Cong. mul : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let neg ik x =
    match x with
    | None -> bot()
    | Some _ ->  mul ik (of_int ik (Ints_t.of_int (-1))) x


  let add ?(no_ov=false) ik x y =
    match (x, y) with
    | None, None -> bot ()
    | None, _ | _, None ->
       raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some (c2, m2) when no_ov -> normalize (Some (c1 +: c2, gcd m1 m2))
    | Some (c1, m1), Some (c2, m2)
         when m1 =: Ints_t.zero && m2 =: Ints_t.zero && not (Cil.isSigned ik) ->
       Some((c1 +: c2) %: max_int ik, Ints_t.zero)
    | _ -> top ()


  let add ?no_ov ik x y =
    let res = add ?no_ov ik x y in
    if M.tracing then
      M.trace "mything" "Cong. add : %a %a becomes %a \n" pretty x pretty y
        pretty res ;
    res

  let sub ?no_ov ik x y = add ?no_ov ik x (neg ik y)

  let sub ?no_ov ik x y =
    let res = sub ?no_ov ik x y in
    if M.tracing then
      M.trace "mything" "Cong. sub : %a %a becomes %a \n" pretty x pretty y
        pretty res ;
    res

  let bitnot ik x = match x with
    | None -> None
    | Some (c, m) -> if (Cil.isSigned ik) then sub ik (neg ik x) one else Some (Ints_t.sub (max_int ik) c, m)

  let bitand ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') when m =: Ints_t.zero && m' =: Ints_t.zero -> Some (Ints_t.bitand c c', Ints_t.zero)
    | Some (c, m), Some (c', m') -> if (c =: Ints_t.zero && c' =: Ints_t.zero && m =: Ints_t.one && m' =: Ints_t.one)
      then top() else Some ((Ints_t.bitand c c'), Ints_t.shift_left Ints_t.one (Ints_t.to_int (min m m')))

   let bitor ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') when m =: Ints_t.zero && m' =: Ints_t.zero -> Some (Ints_t.bitor c c', Ints_t.zero)
    | Some (c, m), Some (c', m') -> if (c =: Ints_t.zero && c' =: Ints_t.zero && m =: Ints_t.one && m' =: Ints_t.one)
      then top() else Some ((Ints_t.bitor c c'), Ints_t.shift_left Ints_t.one (Ints_t.to_int (min m m')))

   let bitxor ik x y = match x, y with
    | None, None -> None
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c, m), Some (c', m') when m =: Ints_t.zero && m' =: Ints_t.zero -> Some (Ints_t.bitxor c c', Ints_t.zero)
    | Some (c, m), Some (c', m') -> if (c =: Ints_t.zero && c' =: Ints_t.zero && m =: Ints_t.one && m' =: Ints_t.one)
      then top() else Some ((Ints_t.bitxor c c'), Ints_t.shift_left Ints_t.one (Ints_t.to_int (min m m')))

  let rem ik x y =
    match x, y with
    | None, None -> bot()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1, m1), Some(c2, m2) -> (if m2 =: Ints_t.zero then (if (c2 |: m1) && (c2 |: c1) then zero else normalize(Some(c1, (gcd m1 c2))))
        else normalize (Some(c1, gcd m1 (gcd c2 m2))))

  let rem ik x y = let res = rem ik x y in
    if M.tracing then  M.trace "mything" "Cong. rem : %a %a -> %a \n" pretty x pretty y pretty res;
    res

  let div ?(no_ov=false) ik x y =
    match x,y with
    | None, None -> bot ()
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, x when leq zero x -> top () (*TODO warn about undefined behaviour when x equals zero*)
    | Some(c1, m1), Some(c2, m2) when not no_ov && m2 = Ints_t.zero && c2 = Ints_t.neg Ints_t.one -> top ()
    | Some(c1, m1), Some(c2, m2) when m1 =: Ints_t.zero && m2 = Ints_t.zero -> Some(c1 /: c2, Ints_t.zero)
    | Some(c1, m1), Some(c2, m2) when m2 =: Ints_t.zero ->  if (c2 |: m1) && (c2 |: c1) then Some(c1 /: c2, m1 /: c2) else top ()
    | _, _ -> top ()


  let div ?no_ov ik x y =
    let res = div ?no_ov ik x y in
    if M.tracing then
      M.trace "mything" "Cong. div : %a %a -> %a \n" pretty x pretty y pretty
        res ;
    res

  let ne ik i1 i2 = if meet ik i1 i2 = None then of_bool ik true else top_bool

  let eq ik (i1: t) (i2: t) = if meet ik i1 i2 <> None then top_bool else of_bool ik false

  let ge ik x y = match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1,m1), Some (c2,m2) -> if (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) then
        if c1 >=: c2 then of_bool ik true else of_bool ik false
      else top_bool

  let ge ik x y =
    let res = ge ik x y in
    if M.tracing then  M.trace "mything" "Cong. greater or equal : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let le ik x y = match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1,m1), Some (c2,m2) -> if (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) then
        if c1 <=: c2 then of_bool ik true else of_bool ik false
      else top_bool

  let le ik x y =
    let res = le ik x y in
    if M.tracing then  M.trace "mything" "Cong. less or equal : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let gt ik x y = match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1,m1), Some (c2,m2) -> if (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) then
        if c1 >: c2 then of_bool ik true else of_bool ik false
      else top_bool

  let gt ik x y =
    let res = gt ik x y in
    if M.tracing then  M.trace "mything" "Cong. greater than : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let lt ik x y = match x, y with
    | None, None -> bot_of ik
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some (c1,m1), Some (c2,m2) -> if (m1 =: Ints_t.zero) && (m2 =: Ints_t.zero) then
        if c1 <: c2 then of_bool ik true else of_bool ik false
      else top_bool

  let lt ik x y =
    let res = lt ik x y in
    if M.tracing then  M.trace "mything" "Cong. less than : %a %a becomes %a \n" pretty x pretty y pretty res;
    res

  let invariant c x = failwith "unimplemented"

  let invariant_ikind c ik x = failwith "unimplemented"

  let arbitrary () =
    let open QCheck in
    let ik = Cil.ILongLong in
    let int_arb = map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 in
    let top_arb = make ~print:show (Gen.return (top ())) in
    let bot_arb = make ~print:show (Gen.return (bot ())) in
    let int_cong_arb = pair int_arb (make (Gen.return Ints_t.zero)) in
    let cong_arb = pair int_arb int_arb in
    oneof
      ((set_print show @@ map (of_pair ik) cong_arb)::
         (set_print show @@ map (of_pair ik) int_cong_arb)::
           top_arb::bot_arb::[])

  let relift x = x

  let refine_with_interval (cong : t) (intv : (int_t * int_t ) option) : t =
    match intv, cong with
    | Some (x, y), Some (c, m) -> None (* TODO: implement *)
    | _ -> cong

  let refine_with_congruence a b = a
  let refine_with_excl_list a b = a
  let refine_with_incl_list a b = a
end

module Congruence = CongruenceFunctor (BI)

(* module Congruence32 =
 *   IntDomWithDefaultIkind
 *     (IntDomLifter (CongruenceFunctor (IntOps.Int64Ops))) (IntIkind) *)

(* The old IntDomList had too much boilerplate since we had to edit every function in S when adding a new domain. With the following, we only have to edit the places where fn are applied, i.e., create, mapp, map, map2. You can search for I3 below to see where you need to extend. *)
(* discussion: https://github.com/goblint/analyzer/pull/188#issuecomment-818928540 *)
module IntDomTupleImpl = struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Batteries
  type int_t = BI.t
  module I1 = DefExc
  module I2 = Interval
  module I3 = Enums
  module I4 = Congruence

  type t = I1.t option * I2.t option * I3.t option * I4.t option
  [@@deriving to_yojson]

  (* The Interval domain can lead to too many contexts for recursive functions (top is [min,max]), but we don't want to drop all ints as with `exp.no-int-context`. TODO better solution? *)
  let no_interval = Tuple4.map2 (const None)

  type 'a m = (module S with type t = 'a)
  type 'a m2 = (module S with type t = 'a and type int_t = int_t )

  (* only first-order polymorphism on functions -> use records to get around monomorphism restriction on arguments *)
  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } (* inject *)
  type 'b poly2_in  = { fi2  : 'a. 'a m2 -> 'b -> 'a } (* inject for functions that depend on int_t *)
  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } (* project *)
  type 'b poly_pr2  = { fp2  : 'a. 'a m2 -> 'a -> 'b } (* project for functions that depend on int_t *)
  type 'b poly2_pr = {f2p: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'b}
  type poly1 = {f1: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a} (* needed b/c above 'b must be different from 'a *)
  type poly2 = {f2: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'a}
  let create r x = (* use where values are introduced *)
    let f n g = if get_bool ("ana.int."^n) then Some (g x) else None in
    f "def_exc" @@ r.fi (module I1), f "interval" @@ r.fi (module I2), f "enums" @@ r.fi (module I3), f "congruence" @@ r.fi (module I4)
  let create2 r x = (* use where values are introduced *)
    let f n g = if get_bool ("ana.int."^n) then Some (g x) else None in
    f "def_exc" @@ r.fi2 (module I1), f "interval" @@ r.fi2 (module I2), f "enums" @@ r.fi2 (module I3), f "congruence" @@ r.fi2 (module I4)

  let no_overflow ik r =
    let ika, ikb = Size.range_big_int ik in
    match I2.minimal r, I2.maximal r with
    | Some ra, Some rb -> ika < ra && rb < ikb
    | _ -> false

  (* map with overflow check *)
  let mapovc ik r (a, b, c, d) =
    let map f ?no_ov = function Some x -> Some (f ?no_ov x) | _ -> None  in
    let intv = map (r.f1 (module I2)) b in
    let no_ov =
      match intv with Some i -> no_overflow ik i | _ -> false
    in
    ( map (r.f1 (module I1)) a
    , intv
    , map (r.f1 (module I3)) c
    , map (r.f1 (module I4)) ~no_ov d )

  let opt_map2 f ?no_ov =
    curry @@ function Some x, Some y -> Some (f ?no_ov x y) | _ -> None

  let to_list x = Tuple4.enum x |> List.of_enum |> List.filter_map identity (* contains only the values of activated domains *)
  let to_list_some x = List.filter_map identity @@ to_list x (* contains only the Some-values of activated domains *)
  let exists, for_all = let f g = g identity % to_list in List.(f exists, f for_all)



  let refine_with_congruence ((a, b, c, d) : t) (cong : (int_t * int_t) option) : t=
    let opt f =
      curry @@ function Some x, y -> Some (f x y) | _ -> None
    in
    ( opt I1.refine_with_congruence a cong
    , opt I2.refine_with_congruence b cong
    , opt I3.refine_with_congruence c cong
    , opt I4.refine_with_congruence d cong )

  let refine_with_interval (a, b, c, d) intv =
    let opt f =
      curry @@ function Some x, y -> Some (f x y) | _ -> None
    in
    ( opt I1.refine_with_interval a intv
    , opt I2.refine_with_interval b intv
    , opt I3.refine_with_interval c intv
    , opt I4.refine_with_interval d intv )

  let refine_with_excl_list (a, b, c, d) excl =
    let opt f =
      curry @@ function Some x, y -> Some (f x y) | _ -> None
    in
    ( opt I1.refine_with_excl_list a excl
    , opt I2.refine_with_excl_list b excl
    , opt I3.refine_with_excl_list c excl
    , opt I4.refine_with_excl_list d excl )

  let refine_with_incl_list (a, b, c, d) incl =
    let opt f =
      curry @@ function Some x, y -> Some (f x y) | _ -> None
    in
    ( opt I1.refine_with_incl_list a incl
    , opt I2.refine_with_incl_list b incl
    , opt I3.refine_with_incl_list c incl
    , opt I4.refine_with_incl_list d incl )


  let refine ((a, b, c, d ) : t ) : t =
    let maybe reffun domtup dom =
      match dom with Some y -> reffun domtup y | _ -> domtup
    in
    maybe refine_with_interval (maybe refine_with_congruence (a, b, c, d) d) b



  (* map2 with overflow check *)
  let map2ovc ik r (xa, xb, xc, xd) (ya, yb, yc, yd) =
    let intv = opt_map2 (r.f2 (module I2)) xb yb in
    let no_ov =
      match intv with Some i -> no_overflow ik i | _ -> false
    in
    refine
      ( opt_map2 (r.f2 (module I1)) xa ya
      , intv
      , opt_map2 (r.f2 (module I3)) xc yc
      , opt_map2 (r.f2 (module I4)) ~no_ov xd yd )


let r ((c, i) : (BI.t * BI.t) * BI.t) : BI.t = match c with
    | p, m ->  if BI.compare m BI.zero < 0 then BI.rem (BI.add i (BI.sub p i)) (BI.neg(m))
      else BI.rem (BI.add i (BI.sub p i)) m

  let l ((c, i) : (BI.t * BI.t) * BI.t) : BI.t = match c with
    | p, m ->  if BI.compare m BI.zero < 0 then BI.rem (BI.sub i (BI.sub i p)) (BI.neg(m))
      else BI.rem (BI.sub i (BI.sub i p)) m

(*ToDo: Check interval infinity *)
(*  let refineIntCong ((i1, i2, i3, i4) : I1.t option * I2.t option * I3.t option * I4.t option)*)
(*      : I1.t option * I2.t option * I3.t option * I4.t option =*)
(*     match i2, i4 with*)
(*     | Some(None), _ | _, Some(None) -> (i1, Some(None), i3, Some(None))*)
(*     | Some(Some(x, y)), Some(Some(c, m)) ->*)
(*        let rca = r((c, m), x) in*)
(*        let lcb = l((c, m), y) in*)
(*        if m = BI.zero && (c < x || c > y) then (i1, Some(None), i3, Some(None))*)
(*        else if m = BI.zero then (i1, Some(Some(c, c)), i3, Some(Some(c, BI.zero)))*)
(*        else if rca > lcb then (i1, Some(None), i3, Some(None))*)
(*        else if rca < lcb then (i1, Some(Some(rca, rca)), i3, Some(Some(rca, BI.zero)))   (*ToDo: doulbe check *)*)
(*        else (i1, i2, i3, i4)*)
(*     | _ -> (i1, i2, i3, i4)*)

  (* (\* Add refinement functions here *\)
   * let reffuns = []
   *
   * let rec refine l (a, b, c, d) =
   *   (\* match a with
   *    * | Some defe ->
   *    *    match defe with
   *    *    | `Excluded -> (a, b, c, d)
   *    *    | _ -> (a, b, c, d)
   *    * | None -> (a, b, c, d) *\)
   *   match l with [] -> (a, b, c, d) | h :: t -> refine t @@ h (a, b, c, d) *)


  let mapp r (a, b, c, d) =
    let map = BatOption.map in
    ( map (r.fp (module I1)) a
    , map (r.fp (module I2)) b
    , map (r.fp (module I3)) c
    , map (r.fp (module I4)) d)

  (* let mapp r (a, b, c, d) =
   *   (\* refine reffuns *\)
   *   let map = BatOption.map in
   *   let intv = map (r.fp (module I2)) b in
   *   let cong = map (r.fp (module I4)) d in
   *   (map (r.fp (module I1)) a
   *   , I2.refine_with_congruence intv cong
   *   , map (r.fp (module I3)) c
   *   , cong) *)


  let mapp2 r (a, b, c, d) =
      BatOption.
        ( map (r.fp2 (module I1)) a
        , map (r.fp2 (module I2)) b
        , map (r.fp2 (module I3)) c
        , map (r.fp2 (module I4)) d )

  let map r (a, b, c, d) =
    refine
      BatOption.
        ( map (r.f1 (module I1)) a
        , map (r.f1 (module I2)) b
        , map (r.f1 (module I3)) c
        , map (r.f1 (module I4)) d )

  let map2 r (xa, xb, xc, xd) (ya, yb, yc, yd) =
    refine
      ( opt_map2 (r.f2 (module I1)) xa ya
      , opt_map2 (r.f2 (module I2)) xb yb
      , opt_map2 (r.f2 (module I3)) xc yc
      , opt_map2 (r.f2 (module I4)) xd yd )

  let map2p r (xa, xb, xc, xd) (ya, yb, yc, yd) =
      ( opt_map2 (r.f2p (module I1)) xa ya
      , opt_map2 (r.f2p (module I2)) xb yb
      , opt_map2 (r.f2p (module I3)) xc yc
      , opt_map2 (r.f2p (module I4)) xd yd )
  let name () = "intdomtuple"

  (* f0: constructors *)
  let top () = create { fi = fun (type a) (module I:S with type t = a) -> I.top } ()
  let bot () = create { fi = fun (type a) (module I:S with type t = a) -> I.bot } ()
  let top_of = create { fi = fun (type a) (module I:S with type t = a) -> I.top_of }
  let bot_of = create { fi = fun (type a) (module I:S with type t = a) -> I.bot_of }
  let of_bool ik = create { fi = fun (type a) (module I:S with type t = a) -> I.of_bool ik }
  let of_excl_list ik = create2 { fi2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.of_excl_list ik}
  let of_int ik = create2 { fi2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.of_int ik }
  let starting ik = create2 { fi2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.starting ik }
  let ending ik = create2 { fi2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.ending ik }
  let of_interval ik = create2 { fi2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.of_interval ik }

  (* f1: unary ops *)
  let neg ik =
    map {f1= (fun (type a) (module I : S with type t = a) ?no_ov -> I.neg ik)}

  let bitnot ik =
    map {f1= (fun (type a) (module I : S with type t = a) ?no_ov -> I.bitnot ik)}

  let lognot ik =
    map {f1= (fun (type a) (module I : S with type t = a) ?no_ov -> I.lognot ik)}

  let cast_to ?torg ?no_ov t =
    mapovc t {f1= (fun (type a) (module I : S with type t = a) ?no_ov -> I.cast_to ?torg t)}

  (* fp: projections *)
  let equal_to i x =
    let xs = mapp2 { fp2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.equal_to i } x |> Tuple4.enum |> List.of_enum |> List.filter_map identity in
    if List.mem `Eq xs then `Eq else
    if List.mem `Neq xs then `Neq else
      `Top
  let same show x = let xs = to_list_some x in let us = List.unique xs in let n = List.length us in
    if n = 1 then Some (List.hd xs)
    else (
      if n>1 then Messages.warn_all @@ "Inconsistent state! "^String.concat "," @@ List.map show us; (* do not want to abort, but we need some unsound category *)
      None
    )
  let flat f x = match to_list_some x with [] -> None | xs -> Some (f xs)
  let to_int = same BI.to_string % mapp2 { fp2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.to_int }
  let to_bool = same string_of_bool % mapp { fp = fun (type a) (module I:S with type t = a) -> I.to_bool }
  let to_excl_list x = mapp2 { fp2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.to_excl_list } x |> flat List.concat
  let minimal = flat List.max % mapp2 { fp2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.minimal }
  let maximal = flat List.min % mapp2 { fp2 = fun (type a) (module I:S with type t = a and type int_t = int_t) -> I.maximal }
  (* exists/for_all *)
  let is_bot = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_bot }
  let is_top = for_all % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_top }
  let is_top_of ik = for_all % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_top_of ik }
  let is_int = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_int }
  let is_bool = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_bool }
  let is_excl_list = exists % mapp { fp = fun (type a) (module I:S with type t = a) -> I.is_excl_list }
  (* others *)
  let show = String.concat "; " % to_list % mapp { fp = fun (type a) (module I:S with type t = a) x -> I.name () ^ ":" ^ (I.show x) }
  let hash = List.fold_left (lxor) 0 % to_list % mapp { fp = fun (type a) (module I:S with type t = a) -> I.hash }

  (* f2: binary ops *)
  let join ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.join ik)}

  let meet ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.meet ik)}

  let widen ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.widen ik)}

  let narrow ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.narrow ik)}

  let add ?no_ov ik =
    map2ovc ik
      {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.add ik)}

  let sub ?no_ov ik =
    map2ovc ik
      {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.sub ik)}

  let mul ?no_ov ik =
    map2ovc ik
      {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.mul ik)}

  let div ?no_ov ik =
    map2ovc ik
      {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.div ik)}

  let rem ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.rem ik)}

  let lt ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.lt ik)}

  let gt ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.gt ik)}

  let le ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.le ik)}

  let ge ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.ge ik)}

  let eq ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.eq ik)}

  let ne ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.ne ik)}

  let bitand ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.bitand ik)}

  let bitor ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.bitor ik)}

  let bitxor ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.bitxor ik)}

  let shift_left ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.shift_left ik)}

  let shift_right ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.shift_right ik)}

  let logand ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.logand ik)}

  let logor ik =
    map2 {f2= (fun (type a) (module I : S with type t = a) ?no_ov -> I.logor ik)}

  (* f2p: binary projections *)
  let (%%) f g x = f % (g x) (* composition for binary function g *)

  let leq =
    for_all
    %% map2p {f2p= (fun (type a) (module I : S with type t = a) ?no_ov -> I.leq)}

  let equal =
    for_all
    %% map2p {f2p= (fun (type a) (module I : S with type t = a) ?no_ov -> I.equal)}

  let compare =
    List.fold_left (fun a x -> if x <> 0 then x else a) 0
    % to_list
    %% map2p {f2p= (fun (type a) (module I : S with type t = a) ?no_ov -> I.compare)} (* idea? same impl. as above... *)

  let pretty () = (fun xs -> text "(" ++ (try List.reduce (fun a b -> a ++ text "," ++ b) xs with _ -> nil) ++ text ")") % to_list % mapp { fp = fun (type a) (module I:S with type t = a) -> (* assert sf==I.short; *) I.pretty () } (* NOTE: the version above does something else. also, we ignore the sf-argument here. *)
  (* printing boilerplate *)
  let pretty_diff () (x,y) = dprintf "%a instead of %a" pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  let invariant _ _ = failwith "invariant not implemented for IntDomTupleImpl. Use invariant_ikind instead"

  let invariant_ikind c ik x =
    match to_int x with
    | Some v ->
      (* If definite, output single equality instead of every subdomain repeating same equality *)
      let c_exp = Cil.(Lval (Option.get c.Invariant.lval)) in
      Invariant.of_exp Cil.(BinOp (Eq, c_exp, kintegerCilint ik (Big v), intType))
    | None ->
      let is = to_list (mapp { fp = fun (type a) (module I:S with type t = a) -> I.invariant_ikind c ik } x)
      in List.fold_left (fun a i ->
          Invariant.(a && i)
        ) Invariant.none is

  let arbitrary () = QCheck.(set_print show @@ quad (option (I1.arbitrary ())) (option (I2.arbitrary ())) (option (I3.arbitrary ())) (option (I4.arbitrary ())))
end

module IntDomTuple =
struct
 module I = IntDomLifter (IntDomTupleImpl)
 include I

 let top () = failwith "top in IntDomTuple not supported. Use top_of instead."
 let no_interval (x: I.t) = {x with v = IntDomTupleImpl.no_interval x.v}

end

let of_const (i, ik, str) =
  match str with
  | Some t -> IntDomTuple.of_int ik @@ BI.of_string t
  | None -> IntDomTuple.of_int ik @@ BI.of_int64 i
