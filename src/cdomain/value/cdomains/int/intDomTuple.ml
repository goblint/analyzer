open IntDomain0
open IntervalDomain
open IntervalSetDomain
open DefExcDomain
open EnumsDomain
open CongruenceDomain
open BitfieldDomain
open GoblintCil
open Pretty
open PrecisionUtil

(* The old IntDomList had too much boilerplate since we had to edit every function in S when adding a new domain. With the following, we only have to edit the places where fn are applied, i.e., create, mapp, map, map2. You can search for I3 below to see where you need to extend. *)
(* discussion: https://github.com/goblint/analyzer/pull/188#issuecomment-818928540 *)
module IntDomTupleImpl = struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Batteries
  type int_t = Z.t
  module I1 = SOverflowLifter (DefExc)
  module I2 = Interval
  module I3 = SOverflowLifter (Enums)
  module I4 = SOverflowLifter (Congruence)
  module I5 = IntervalSetFunctor (IntOps.BigIntOps)
  module I6 = BitfieldFunctor (IntOps.BigIntOps)

  type t = I1.t option * I2.t option * I3.t option * I4.t option * I5.t option * I6.t option
  [@@deriving eq, ord, hash]

  let name () = "intdomtuple"

  (* The Interval domain can lead to too many contexts for recursive functions (top is [min,max]), but we don't want to drop all ints as with `ana.base.context.int`. TODO better solution? *)
  let no_interval = GobTuple.Tuple6.map2 (const None)
  let no_intervalSet = GobTuple.Tuple6.map5 (const None)
  let no_bitfield = GobTuple.Tuple6.map6 (const None)

  type 'a m = (module SOverflow with type t = 'a)
  type 'a m2 = (module SOverflow with type t = 'a and type int_t = int_t )

  (* only first-order polymorphism on functions -> use records to get around monomorphism restriction on arguments *)
  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } [@@unboxed] (* inject *)
  type 'b poly2_in  = { fi2  : 'a. 'a m2 -> 'b -> 'a } [@@unboxed] (* inject for functions that depend on int_t *)
  type 'b poly2_in_ovc  = { fi2_ovc  : 'a. 'a m2 -> 'b -> 'a * overflow_info} [@@unboxed] (* inject for functions that depend on int_t *)

  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } [@@unboxed] (* project *)
  type 'b poly_pr2  = { fp2  : 'a. 'a m2 -> 'a -> 'b } [@@unboxed] (* project for functions that depend on int_t *)
  type 'b poly2_pr = {f2p: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'b} [@@unboxed]
  type poly1 = {f1: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a} [@@unboxed] (* needed b/c above 'b must be different from 'a *)
  type poly1_ovc = {f1_ovc: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a * overflow_info } [@@unboxed] (* needed b/c above 'b must be different from 'a *)
  type poly2 = {f2: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'a} [@@unboxed]
  type poly2_ovc = {f2_ovc: 'a. 'a m -> ?no_ov:bool -> 'a -> 'a -> 'a * overflow_info } [@@unboxed]
  type 'b poly3 = { f3: 'a. 'a m -> 'a option } [@@unboxed] (* used for projection to given precision *)
  let create r x ((p1, p2, p3, p4, p5, p6): int_precision) =
    let f b g = if b then Some (g x) else None in
    f p1 @@ r.fi (module I1), f p2 @@ r.fi (module I2), f p3 @@ r.fi (module I3), f p4 @@ r.fi (module I4), f p5 @@ r.fi (module I5), f p6 @@ r.fi (module I6)
  let create r x = (* use where values are introduced *)
    create r x (int_precision_from_node_or_config ())
  let create2 r x ((p1, p2, p3, p4, p5, p6): int_precision) =
    let f b g = if b then Some (g x) else None in
    f p1 @@ r.fi2 (module I1), f p2 @@ r.fi2 (module I2), f p3 @@ r.fi2 (module I3), f p4 @@ r.fi2 (module I4), f p5 @@ r.fi2 (module I5) , f p6 @@ r.fi2 (module I6)
  let create2 r x = (* use where values are introduced *)
    create2 r x (int_precision_from_node_or_config ())


  let overflow_true = {underflow=true; overflow=true}
  let get_overflow = function
    | Some (_, overflow) -> overflow
    | None -> overflow_true
  let active_overflow = Option.is_some

  let check_ov ?(suppress_ovwarn = false) ~op ik intv intv_set bf =
    let {underflow=underflow_intv; overflow=overflow_intv} = get_overflow intv in
    let {underflow=underflow_intv_set; overflow=overflow_intv_set} = get_overflow intv_set in
    let {underflow=underflow_bf; overflow=overflow_bf} = get_overflow bf in
    let underflow = underflow_intv && underflow_intv_set && underflow_bf in
    let overflow = overflow_intv && overflow_intv_set && overflow_bf in
    if not suppress_ovwarn && (active_overflow intv || active_overflow intv_set || active_overflow bf) then
      add_overflow_check ~op ~underflow ~overflow ik;
    not (underflow || overflow)

  let create2_ovc ?(suppress_ovwarn = false) ik r x ((p1, p2, p3, p4, p5, p6): int_precision) =
    let f b g = if b then Some (g x) else None in
    let map x = Option.map fst x in
    let intv =  f p2 @@ r.fi2_ovc (module I2) in
    let intv_set = f p5 @@ r.fi2_ovc (module I5) in
    let bf = f p6 @@ r.fi2_ovc (module I6) in
    ignore (check_ov ~suppress_ovwarn ~op:Internal ik intv intv_set bf);
    map @@ f p1 @@ r.fi2_ovc (module I1), map intv, map @@ f p3 @@ r.fi2_ovc (module I3), map @@ f p4 @@ r.fi2_ovc (module I4), map intv_set, map bf

  let create2_ovc ?(suppress_ovwarn = false) ik r x = (* use where values are introduced *)
    create2_ovc ~suppress_ovwarn ik r x (int_precision_from_node_or_config ())


  let opt_map2 f ?no_ov =
    curry @@ function Some x, Some y -> Some (f ?no_ov x y) | _ -> None

  let to_list (a,b,c,d,e,f) = List.filter_map identity [a;b;c;d;e;f] (* contains only the values of activated domains *)
  let to_list_some x = List.filter_map identity @@ to_list x (* contains only the Some-values of activated domains *)

  let exists = function
    | (Some true, _, _, _, _,_)
    | (_, Some true, _, _, _,_)
    | (_, _, Some true, _, _,_)
    | (_, _, _, Some true, _,_)
    | (_, _, _, _, Some true,_)
    | (_, _, _, _, _, Some true) -> true
    | _ -> false

  let for_all = function
    | (Some false, _, _, _, _,_)
    | (_, Some false, _, _, _,_)
    | (_, _, Some false, _, _,_)
    | (_, _, _, Some false, _,_)
    | (_, _, _, _, Some false,_)
    | (_, _, _, _, _, Some false) -> false
    | _ -> true

  (* f0: constructors *)
  let bot () = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.bot } ()
  let top_of ?bitfield = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.top_of ?bitfield }
  let bot_of = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.bot_of }
  let of_bool ik = create { fi = fun (type a) (module I:SOverflow with type t = a) -> I.of_bool ik }
  let of_excl_list ik = create2 { fi2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_excl_list ik}
  let of_int ?(suppress_ovwarn=false) ik = create2_ovc ~suppress_ovwarn ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_int ik }
  let starting ?(suppress_ovwarn=false) ik = create2_ovc ~suppress_ovwarn ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.starting ik }
  let ending ?(suppress_ovwarn=false) ik = create2_ovc ~suppress_ovwarn ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.ending ik }
  let of_interval ?(suppress_ovwarn=false) ik = create2_ovc ~suppress_ovwarn ik { fi2_ovc = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_interval ik }
  let of_congruence ik = create2 { fi2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_congruence ik }
  let of_bitfield ik = create2 { fi2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.of_bitfield ik }

  let refine_with_congruence ik ((a, b, c, d, e, f) : t) (cong : (int_t * int_t) option) : t=
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_congruence ik a cong
    , opt I2.refine_with_congruence ik b cong
    , opt I3.refine_with_congruence ik c cong
    , opt I4.refine_with_congruence ik d cong
    , opt I5.refine_with_congruence ik e cong
    , opt I6.refine_with_congruence ik f cong
    )

  let refine_with_interval ik (a, b, c, d, e,f) intv =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_interval ik a intv
    , opt I2.refine_with_interval ik b intv
    , opt I3.refine_with_interval ik c intv
    , opt I4.refine_with_interval ik d intv
    , opt I5.refine_with_interval ik e intv
    , opt I6.refine_with_interval ik f intv )

  let refine_with_bitfield ik (a, b, c, d, e,f) bf =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_bitfield ik a bf
    , opt I2.refine_with_bitfield ik b bf
    , opt I3.refine_with_bitfield ik c bf
    , opt I4.refine_with_bitfield ik d bf
    , opt I5.refine_with_bitfield ik e bf
    , opt I6.refine_with_bitfield ik f bf )

  let refine_with_excl_list ik (a, b, c, d, e,f) excl =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_excl_list ik a excl
    , opt I2.refine_with_excl_list ik b excl
    , opt I3.refine_with_excl_list ik c excl
    , opt I4.refine_with_excl_list ik d excl
    , opt I5.refine_with_excl_list ik e excl
    , opt I6.refine_with_excl_list ik f excl )

  let refine_with_incl_list ik (a, b, c, d, e,f) incl =
    let opt f a =
      curry @@ function Some x, y -> Some (f a x y) | _ -> None
    in
    ( opt I1.refine_with_incl_list ik a incl
    , opt I2.refine_with_incl_list ik b incl
    , opt I3.refine_with_incl_list ik c incl
    , opt I4.refine_with_incl_list ik d incl
    , opt I5.refine_with_incl_list ik e incl
    , opt I6.refine_with_incl_list ik f incl )


  let mapp r (a, b, c, d, e, f) =
    let map = BatOption.map in
    ( map (r.fp (module I1)) a
    , map (r.fp (module I2)) b
    , map (r.fp (module I3)) c
    , map (r.fp (module I4)) d
    , map (r.fp (module I5)) e
    , map (r.fp (module I6)) f)


  let mapp2 r (a, b, c, d, e, f) =
    BatOption.
      ( map (r.fp2 (module I1)) a
      , map (r.fp2 (module I2)) b
      , map (r.fp2 (module I3)) c
      , map (r.fp2 (module I4)) d
      , map (r.fp2 (module I5)) e
      , map (r.fp2 (module I6)) f)


  (* exists/for_all *)
  let is_bot = exists % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_bot }
  let is_top_of ik = for_all % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_top_of ik }
  let is_excl_list = exists % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.is_excl_list }

  let map2p r (xa, xb, xc, xd, xe, xf) (ya, yb, yc, yd, ye, yf) =
    ( opt_map2 (r.f2p (module I1)) xa ya
    , opt_map2 (r.f2p (module I2)) xb yb
    , opt_map2 (r.f2p (module I3)) xc yc
    , opt_map2 (r.f2p (module I4)) xd yd
    , opt_map2 (r.f2p (module I5)) xe ye
    , opt_map2 (r.f2p (module I6)) xf yf)

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
      (List.concat vs |> List.sort_uniq Z.compare, (List.min mins, List.max maxs))
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

  let to_bitfield ik x =
    let bf_meet (z1,o1) (z2,o2) = (Z.logand z1 z2, Z.logand o1 o2) in
    let bf_top = (Z.lognot Z.zero, Z.lognot Z.zero) in
    let res_tup = mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.to_bitfield ik } x
    in List.fold bf_meet bf_top (to_list res_tup)

  let same show x = let xs = to_list_some x in let us = List.unique xs in let n = List.length us in
    if n = 1 then Some (List.hd xs)
    else (
      if n>1 then Messages.info ~category:Unsound "Inconsistent state! %a" (Pretty.docList ~sep:(Pretty.text ",") (Pretty.text % show)) us; (* do not want to abort *)
      None
    )
  let to_int = same Z.to_string % mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.to_int }

  let pretty () x =
    match to_int x with
    | Some v when not (GobConfig.get_bool "dbg.full-output") -> Pretty.text (Z.to_string v)
    | _ ->
      mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> (* assert sf==I.short; *) I.pretty () } x
      |> to_list
      |> (fun xs ->
          text "(" ++ (
            try
              List.reduce (fun a b -> a ++ text "," ++ b) xs
            with Invalid_argument _ ->
              nil)
          ++ text ")") (* NOTE: the version above does something else. also, we ignore the sf-argument here. *)

  let refine_functions ik : (t -> t) list =
    let maybe reffun ik domtup dom =
      match dom with Some y -> reffun ik domtup y | _ -> domtup
    in
    [(fun (a, b, c, d, e, f) -> refine_with_excl_list ik (a, b, c, d, e,f) (to_excl_list (a, b, c, d, e,f)));
     (fun (a, b, c, d, e, f) -> refine_with_incl_list ik (a, b, c, d, e,f) (to_incl_list (a, b, c, d, e,f)));
     (fun (a, b, c, d, e, f) -> maybe refine_with_interval ik (a, b, c, d, e, f) b); (* TODO: get interval across all domains with minimal and maximal *)
     (fun (a, b, c, d, e, f) -> maybe refine_with_congruence ik (a, b, c, d, e, f) d);
     (fun (a, b, c, d, e, f) -> maybe refine_with_bitfield ik (a, b, c, d, e, f) f)]

  let refine ik ((a, b, c, d, e, f) : t ) : t =
    let dt = ref (a, b, c, d, e, f) in
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
         if is_bot !dt then (dt := bot_of ik; quit_loop := true);
         if M.tracing then M.trace "cong-refine-loop" "old: %a, new: %a" pretty old_dt pretty !dt;
       done;
     | _ -> ()
    ); !dt


  (* map with overflow check *)
  let mapovc ?(suppress_ovwarn=false) ~op ik r (a, b, c, d, e, f) =
    let map f ?no_ov = function Some x -> Some (f ?no_ov x) | _ -> None  in
    let intv = map (r.f1_ovc (module I2)) b in
    let intv_set = map (r.f1_ovc (module I5)) e in
    let bf = map (r.f1_ovc (module I6)) f in
    let no_ov = check_ov ~suppress_ovwarn ~op ik intv intv_set bf in
    let no_ov = no_ov || should_ignore_overflow ik in
    refine ik
      ( map (fun ?no_ov x -> r.f1_ovc ?no_ov (module I1) x |> fst) a
      , BatOption.map fst intv
      , map (fun ?no_ov x -> r.f1_ovc ?no_ov (module I3) x |> fst) c
      , map (fun ?no_ov x -> r.f1_ovc ?no_ov (module I4) x |> fst) ~no_ov d
      , BatOption.map fst intv_set
      , BatOption.map fst bf)

  (* map2 with overflow check *)
  let map2ovc ~op ik r (xa, xb, xc, xd, xe, xf) (ya, yb, yc, yd, ye, yf) =
    let intv = opt_map2 (r.f2_ovc (module I2)) xb yb in
    let intv_set = opt_map2 (r.f2_ovc (module I5)) xe ye in
    let bf = opt_map2 (r.f2_ovc (module I6)) xf yf in
    let no_ov = check_ov ~op ik intv intv_set bf in
    let no_ov = no_ov || should_ignore_overflow ik in
    refine ik
      ( opt_map2 (fun ?no_ov x y -> r.f2_ovc ?no_ov (module I1) x y |> fst) xa ya
      , BatOption.map fst intv
      , opt_map2 (fun ?no_ov x y -> r.f2_ovc ?no_ov (module I3) x y |> fst) xc yc
      , opt_map2 (fun ?no_ov x y -> r.f2_ovc ?no_ov (module I4) x y |> fst) ~no_ov:no_ov xd yd
      , BatOption.map fst intv_set
      , BatOption.map fst bf)

  let map ik r (a, b, c, d, e, f) =
    refine ik
      BatOption.
        ( map (r.f1 (module I1)) a
        , map (r.f1 (module I2)) b
        , map (r.f1 (module I3)) c
        , map (r.f1 (module I4)) d
        , map (r.f1 (module I5)) e
        , map (r.f1 (module I6)) f)

  let map2 ?(norefine=false) ik r (xa, xb, xc, xd, xe, xf) (ya, yb, yc, yd, ye, yf) =
    let r =
      ( opt_map2 (r.f2 (module I1)) xa ya
      , opt_map2 (r.f2 (module I2)) xb yb
      , opt_map2 (r.f2 (module I3)) xc yc
      , opt_map2 (r.f2 (module I4)) xd yd
      , opt_map2 (r.f2 (module I5)) xe ye
      , opt_map2 (r.f2 (module I6)) xf yf)
    in
    if norefine then r else refine ik r


  (* f1: unary ops *)
  let neg ?no_ov ik =
    mapovc ~op:(Unop Neg) ik {f1_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.neg ?no_ov ik)}

  let lognot ik =
    map ik {f1 = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.lognot ik)}

  let c_lognot ik =
    map ik {f1 = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.c_lognot ik)}

  let cast_to ?(suppress_ovwarn=false) ?torg ?no_ov t =
    mapovc ~suppress_ovwarn ~op:Cast t {f1_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.cast_to ?torg ?no_ov t)}

  (* fp: projections *)
  let equal_to i x =
    let xs = mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.equal_to i } x |> to_list in
    if List.mem `Eq xs then `Eq else
    if List.mem `Neq xs then `Neq else
      `Top

  let to_bool = same string_of_bool % mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.to_bool }
  let minimal = flat (List.max ~cmp:Z.compare) % mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.minimal }
  let maximal = flat (List.min ~cmp:Z.compare) % mapp2 { fp2 = fun (type a) (module I:SOverflow with type t = a and type int_t = int_t) -> I.maximal }
  (* others *)
  let show x =
    match to_int x with
    | Some v  when not (GobConfig.get_bool "dbg.full-output") -> Z.to_string v
    | _ -> mapp { fp = fun (type a) (module I:SOverflow with type t = a) x -> I.name () ^ ":" ^ (I.show x) } x
           |> to_list
           |> String.concat "; "
  let to_yojson = [%to_yojson: Yojson.Safe.t list] % to_list % mapp { fp = fun (type a) (module I:SOverflow with type t = a) x -> I.to_yojson x }

  (* `map/opt_map` are used by `project` *)
  let opt_map b f =
    curry @@ function None, true -> f | x, y when y || b -> x | _ -> None
  let map ~keep r (i1, i2, i3, i4, i5, i6) (b1, b2, b3, b4, b5, b6) =
    ( opt_map keep (r.f3 (module I1)) i1 b1
    , opt_map keep (r.f3 (module I2)) i2 b2
    , opt_map keep (r.f3 (module I3)) i3 b3
    , opt_map keep (r.f3 (module I4)) i4 b4
    , opt_map keep (r.f3 (module I5)) i5 b5
    , opt_map keep (r.f3 (module I6)) i6 b6)

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
    map2ovc ~op:(Binop PlusA) ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.add ?no_ov ik)}

  let sub ?no_ov ik =
    map2ovc ~op:(Binop MinusA) ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.sub ?no_ov ik)}

  let mul ?no_ov ik =
    map2ovc ~op:(Binop Mult) ik
      {f2_ovc = (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.mul ?no_ov ik)}

  let div ?no_ov ik =
    map2ovc ~op:(Binop Div) ik
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

  let logand ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.logand ik)}

  let logor ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.logor ik)}

  let logxor ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.logxor ik)}

  let shift_left ik =
    map2ovc ~op:(Binop Shiftlt) ik {f2_ovc= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.shift_left ik)}

  let shift_right ik =
    map2ovc ~op:(Binop Shiftrt) ik {f2_ovc= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.shift_right ik)}

  let c_logand ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.c_logand ik)}

  let c_logor ik =
    map2 ik {f2= (fun (type a) (module I : SOverflow with type t = a) ?no_ov -> I.c_logor ik)}


  (* printing boilerplate *)
  let pretty_diff () (x,y) = dprintf "%a instead of %a" pretty x pretty y
  let printXml f x =
    match to_int x with
    | Some v when not (GobConfig.get_bool "dbg.full-output") -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Z.to_string v)
    | _ -> BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  let invariant_ikind e ik ((_, _, _, x_cong, x_intset, x_bf) as x) =
    (* TODO: do refinement before to ensure incl_list being more precise than intervals, etc (https://github.com/goblint/analyzer/pull/1517#discussion_r1693998515), requires refine functions to actually refine that *)
    let simplify_int fallback =
      match to_int x with
      | Some v ->
        (* If definite, output single equality instead of every subdomain repeating same equality (or something less precise). *)
        IntInvariant.of_int e ik v
      | None ->
        fallback ()
    in
    let simplify_all () =
      match to_incl_list x with
      | Some ps ->
        (* If inclusion set, output disjunction of equalities because it subsumes interval(s), exclusion set and congruence. *)
        IntInvariant.of_incl_list e ik ps
      | None ->
        (* Get interval bounds from all domains (intervals and exclusion set ranges). *)
        let min = minimal x in
        let max = maximal x in
        let ns = Option.map fst (to_excl_list x) |? [] in (* Ignore exclusion set bit range, known via interval bounds already. *)
        (* "Refine" out-of-bounds exclusions for simpler output. *)
        let ns = Option.map_default (fun min -> List.filter (Z.leq min) ns) ns min in
        let ns = Option.map_default (fun max -> List.filter (Z.geq max) ns) ns max in
        Invariant.(
          IntInvariant.of_interval_opt e ik (min, max) && (* Output best interval bounds once instead of multiple subdomains repeating them (or less precise ones). *)
          IntInvariant.of_excl_list e ik ns &&
          Option.map_default (I4.invariant_ikind e ik) Invariant.none x_cong && (* Output congruence as is. *)
          Option.map_default (I5.invariant_ikind e ik) Invariant.none x_intset && (* Output interval sets as is. *)
          Option.map_default (I6.invariant_ikind e ik) Invariant.none x_bf (* Output bitmask as is. *)
        )
    in
    let simplify_none () =
      let is = to_list (mapp { fp = fun (type a) (module I:SOverflow with type t = a) -> I.invariant_ikind e ik } x) in
      List.fold_left (fun a i ->
          Invariant.(a && i)
        ) (Invariant.top ()) is
    in
    match GobConfig.get_string "ana.base.invariant.int.simplify" with
    | "none" -> simplify_none ()
    | "int" -> simplify_int simplify_none
    | "all" -> simplify_int simplify_all
    | _ -> assert false

  let arbitrary ik = QCheck.(set_print show @@ tup6 (option (I1.arbitrary ik)) (option (I2.arbitrary ik)) (option (I3.arbitrary ik)) (option (I4.arbitrary ik)) (option (I5.arbitrary ik)) (option (I6.arbitrary ik)))

  let relift (a, b, c, d, e, f) =
    (Option.map I1.relift a, Option.map I2.relift b, Option.map I3.relift c, Option.map I4.relift d, Option.map I5.relift e, Option.map I6.relift f)
end

module IntDomTuple =
struct
  module I = IntDomLifter (IntDomTupleImpl)
  include I

  let top () = failwith "top in IntDomTuple not supported. Use top_of instead."
  let no_interval (x: I.t) = {x with v = IntDomTupleImpl.no_interval x.v}

  let no_intervalSet (x: I.t) = {x with v = IntDomTupleImpl.no_intervalSet x.v}

  let no_bitfield (x: I.t) = {x with v = IntDomTupleImpl.no_bitfield x.v}
end

let of_const (i, ik, str) = IntDomTuple.of_int ~suppress_ovwarn:true ik i