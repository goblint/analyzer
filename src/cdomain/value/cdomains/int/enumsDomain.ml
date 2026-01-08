open IntDomain0
open DefExcDomain
open GoblintCil


(* Inclusion/Exclusion sets. Go to top on arithmetic operations (except for some easy cases, e.g. multiplication with 0). Joins on widen, i.e. precise integers as long as not derived from arithmetic expressions. *)
module Enums : S with type int_t = Z.t = struct
  module R = Exclusion.R (* range for exclusion *)

  let size t = let a,b = Size.bits t in -a,b

  type t =
    | Inc of BISet.t (* Inclusion set. *)
    | Exc of BISet.t * R.t (** Exclusion set. Bit range always includes 0. *)
  [@@deriving eq, ord, hash]

  type int_t = Z.t
  let name () = "enums"
  let bot () = Inc (BISet.empty ())
  let bot_of ik = bot ()
  let top_bool = Inc (BISet.of_list [Z.zero; Z.one])

  let top_of ?bitfield ik =
    match ik with
    | IBool -> top_bool
    | _ ->
      match bitfield with
      | Some b when b <= Z.numbits (Size.range ik |> snd) ->
        let range =
          if Cil.isSigned ik then
            (-(b - 1), b)
          else
            (0, b)
        in
        Exc (BISet.empty (), range)
      | _ -> Exc (BISet.empty (), size ik)

  let range ik = Size.range ik

(*
  let max_of_range r = Size.max_from_bit_range (Option.get (R.maximal r))
  let min_of_range r = Size.min_from_bit_range (Option.get (R.minimal r))
  let cardinality_of_range r = Z.add (Z.neg (min_of_range r)) (max_of_range r) *)
  let value_in_range (min, max) v = Z.compare min v <= 0 && Z.compare v max <= 0

  let show = function
    | Inc xs when BISet.is_empty xs -> "bot"
    | Inc xs -> "{" ^ (String.concat ", " (List.map Z.to_string (BISet.elements  xs))) ^ "}"
    | Exc (xs,r) -> "not {" ^ (String.concat ", " (List.map Z.to_string (BISet.elements xs))) ^ "} " ^ "("^R.show r^")"

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  (* Normalization function for enums, that handles overflows for Inc.
     As we do not compute on Excl, we do not have to perform any overflow handling for it. *)
  let norm ikind v =
    let min, max = range ikind in
    (* Whether the value v lies within the values of the specified ikind. *)
    let value_in_ikind v =
      Z.compare min v <= 0 && Z.compare v max <= 0
    in
    match v with
    | Inc xs when BISet.for_all value_in_ikind xs -> v
    | Inc xs ->
      if should_wrap ikind then
        Inc (BISet.map (Size.cast ikind) xs)
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
          begin match BISet.mem Z.zero xs, BISet.mem Z.one xs with
            | false, false -> top_bool  (* Not {} -> {0, 1} *)
            | true, false -> Inc (BISet.singleton Z.one) (* Not {0} -> {1} *)
            | false, true -> Inc (BISet.singleton Z.zero) (* Not {1} -> {0} *)
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

  let cast_to ?(suppress_ovwarn=false) ~kind ?torg ?no_ov ik v = norm ik @@ match v with
    | Exc (s,r) ->
      let r' = size ik in
      if R.leq r r' then (* upcast -> no change *)
        Exc (s, r)
      else if ik = IBool then (* downcast to bool *)
        if BISet.mem Z.zero s then
          Inc (BISet.singleton Z.one)
        else
          Exc (BISet.empty(), r')
      else (* downcast: may overflow *)
        Exc ((BISet.empty ()), r')
    | Inc xs ->
      let casted_xs = BISet.map (Size.cast ik) xs in
      if Cil.isSigned ik && not (BISet.equal xs casted_xs)
      then top_of ik (* When casting into a signed type and the result does not fit, the behavior is implementation-defined *)
      else Inc casted_xs

  let of_int ikind x = cast_to ~kind:Internal ikind (Inc (BISet.singleton x)) (* TODO: proper castkind *)

  let of_interval ik (x, y) =
    if Z.compare x y = 0 then
      of_int ik x
    else
      let a, b = Size.min_range_sign_agnostic x, Size.min_range_sign_agnostic y in
      let r = R.join a b in
      let ex = if Z.gt x Z.zero || Z.lt y Z.zero then BISet.singleton Z.zero else BISet.empty () in
      norm ik @@ (Exc (ex, r))

  let join _ x y =
    match x, y with
    | Inc x, Inc y -> Inc (BISet.union x y)
    | Exc (x,r1), Exc (y,r2) -> Exc (BISet.inter x y, R.join r1 r2)
    | Exc (x,r), Inc y
    | Inc y, Exc (x,r) ->
      let r = if BISet.is_empty y
        then r
        else
          let (min_el_range, max_el_range) = Batteries.Tuple2.mapn Size.min_range_sign_agnostic (BISet.min_elt y, BISet.max_elt y) in
          let range = R.join min_el_range max_el_range in
          R.join r range
      in
      Exc (BISet.diff x y, r)

  let meet ik x y =
    match x, y with
    | Inc x, Inc y -> Inc (BISet.inter x y)
    | Exc (x,r1), Exc (y,r2) ->
      begin match R.meet r1 r2 with
        | None -> bot ()
        | Some r ->
          let r_min, r_max = Exclusion.min_of_range r, Exclusion.max_of_range r in
          let filter_by_range = BISet.filter (value_in_range (r_min, r_max)) in
          (* We remove those elements from the exclusion set that do not fit in the range anyway *)
          let excl = BISet.union (filter_by_range x) (filter_by_range y) in
          Exc (excl, r)
      end
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
        Z.compare min_b min_a <= 0 && Z.compare max_a max_b <= 0 &&
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

  let neg ?no_ov = lift1 Z.neg
  let add ?no_ov ikind a b =
    match a, b with
    | Inc z,x when BISet.is_singleton z && BISet.choose z = Z.zero -> x
    | x,Inc z when BISet.is_singleton z && BISet.choose z = Z.zero -> x
    | x,y -> lift2 Z.add ikind x y
  let sub ?no_ov = lift2 Z.sub
  let mul ?no_ov ikind a b =
    match a, b with
    | Inc one,x when BISet.is_singleton one && BISet.choose one = Z.one -> x
    | x,Inc one when BISet.is_singleton one && BISet.choose one = Z.one -> x
    | Inc zero,_ when BISet.is_singleton zero && BISet.choose zero = Z.zero -> a
    | _,Inc zero when BISet.is_singleton zero && BISet.choose zero = Z.zero -> b
    | x,y -> lift2 Z.mul ikind x y

  let div ?no_ov ikind a b = match a, b with
    | x,Inc one when BISet.is_singleton one && BISet.choose one = Z.one -> x
    | _,Inc zero when BISet.is_singleton zero && BISet.choose zero = Z.zero -> top_of ikind
    | Inc zero,_ when BISet.is_singleton zero && BISet.choose zero = Z.zero -> a
    | x,y -> lift2 Z.div ikind x y

  let rem = lift2 Z.rem

  (* TODO: should be used by lognot? *)
  let apply_range f r = (* apply f to the min/max of the old range r to get a new range *)
    let rf m = (size % Size.min_for % f) (m r) in
    let r1, r2 = rf Exclusion.min_of_range, rf Exclusion.max_of_range in
    R.join r1 r2

  let lognot ikind v = norm ikind @@ match v with
    | Inc x when BISet.is_empty x -> v
    | Inc x when BISet.is_singleton x -> Inc (BISet.singleton (Z.lognot (BISet.choose x)))
    | Inc x -> Inc (BISet.map Z.lognot x) (* TODO: don't operate on Inc? *)
    | Exc (s, (min, max)) ->
      let s' = BISet.map Z.lognot s in
      let r' = (-max, -min) in (* TODO: why missing conditions and [apply_range Z.lognot r] compared to DefExc lognot? *)
      Exc (s', r')

  let logand ikind u v = handle_bot u v (fun () ->
      norm ikind @@ match u, v with
      | Inc x, Inc y when BISet.is_singleton x && BISet.is_singleton y ->
        Inc (BISet.singleton (Z.logand (BISet.choose x) (BISet.choose y)))
      | Inc x, Exc (s, (r1, r2))
      | Exc (s, (r1, r2)), Inc x ->
        let f i =
          match Z.compare i Z.zero >= 0, r1 >= 0 with
          | true, true -> (0, Int.min r2 (Z.numbits i))
          | true, _ -> (0, Z.numbits i)
          | _, true -> (0, r2)
          | _, _ ->
            let b = Int.max (Z.numbits i) (Int.max (Int.abs r1) (Int.abs r2)) in
            (-b, b)
        in
        let r' = BISet.map_reduce f R.join x in (* reduce is safe because arguments are not bot here *)
        Exc (BISet.empty (), r')
      | Exc (_, ((p1, p2) as p)), Exc (_, ((r1, r2) as r)) ->
        begin match p1 >= 0, r1 >= 0 with
          | true, true -> Exc (BISet.empty (), (0, Int.min p2 r2))
          | true, _ -> Exc (BISet.empty (), (0, p2))
          | _, true -> Exc (BISet.empty (), (0, r2))
          | _, _ -> Exc (BISet.empty (), R.join p r)
        end
      | _, _ -> top_of ikind
    )

  let logor ikind u v = handle_bot u v (fun () ->
      norm ikind @@ match u, v with
      | Inc x, Inc y when BISet.is_singleton x && BISet.is_singleton y ->
        Inc (BISet.singleton (Z.logor (BISet.choose x) (BISet.choose y)))
      | Inc x, Exc (_, ((r1, r2) as r))
      | Exc (_, ((r1, r2) as r)), Inc x ->
        let f i =
          if Z.compare i Z.zero >= 0 then
            R.join r (0, Z.numbits i)
          else (
            let b = Int.max (Z.numbits i) (Int.max (Int.abs r1) (Int.abs r2)) in
            (-b, b)
          )
        in
        let r' = BISet.map_reduce f R.join x in (* reduce is safe because arguments are not bot here *)
        Exc (BISet.empty (), r')
      | Exc (_, r1), Exc (_, r2) -> Exc (BISet.empty (), R.join r1 r2)
      | _ -> top_of ikind
    )

  let logxor ikind u v = handle_bot u v (fun () ->
      norm ikind @@ match u, v with
      | Inc x, Inc y when BISet.is_singleton x && BISet.is_singleton y ->
        Inc (BISet.singleton (Z.logxor (BISet.choose x) (BISet.choose y)))
      | Inc x, Exc (_, (r1, r2))
      | Exc (_, (r1, r2)), Inc x ->
        let f i =
          let b = Int.max (Z.numbits i) (Int.max (Int.abs r1) (Int.abs r2)) in
          if r1 >= 0 && Z.compare i Z.zero >= 0 then
            (0, b)
          else
            (-b, b)
        in
        let r' = BISet.map_reduce f R.join x in (* reduce is safe because arguments are not bot here *)
        Exc (BISet.empty (), r')
      | Exc (_, (p1, p2)), Exc (_, (r1, r2)) ->
        if p1 >= 0 && r1 >= 0 then
          Exc (BISet.empty (), (0, Int.max p2 r2))
        else (
          let b = List.fold_left Int.max 0 (List.map Int.abs [p1; p2; r1; r2]) in
          Exc (BISet.empty (), (-b, b))
        )
      | _ -> top_of ikind
    )

  let shift (shift_op: int_t -> int -> int_t) (ik: Cil.ikind) (x: t) (y: t) =
    handle_bot x y (fun () ->
        (* BigInt only accepts int as second argument for shifts; perform conversion here *)
        let shift_op_big_int a (b: int_t) =
          let (b : int) = Z.to_int b in
          shift_op a b
        in
        (* If one of the parameters of the shift is negative, the result is undefined *)
        let is_negative = GobOption.for_all (fun x -> Z.lt x Z.zero) in
        if is_negative (minimal x) || is_negative (minimal y) then
          top_of ik
        else
          lift2 shift_op_big_int ik x y)

  let shift_left =
    shift Z.shift_left

  let shift_right =
    shift Z.shift_right

  let of_bool ikind x = Inc (BISet.singleton (if x then Z.one else Z.zero))
  let to_bool  = function
    | Inc e when BISet.is_empty e -> None
    | Exc (e,_) when BISet.is_empty e -> None
    | Inc zero when BISet.is_singleton zero && BISet.choose zero = Z.zero -> Some false
    | Inc xs when BISet.for_all ((<>) Z.zero) xs -> Some true
    | Exc (xs,_) when BISet.exists ((=) Z.zero) xs -> Some true
    | _ -> None
  let to_int = function Inc x when BISet.is_singleton x -> Some (BISet.choose x) | _ -> None

  let to_excl_list = function Exc (x,r) when not (BISet.is_empty x) -> Some (BISet.elements x, r) | _ -> None
  let of_excl_list ik xs =
    let min_ik, max_ik = Size.range ik in
    let exc = BISet.of_list @@ List.filter (value_in_range (min_ik, max_ik)) xs in
    norm ik @@ Exc (exc, size ik)
  let is_excl_list = BatOption.is_some % to_excl_list
  let to_incl_list = function Inc s when not (BISet.is_empty s) -> Some (BISet.elements s) | _ -> None

  let to_bitfield ik x =
    let ik_mask = snd (Size.range ik) in
    let one_mask = Z.lognot Z.zero in
    match x with
    | Inc i when BISet.is_empty i -> (Z.zero, Z.zero)
    | Inc i when BISet.is_singleton i ->
      let o = BISet.choose i in
      let o = if Cil.isSigned ik then o else Z.logand ik_mask o in
      (Z.lognot o, o)
    | Inc i -> BISet.fold (fun o (az, ao) -> (Z.logor (Z.lognot o) az, Z.logor (if Cil.isSigned ik then o else Z.logand ik_mask o) ao)) i (Z.zero, Z.zero)
    | _ when Cil.isSigned ik -> (one_mask, one_mask)
    | _ -> (one_mask, ik_mask)

  let starting ikind x =
    let _,u_ik = Size.range ikind in
    of_interval ikind (x, u_ik)

  let ending ikind x =
    let l_ik,_ = Size.range ikind in
    of_interval ikind (l_ik, x)

  let c_lognot ik x =
    if is_bot x
    then x
    else
      match to_bool x with
      | Some b -> of_bool ik (not b)
      | None -> top_bool

  let c_logand = lift2 IntOps.BigIntOps.c_logand
  let c_logor  = lift2 IntOps.BigIntOps.c_logor
  let maximal = function
    | Inc xs when not (BISet.is_empty xs) -> Some (BISet.max_elt xs)
    | Exc (excl,r) ->
      let rec decrement_while_contained v =
        if BISet.mem v excl
        then decrement_while_contained (Z.pred v)
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
        then increment_while_contained (Z.succ v)
        else v
      in
      let range_min = Exclusion.min_of_range r in
      Some (increment_while_contained range_min)
    | _ (* bottom case *) -> None

  let lt ik x y =
    handle_bot x y (fun () ->
        match minimal x, maximal x, minimal y, maximal y with
        | _, Some x2, Some y1, _ when Z.compare x2 y1 < 0 -> of_bool ik true
        | Some x1, _, _, Some y2 when Z.compare x1 y2 >= 0 -> of_bool ik false
        | _, _, _, _ -> top_bool)

  let gt ik x y = lt ik y x

  let le ik x y =
    handle_bot x y (fun () ->
        match minimal x, maximal x, minimal y, maximal y with
        | _, Some x2, Some y1, _ when Z.compare x2 y1 <= 0 -> of_bool ik true
        | Some x1, _, _, Some y2 when Z.compare x1 y2 > 0 -> of_bool ik false
        | _, _, _, _ -> top_bool)

  let ge ik x y = le ik y x

  let eq ik x y =
    handle_bot x y (fun () ->
        match x, y with
        | Inc xs, Inc ys when BISet.is_singleton xs && BISet.is_singleton ys -> of_bool ik (Z.equal (BISet.choose xs) (BISet.choose ys))
        | _, _ ->
          if is_bot (meet ik x y) then
            (* If the meet is empty, there is no chance that concrete values are equal *)
            of_bool ik false
          else
            top_bool)

  let ne ik x y = c_lognot ik (eq ik x y)

  let invariant_ikind e ik x =
    match x with
    | Inc ps ->
      IntInvariant.of_incl_list e ik (BISet.elements ps)
    | Exc (ns, r) ->
      (* Emit range invariant if tighter than ikind bounds.
         This can be more precise than interval, which has been widened. *)
      let (rmin, rmax) = (Exclusion.min_of_range r, Exclusion.max_of_range r) in
      let ri = IntInvariant.of_interval e ik (rmin, rmax) in
      let nsi = IntInvariant.of_excl_list e ik (BISet.elements ns) in
      Invariant.(ri && nsi)


  let arbitrary ik =
    let open QCheck.Iter in
    let neg s = of_excl_list ik (BISet.elements s) in
    let pos s = norm ik (Inc s) in
    let shrink = function
      | Exc (s, _) -> GobQCheck.shrink (BISet.arbitrary ()) s >|= neg (* S TODO: possibly shrink neg to pos *)
      | Inc s -> GobQCheck.shrink (BISet.arbitrary ()) s >|= pos
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map neg (BISet.arbitrary ());
      10, QCheck.map pos (BISet.arbitrary ());
    ] (* S TODO: decide frequencies *)


  (* One needs to be exceedingly careful here to not cause new elements to appear that are not originally tracked by the domain *)
  (* to avoid breaking the termination guarantee that only constants from the program can appear in exclusion or inclusion sets here *)
  (* What is generally safe is shrinking an inclusion set as no new elements appear here. *)
  (* What is not safe is growing an exclusion set or switching from an exclusion set to an inclusion set *)

  let refine_with_congruence ik a b =
    let contains c m x = if Z.equal m Z.zero then Z.equal c x else Z.equal (Z.rem (Z.sub x c) m) Z.zero in
    match a, b with
    | Inc e, None -> bot_of ik
    | Inc e, Some (c, m) -> Inc (BISet.filter (contains c m) e)
    | _ -> a

  let refine_with_bitfield ik x (z,o) =
    match x, BitfieldDomain.Bitfield.to_int (z,o) with
    | Inc _, Some y ->
      meet ik x (Inc (BISet.singleton y))
    | _ ->
      x

  let refine_with_interval ik a b =
    match a, b with
    | Inc _, None -> bot_of ik
    | Inc e, Some (l, u) -> Inc (BISet.filter (value_in_range (l,u)) e)
    | _ -> a

  let refine_with_excl_list ik a b =
    match a, b with
    | Inc _, Some (ls, _) -> meet ik a (of_excl_list ik ls) (* TODO: refine with excl range? *)
    | _ -> a

  let refine_with_incl_list ik a b =
    match a, b with
    | Inc x, Some (ls) -> meet ik (Inc x) (Inc (BISet.of_list ls))
    | _ -> a

  let project ik p t = t
end
