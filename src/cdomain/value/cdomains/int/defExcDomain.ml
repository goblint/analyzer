open IntDomain0
open GoblintCil


module BISet = struct
  include SetDomain.Make (IntOps.BigIntOps)
  let is_singleton s = cardinal s = 1

  let map_reduce f g s =
    s |> to_seq |> Seq.map f |> BatSeq.reduce g
end

(* The module [Exclusion] constains common functionality about handling of exclusion sets between [DefExc] and [Enums] *)
module Exclusion =
struct
  module R = IntervalArith (IntOps.NIntOps)
  (* We use these types for the functions in this module to make the intended meaning more explicit *)
  type t = Exc of BISet.t * R.t
  type inc = Inc of BISet.t [@@unboxed]
  let max_of_range r = Size.max_from_bit_range (R.maximal r)
  let min_of_range r = Size.min_from_bit_range (R.minimal r)
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

module DefExc : S with type int_t = Z.t = (* definite or set of excluded values *)
struct
  module S = BISet
  module R = Exclusion.R (* range for exclusion *)

  let size t = let a,b = Size.bits t in -a,b


  type t = [
    | `Excluded of S.t * R.t (** Bit range always includes 0. *)
    | `Definite of Z.t
    | `Bot
  ] [@@deriving eq, ord, hash]
  type int_t = Z.t
  let name () = "def_exc"


  let overflow_range = (-999, 999) (* Since there is no top ikind we use a range that includes both IInt128 [-127,127] and IUInt128 [0,128]. Only needed for intermediate range computation on longs. Correct range is set by cast. *)
  let top_overflow () = `Excluded (S.empty (), overflow_range)
  let bot () = `Bot
  let top_of ?bitfield ik =
    match bitfield with
    | Some b when b <= Z.numbits (Size.range ik |> snd) ->
      let range =
        if Cil.isSigned ik then
          (-(b - 1), b)
        else
          (0, b)
      in
      `Excluded (S.empty (), range)
    | _ -> `Excluded (S.empty (), size ik)
  let bot_of ik = bot ()

  let show x =
    let short_size x = "("^R.show x^")" in
    match x with
    | `Bot -> "Error int"
    | `Definite x -> Z.to_string x
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
    if Z.compare i Z.zero < 0 then
      let lowerb = Exclusion.min_of_range r in
      Z.compare lowerb i <= 0
    else
      let upperb = Exclusion.max_of_range r in
      Z.compare i upperb <= 0

  let equal_to i = function
    | `Bot -> failwith "unsupported: equal_to with bottom"
    | `Definite x -> if i = x then `Eq else `Neq
    | `Excluded (s,r) -> if S.mem i s then `Neq else `Top

  let cast_to ?(suppress_ovwarn=false) ~kind ?torg ?no_ov ik = function
    | `Excluded (s,r) ->
      let r' = size ik in
      if R.leq r r' then (* upcast -> no change *)
        `Excluded (s, r)
      else if ik = IBool then (* downcast to bool *)
        if S.mem Z.zero s then
          `Definite Z.one
        else
          `Excluded (S.empty(), r')
      else
        (* downcast: may overflow *)
        (* let s' = S.map (Size.cast ik) s in *)
        (* We want to filter out all i in s' where (t)x with x in r could be i. *)
        (* Since this is hard to compute, we just keep all i in s' which overflowed, since those are safe - all i which did not overflow may now be possible due to overflow of r. *)
        (* S.diff s' s, r' *)
        (* The above is needed for test 21/03, but not sound! See example https://github.com/goblint/analyzer/pull/95#discussion_r483023140 *)
        `Excluded (S.empty (), r')
    | `Definite x -> `Definite (Size.cast ik x)
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
        let mapped_excl = S.map (fun excl -> Size.cast ik excl) s in
        match ik with
        | IBool ->
          begin match S.mem Z.zero mapped_excl, S.mem Z.one mapped_excl with
            | false, false -> `Excluded (mapped_excl, r) (* Not {} -> Not {} *)
            | true, false -> `Definite Z.one (* Not {0} -> 1 *)
            | false, true -> `Definite Z.zero (* Not {1} -> 0 *)
            | true, true -> `Bot (* Not {0, 1} -> bot *)
          end
        | ik ->
          `Excluded (mapped_excl, r)
      )
    | `Definite x ->
      let min, max = Size.range ik in
      (* Perform a wrap-around for unsigned values and for signed values (if configured). *)
      if should_wrap ik then (
        cast_to ~kind:Internal ik v (* TODO: proper castkind *)
      )
      else if Z.compare min x <= 0 && Z.compare x max <= 0 then (
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
        let r = R.join a b in
        `Excluded ((if Z.equal x Z.zero || Z.equal y Z.zero then S.empty () else S.singleton Z.zero), r)
    (* A known value and an exclusion set... the definite value should no
     * longer be excluded: *)
    | `Excluded (s,r), `Definite x
    | `Definite x, `Excluded (s,r) ->
      if not (in_range r x) then
        let a = Size.min_range_sign_agnostic x in
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
      match R.meet r1 r2 with
      | None -> `Bot
      | Some r' ->
        let s' = S.union x y |> S.filter (in_range r') in
        `Excluded (s', r')

  let narrow ik x y = x

  let of_int ik x = norm ik @@ `Definite x
  let to_int x = match x with
    | `Definite x -> Some x
    | _ -> None

  let from_excl ikind (s: S.t) = norm ikind @@ `Excluded (s, size ikind)

  let of_bool_cmp ik x = of_int ik (if x then Z.one else Z.zero)
  let of_bool = of_bool_cmp
  let to_bool x =
    match x with
    | `Definite x -> Some (IntOps.BigIntOps.to_bool x)
    | `Excluded (s,r) when S.mem Z.zero s -> Some true
    | _ -> None
  let top_bool = `Excluded (S.empty (), (0, 1))

  let of_interval ik (x,y) =
    if Z.compare x y = 0 then
      of_int ik x
    else
      let a, b = Size.min_range_sign_agnostic x, Size.min_range_sign_agnostic y in
      let r = R.join a b in
      let ex = if Z.gt x Z.zero || Z.lt y Z.zero then S.singleton Z.zero else  S.empty () in
      norm ik @@ (`Excluded (ex, r))

  let to_bitfield ik x =
    match x with
    | `Definite c -> (Z.lognot c, c)
    | _ when Cil.isSigned ik ->
      let one_mask = Z.lognot Z.zero in
      (one_mask, one_mask)
    | _ ->
      let one_mask = Z.lognot Z.zero in
      let ik_mask = snd (Size.range ik) in
      (one_mask, ik_mask)

  let starting ikind x =
    let _,u_ik = Size.range ikind in
    of_interval ikind (x, u_ik)

  let ending ikind x =
    let l_ik,_ = Size.range ikind in
    of_interval ikind (l_ik, x)

  let of_excl_list t l =
    let r = size t in (* elements in l are excluded from the full range of t! *)
    `Excluded (S.of_list l, r)

  let is_excl_list l = match l with `Excluded _ -> true | _ -> false
  let to_excl_list (x:t) = match x with
    | `Definite _ -> None
    | `Excluded (s,r) -> Some (S.elements s, r)
    | `Bot -> None

  let to_incl_list x = match x with
    | `Definite x -> Some [x]
    | `Excluded _ -> None
    | `Bot -> None

  let apply_range f r = (* apply f to the min/max of the old range r to get a new range *)
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
      | `Excluded _, `Excluded _ -> top_overflow ()
      (* The good case: *)
      | `Definite x, `Definite y ->
        (try `Definite (f x y) with | Division_by_zero -> top_overflow ())
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
    | `Excluded _, `Excluded _ -> top_overflow ()
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
    | `Excluded _, `Excluded _ -> top_of IInt
    (* Is x equal to an exclusion set, if it is a member then NO otherwise we
     * don't know: *)
    | `Definite x, `Excluded (s,r) -> if S.mem x s then of_bool IInt false else top_of IInt
    | `Excluded (s,r), `Definite x -> if S.mem x s then of_bool IInt false else top_of IInt
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool IInt (x = y)
    | `Bot, `Bot -> `Bot
    | _ ->
      (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))

  (* The inequality check: *)
  let ne ik x y = match x,y with
    (* Not much to do with two exclusion sets: *)
    | `Excluded _, `Excluded _ -> top_of IInt
    (* Is x unequal to an exclusion set, if it is a member then Yes otherwise we
     * don't know: *)
    | `Definite x, `Excluded (s,r) -> if S.mem x s then of_bool IInt true else top_of IInt
    | `Excluded (s,r), `Definite x -> if S.mem x s then of_bool IInt true else top_of IInt
    (* The good case: *)
    | `Definite x, `Definite y -> of_bool IInt (x <> y)
    | `Bot, `Bot -> `Bot
    | _ ->
      (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
      raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))

  let neg ?no_ov ik (x :t) = norm ik @@ lift1 Z.neg ik x
  let add ?no_ov ik x y = norm ik @@ lift2_inj Z.add ik x y

  let sub ?no_ov ik x y = norm ik @@ lift2_inj Z.sub ik x y
  let mul ?no_ov ik x y = norm ik @@ match x, y with
    | `Definite z, (`Excluded _ | `Definite _) when Z.equal z Z.zero -> x
    | (`Excluded _ | `Definite _), `Definite z when Z.equal z Z.zero -> y
    | `Definite a, `Excluded (s,r)
    (* Integer multiplication with even numbers is not injective. *)
    (* Thus we cannot exclude the values to which the exclusion set would be mapped to. *)
    | `Excluded (s,r),`Definite a when Z.equal (Z.rem a (Z.of_int 2)) Z.zero -> `Excluded (S.empty (), apply_range (Z.mul a) r)
    | _ -> lift2_inj Z.mul ik x y
  let div ?no_ov ik x y = lift2 Z.div ik x y
  let rem ik x y = lift2 Z.rem ik x y

  (* Comparison handling copied from Enums. *)
  let handle_bot x y f = match x, y with
    | `Bot, `Bot -> `Bot
    | `Bot, _
    | _, `Bot -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, _ -> f ()

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

  let lognot ik x = norm ik @@ match x with
    | `Excluded (s, ((min, max) as r)) ->
      let s' = S.map Z.lognot s in
      let r' =
        if -max <= 0 && -min > 0 then
          (-max, -min)
        else
          apply_range Z.lognot r
      in
      `Excluded (s', r')
    | `Definite x -> `Definite (Z.lognot x)
    | `Bot -> `Bot

  let logand ik x y = norm ik (match x,y with
      | `Excluded (_, (r1, r2)), `Definite i
      | `Definite i, `Excluded (_, (r1, r2)) ->
        if Z.equal i Z.zero then
          `Definite Z.zero
        else if Z.equal i Z.one then
          of_interval IBool (Z.zero, Z.one)
        else (
          match Z.compare i Z.zero >= 0, r1 >= 0 with
          | true, true -> `Excluded (S.empty (), (0, Int.min r2 (Z.numbits i)))
          | true, _ -> `Excluded (S.empty (), (0, Z.numbits i))
          | _, true -> `Excluded (S.empty (), (0, r2))
          | _, _ ->
            let b = Int.max (Z.numbits i) (Int.max (Int.abs r1) (Int.abs r2)) in
            `Excluded (S.empty (), (-b, b))
        )
      | `Excluded (_, ((p1, p2) as p)), `Excluded (_, ((r1, r2) as r)) ->
        begin match p1 >= 0, r1 >= 0 with
          | true, true -> `Excluded (S.empty (), (0, Int.min p2 r2))
          | true, _ -> `Excluded (S.empty (), (0, p2))
          | _, true -> `Excluded (S.empty (), (0, r2))
          | _, _ -> `Excluded (S.empty (), R.join p r)
        end
      (* The good case: *)
      | `Definite x, `Definite y ->
        (try `Definite (Z.logand x y) with | Division_by_zero -> top_of ik)
      | `Bot, `Bot -> `Bot
      | _ ->
        (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
        raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y))))

  let logor ik x y = norm ik (match x,y with
      | `Excluded (_, ((r1, r2) as r)), `Definite i
      | `Definite i, `Excluded (_, ((r1, r2) as r)) ->
        if Z.compare i Z.zero >= 0 then
          `Excluded (S.empty (), R.join r (0, Z.numbits i))
        else (
          let b = Int.max (Z.numbits i) (Int.max (Int.abs r1) (Int.abs r2)) in
          `Excluded (S.empty (), (-b, b))
        )
      | `Excluded (_, r1), `Excluded (_, r2) -> `Excluded (S.empty (), R.join r1 r2)
      | `Definite x, `Definite y ->
        (try `Definite (Z.logor x y) with | Division_by_zero -> top_of ik)
      | `Bot, `Bot -> `Bot
      | _ ->
        (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
        raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y))))

  let logxor ik x y = norm ik (match x,y with
      | `Definite i, `Excluded (_, (r1, r2))
      | `Excluded (_, (r1, r2)), `Definite i ->
        let b = Int.max (Z.numbits i) (Int.max (Int.abs r1) (Int.abs r2)) in
        if r1 >= 0 && Z.compare i Z.zero >= 0 then
          `Excluded (S.empty (), (0, b))
        else
          `Excluded (S.empty (), (-b, b))
      | `Excluded (_, (p1, p2)), `Excluded (_, (r1, r2)) ->
        if p1 >= 0 && r1 >= 0 then
          `Excluded (S.empty (), (0, Int.max p2 r2))
        else (
          let b = List.fold_left Int.max 0 (List.map Int.abs [p1; p2; r1; r2]) in
          `Excluded (S.empty (), (-b, b))
        )
      (* The good case: *)
      | `Definite x, `Definite y ->
        (try `Definite (Z.logxor x y) with | Division_by_zero -> top_of ik)
      | `Bot, `Bot -> `Bot
      | _ ->
        (* If only one of them is bottom, we raise an exception that eval_rv will catch *)
        raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y))))

  let shift (shift_op: int_t -> int -> int_t) (ik: Cil.ikind) (x: t) (y: t) =
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
      norm ik @@ lift2 shift_op_big_int ik x y

  let shift_left =
    shift Z.shift_left

  let shift_right =
    shift Z.shift_right
  (* TODO: lift does not treat Not {0} as true. *)
  let c_logand ik x y =
    match to_bool x, to_bool y with
    | Some false, _
    | _, Some false ->
      of_bool ik false
    | _, _ ->
      lift2 IntOps.BigIntOps.c_logand ik x y
  let c_logor ik x y =
    match to_bool x, to_bool y with
    | Some true, _
    | _, Some true ->
      of_bool ik true
    | _, _ ->
      lift2 IntOps.BigIntOps.c_logor ik x y
  let c_lognot ik = eq ik (of_int ik Z.zero)

  let invariant_ikind e ik (x:t) =
    match x with
    | `Definite x ->
      IntInvariant.of_int e ik x
    | `Excluded (s, r) ->
      (* Emit range invariant if tighter than ikind bounds.
         This can be more precise than interval, which has been widened. *)
      let (rmin, rmax) = (Exclusion.min_of_range r, Exclusion.max_of_range r) in
      let ri = IntInvariant.of_interval e ik (rmin, rmax) in
      let si = IntInvariant.of_excl_list e ik (S.elements s) in
      Invariant.(ri && si)
    | `Bot -> Invariant.none

  let arbitrary ik =
    let open QCheck.Iter in
    let excluded s = from_excl ik s in
    let definite x = of_int ik x in
    let shrink = function
      | `Excluded (s, _) -> GobQCheck.shrink (S.arbitrary ()) s >|= excluded (* S TODO: possibly shrink excluded to definite *)
      | `Definite x -> (return `Bot) <+> (GobQCheck.shrink (IntOps.BigIntOps.arbitrary ()) x >|= definite)
      | `Bot -> empty
    in
    QCheck.frequency ~shrink ~print:show [
      20, QCheck.map excluded (S.arbitrary ());
      10, QCheck.map definite (IntOps.BigIntOps.arbitrary ());
      1, QCheck.always `Bot
    ] (* S TODO: decide frequencies *)

  let refine_with_congruence ik a b = a

  let refine_with_bitfield ik x (z,o) =
    match BitfieldDomain.Bitfield.to_int (z,o) with
    | Some y ->
      meet ik x (`Definite y)
    | _ ->
      x

  let refine_with_interval ik a b = match a, b with
    | x, Some(i) -> meet ik x (of_interval ik i)
    | _ -> a
  let refine_with_excl_list ik a b = match a, b with
    | `Excluded (s, r), Some(ls, _) -> meet ik (`Excluded (s, r)) (of_excl_list ik ls) (* TODO: refine with excl range? *)
    | _ -> a
  let refine_with_incl_list ik a b = a

  let project ik p t = t
end
