open IntDomain0

module IntervalFunctor (Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option =
struct
  let name () = "intervals"
  type int_t = Ints_t.t
  type t = (Ints_t.t * Ints_t.t) option [@@deriving eq, ord, hash]
  module IArith = IntervalArith (Ints_t)

  let range ik = BatTuple.Tuple2.mapn Ints_t.of_bigint (Size.range ik)

  let top_of ?bitfield ik =
    match bitfield with
    | Some b when b <= Z.numbits (Size.range ik |> snd) ->
      let signed_lower_bound = Ints_t.neg @@ Ints_t.shift_left Ints_t.one (b - 1) in
      let unsigned_upper_bound = Ints_t.sub (Ints_t.shift_left Ints_t.one b) Ints_t.one in
      if GoblintCil.isSigned ik then
        (* An "int" can also store unsigned int values in a bit-field. Goblint doesn't differentiate between implicit and explicit signed ints.*)
        Some (signed_lower_bound, unsigned_upper_bound)
      else
        Some (Ints_t.zero, unsigned_upper_bound)
    | _ -> Some (range ik)
  let bot () = None
  let bot_of ik = bot () (* TODO: improve *)

  let show = function None -> "bottom" | Some (x,y) -> "["^Ints_t.to_string x^","^Ints_t.to_string y^"]"

  include Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let equal_to i = function
    | None -> failwith "unsupported: equal_to with bottom"
    | Some (a, b) ->
      if a = b && b = i then `Eq else if Ints_t.compare a i <= 0 && Ints_t.compare i b <=0 then `Top else `Neq

  let norm ?(cast=false) ik : (t -> t * overflow_info) = function None -> (None, {underflow=false; overflow=false}) | Some (x,y) ->
    if Ints_t.compare x y > 0 then
      (None,{underflow=false; overflow=false})
    else (
      let (min_ik, max_ik) = range ik in
      let underflow = Ints_t.compare min_ik x > 0 in
      let overflow = Ints_t.compare max_ik y < 0 in
      let ov_info = { underflow; overflow } in
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

  let join_no_norm ik (x:t) y =
    match x, y with
    | None, z | z, None -> z
    | Some (x1,x2), Some (y1,y2) -> Some (Ints_t.min x1 y1, Ints_t.max x2 y2)

  let join ik (x:t) y = norm ik @@ join_no_norm ik x y |> fst

  let meet ik (x:t) y =
    match x, y with
    | None, z | z, None -> None
    | Some (x1,x2), Some (y1,y2) -> norm ik @@ Some (Ints_t.max x1 y1, Ints_t.min x2 y2) |> fst

  (* TODO: change to_int signature so it returns a big_int *)
  let to_int x = Option.bind x (IArith.to_int)
  let of_interval ik (x,y) = norm ik @@ Some (x,y)

  let of_bitfield ik x =
    let min ik (z,o) =
      let signBit = Ints_t.shift_left Ints_t.one ((Size.bit ik) - 1) in
      let signMask = Ints_t.lognot (Ints_t.of_bigint (snd (Size.range ik))) in
      let isNegative = Ints_t.logand signBit o <> Ints_t.zero in
      if GoblintCil.isSigned ik && isNegative then
        Ints_t.logor signMask (Ints_t.lognot z)
      else
        Ints_t.lognot z
    in
    let max ik (z,o) =
      let signBit = Ints_t.shift_left Ints_t.one ((Size.bit ik) - 1) in
      let signMask = Ints_t.of_bigint (snd (Size.range ik)) in
      let isPositive = Ints_t.logand signBit z <> Ints_t.zero in
      if GoblintCil.isSigned ik && isPositive then
        Ints_t.logand signMask o
      else
        o
    in
    fst (norm ik (Some (min ik x, max ik x)))

  let of_int ik (x: int_t) = of_interval ik (x,x)
  let zero = Some IArith.zero
  let one  = Some IArith.one
  let top_bool = Some IArith.top_bool

  let to_bitfield ik z =
    match z with
    | None -> (Ints_t.lognot Ints_t.zero, Ints_t.lognot Ints_t.zero)
    | Some (x,y) ->
      let (z,o) = fst(BitfieldDomain.Bitfield.of_interval ik (Ints_t.to_bigint x, Ints_t.to_bigint y)) in
      (Ints_t.of_bigint z, Ints_t.of_bigint o)

  let of_bool _ik = function true -> one | false -> zero
  let to_bool (a: t) = match a with
    | None -> None
    | Some (l, u) when Ints_t.compare l Ints_t.zero = 0 && Ints_t.compare u Ints_t.zero = 0 -> Some false
    | x -> if leq zero x then None else Some true

  let starting ik n =
    norm ik @@ Some (n, snd (range ik))

  let ending ik n =
    norm ik @@ Some (fst (range ik), n)

  let maximal = function None -> None | Some (x,y) -> Some y
  let minimal = function None -> None | Some (x,y) -> Some x

  let cast_to ~kind ?torg ?no_ov t = norm ~cast:true t (* norm does all overflow handling *)

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

  let min_val_bit_constrained n =
    if Ints_t.equal n Ints_t.zero then
      Ints_t.neg Ints_t.one
    else
      Ints_t.neg @@ Ints_t.shift_left Ints_t.one (Z.numbits (Z.pred (Z.abs @@ Ints_t.to_bigint n)))

  let max_val_bit_constrained n =
    let x =
      if Ints_t.compare n Ints_t.zero < 0 then
        Ints_t.sub (Ints_t.neg n) Ints_t.one
      else
        n
    in
    Ints_t.sub (Ints_t.shift_left Ints_t.one (Z.numbits @@ Z.abs @@ Ints_t.to_bigint x)) Ints_t.one

  let logxor ik i1 i2 =
    match bit (fun _ik -> Ints_t.logxor) ik i1 i2 with
    | result when not (is_top_of ik result) && not (is_bot result) -> result
    | _ ->
      match i1, i2 with
      | Some (x1, x2), Some (y1, y2) ->
        let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
        begin match is_nonneg x1, is_nonneg x2, is_nonneg y1, is_nonneg y2 with
          | true, _, true, _ ->
            of_interval ik (Ints_t.zero, max_val_bit_constrained @@ Ints_t.max x2 y2) |> fst
          | _, false, _, false ->
            let upper = max_val_bit_constrained @@ Ints_t.min x1 y1 in
            of_interval ik (Ints_t.zero, upper) |> fst
          | true, _, _, false
          | _, false, true, _ ->
            let lower =
              List.fold_left Ints_t.min Ints_t.zero
                (List.map min_val_bit_constrained [x1; y1] @ List.map (fun i -> Ints_t.neg @@ Ints_t.add (max_val_bit_constrained i) Ints_t.one) [x2; y2])
            in
            of_interval ik (lower, Ints_t.zero) |> fst
          | _, _, _, _ ->
            let lower =
              List.fold_left Ints_t.min Ints_t.zero
                (List.map min_val_bit_constrained [x1; y1] @ List.map (fun i -> Ints_t.neg @@ Ints_t.add (max_val_bit_constrained i) Ints_t.one) [x2; y2])
            in
            let upper = List.fold_left Ints_t.max Ints_t.zero (List.map max_val_bit_constrained [x1; x2; y1; y2]) in
            of_interval ik (lower, upper) |> fst
        end
      | _ -> top_of ik

  let logand ik i1 i2 =
    match bit (fun _ik -> Ints_t.logand) ik i1 i2 with
    | result when not (is_top_of ik result) && not (is_bot result) -> result
    | _ ->
      match i1, i2 with
      | Some (x1, x2), Some (y1, y2) ->
        let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
        begin match is_nonneg x1, is_nonneg x2, is_nonneg y1, is_nonneg y2 with
          | true, _, true, _ ->
            of_interval ik (Ints_t.zero, Ints_t.min x2 y2) |> fst
          | _, false, _, false ->
            of_interval ik (min_val_bit_constrained @@ Ints_t.min x1 y1, Ints_t.zero) |> fst
          | true, _, _, _ -> of_interval ik (Ints_t.zero, x2) |> fst
          | _, _, true, _ -> of_interval ik (Ints_t.zero, y2) |> fst
          | _, _, _, _ ->
            let lower = min_val_bit_constrained @@ Ints_t.min x1 y1 in
            let upper = Ints_t.max x2 y2 in
            of_interval ik (lower, upper) |> fst
        end
      | _ -> top_of ik

  let logor ik i1 i2 =
    match bit (fun _ik -> Ints_t.logor) ik i1 i2 with
    | result when not (is_top_of ik result) && not (is_bot result) -> result
    | _ ->
      match i1, i2 with
      | Some (x1, x2), Some (y1, y2) ->
        let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
        begin match is_nonneg x1, is_nonneg x2, is_nonneg y1, is_nonneg y2 with
          | true, _, true, _ ->
            of_interval ik (Ints_t.max x1 y1, max_val_bit_constrained (Ints_t.max x2 y2)) |> fst
          | _, false, _, false -> of_interval ik (Ints_t.max x1 y1, Ints_t.zero) |> fst
          | _, false, _, _ -> of_interval ik (x1, Ints_t.zero) |> fst
          | _, _, _, false -> of_interval ik (y1, Ints_t.zero) |> fst
          | _, _, _, _ ->
            let lower = Ints_t.min x1 y1 in
            let upper = max_val_bit_constrained @@ Ints_t.max x2 y2 in
            of_interval ik (lower, upper) |> fst
        end
      | _ -> top_of ik

  let bit1 f ik i1 f' =
    if is_bot i1 then
      bot_of ik
    else
      match to_int i1 with
      | Some x -> of_int ik (f ik x) |> fst
      | _      -> f' ()

  let lognot ik i1 =
    bit1 (fun _ik -> Ints_t.lognot) ik i1 (fun () ->
        match i1 with
        | Some (x1, x2) -> of_interval ik (Ints_t.lognot x2, Ints_t.lognot x1) |> fst
        | _ -> top_of ik
      )

  let shift_right ik i1 i2 =
    match is_bot i1, is_bot i2 with
    | true, true -> (bot_of ik, {underflow=false; overflow=false})
    | true, _
    | _, true -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show i1) (show i2)))
    | _, _ ->
      match to_int i1, to_int i2 with
      | Some x, Some y -> (try of_int ik (Ints_t.shift_right x (Ints_t.to_int y)) with Division_by_zero | Invalid_argument _ -> (top_of ik, {underflow=false; overflow=false}))
      | _, _->
        let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
        match i1, i2 with
        | Some (x1, x2), Some (y1,y2) when is_nonneg x1 && is_nonneg y1 ->
          of_interval ik (Ints_t.zero, Ints_t.div x2 (Ints_t.shift_left Ints_t.one (Ints_t.to_int y1)))
        | _ -> (top_of ik, {underflow=false; overflow=false})

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

  let div ?no_ov ik x y =
    match x, y with
    | None, None -> (bot (),{underflow=false; overflow=false})
    | None, _ | _, None -> raise (ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | Some x, Some y ->
      let (neg, pos) = IArith.div x y in
      let (r, ov) = norm ik (join_no_norm ik neg pos) in (* normal join drops overflow info *)
      if leq (of_int ik Ints_t.zero |> fst) (Some y) then
        (top_of ik, ov)
      else
        (r, ov)

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

  let refine_with_bitfield ik a b =
    let interv = of_bitfield ik b in
    meet ik a interv

  let refine_with_interval ik a b = meet ik a b

  let refine_with_excl_list ik (intv : t) (excl : (int_t list * (int * int)) option) : t =
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
      let range = norm ik (Some (Ints_t.of_bigint (Size.min_from_bit_range rl), Ints_t.of_bigint (Size.max_from_bit_range rh))) |> fst in
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

module Interval = IntervalFunctor (IntOps.BigIntOps)
