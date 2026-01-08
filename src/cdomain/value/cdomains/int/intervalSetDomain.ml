open IntDomain0
open IntervalDomain
open GoblintCil


(** IntervalSetFunctor that is not just disjunctive completion of intervals, but attempts to be precise for wraparound arithmetic for unsigned types *)
module IntervalSetFunctor (Ints_t : IntOps.IntOps): SOverflow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) list =
struct

  module Interval = IntervalFunctor (Ints_t)
  module IArith = IntervalArith (Ints_t)


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

  let top_of ?bitfield ik =
    let top_interval =
      match bitfield with
      | None -> range ik
      | Some b -> (* TODO: why no bound check like in IntervalDomain? *)
        let signed_lower_bound = Ints_t.neg @@ Ints_t.shift_left Ints_t.one (b - 1) in
        let unsigned_upper_bound = Ints_t.sub (Ints_t.shift_left Ints_t.one b) Ints_t.one in
        if GoblintCil.isSigned ik then
          (signed_lower_bound, unsigned_upper_bound)
        else
          (Ints_t.zero, unsigned_upper_bound)
    in
    [top_interval]

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
    | _, _ -> canonize @@ GobList.cartesian_concat_map op x y


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

  let norm_interval ?(cast=false) ik (x,y) : t*overflow_info =
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
      (v, {underflow; overflow})

  let norm_intvs ?(cast=false) (ik:ikind) (xs: t) : t*overflow_info =
    let res = List.map (norm_interval ~cast ik) xs in
    let intvs = List.concat_map fst res in
    let underflow = List.exists (fun (_,{underflow; _}) -> underflow) res in
    let overflow = List.exists (fun (_,{overflow; _}) -> overflow) res in
    (canonize intvs,{underflow; overflow})

  let binary_op_with_norm op (ik:ikind) (x: t) (y: t) : t*overflow_info = match x, y with
    | [], _ -> ([],{overflow=false; underflow=false})
    | _, [] -> ([],{overflow=false; underflow=false})
    | _, _ -> norm_intvs ik @@ GobList.cartesian_map op x y

  let binary_op_concat_with_norm op (ik:ikind) (x: t) (y: t) : t*overflow_info = match x, y with
    | [], _ -> ([],{overflow=false; underflow=false})
    | _, [] -> ([],{overflow=false; underflow=false})
    | _, _ -> norm_intvs ik @@ GobList.cartesian_concat_map op x y

  let binary_op_with_ovc (x: t) (y: t) op : t*overflow_info = match x, y with
    | [], _ -> ([],{overflow=false; underflow=false})
    | _, [] -> ([],{overflow=false; underflow=false})
    | _, _ ->
      let res = GobList.cartesian_map op x y in
      let intvs = List.concat_map fst res in
      let underflow = List.exists (fun (_,{underflow; _}) -> underflow) res in
      let overflow = List.exists (fun (_,{overflow; _}) -> underflow) res in
      (canonize intvs,{underflow; overflow})

  let unary_op_with_norm op (ik:ikind) (x: t) = match x with
    | [] -> ([],{overflow=false; underflow=false})
    | _ -> norm_intvs ik @@ List.map op x

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

  let of_interval ik (x,y) =  norm_interval ~cast:false ik (x,y)

  let of_bitfield ik x =
    match Interval.of_bitfield ik x with
    | None -> []
    | Some (a,b) -> norm_interval ik (a,b) |> fst

  let to_bitfield ik x =
    let joinbf (z1,o1) (z2,o2) = (Ints_t.logor z1 z2, Ints_t.logor o1 o2) in
    List.fold_left (fun acc i -> joinbf acc (Interval.to_bitfield ik (Some i))) (Ints_t.zero, Ints_t.zero) x

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

  let log f ik i1 i2 =
    match (interval_to_bool i1, interval_to_bool i2) with
    | Some x, Some y -> of_bool ik (f x y)
    | _ -> top_of ik


  let bit f ik (i1, i2) =
    match (interval_to_int i1), (interval_to_int i2) with
    | Some x, Some y -> (try of_int ik (f x y) |> fst with Division_by_zero -> top_of ik)
    | _ -> top_of ik


  let bitcomp f ik i1 i2 =
    match (interval_to_int i1, interval_to_int i2) with
    | Some x, Some y -> (try of_int ik (f x y) with Division_by_zero | Invalid_argument _ -> (top_of ik,{overflow=false; underflow=false}))
    | _, _ -> (top_of ik,{overflow=false; underflow=false})

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

  (* TODO: deduplicate with IntervalDomain? *)
  let interval_logand ik i1 i2 =
    match bit Ints_t.logand ik (i1, i2) with
    | result when not (is_top_of ik result) -> result
    | _ ->
      let (x1, x2), (y1, y2) = i1, i2 in
      let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
      match is_nonneg x1, is_nonneg x2, is_nonneg y1, is_nonneg y2 with
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

  let logand ik x y = binop x y (interval_logand ik)

  (* TODO: deduplicate with IntervalDomain? *)
  let interval_logor ik i1 i2 =
    match bit Ints_t.logor ik (i1, i2) with
    | result when not (is_top_of ik result) -> result
    | _ ->
      let (x1, x2), (y1, y2) = i1, i2 in
      let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
      match is_nonneg x1, is_nonneg x2, is_nonneg y1, is_nonneg y2 with
      | true, _, true, _ ->
        of_interval ik (Ints_t.max x1 y1, max_val_bit_constrained (Ints_t.max x2 y2)) |> fst
      | _, false, _, false -> of_interval ik (Ints_t.max x1 y1, Ints_t.zero) |> fst
      | _, false, _, _ -> of_interval ik (x1, Ints_t.zero) |> fst
      | _, _, _, false -> of_interval ik (y1, Ints_t.zero) |> fst
      | _, _, _, _ ->
        let lower = Ints_t.min x1 y1 in
        let upper = max_val_bit_constrained @@ Ints_t.max x2 y2 in
        of_interval ik (lower, upper) |> fst

  let logor ik x y = binop x y (interval_logor ik)

  (* TODO: deduplicate with IntervalDomain? *)
  let interval_logxor ik i1 i2 =
    match bit Ints_t.logxor ik (i1, i2) with
    | result when not (is_top_of ik result) && not (is_bot result) -> result (* TODO: why bot check here, but not elsewhere? *)
    | _ ->
      let (x1, x2), (y1, y2) = i1, i2 in
      let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
      match is_nonneg x1, is_nonneg x2, is_nonneg y1, is_nonneg y2 with
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

  let logxor ik x y = binop x y (interval_logxor ik)

  let lognot ik x =
    (* TODO: deduplicate with IntervalDomain? *)
    let interval_lognot i =
      match interval_to_int i with
      | Some x -> of_int ik (Ints_t.lognot x) |> fst
      | _ ->
        let (x1, x2) = i in
        of_interval ik (Ints_t.lognot x2, Ints_t.lognot x1) |> fst
    in
    unop x interval_lognot

  let shift_left ik x y =
    let interval_shiftleft = bitcomp (fun x y -> Ints_t.shift_left x (Ints_t.to_int y)) ik in
    binary_op_with_ovc x y interval_shiftleft

  (* TODO: deduplicate with IntervalDomain? *)
  let interval_shiftright ik i1 i2 =
    match interval_to_int i1, interval_to_int i2 with
    | Some x, Some y -> (try of_int ik (Ints_t.shift_right x (Ints_t.to_int y)) with Division_by_zero | Invalid_argument _ -> (top_of ik, {overflow=false; underflow=false}))
    | _, _ ->
      let is_nonneg x = Ints_t.compare x Ints_t.zero >= 0 in
      match i1, i2 with
      | (x1, x2), (y1,y2) when is_nonneg x1 && is_nonneg y1 ->
        of_interval ik (Ints_t.zero, Ints_t.div x2 (Ints_t.shift_left Ints_t.one (Ints_t.to_int y1)))
      | _, _ -> (top_of ik, {underflow=false; overflow=false})

  let shift_right ik x y = binary_op_with_ovc x y (interval_shiftright ik)

  let c_lognot ik x =
    let log1 f ik i1 =
      match interval_to_bool i1 with
      | Some x -> of_bool ik (f x)
      | _ -> top_of ik
    in
    let interval_lognot = log1 not ik in
    unop x interval_lognot

  let c_logand ik x y =
    let interval_logand = log (&&) ik in
    binop x y interval_logand

  let c_logor ik x y =
    let interval_logor = log (||) ik in
    binop x y interval_logor

  let add ?no_ov = binary_op_with_norm IArith.add
  let sub ?no_ov = binary_op_with_norm IArith.sub
  let mul ?no_ov = binary_op_with_norm IArith.mul
  let neg ?no_ov = unary_op_with_norm IArith.neg

  let div ?no_ov ik x y =
    let interval_div x y =
      let (neg, pos) = IArith.div x y in
      let r = List.filter_map Fun.id [neg; pos] in
      if leq (of_int ik Ints_t.zero |> fst) [y] then
        top_of ik @ r (* keep r because they might overflow, but top doesn't *)
      else
        r (* should always be singleton, because if there's a negative and a positive side, then it must've included zero, which is already handled by previous case *)
    in
    binary_op_concat_with_norm interval_div ik x y

  let rem ik x y =
    let interval_rem x y =
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

  let cast_to ~kind ?torg ?no_ov ik x = norm_intvs ~cast:true ik x

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
      let threshold = get_interval_threshold_widening () in
      let min = if min_xs =. min_range || threshold && min_ys >. min_xs && IArith.is_lower_threshold min_xs then min_ys else min_xs in
      let max = if max_xs =. max_range || threshold && max_ys <. max_xs && IArith.is_upper_threshold max_xs then max_ys else max_xs in
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
    let threshold = GobConfig.get_bool "ana.int.interval_threshold_widening" in
    let upper_threshold (_,u) = IArith.upper_threshold u max_ik in
    let lower_threshold (l,_) = IArith.lower_threshold l min_ik in
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

  let starting ik n = norm_interval ik (n, snd (range ik))

  let ending ik n = norm_interval ik (fst (range ik), n)

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
    List.concat_map (fun x -> refine_with_congruence_interval ik cong (Some x)) intvs

  let refine_with_interval ik xs = function None -> [] | Some (a,b) -> meet ik xs [(a,b)]

  let refine_with_bitfield ik x y =
    let interv = of_bitfield ik y in
    norm_intvs ik (meet ik x interv) |> fst

  let refine_with_incl_list ik intvs  = function
    | None -> intvs
    | Some xs -> meet ik intvs (List.map (fun x -> (x,x)) xs)

  let excl_range_to_intervalset (ik: ikind) ((min, max): int_t * int_t) (excl: int_t): t =
    let intv1 = (min, excl -. Ints_t.one) in
    let intv2 = (excl +. Ints_t.one, max) in
    norm_intvs ik [intv1 ; intv2] |> fst

  let of_excl_list ik (excls: int_t list) =
    let excl_list = List.map (excl_range_to_intervalset ik (range ik)) excls in
    let res = List.fold_left (meet ik) (top_of ik) excl_list in
    res

  let refine_with_excl_list ik (intv : t) = function
    | None -> intv
    | Some (xs, range) ->
      let excl_to_intervalset (ik: ikind) ((rl, rh): (int * int)) (excl: int_t): t =
        excl_range_to_intervalset ik (Ints_t.of_bigint (Size.min_from_bit_range rl),Ints_t.of_bigint (Size.max_from_bit_range rh)) excl
      in
      let excl_list = List.map (excl_to_intervalset ik range) xs in
      List.fold_left (meet ik) intv excl_list

  let project ik p t = t

  let arbitrary ik =
    let open QCheck.Iter in
    (* let int_arb = QCheck.map ~rev:Ints_t.to_bigint Ints_t.of_bigint GobQCheck.Arbitrary.big_int in *)
    (* TODO: apparently bigints are really slow compared to int64 for domaintest *)
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 GobQCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let list_pair_arb = QCheck.small_list pair_arb in
    let canonize_randomly_generated_list = (fun x -> norm_intvs ik  x |> fst) in
    let shrink xs = GobQCheck.shrink list_pair_arb xs >|= canonize_randomly_generated_list
    in QCheck.(set_shrink shrink @@ set_print show @@ map (*~rev:BatOption.get*) canonize_randomly_generated_list list_pair_arb)
end

module IntervalSet = IntervalSetFunctor (IntOps.BigIntOps)
