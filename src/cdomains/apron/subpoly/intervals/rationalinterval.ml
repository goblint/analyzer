module RationalInterval : Intervalsig.IntervalSig with type bound = Q.t = struct
  type bound = Q.t
  type t = Q.t option * Q.t option

  let equal ((l1, u1) as interval_1 : t) ((l2, u2) as interval_2 : t) =
    match interval_1, interval_2 with
    | (None, None), (None, None) -> true
    | (None, Some u1), (None, Some u2) -> Q.equal u1 u2
    | (Some l1, None), (Some l2, None) -> Q.equal l1 l2
    | (Some l1, Some u1), (Some l2, Some u2) -> Q.equal l1 l2 && Q.equal u1 u2
    | _ -> false

  let compare_bound_opt (a : Q.t option) (b : Q.t option) =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> Q.compare a b

  let compare ((l1, u1) : t) ((l2, u2) : t) =
    let c_lower = compare_bound_opt l1 l2 in
    if c_lower <> 0 then c_lower else compare_bound_opt u1 u2

  let hash_q (q : bound) =
    Hashtbl.hash (q.Q.num, q.Q.den)

  let hash ((l, u) : t) =
    Hashtbl.hash (Option.map hash_q l, Option.map hash_q u)

  (* top *)

  let top = (None, None)

  (* is_top *)

  let is_top ((l, u) : t) =
    match l, u with
    | None, None -> true
    | _ -> false

  (* of_bounds *)

  let of_bounds ~lower ~upper = (lower, upper)

  (* bounds *)

  let bounds ((l, u) : t) = (l, u)

  (* of_const *)

  let of_const (c : bound) = (Some c, Some c)

  (* add *)

  let add ((l1, u1) : t) ((l2, u2) : t) =
    let add_opt a b =
      match a, b with
      | Some a, Some b -> Some (Q.add a b)
      | _ -> None
    in
    add_opt l1 l2, add_opt u1 u2

  (* scale *)

  let scale_bound (factor : bound) (b : bound option) =
    Option.map (fun x -> Q.mul factor x) b

  let scale (factor : bound) ((l, u) : t) =
    if Q.compare factor Q.zero < 0 then
      scale_bound factor u, scale_bound factor l
    else
      scale_bound factor l, scale_bound factor u


  let add_const (c : bound) ((l, u) : t) =
    let add_opt = Option.map (fun x -> Q.add x c) in
    add_opt l, add_opt u

  (* meet *)

  (* [None] is -inf in lower-bound position and +inf in upper-bound position,
     so the neutral/dominant handling of [None] differs in all four cases. *)

  let max_lower (a : bound option) (b : bound option) = (* None neutral *)
    match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (if Q.compare a b >= 0 then a else b)

  let min_upper (a : bound option) (b : bound option) = (* None neutral *)
    match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (if Q.compare a b <= 0 then a else b)

  let meet ((l1, u1) : t) ((l2, u2) : t) =
    let lower = max_lower l1 l2 in
    let upper = min_upper u1 u2 in
    match lower, upper with
    | Some l, Some u when Q.compare l u > 0 -> None
    | _ -> Some (lower, upper)

  (* join *)

  let min_lower (a : bound option) (b : bound option) = (* None dominant *)
    match a, b with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (if Q.compare a b <= 0 then a else b)

  let max_upper (a : bound option) (b : bound option) = (* None dominant *)
    match a, b with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (if Q.compare a b >= 0 then a else b)

  let join ((l1, u1) : t) ((l2, u2) : t) =
    min_lower l1 l2, max_upper u1 u2

  (* leq *)

  let lower_leq (a : bound option) (b : bound option) =
    match a, b with
    | None, _ -> true
    | Some _, None -> false
    | Some a, Some b -> Q.compare a b <= 0

  let upper_leq (a : bound option) (b : bound option) =
    match a, b with
    | _, None -> true
    | None, Some _ -> false
    | Some a, Some b -> Q.compare a b <= 0

  let leq ((l1, u1) : t) ((l2, u2) : t) =
    lower_leq l2 l1 && upper_leq u1 u2

  (* widen *)

  let widen ((l1, u1) : t) ((l2, u2) : t) =
    (match l1, l2 with
     | Some a, Some b when Q.geq b a -> Some a
     | _ -> None),
    (match u1, u2 with
     | Some a, Some b when Q.leq b a -> Some a
     | _ -> None)

  let widen_thresholds ~(lower : bound -> bound option) ~(upper : bound -> bound option) ((l1, u1) : t) ((l2, u2) : t) =
    (match l1, l2 with
     | Some a, Some b when Q.geq b a -> Some a
     | Some _, Some b -> lower b
     | _ -> None),
    (match u1, u2 with
     | Some a, Some b when Q.leq b a -> Some a
     | Some _, Some b -> upper b
     | _ -> None)

  (* narrow *)

  let narrow ((l1, u1) : t) ((l2, u2) : t) =
    (match l1 with None -> l2 | Some _ -> l1),
    (match u1 with None -> u2 | Some _ -> u1)

  (* show *)

  let show_bound (b : bound option) =
    match b with
    | None -> "inf"
    | Some x ->
      if Z.equal x.Q.den Z.one then Z.to_string x.Q.num
      else Z.to_string x.Q.num ^ "/" ^ Z.to_string x.Q.den

  let show ((l, u) : t) =
    let lower = match l with None -> "-inf" | Some _ -> show_bound l in
    let upper = show_bound u in
    "[" ^ lower ^ ", " ^ upper ^ "]"
end
