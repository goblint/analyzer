module Mpqf = SharedFunctions.Mpqf

module RationalInterval : Intervalsig.IntervalSig with type bound = Mpqf.t = struct
  type bound = Mpqf.t
  (* TODO: make optional maybe? *)
  type t = (Mpqf.t option * Mpqf.t option)

  let equal ((l1, u1) as interval_1 : t) ((l2, u2) as interval_2 : t) =
    match interval_1, interval_2 with
    | (None, None), (None, None) -> true
    | (None, Some u1), (None, Some u2) -> Mpqf.equal u1 u2
    | (Some l1, None), (Some l2, None) -> Mpqf.equal l1 l2
    | (Some l1, Some u1), (Some l2, Some u2) -> Mpqf.equal l1 l2 && Mpqf.equal u1 u2
    | _ -> false

  let compare ((l1, u1) as interval_1 : t) ((l2, u2) as interval_2 : t) =
    match interval_1, interval_2 with
    | (None, None), (None, None) -> 0
    | (Some l1, Some u1), (Some l2, Some u2) ->
      let c_lower = Mpqf.compare l1 l2 in
      if c_lower <> 0 then c_lower else Mpqf.compare u1 u2
    | _ -> 1

  let hash ((l, u) : t) =
    Hashtbl.hash (Option.map Mpqf.hash l, Option.map Mpqf.hash u)

  (* top *)

  let top = (None, None)

  (* is_top *)

  let is_top ((l, u) : t) =
    match l, u with
    | None, None -> true
    | _ -> false

  (* of_bounds *)

  let of_bounds ~lower ~upper = (lower, upper)

  (* scale *)

  let scale_bound (factor : bound) (b : bound option) =
    Option.map (fun x -> Mpqf.mul factor x) b

  let scale (factor : bound) ((l, u) : t) =
    if Mpqf.compare factor Mpqf.zero < 0 then
      scale_bound factor u, scale_bound factor l
    else
      scale_bound factor l, scale_bound factor u


  let add_const (c : bound) ((l, u) : t) =
    let add_opt = Option.map (fun x -> Mpqf.add x c) in
    add_opt l, add_opt u

  (* meet *)

  let min_bound (a : bound option) (b : bound option) =
    match a, b with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (if Mpqf.compare a b <= 0 then a else b)

  let max_bound (a : bound option) (b : bound option) =
    match a, b with
    | None, x | x, None -> x
    | Some a, Some b -> Some (if Mpqf.compare a b >= 0 then a else b)

  let meet ((l1, u1) : t) ((l2, u2) : t) =
    let lower = max_bound l1 l2 in
    let upper = min_bound u1 u2 in
    match lower, upper with
    | Some l, Some u when Mpqf.compare l u > 0 -> None
    | _ -> Some (lower, upper)

  (* join *)

  let join ((l1, u1) : t) ((l2, u2) : t) =
    min_bound l1 l2, max_bound u1 u2

  (* leq *)

  let lower_leq (a : bound option) (b : bound option) =
    match a, b with
    | None, _ -> true
    | Some _, None -> false
    | Some a, Some b -> Mpqf.compare a b <= 0

  let upper_leq (a : bound option) (b : bound option) =
    match a, b with
    | _, None -> true
    | None, Some _ -> false
    | Some a, Some b -> Mpqf.compare a b <= 0

  let leq ((l1, u1) : t) ((l2, u2) : t) =
    lower_leq l2 l1 && upper_leq u1 u2

  (* show *)

  let show_bound (b : bound option) =
    match b with
    | None -> "inf"
    | Some x ->
      if Z.equal (Mpqf.get_den x) Z.one then Z.to_string (Mpqf.get_num x)
      else Z.to_string (Mpqf.get_num x) ^ "/" ^ Z.to_string (Mpqf.get_den x)

  let show ((l, u) : t) =
    let lower = match l with None -> "-inf" | Some _ -> show_bound l in
    let upper = show_bound u in
    "[" ^ lower ^ ", " ^ upper ^ "]"
end
