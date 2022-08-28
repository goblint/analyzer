module type FlagError = sig
  val msg: string
  val name: string
end


module FlagHelper (L:Printable.S) (R:Printable.S) (Msg: FlagError) =
struct
  type t = L.t option * R.t option

  let unop opl opr (h,r) = match (h, r) with
    | (Some l, None) -> opl l
    | (None, Some r) -> opr r
    | _ -> failwith Msg.msg

  let binop opl opr (l1,r1) (l2,r2) = match (l1, r1),(l2, r2) with
    | (Some l1, None), (Some l2, None) -> opl l1 l2
    | (None, Some r1), (None, Some r2) -> opr r1 r2
    | _ -> failwith Msg.msg

  let unop_to_t opl opr (l,t) = match (l, t) with
    | (Some p, None) -> (Some (opl p), None)
    | (None, Some t) -> (None, Some(opr t))
    | _ -> failwith Msg.msg

  let binop_to_t opl opr (l1,r1) (l2,r2)= match (l1, r1),(l2, r2) with
    | (Some p1, None), (Some p2, None) -> (Some (opl p1 p2), None)
    | (None, Some t1), (None, Some t2) -> (None, Some(opr t1 t2))
    | _ -> failwith Msg.msg

  let equal = binop L.equal R.equal
  let hash = unop L.hash R.hash
  let compare = binop L.compare R.compare
  let show = unop L.show R.show
  let pretty ppf = unop (L.pretty ppf) (R.pretty ppf)
  let printXml f = unop (L.printXml f) (R.printXml f)
  let to_yojson = unop L.to_yojson R.to_yojson
  let relift = unop_to_t L.relift R.relift

  let tag _ = failwith (Msg.name ^ ": no tag")
  let arbitrary () = failwith (Msg.name ^ ": no arbitrary")
end

module GroupableFlagHelper (L:MapDomain.Groupable) (R:MapDomain.Groupable) (Msg: FlagError) =
struct
  include FlagHelper (L) (R) (Msg)
  type group = L.group option * R.group option

  let trace_enabled = false

  let show_group = unop L.show_group R.show_group
  let to_group (h,p) = match (h, p) with
    | (Some h, None) ->
      (let r = L.to_group h in
       match r with
       | Some r -> Some (Some r, None)
       | _ -> None)
    | (None, Some p) ->
      (let r = R.to_group p in
       match r with
       | Some r -> Some (None, Some r)
       | _ -> None)
    | _ -> failwith Msg.msg
end

module type LatticeFlagHelperArg = sig
  include Lattice.PO
  val is_top: t -> bool
  val is_bot: t -> bool
end

module LatticeFlagHelper (L:LatticeFlagHelperArg) (R:LatticeFlagHelperArg) (Msg: FlagError) =
struct
  include FlagHelper (L) (R) (Msg)

  let leq = binop L.leq R.leq
  let join = binop_to_t L.join R.join
  let meet = binop_to_t L.meet R.meet
  let widen = binop_to_t L.widen R.widen
  let narrow = binop_to_t L.narrow R.narrow
  let is_top = unop L.is_top R.is_top
  let is_bot = unop L.is_bot R.is_bot
  let pretty_diff ppf ((l1,r1),(l2,r2)) = match (l1, r1),(l2, r2) with
    | (Some p1, None), (Some p2, None) -> L.pretty_diff ppf (p1, p2)
    | (None, Some t1), (None, Some t2) -> R.pretty_diff ppf (t1, t2)
    | _ -> failwith Msg.msg
end
