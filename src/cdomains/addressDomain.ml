open GoblintCil
open IntOps

module GU = Goblintutil
module M = Messages

module type S =
sig
  include Lattice.S
  type idx
  type field

  val from_var: varinfo -> t
  val from_var_offset: (varinfo * (idx,field) Lval.offs) -> t
  val to_var_offset: t -> (varinfo * (idx,field) Lval.offs) list
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  val get_type: t -> typ
end

module AddressSet (Idx: IntDomain.Z) =
struct
  module Addr = Lval.NormalLatRepr (Idx)
  module J = SetDomain.Joined (Addr)
  (* module H = HoareDomain.SetEM (Addr) *)
  (* Hoare set for bucket doesn't play well with StrPtr limiting:
     https://github.com/goblint/analyzer/pull/808 *)
  include DisjointDomain.ProjectiveSet (Addr) (J) (Addr.R)

  (* short-circuit with physical equality,
     makes a difference at long-scale: https://github.com/goblint/analyzer/pull/809#issuecomment-1206174751 *)
  let equal x y = x == y || equal x y

  let widen x y =
    if M.tracing then M.traceli "ad" "widen %a %a\n" pretty x pretty y;
    let r = widen x y in
    if M.tracing then M.traceu "ad" "-> %a\n" pretty r;
    r
  let join x y =
    if M.tracing then M.traceli "ad" "join %a %a\n" pretty x pretty y;
    let r = join x y in
    if M.tracing then M.traceu "ad" "-> %a\n" pretty r;
    r
  let leq x y =
    if M.tracing then M.traceli "ad" "leq %a %a\n" pretty x pretty y;
    let r = x == y || leq x y in (* short-circuit with physical equality, not benchmarked *)
    if M.tracing then M.traceu "ad" "-> %B\n" r;
    r

  type field = Addr.field
  type idx = Idx.t
  type offs = [`NoOffset | `Field of (field * offs) | `Index of (idx * offs)]

  let null_ptr       = singleton Addr.NullPtr
  let unknown_ptr    = singleton Addr.UnknownPtr
  let not_null       = unknown_ptr
  let top_ptr        = of_list Addr.([UnknownPtr; NullPtr])
  let may_be_unknown x = exists (fun e -> e = Addr.UnknownPtr) x
  let is_element a x = cardinal x = 1 && Addr.equal (choose x) a
  let is_null x      = is_element Addr.NullPtr x
  let is_not_null x  = for_all (fun e -> e <> Addr.NullPtr) x
  let may_be_null x = exists (fun e -> e = Addr.NullPtr) x
  let to_bool x      = if is_null x then Some false else if is_not_null x then Some true else None
  let has_unknown x  = mem Addr.UnknownPtr x

  let of_int (type a) (module ID : IntDomain.Z with type t = a) i =
    match ID.to_int i with
    | x when GobOption.exists BigIntOps.(equal (zero)) x -> null_ptr
    | x when GobOption.exists BigIntOps.(equal (one)) x -> not_null
    | _ -> match ID.to_excl_list i with
      | Some (xs, _) when List.exists BigIntOps.(equal (zero)) xs -> not_null
      | _ -> top_ptr

  let to_int (type a) (module ID : IntDomain.Z with type t = a) x =
    let ik = Cilfacade.ptr_ikind () in
    if equal x null_ptr then
      ID.of_int ik Z.zero
    else if is_not_null x then
      ID.of_excl_list ik [Z.zero]
    else
      ID.top_of ik

  let get_type xs =
    try Addr.get_type (choose xs)
    with (* WTF? Returns TVoid when it is unknown and stuff??? *)
    | _ -> voidType

  let from_var x = singleton (Addr.from_var x)
  let from_var_offset x = singleton (Addr.from_var_offset x)
  let to_var_may x = List.filter_map Addr.to_var_may (elements x)
  let to_var_must x = List.filter_map Addr.to_var_must (elements x)
  let to_var_offset x = List.filter_map Addr.to_var_offset (elements x)
  let is_definite x = match elements x with
    | [x] when Addr.is_definite x -> true
    | _ -> false

  (* strings *)
  let from_string x = singleton (Addr.from_string x)
  let to_string x = List.filter_map Addr.to_string (elements x)

  (* add an & in front of real addresses *)
  module ShortAddr =
  struct
    include Addr

    let show a =
      match Addr.to_var a with
      | Some _ -> "&" ^ Addr.show a
      | None -> Addr.show a

    let pretty () a = Pretty.text (show a)
  end

  include SetDomain.Print (ShortAddr) (
    struct
      type nonrec t = t
      type nonrec elt = elt
      let elements = elements
      let iter = iter
    end
    )

  (*
  let leq = if not fast_addr_sets then leq else fun x y ->
      match mem Addr.UnknownPtr x, mem Addr.UnknownPtr y with
      | true, false -> false
      | false, true -> true
      | true, true -> true
      | false, false -> leq x y

  let join = if not fast_addr_sets then join else fun x y ->
      match mem Addr.UnknownPtr x, mem Addr.UnknownPtr y with
      | true, false
      | false, true
      | true, true -> unknown_ptr
      | false, false -> join x y
  *)

  (* TODO: overrides is_top, but not top? *)
  let is_top a = mem Addr.UnknownPtr a

  let merge uop cop x y =
    let no_null x y =
      if mem Addr.NullPtr y then x
      else remove Addr.NullPtr x
    in
    match is_top x, is_top y with
    | true, true -> no_null (no_null (uop x y) x) y
    | false, true -> no_null x y
    | true, false -> no_null y x
    | false, false -> cop x y

  let meet x y   = merge join meet x y
  let narrow x y = merge (fun x y -> widen x (join x y)) narrow x y

  let meet x y =
    if M.tracing then M.traceli "ad" "meet %a %a\n" pretty x pretty y;
    let r = meet x y in
    if M.tracing then M.traceu "ad" "-> %a\n" pretty r;
    r

  let narrow x y =
    if M.tracing then M.traceli "ad" "narrow %a %a\n" pretty x pretty y;
    let r = narrow x y in
    if M.tracing then M.traceu "ad" "-> %a\n" pretty r;
    r
end
