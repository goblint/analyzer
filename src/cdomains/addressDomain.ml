open Cil
open Pretty
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
  include Printable.Std (* for default invariant, tag, ... *)

  module Addr =
  struct
    include Lval.NormalLat (Idx)
    let top () = failwith "top"
    let bot () = failwith "bot"
    let is_top _ = false
    let is_bot _ = false
  end
  module R =
  struct
    include Addr
    type elt = Addr.t

    let rec of_elt_offset: Offs.t -> Offs.t =
      function
      | `NoOffset -> `NoOffset
      | `Field (f,o) when not (Offs.is_first_field f) -> `Field (f, of_elt_offset o)
      | `Field (_,o) (* zero offsets need to yield the same hash as `NoOffset! *)
      | `Index (_,o) -> of_elt_offset o (* index might become top during fp -> might be zero offset *)
      (* function
      | `NoOffset -> `NoOffset
      | `Index (_, o') as o when Offs.cmp_zero_offset o <> `MustNonzero ->
        of_elt_offset o'
      | `Index (i, o') -> `Index (Idx.top (), of_elt_offset o')
      | `Field (_, o') as o when Offs.cmp_zero_offset o <> `MustNonzero ->
        of_elt_offset o'
      | `Field (f, o') -> `Field (f, of_elt_offset o') (* incorrect *) *)
      (* function _ -> `NoOffset (* very crude *) *)
    let of_elt = function
      | Addr (v, o) -> Addr (v, of_elt_offset o)
      | a -> a
  end
  module R2 =
  struct
    include IntDomain.Integers (IntOps.NIntOps)
    type elt = Addr.t
    let of_elt = Addr.hash
  end
  module Q =
  struct
    type elt = Addr.t
    let cong x y =
      (* ignore (Pretty.eprintf "cong %a %a\n" Addr.pretty x Addr.pretty y); *)
      if M.tracing then M.tracei "ad" "cong %a %a\n" Addr.pretty x Addr.pretty y;
      (* begin match Addr.to_var_offset y with
        | Some (_, `Index (i, `NoOffset)) when Idx.to_int i = Some (Z.of_int 2) -> failwith "THIS"
        | _ -> ()
      end; *)
      let r = match Addr.join x y with
      | exception Lattice.Uncomparable -> false
      | _ -> true
      in
      if M.tracing then M.traceu "ad" "-> %B\n" r;
      r
  end
  module J = HoareDomain.Joined (Addr)
  module H = HoareDomain.Set2 (Addr)
  (* include SensitiveDomain.Pairwise (Addr) (H) (Q) *)
  module PW = SensitiveDomain.Pairwise (Addr) (H) (Q)
  include SensitiveDomain.Projective (Addr) (PW) (R)
  (* include HoareDomain.HoarePO (Addr) *)

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
    let r = leq x y in
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
  let short_addr a =
    match Addr.to_var a with
    | Some _ -> "&" ^ Addr.show a
    | None -> Addr.show a

  let pretty () x =
    try
      let content = List.map (fun a -> text (short_addr a)) (elements x) in
      let rec separate x =
        match x with
        | [] -> []
        | [x] -> [x]
        | (x::xs) -> x ++ (text ", ") :: separate xs
      in
      let separated = separate content in
      let content = List.fold_left (++) nil separated in
      (text "{") ++ content ++ (text "}")
    with SetDomain.Unsupported _ -> pretty () x

  let show x : string =
    try
      let all_elems : string list = List.map short_addr (elements x) in
      Printable.get_short_list "{" "}" all_elems
    with SetDomain.Unsupported _ -> show x

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

  let narrow x y =
    if M.tracing then M.traceli "ad" "narrow %a %a\n" pretty x pretty y;
    let r = narrow x y in
    if M.tracing then M.traceu "ad" "-> %a\n" pretty r;
    r
end
