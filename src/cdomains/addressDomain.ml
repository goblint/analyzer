open Cil
open Pretty
open Goblintutil
open IntOps
let fast_addr_sets = false (* unknown addresses for fast sets == top, for slow == {?}*)

module GU = Goblintutil

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

  module Addr = Lval.NormalLat (Idx)
  include HoareDomain.HoarePO (Addr)

  type field = Addr.field
  type idx = Idx.t
  type offs = [`NoOffset | `Field of (field * offs) | `Index of (idx * offs)]

  let null_ptr       = singleton Addr.NullPtr
  let safe_ptr       = singleton Addr.SafePtr
  let unknown_ptr    = singleton Addr.UnknownPtr
  let not_null       = unknown_ptr
  let top_ptr        = of_list Addr.([UnknownPtr; NullPtr])
  let is_unknown x   = is_element Addr.UnknownPtr x
  let may_be_unknown x = exists (fun e -> e = Addr.UnknownPtr) x
  let is_null x      = is_element Addr.NullPtr x
  let is_not_null x  = for_all (fun e -> e <> Addr.NullPtr) x
  let may_be_null x = exists (fun e -> e = Addr.NullPtr) x
  let to_bool x      = if is_null x then Some false else if is_not_null x then Some true else None
  let has_unknown x  = mem Addr.UnknownPtr x

  let of_int (type a) (module ID : IntDomain.Z with type t = a) i =
    match ID.to_int i with
    | x when opt_predicate BigIntOps.(equal (zero)) x -> null_ptr
    | x when opt_predicate BigIntOps.(equal (one)) x -> not_null
    | _ -> match ID.to_excl_list i with
      | Some xs when List.exists BigIntOps.(equal (zero)) xs -> not_null
      | _ -> top_ptr

  let get_type xs =
    try Addr.get_type (choose xs)
    with (* WTF? Returns TVoid when it is unknown and stuff??? *)
    | _ -> voidType

  let from_var x = singleton (Addr.from_var x)
  let from_var_offset x = singleton (Addr.from_var_offset x)
  let to_var_may x = List.concat (List.map Addr.to_var_may (elements x))
  let to_var_must x = List.concat (List.map Addr.to_var_must (elements x))
  let to_var_offset x = List.concat (List.map Addr.to_var_offset (elements x))
  let is_definite x = match elements x with
    | [x] when Addr.is_definite x -> true
    | _ -> false

  (* strings *)
  let from_string x = singleton (Addr.from_string x)
  let to_string x = List.concat (List.map Addr.to_string (elements x))

  (* add an & in front of real addresses *)
  let short_addr a =
    match Addr.to_var a with
    | [_] -> "&" ^ Addr.show a
    | _ -> Addr.show a

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

  let is_top a = mem Addr.UnknownPtr a

  let merge uop cop x y =
    let no_null x y =
      if mem Addr.NullPtr y then x
      else remove Addr.NullPtr x
    in
    match is_top x, is_top y with
    | true, true -> uop x y
    | false, true -> no_null x y
    | true, false -> no_null y x
    | false, false -> cop x y

  let meet x y   = merge join meet x y
  let narrow x y = merge widen narrow x y

  let invariant c x =
    let c_exp = Cil.(Lval (BatOption.get c.Invariant.lval)) in
    let i_opt = fold (fun addr acc_opt ->
        BatOption.bind acc_opt (fun acc ->
            match addr with
            | Addr.UnknownPtr
            | Addr.SafePtr ->
              None
            | Addr.Addr (vi, offs) when Addr.Offs.is_definite offs ->
              let rec offs_to_offset = function
                | `NoOffset -> NoOffset
                | `Field (f, offs) -> Field (f, offs_to_offset offs)
                | `Index (i, offs) ->
                  (* Addr.Offs.is_definite implies Idx.is_int *)
                  let i_definite = BatOption.get (Idx.to_int i) in
                  let i_exp = Cil.(kinteger64 ILongLong (BigIntOps.to_int64 i_definite)) in
                  Index (i_exp, offs_to_offset offs)
              in
              let offset = offs_to_offset offs in

              let i =
                if not (InvariantCil.var_is_heap vi) then
                  let addr_exp = AddrOf (Var vi, offset) in (* AddrOf or Lval? *)
                  Invariant.of_exp Cil.(BinOp (Eq, c_exp, addr_exp, intType))
                else
                  Invariant.none
              in
              let i_deref =
                c.Invariant.deref_invariant vi offset (Mem c_exp, NoOffset)
              in

              Some (Invariant.(acc || (i && i_deref)))
            | Addr.NullPtr ->
              let i =
                let addr_exp = integer 0 in
                Invariant.of_exp Cil.(BinOp (Eq, c_exp, addr_exp, intType))
              in
              Some (Invariant.(acc || i))
            (* TODO: handle Addr.StrPtr? *)
            | _ ->
              None
          )
      ) x (Some Invariant.none)
    in
    match i_opt with
    | Some i -> i
    | None -> Invariant.none
end
