include AddressDomain_intf

open GoblintCil
open IntOps

module M = Messages
module Mval_outer = Mval


module AddressBase (Mval: Printable.S) =
struct
  include Printable.StdLeaf
  type t =
    | Addr of Mval.t
    | NullPtr
    | UnknownPtr
    | StrPtr of string option
  [@@deriving eq, ord, hash] (* TODO: StrPtr equal problematic if the same literal appears more than once *)

  let name () = Format.sprintf "address (%s)" (Mval.name ())

  let hash x = match x with
    | StrPtr _ ->
      if GobConfig.get_bool "ana.base.limit-string-addresses" then
        13859
      else
        hash x
    | _ -> hash x

  let show = function
    | Addr m -> Mval.show m
    | StrPtr (Some x)   -> "\"" ^ x ^ "\""
    | StrPtr None -> "(unknown string)"
    | UnknownPtr -> "?"
    | NullPtr    -> "NULL"

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  (* strings *)
  let of_string x = StrPtr (Some x)
  let to_string = function
    | StrPtr (Some x) -> Some x
    | _        -> None
  (* only keep part before first null byte *)
  let to_c_string = function
    | StrPtr (Some x) ->
      begin match String.split_on_char '\x00' x with
        | s::_ -> Some s
        | [] -> None
      end
    | _ -> None
  let to_n_c_string n x =
    match to_c_string x with
    | Some x ->
      if n > String.length x then
        Some x
      else if n < 0 then
        None
      else
        Some (String.sub x 0 n)
    | _ -> None
  let to_string_length x =
    match to_c_string x with
    | Some x -> Some (String.length x)
    | _ -> None

  let arbitrary () = QCheck.always UnknownPtr (* S TODO: non-unknown *)
end

module AddressPrintable (Mval: Mval.Printable) =
struct
  include AddressBase (Mval)

  let trace_enabled = true (* TODO: remove *)
  type group = Basetype.Variables.group
  let show_group = Basetype.Variables.show_group
  let to_group = function
    | Addr (x,_) -> Basetype.Variables.to_group x
    | _ -> Some Basetype.Variables.Local

  let of_var x = Addr (x, `NoOffset)
  let of_mval (x, o) = Addr (x, o)

  let to_var = function
    | Addr (x,_) -> Some x
    | _          -> None
  let to_var_may = function
    | Addr (x,_) -> Some x
    | _          -> None
  let to_var_must = function
    | Addr (x,`NoOffset) -> Some x
    | _                  -> None
  let to_mval = function
    | Addr (x, o) -> Some (x, o)
    | _      -> None

  let type_of = function
    | Addr (x, o) -> Mval.type_of (x, o)
    | StrPtr _ -> charPtrType (* TODO Cil.charConstPtrType? *)
    | NullPtr  -> voidType
    | UnknownPtr -> voidPtrType

  (* TODO: seems to be unused *)
  let to_exp = function
    | Addr m -> AddrOf (Mval.to_cil m)
    | StrPtr (Some x) -> mkString x
    | StrPtr None -> raise (Lattice.Unsupported "Cannot express unknown string pointer as expression.")
    | NullPtr -> integer 0
    | UnknownPtr -> raise Lattice.TopValue
  (* TODO: unused *)
  let add_offset x o = match x with
    | Addr m -> Addr (Mval.add_offset m o)
    | x -> x


  let is_definite = function
    | NullPtr -> true
    | Addr m -> Mval.is_definite m
    | _ -> false
end

module AddressLattice (Mval: Mval.Lattice) =
struct
  include AddressPrintable (Mval)

  let semantic_equal x y = match x, y with
    | Addr x, Addr y -> Mval.semantic_equal x y
    | StrPtr None, StrPtr _
    | StrPtr _, StrPtr None -> Some true
    | StrPtr (Some a), StrPtr (Some b) -> if a = b then None else Some false
    | NullPtr, NullPtr -> Some true
    | UnknownPtr, UnknownPtr
    | UnknownPtr, Addr _
    | Addr _, UnknownPtr
    | UnknownPtr, StrPtr _
    | StrPtr _, UnknownPtr -> None
    | _, _ -> Some false

  let leq x y = match x, y with
    | StrPtr _, StrPtr None -> true
    | StrPtr a, StrPtr b   -> a = b
    | Addr x, Addr y -> Mval.leq x y
    | _                      -> x = y

  let top_indices = function
    | Addr x -> Addr (Mval.top_indices x)
    | x -> x

  let join_string_ptr x y = match x, y with
    | None, _
    | _, None -> None
    | Some a, Some b when a = b -> Some a
    | Some a, Some b (* when a <> b *) ->
      if GobConfig.get_bool "ana.base.limit-string-addresses" then
        None
      else
        raise Lattice.Uncomparable

  let meet_string_ptr x y = match x, y with
    | None, a
    | a, None -> a
    | Some a, Some b when a = b -> Some a
    | Some a, Some b (* when a <> b *) ->
      if GobConfig.get_bool "ana.base.limit-string-addresses" then
        raise Lattice.BotValue
      else
        raise Lattice.Uncomparable

  let merge mop sop x y =
    match x, y with
    | UnknownPtr, UnknownPtr -> UnknownPtr
    | NullPtr   , NullPtr -> NullPtr
    | StrPtr a, StrPtr b -> StrPtr (sop a b)
    | Addr x, Addr y -> Addr (mop x y)
    | _ -> raise Lattice.Uncomparable

  let join = merge Mval.join join_string_ptr
  let widen = merge Mval.widen join_string_ptr
  let meet = merge Mval.meet meet_string_ptr
  let narrow = merge Mval.narrow meet_string_ptr

  include Lattice.NoBotTop

  let pretty_diff () (x,y) = Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module AddressLatticeRepr (Mval: Mval.Lattice) =
struct
  include AddressLattice (Mval)

  module VariableRepr: DisjointDomain.Representative with type elt = t =
  struct
    type elt = t

    include AddressBase (Basetype.Variables)

    let of_elt (x: elt): t = match x with
      | Addr (v, o) -> Addr v
      | StrPtr _ when GobConfig.get_bool "ana.base.limit-string-addresses" -> StrPtr None (* all strings together if limited *)
      | StrPtr x -> StrPtr x (* everything else is kept separate, including strings if not limited *)
      | NullPtr -> NullPtr
      | UnknownPtr -> UnknownPtr
  end

  module UnitOffsetRepr: DisjointDomain.Representative with type elt = t =
  struct
    type elt = t

    (* Offset module for representative without abstract values for index offsets, i.e. with unit index offsets.
       Reason: The offset in the representative (used for buckets) should not depend on the integer domains,
       since different integer domains may be active at different program points. *)
    include AddressPrintable (Mval_outer.Unit)

    let of_elt (x: elt): t = match x with
      | Addr (v, o) -> Addr (v, Offset.Unit.of_offs o) (* addrs grouped by var and part of offset *)
      | StrPtr _ when GobConfig.get_bool "ana.base.limit-string-addresses" -> StrPtr None (* all strings together if limited *)
      | StrPtr x -> StrPtr x (* everything else is kept separate, including strings if not limited *)
      | NullPtr -> NullPtr
      | UnknownPtr -> UnknownPtr
  end
end

module AddressSet (Mval: Mval.Lattice) (ID: IntDomain.Z) =
struct
  module Addr = AddressLatticeRepr (Mval)
  module J =
  struct
    include SetDomain.Joined (Addr)
    let may_be_equal a b = Option.value (Addr.semantic_equal a b) ~default:true
  end
  module OffsetSplit = DisjointDomain.ProjectiveSetPairwiseMeet (Addr) (J) (Addr.UnitOffsetRepr)

  (* module H = HoareDomain.SetEM (Addr) *)
  (* Hoare set for bucket doesn't play well with StrPtr limiting:
     https://github.com/goblint/analyzer/pull/808 *)
  module AddressSet: SetDomain.S with type elt = Addr.t = DisjointDomain.ProjectiveSet (Addr) (OffsetSplit) (Addr.VariableRepr)
  include AddressSet

  let name () = Format.sprintf "address set (%s)" (Mval.name ())

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

  let null_ptr       = singleton Addr.NullPtr
  let unknown_ptr    = singleton Addr.UnknownPtr
  let not_null       = unknown_ptr
  let top_ptr        = of_list Addr.[UnknownPtr; NullPtr]

  let is_element a x = cardinal x = 1 && Addr.equal (choose x) a
  let is_null x = is_element Addr.NullPtr x
  let may_be_null x = mem Addr.NullPtr x
  let is_not_null x = not (may_be_null x)
  let may_be_unknown x = mem Addr.UnknownPtr x
  let to_bool x      = if is_null x then Some false else if is_not_null x then Some true else None

  let of_int i =
    match ID.to_int i with
    | x when GobOption.exists BigIntOps.(equal (zero)) x -> null_ptr
    | x when GobOption.exists BigIntOps.(equal (one)) x -> not_null
    | _ -> match ID.to_excl_list i with
      | Some (xs, _) when List.exists BigIntOps.(equal (zero)) xs -> not_null
      | _ -> top_ptr

  let to_int x =
    let ik = Cilfacade.ptr_ikind () in
    if equal x null_ptr then
      ID.of_int ik Z.zero
    else if is_not_null x then
      ID.of_excl_list ik [Z.zero]
    else
      ID.top_of ik

  let type_of xs =
    try Addr.type_of (choose xs)
    with (* WTF? Returns TVoid when it is unknown and stuff??? *)
    | _ -> voidType

  let of_var x = singleton (Addr.of_var x)
  let of_mval x = singleton (Addr.of_mval x)
  let to_var_may x = List.filter_map Addr.to_var_may (elements x)
  let to_var_must x = List.filter_map Addr.to_var_must (elements x)
  let to_mval x = List.filter_map Addr.to_mval (elements x)
  let is_definite x = cardinal x = 1 && Addr.is_definite (choose x)

  (* strings *)
  let of_string x = singleton (Addr.of_string x)

  let to_string x = List.filter_map Addr.to_string (elements x)

  let to_string_length x =
    let transform elem =
      match Addr.to_string_length elem with
      | Some x -> ID.of_int !Cil.kindOfSizeOf (Z.of_int x)
      | None -> ID.top_of !Cil.kindOfSizeOf in
    (* maps any StrPtr to the length of its content, otherwise maps to top *)
    List.map transform (elements x)
    (* and returns the least upper bound of computed IntDomain values *)
    |> List.fold_left ID.join (ID.bot_of !Cil.kindOfSizeOf)

  let substring_extraction haystack needle =
    (* map all StrPtr elements in input address sets to contained strings *)
    let haystack' = List.map Addr.to_c_string (elements haystack) in
    let needle' = List.map Addr.to_c_string (elements needle) in

    (* helper functions *)
    let extract_lval_string = function
      | Some s -> of_string s
      | None -> null_ptr in
    let compute_substring s1 s2 =
      try
        let i = Str.search_forward (Str.regexp_string s2) s1 0 in
        Some (String.sub s1 i (String.length s1 - i))
      with Not_found -> None in

    (* if any of the input address sets contains an element that isn't a StrPtr, return top *)
    if List.mem None haystack' || List.mem None needle' then
      top_ptr
    else
      (* else try to find the first occurrence of all strings in needle' in all strings s of haystack',
         collect s starting from that occurrence or if there is none, collect a NULL pointer,
         and return the least upper bound *)
      BatList.cartesian_product haystack' needle'
      |> List.map (fun (s1, s2) -> extract_lval_string (compute_substring (Option.get s1) (Option.get s2)))
      |> List.fold_left join (bot ())

  let string_comparison x y n =
    let f = match n with
      | Some num -> Addr.to_n_c_string num
      | None -> Addr.to_c_string in

    (* map all StrPtr elements in input address sets to contained strings / n-substrings *)
    let x' = List.map f (elements x) in
    let y' = List.map f (elements y) in

    (* helper functions *)
    let compare s1 s2 =
      let res = String.compare s1 s2 in
      if res = 0 then
        ID.of_int IInt Z.zero
      else if res > 0 then
        ID.starting IInt Z.one
      else
        ID.ending IInt Z.minus_one in

    (* if any of the input address sets contains an element that isn't a StrPtr, return top *)
    if List.mem None x' || List.mem None y' then
      ID.top_of IInt
    else
      (* else compare every string of x' with every string of y' and return the least upper bound *)
      BatList.cartesian_product x' y'
      |> List.map (fun (s1, s2) -> compare (Option.get s1) (Option.get s2))
      |> List.fold_left ID.join (ID.bot_of IInt)

  let string_writing_defined dest =
    (* if the destination address set contains a StrPtr, writing to such a string literal is undefined behavior *)
    if List.exists Option.is_some (List.map Addr.to_c_string (elements dest)) then
      (M.warn ~category:M.Category.Behavior.Undefined.other "May write to a string literal, which leads to a segmentation fault in most cases";
       false)
    else
      true

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

  let is_top = may_be_unknown
  let top () = top_ptr

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
