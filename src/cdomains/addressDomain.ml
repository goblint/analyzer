(** Domains for addresses/pointers. *)

open GoblintCil
open IntOps
open Mval

module M = Messages

module type SAddr =
sig
  type field
  type idx
  include Printable.S

  val null_ptr: unit -> t
  val str_ptr: unit -> t
  val is_null: t -> bool
  val get_location: t -> location

  val from_var: varinfo -> t
  (** Creates an address from variable. *)

  val from_var_offset: (varinfo * idx Offset.t) -> t
  (** Creates an address from a variable and offset. *)

  val to_var_offset: t -> (varinfo * idx Offset.t) list
  (** Get the offset *)

  val to_var: t -> varinfo list
  (** Strips the varinfo out of the address representation. *)

  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  (** Strips the varinfo out of the address representation. *)

  val get_type: t -> typ
  (** Finds the type of the address location. *)
end

module PreNormal (Offset: Printable.S) =
struct
  include Printable.StdLeaf
  type t =
    | Addr of CilType.Varinfo.t * Offset.t (** Pointer to offset of a variable. *)
    | NullPtr (** NULL pointer. *)
    | UnknownPtr (** Unknown pointer. Could point to globals, heap and escaped variables. *)
    | StrPtr of string option (** String literal pointer. [StrPtr None] abstracts any string pointer *)
  [@@deriving eq, ord, hash] (* TODO: StrPtr equal problematic if the same literal appears more than once *)

  let hash x = match x with
    | StrPtr _ ->
      if GobConfig.get_bool "ana.base.limit-string-addresses" then
        13859
      else
        hash x
    | _ -> hash x

  let show_addr (x, o) =
    if RichVarinfo.BiVarinfoMap.Collection.mem_varinfo x then
      let description = RichVarinfo.BiVarinfoMap.Collection.describe_varinfo x in
      "(" ^ x.vname ^ ", " ^ description ^ ")" ^ Offset.show o
    else x.vname ^ Offset.show o

  let show = function
    | Addr (x, o)-> show_addr (x, o)
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
end

module Normal (Idx: Offset.Index.Printable) =
struct
  type field = fieldinfo
  type idx = Idx.t
  module Offs = Offset.MakePrintable (Idx)
  include PreNormal (Offs)

  let name () = "Normal Lvals"

  type group = Basetype.Variables.group
  let show_group = Basetype.Variables.show_group
  let to_group = function
    | Addr (x,_) -> Basetype.Variables.to_group x
    | _ -> Some Basetype.Variables.Local

  let from_var x = Addr (x, `NoOffset)
  let from_var_offset (x, o) = Addr (x, o)

  let to_var = function
    | Addr (x,_) -> Some x
    | _          -> None
  let to_var_may = function
    | Addr (x,_) -> Some x
    | _          -> None
  let to_var_must = function
    | Addr (x,`NoOffset) -> Some x
    | _                  -> None
  let to_var_offset = function
    | Addr (x, o) -> Some (x, o)
    | _      -> None

  (* strings *)
  let from_string x = StrPtr (Some x)
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

  (* exception if the offset can't be followed completely *)
  exception Type_offset of typ * string
  (* tries to follow o in t *)
  let rec type_offset t o = match unrollType t, o with (* resolves TNamed *)
    | t, `NoOffset -> t
    | TArray (t,_,_), `Index (i,o)
    | TPtr (t,_), `Index (i,o) -> type_offset t o
    | TComp (ci,_), `Field (f,o) ->
      let fi = try getCompField ci f.fname
        with Not_found ->
          let s = GobPretty.sprintf "Addr.type_offset: field %s not found in type %a" f.fname d_plaintype t in
          raise (Type_offset (t, s))
      in type_offset fi.ftype o
    | TComp _, `Index (_,o) -> type_offset t o (* this happens (hmmer, perlbench). safe? *)
    | t,o ->
      let s = GobPretty.sprintf "Addr.type_offset: could not follow offset in type. type: %a, offset: %a" d_plaintype t Offs.pretty o in
      raise (Type_offset (t, s))

  let get_type_addr (v,o) = try type_offset v.vtype o with Type_offset (t,_) -> t

  let get_type = function
    | Addr (x, o) -> get_type_addr (x, o)
    | StrPtr _ -> charPtrType (* TODO Cil.charConstPtrType? *)
    | NullPtr  -> voidType
    | UnknownPtr -> voidPtrType

  let is_zero_offset x = Offs.cmp_zero_offset x = `MustZero

  (* TODO: seems to be unused *)
  let to_exp (f:idx -> exp) x =
    (* TODO: Offset *)
    let rec to_cil c =
      match c with
      | `NoOffset -> NoOffset
      | `Field (fld, ofs) -> Field (fld  , to_cil ofs)
      | `Index (idx, ofs) -> Index (f idx, to_cil ofs)
    in
    match x with
    | Addr (v,o) -> AddrOf (Var v, to_cil o)
    | StrPtr (Some x) -> mkString x
    | StrPtr None -> raise (Lattice.Unsupported "Cannot express unknown string pointer as expression.")
    | NullPtr -> integer 0
    | UnknownPtr -> raise Lattice.TopValue
  (* TODO: unused *)
  let add_offset x o = match x with
    | Addr (v, u) -> Addr (v, Offs.add_offset u o)
    | x -> x

  let arbitrary () = QCheck.always UnknownPtr (* S TODO: non-unknown *)
end

(** Lvalue lattice.

    Actually a disjoint union of lattices without top or bottom.
    Lvalues are grouped as follows:

    - Each {!Addr}, modulo precise index expressions in offset, is a sublattice with ordering induced by {!Offset}.
    - {!NullPtr} is a singleton sublattice.
    - {!UnknownPtr} is a singleton sublattice.
    - If [ana.base.limit-string-addresses] is enabled, then all {!StrPtr} are together in one sublattice with flat ordering. If [ana.base.limit-string-addresses] is disabled, then each {!StrPtr} is a singleton sublattice. *)
module NormalLat (Idx: Offset.Index.Lattice) =
struct
  include Normal (Idx)
  module Offs = Offset.MakeLattice (Idx)

  (** Semantic equal. [Some true] if definitely equal,  [Some false] if definitely not equal, [None] otherwise *)
  let semantic_equal x y = match x, y with
    | Addr (x, xoffs), Addr (y, yoffs) ->
      if CilType.Varinfo.equal x y then
        let xtyp = x.vtype in
        let ytyp = y.vtype in
        Offs.semantic_equal ~xtyp ~xoffs ~ytyp ~yoffs
      else
        Some false
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

  let is_definite = function
    | NullPtr -> true
    | Addr (v,o) when Offs.is_definite o -> true
    | _ -> false

  let leq x y = match x, y with
    | StrPtr _, StrPtr None -> true
    | StrPtr a, StrPtr b   -> a = b
    | Addr (x,o), Addr (y,u) -> CilType.Varinfo.equal x y && Offs.leq o u
    | _                      -> x = y

  let drop_ints = function
    | Addr (x, o) -> Addr (x, Offs.top_indices o)
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

  let merge cop x y =
    match x, y with
    | UnknownPtr, UnknownPtr -> UnknownPtr
    | NullPtr   , NullPtr -> NullPtr
    | StrPtr a, StrPtr b ->
      StrPtr
        begin match cop with
          |`Join | `Widen -> join_string_ptr a b
          |`Meet | `Narrow -> meet_string_ptr a b
        end
    | Addr (x,o), Addr (y,u) when CilType.Varinfo.equal x y -> Addr (x, Offs.merge cop o u)
    | _ -> raise Lattice.Uncomparable

  let join = merge `Join
  let widen = merge `Widen
  let meet = merge `Meet
  let narrow = merge `Narrow

  include Lattice.NoBotTop

  let pretty_diff () (x,y) = Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

(** Lvalue lattice with sublattice representatives for {!DisjointDomain}. *)
module BaseAddrRepr (Idx: Offset.Index.Lattice) =
struct
  include NormalLat (Idx)

  module R: DisjointDomain.Representative with type elt = t =
  struct
    type elt = t

    module AnyOffset = Printable.UnitConf (struct let name = "" end)
    include PreNormal (AnyOffset)

    let name () = "BaseAddrRepr.R"

    let of_elt (x: elt): t = match x with
      | Addr (v, o) -> Addr (v, ())
      | StrPtr _ when GobConfig.get_bool "ana.base.limit-string-addresses" -> StrPtr None (* all strings together if limited *)
      | StrPtr x -> StrPtr x (* everything else is kept separate, including strings if not limited *)
      | NullPtr -> NullPtr
      | UnknownPtr -> UnknownPtr
  end
end

(** Lvalue lattice with sublattice representatives for {!DisjointDomain}. *)
module NormalLatRepr (Idx: Offset.Index.Lattice) =
struct
  include NormalLat (Idx)

  (** Representatives for lvalue sublattices as defined by {!NormalLat}. *)
  module R: DisjointDomain.Representative with type elt = t =
  struct
    type elt = t
    open Offset.Unit

    (* Offset module for representative without abstract values for index offsets, i.e. with unit index offsets.
       Reason: The offset in the representative (used for buckets) should not depend on the integer domains,
       since different integer domains may be active at different program points. *)
    include Normal (Offset.Index.Unit)

    let of_elt_offset: Idx.t Offset.t -> Offset.Unit.t = of_offs

    let of_elt (x: elt): t = match x with
      | Addr (v, o) -> Addr (v, of_elt_offset o) (* addrs grouped by var and part of offset *)
      | StrPtr _ when GobConfig.get_bool "ana.base.limit-string-addresses" -> StrPtr None (* all strings together if limited *)
      | StrPtr x -> StrPtr x (* everything else is kept separate, including strings if not limited *)
      | NullPtr -> NullPtr
      | UnknownPtr -> UnknownPtr
  end
end


module type S =
sig
  include Lattice.S
  type idx
  type field

  val from_var: varinfo -> t
  val from_var_offset: (varinfo * idx Offset.t) -> t
  val to_var_offset: t -> (varinfo * idx Offset.t) list
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  val get_type: t -> typ
end

module AddressSet (Idx: IntDomain.Z) =
struct
  module BaseAddr = BaseAddrRepr (Idx)
  module Addr = NormalLatRepr (Idx)
  module J = (struct
    include SetDomain.Joined (Addr)
    let may_be_equal a b = Option.value (Addr.semantic_equal a b) ~default:true
  end)
  module OffsetSplit = DisjointDomain.ProjectiveSetPairwiseMeet (Addr) (J) (Addr.R)

  (* module H = HoareDomain.SetEM (Addr) *)
  (* Hoare set for bucket doesn't play well with StrPtr limiting:
     https://github.com/goblint/analyzer/pull/808 *)
  module AddressSet : SetDomain.S with type elt = Addr.t = DisjointDomain.ProjectiveSet (BaseAddr) (OffsetSplit) (BaseAddr.R)
  include AddressSet

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

  let to_string_length x =
    let transform elem =
      match Addr.to_string_length elem with
      | Some x -> Idx.of_int !Cil.kindOfSizeOf (Z.of_int x)
      | None -> Idx.top_of !Cil.kindOfSizeOf in
    (* maps any StrPtr to the length of its content, otherwise maps to top *)
    List.map transform (elements x)
    (* and returns the least upper bound of computed IntDomain values *)
    |> List.fold_left Idx.join (Idx.bot_of !Cil.kindOfSizeOf)

  let substring_extraction haystack needle =
    (* map all StrPtr elements in input address sets to contained strings *)
    let haystack' = List.map Addr.to_c_string (elements haystack) in
    let needle' = List.map Addr.to_c_string (elements needle) in

    (* helper functions *)
    let extract_lval_string = function
      | Some s -> from_string s
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
        Idx.of_int IInt Z.zero
      else if res > 0 then
        Idx.starting IInt Z.one
      else
        Idx.ending IInt Z.minus_one in

    (* if any of the input address sets contains an element that isn't a StrPtr, return top *)
    if List.mem None x' || List.mem None y' then
      Idx.top_of IInt
    else
      (* else compare every string of x' with every string of y' and return the least upper bound *)
      BatList.cartesian_product x' y'
      |> List.map (fun (s1, s2) -> compare (Option.get s1) (Option.get s2))
      |> List.fold_left Idx.join (Idx.bot_of IInt)

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
