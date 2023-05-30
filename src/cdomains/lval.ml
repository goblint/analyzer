(** Domains for lvalues. *)

open GoblintCil
open Pretty

module M = Messages

type ('f, 'i) offs = 'i Offset.t [@@deriving eq, ord, hash]


module OffsetLatWithSemanticEqual (Idx: Offset.Index.Lattice) =
struct
  include Offset.MakeLattice (Idx)

  let ikind () = Cilfacade.ptrdiff_ikind ()

  let offset_to_index_offset typ offs =
    let idx_of_int x =
      Idx.of_int (ikind ()) (Z.of_int x)
    in
    let rec offset_to_index_offset ?typ offs = match offs with
      | `NoOffset -> idx_of_int 0
      | `Field (field, o) ->
        let field_as_offset = Field (field, NoOffset) in
        let bits_offset, _size = GoblintCil.bitsOffset (TComp (field.fcomp, [])) field_as_offset  in
        let bits_offset = idx_of_int bits_offset in
        let remaining_offset = offset_to_index_offset ~typ:field.ftype o in
        Idx.add bits_offset remaining_offset
      | `Index (x, o) ->
        let (item_typ, item_size_in_bits) =
          match Option.map unrollType typ with
          | Some TArray(item_typ, _, _) ->
            let item_size_in_bits = bitsSizeOf item_typ in
            (Some item_typ, idx_of_int item_size_in_bits)
          | _ ->
            (None, Idx.top ())
        in
        let bits_offset = Idx.mul item_size_in_bits x in
        let remaining_offset = offset_to_index_offset ?typ:item_typ o in
        Idx.add bits_offset remaining_offset
    in
    offset_to_index_offset ~typ offs

  let semantic_equal ~xtyp ~xoffs ~ytyp ~yoffs =
    let x_index = offset_to_index_offset xtyp xoffs in
    let y_index = offset_to_index_offset ytyp yoffs in
    if M.tracing then M.tracel "addr" "xoffs=%a xtyp=%a xindex=%a yoffs=%a ytyp=%a yindex=%a\n" pretty xoffs d_plaintype xtyp Idx.pretty x_index pretty yoffs d_plaintype ytyp Idx.pretty y_index;
    Idx.to_bool (Idx.eq x_index y_index)

end

module type S =
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

  val from_var_offset: (varinfo * (field,idx) offs) -> t
  (** Creates an address from a variable and offset. *)

  val to_var_offset: t -> (varinfo * (field,idx) offs) list
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
  module Offs = OffsetLatWithSemanticEqual (Idx)

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

  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
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

    let of_elt_offset: (fieldinfo, Idx.t) offs -> (fieldinfo, unit) offs = of_offs

    let of_elt (x: elt): t = match x with
      | Addr (v, o) -> Addr (v, of_elt_offset o) (* addrs grouped by var and part of offset *)
      | StrPtr _ when GobConfig.get_bool "ana.base.limit-string-addresses" -> StrPtr None (* all strings together if limited *)
      | StrPtr x -> StrPtr x (* everything else is kept separate, including strings if not limited *)
      | NullPtr -> NullPtr
      | UnknownPtr -> UnknownPtr
  end
end

module Fields =
struct
  module F = CilType.Fieldinfo
  module I = Basetype.CilExp
  module FI = Printable.Either (F) (I)

  include Offset.Exp

  let listify: offset -> t = of_cil
  let to_offs': t -> t = Fun.id


  let rec kill v (fds: t): t = match fds with
    | `Index (x, xs) when I.occurs v x -> `NoOffset
    | `Index (x, xs) -> `Index (x, kill v xs)
    | `Field (x, xs) -> `Field (x, kill v xs)
    | `NoOffset -> `NoOffset

  let rec replace x exp ofs =
    match ofs with
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, replace x exp o)
    | `Index (e, o) -> `Index (I.replace x exp e, replace x exp o)

  let top () = `NoOffset
  let is_top x = x = `NoOffset
  let bot () = failwith "Bottom offset list!"
  let is_bot x = false

  let rec leq x y =
    match x,y with
    | _, `NoOffset -> true
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> leq xs ys
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> leq xs ys
    | _ -> false

  let rec meet x y =
    match x,y with
    | `NoOffset, x | x, `NoOffset -> x
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> `Index (x, meet xs ys)
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> `Field (x, meet xs ys)
    | _ -> failwith "Arguments do not meet"

  let narrow = meet

  let rec join x y =
    match x,y with
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> `Index (x, join xs ys)
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> `Field (x, join xs ys)
    | _ -> `NoOffset

  let widen = join

  let rec collapse x y =
    match x,y with
    | `NoOffset, x | x, `NoOffset -> true
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> collapse xs ys
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> collapse xs ys
    | `Field (x, xs), `Field (y, ys) -> false
    | `Index (x, xs), `Index (y, ys) -> true
    | _ -> failwith "Type mismatch!"

  (* TODO: use the type information to do this properly. Currently, this assumes
   * there are no nested arrays, so all indexing is eliminated. *)
  let rec real_region (fd:t) typ: bool =
    match fd with
    | `NoOffset -> true
    | `Field (_, xs) -> real_region xs typ
    | `Index (i, _) -> false

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y
end

module Exp =
struct
  include Printable.StdLeaf
  type t = CilType.Varinfo.t * Offset.Exp.t [@@deriving eq, ord, hash]

  let name () = "lval with exp indices"

  let show ((v, o): t): string = CilType.Varinfo.show v ^ Offset.Exp.show o
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  let to_cil ((v, o): t): lval = (Var v, Offset.Exp.to_cil o)
  let to_cil_exp lv = Lval (to_cil lv)
end

module CilLval =
struct
  include Exp

  let class_tag (v,o) =
    match v with
    | _ when v.vglob -> `Global
    | _ when v.vdecl.line = -1 -> `Temp
    | _ when Cilfacade.is_varinfo_formal v -> `Parameter
    | _ -> `Local

  let to_exp = to_cil_exp (* TODO: remove *)
end
