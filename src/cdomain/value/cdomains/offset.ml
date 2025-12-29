include Offset_intf

open GoblintCil

module M = Messages


module Index =
struct
  include Index

  module Unit: Printable with type t = unit =
  struct
    include Lattice.UnitConf (struct let name = "?" end)
    let name () = "unit index"
    let equal_to _ _ = `Top
    let to_int _ = None
  end

  module Exp =
  struct
    include CilType.Exp
    let name () = "exp index"

    let any = Cilfacade.any_index_exp
    let is_any = function
      | CastE (_, TInt (ik, []), Const (CStr ("any_index", No_encoding))) when CilType.Ikind.equal ik (Cilfacade.ptrdiff_ikind ()) -> true
      | _ -> false

    let all = lazy (CastE (Explicit, TInt (Cilfacade.ptrdiff_ikind (), []), mkString "all_index"))
    let is_all = function
      | CastE (_, TInt (ik, []), Const (CStr ("all_index", No_encoding))) when CilType.Ikind.equal ik (Cilfacade.ptrdiff_ikind ()) -> true
      | _ -> false

    (* Override output *)
    let pretty () x =
      if equal x (Lazy.force any) then
        Pretty.text "?"
      else
        dn_exp () x

    include Printable.SimplePretty (
      struct
        type nonrec t = t
        let pretty = pretty
      end
      )

    let equal_to _ _ = `Top (* TODO: more precise for definite indices *)
    let to_int _ = None (* TODO: more precise for definite indices *)
  end

  module Z =
  struct
    include Printable.Z
    let name () = "Z index"
    let to_int z = Some z
    let equal_to z1 z2 =
      if Z.equal z2 z2 then
        `Eq
      else
        `Neq
  end
end


module Poly =
struct
  let rec map_indices g: 'a t -> 'b t = function
    | `NoOffset -> `NoOffset
    | `Field (f, o) -> `Field (f, map_indices g o)
    | `Index (i, o) -> `Index (g i, map_indices g o)
end

module MakePrintable (Idx: Index.Printable): Printable with type idx = Idx.t =
struct
  type idx = Idx.t
  type t = Idx.t offs [@@deriving eq, ord, hash]
  include Printable.StdLeaf

  let name () = Format.sprintf "offset (%s)" (Idx.name ())

  let rec cmp_zero_offset : t -> [`MustZero | `MustNonzero | `MayZero] = function
    | `NoOffset -> `MustZero
    | `Index (x, o) ->
      begin match cmp_zero_offset o, Idx.equal_to Z.zero x with
        | `MustNonzero, _
        | _, `Neq -> `MustNonzero
        | `MustZero, `Eq -> `MustZero
        | _, _ -> `MayZero
      end
    | `Field (x, o) ->
      if Cilfacade.is_first_field x then cmp_zero_offset o else `MustNonzero

  let rec show: t -> string = function
    | `NoOffset -> ""
    | `Index (x,o) -> "[" ^ (Idx.show x) ^ "]" ^ (show o)
    | `Field (x,o) -> "." ^ (x.fname) ^ (show o)

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  include Poly

  let rec is_definite: t -> bool = function
    | `NoOffset -> true
    | `Field (f,o) -> is_definite o
    | `Index (i,o) ->  Idx.to_int i <> None && is_definite o

  (* append offset o2 to o1 *)
  let rec add_offset (o1: t) (o2: t): t =
    match o1 with
    | `NoOffset -> o2
    | `Field (f1,o1) -> `Field (f1,add_offset o1 o2)
    | `Index (i1,o1) -> `Index (i1,add_offset o1 o2)

  let rec remove_offset: t -> t = function
    | `NoOffset -> `NoOffset
    | `Index (_,`NoOffset) | `Field (_,`NoOffset) -> `NoOffset
    | `Index (i,o) -> `Index (i, remove_offset o)
    | `Field (f,o) -> `Field (f, remove_offset o)

  let rec to_cil_offset (x:t) = (* TODO: rename/move *)
    match x with
    | `NoOffset -> NoOffset
    | `Field(f,o) -> Field(f, to_cil_offset o)
    | `Index(i,o) -> NoOffset (* array domain can not deal with this -> leads to being handeled as access to unknown part *)

  let rec to_exp: t -> exp offs = function (* TODO: Poly.map_indices *)
    | `NoOffset    -> `NoOffset
    | `Index (i,o) ->
      let i_exp = match Idx.to_int i with
        | Some i -> Const (CInt (i, Cilfacade.ptrdiff_ikind (), Some (Z.to_string i)))
        | None -> Lazy.force Index.Exp.any
      in
      `Index (i_exp, to_exp o)
    | `Field (f,o) -> `Field (f, to_exp o)

  let rec to_cil: t -> offset = function
    | `NoOffset    -> NoOffset
    | `Index (i,o) ->
      let i_exp = match Idx.to_int i with
        | Some i -> Const (CInt (i, Cilfacade.ptrdiff_ikind (), Some (Z.to_string i)))
        | None -> Lazy.force Index.Exp.any
      in
      Index (i_exp, to_cil o)
    | `Field (f,o) -> Field (f, to_cil o)

  let rec contains_index: t -> bool = function
    | `NoOffset -> false
    | `Field (_, os) -> contains_index os
    | `Index _ -> true

  (* tries to follow o in t *)
  let rec type_of ~base:t o = match unrollType t, o with (* resolves TNamed *)
    | t, `NoOffset -> t
    | TArray (t,_,_), `Index (i,o)
    | TPtr (t,_), `Index (i,o) -> type_of ~base:t o
    | TComp (ci,_), `Field (f,o) ->
      let fi = try getCompField ci f.fname
        with Not_found -> raise (Type_of_error (t, show o))
      in type_of ~base:fi.ftype o
    (* TODO: Why? Imprecise on zstd-thread-pool regression tests. *)
    (* | TComp _, `Index (_,o) -> type_of ~base:t o (* this happens (hmmer, perlbench). safe? *) *)
    | t, o -> raise (Type_of_error (t, show o))

  let rec prefix (x: t) (y: t): t option = match x,y with
    | `Index (x, xs), `Index (y, ys) when Idx.equal x y -> prefix xs ys
    | `Field (x, xs), `Field (y, ys) when CilType.Fieldinfo.equal x y -> prefix xs ys
    | `NoOffset, ys -> Some ys
    | _ -> None
end

module MakeLattice (Idx: Index.Lattice): Lattice with type idx = Idx.t =
struct
  include MakePrintable (Idx)

  let rec leq x y =
    match x, y with
    | `NoOffset, `NoOffset -> true
    | `Index (i1,o1), `Index (i2,o2) when Idx.leq i1 i2 -> leq o1 o2
    | `Field (f1,o1), `Field (f2,o2) when CilType.Fieldinfo.equal f1 f2 -> leq o1 o2
    | _ -> false

  let rec merge cop x y =
    let op = match cop with `Join -> Idx.join | `Meet -> Idx.meet | `Widen -> Idx.widen | `Narrow -> Idx.narrow in
    match x, y with
    | `NoOffset, `NoOffset -> `NoOffset
    | `Field (x1,y1), `Field (x2,y2) when CilType.Fieldinfo.equal x1 x2 -> `Field (x1, merge cop y1 y2)
    | `Index (x1,y1), `Index (x2,y2) -> `Index (op x1 x2, merge cop y1 y2)
    | _ -> raise Lattice.Uncomparable (* special case not used for AddressDomain any more due to splitting *)

  let join x y = merge `Join x y
  let meet x y = merge `Meet x y
  let widen x y = merge `Widen x y
  let narrow x y = merge `Narrow x y

  let top_indices = map_indices (fun _ -> Idx.top ())

  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with any_index_exp on all non-concrete cases. *)
  let rec of_exp: exp offs -> t = function (* TODO: Poly.map_indices *)
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt (i,ik,s)),o) -> `Index (Idx.of_int ik i, of_exp o)
    | `Index (_,o) -> `Index (Idx.top (), of_exp o)
    | `Field (f,o) -> `Field (f, of_exp o)

  let eight = Z.of_int 8

  let to_index ?typ (offs: t): Idx.t =
    let idx_of_int x =
      Idx.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int x)
    in
    let rec offset_to_index_offset ?typ offs = match offs with
      | `NoOffset -> idx_of_int 0
      | `Field (field, o) ->
        let field_as_offset = Field (field, NoOffset) in
        let bits_offset, _size = GoblintCil.bitsOffset (TComp (field.fcomp, [])) field_as_offset  in
        let bits_offset = Z.of_int bits_offset in
        (* Interval of floor and ceil division in case bitfield offset. *)
        let bytes_offset = Idx.of_interval (Cilfacade.ptrdiff_ikind ()) Z.(fdiv bits_offset eight, cdiv bits_offset eight) in
        let remaining_offset = offset_to_index_offset ~typ:field.ftype o in
        GobRef.wrap AnalysisState.executing_speculative_computations true @@ fun () -> Idx.add bytes_offset remaining_offset
      | `Index (x, o) ->
        let (item_typ, item_size_in_bytes) =
          match Option.map unrollType typ with
          | Some TArray(item_typ, _, _) ->
            let item_size_in_bytes = Cilfacade.bytesSizeOf item_typ in
            (Some item_typ, idx_of_int item_size_in_bytes)
          | _ ->
            (None, Idx.top ())
        in
        (* Binary operations on offsets should not generate overflow warnings in SV-COMP *)
        let bytes_offset = GobRef.wrap AnalysisState.executing_speculative_computations true @@ fun () -> Idx.mul item_size_in_bytes x in
        let remaining_offset = offset_to_index_offset ?typ:item_typ o in
        GobRef.wrap AnalysisState.executing_speculative_computations true @@ fun () -> Idx.add bytes_offset remaining_offset
    in
    offset_to_index_offset ?typ offs

  let semantic_equal ~typ1 xoffs ~typ2 yoffs =
    let x_index = to_index ~typ:typ1 xoffs in
    let y_index = to_index ~typ:typ2 yoffs in
    if M.tracing then M.tracel "addr" "xoffs=%a typ1=%a xindex=%a yoffs=%a typ2=%a yindex=%a" pretty xoffs d_plaintype typ1 Idx.pretty x_index pretty yoffs d_plaintype typ2 Idx.pretty y_index;
    Idx.to_bool (Idx.eq x_index y_index)

  include Lattice.NoBotTop

  let pretty_diff () (x,y) =
    Pretty.dprintf "%s: %a not equal %a" (name ()) pretty x pretty y
end

module Unit =
struct
  include MakePrintable (Index.Unit)

  (* TODO: rename to of_poly? *)
  let rec of_offs: 'i offs -> t = function (* TODO: Poly.map_indices *)
    | `NoOffset -> `NoOffset
    | `Field (f,o) -> `Field (f, of_offs o)
    | `Index (i,o) -> `Index ((), of_offs o)

  let rec of_cil: offset -> t = function
    | NoOffset    -> `NoOffset
    | Index (i,o) -> `Index ((), of_cil o)
    | Field (f,o) -> `Field (f, of_cil o)
end

module Exp =
struct
  include MakePrintable (Index.Exp)

  let top_indices = map_indices (fun _ -> Lazy.force Index.Exp.any)

  let rec of_cil: offset -> t = function
    | NoOffset    -> `NoOffset
    | Index (i,o) -> `Index (i, of_cil o)
    | Field (f,o) -> `Field (f, of_cil o)

  (* Overrides MakePrintable.to_cil. *)
  let rec to_cil: t -> offset = function
    | `NoOffset    -> NoOffset
    | `Index (i,o) -> Index (i, to_cil o)
    | `Field (f,o) -> Field (f, to_cil o)
end

module Z =
struct
  include MakePrintable (Index.Z)

  let is_definite _ = true (* override to avoid iterating over offset *)
end


let () = Printexc.register_printer (function
    | Type_of_error (t, o) -> Some (GobPretty.sprintf "Offset.Type_of_error(%a, %s)" d_plaintype t o)
    | _ -> None (* for other exceptions *)
  )
