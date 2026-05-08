(** An analysis for the detection of out-of-bounds memory accesses ([memOutOfBounds]).*)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AS = AnalysisState
module VDQ = ValueDomainQueries
module ID = IntDomain.IntDomTuple

(*
  Note:
  * This functionality is implemented as an analysis solely for the sake of maintaining
    separation of concerns, as well as for having the ablility to conveniently turn it on or off
  * It doesn't track any internal state
*)
module Spec =
struct
  include Analyses.IdentitySpec

  module D = Lattice.Unit
  include Analyses.ValueContexts(D)

  let context man _ _ = ()

  let name () = "memOutOfBounds"

  (* HELPER FUNCTIONS *)

  let report_oob fmt =
    set_mem_safety_flag InvalidDeref;
    Pretty.gprintf (fun doc ->
        M.warn ~category:(Behavior (Undefined MemoryOutOfBoundsAccess)) ~tags:[CWE 823] "%a" Pretty.insert doc;
        Checks.warn Checks.Category.InvalidMemoryAccess "%a" Pretty.insert doc;
      ) fmt

  let intdom_of_int x =
    ID.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int x)

  let size_of_type_in_bytes typ =
    intdom_of_int (Cilfacade.bytesSizeOf typ)

  let offs_lt_zero offs =
    try ID.lt offs (intdom_of_int 0)
    with IntDomain.ArithmeticOnIntegerBot _ -> None

  let check_deref_offset_bounds ptr_size offs =
    let ptr_size_le_offs =
      try ID.le ptr_size offs
      with IntDomain.ArithmeticOnIntegerBot _ -> None
    in
    offs_lt_zero offs, ptr_size_le_offs

  let check_ptr_offset_bounds ptr_size offs =
    let ptr_size_lt_offs =
      try ID.lt ptr_size offs
      with IntDomain.ArithmeticOnIntegerBot _ -> None
    in
    offs_lt_zero offs, ptr_size_lt_offs

  let add_offsets x y =
    try ID.add x y
    with IntDomain.ArithmeticOnIntegerBot _ -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()

  let points_to_alloc_only man ptr =
    match man.ask (Queries.MayPointTo ptr) with
    | a when not (Queries.AD.is_top a)->
      Queries.AD.for_all (function
          | Addr (v, o) -> man.ask (Queries.IsAllocVar v)
          | _ -> false
        ) a
    | _ -> false

  let get_size_of_ptr_target man ptr = (* TODO: deduplicate with base *)
    if points_to_alloc_only man ptr then
      (* Ask for BlobSize from the base address (the second component being set to true) in order to avoid BlobSize giving us bot *)
      man.ask (Queries.BlobSize {exp = ptr; base_address = true})
    else
      match man.ask (Queries.MayPointTo ptr) with
      | a when not (Queries.AD.is_top a) ->
        let pts_list = Queries.AD.elements a in
        let pts_elems_to_sizes (addr: Queries.AD.elt) =
          begin match addr with
            | Addr (v, _) ->
              if hasAttribute "goblint_cil_nested" v.vattr then (
                report_oob "Var %a is potentially accessed out-of-scope. Invalid memory access may occur" CilType.Varinfo.pretty v
              );
              begin match Cil.unrollType v.vtype with
                | TArray (item_typ, _, _) ->
                  let item_typ_size_in_bytes = size_of_type_in_bytes item_typ in
                  begin match man.ask (Queries.EvalLength ptr) with
                    | `Lifted arr_len ->
                      let arr_len_casted = ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) arr_len in (* TODO: proper castkind *)
                      begin
                        try `Lifted (ID.mul item_typ_size_in_bytes arr_len_casted)
                        with IntDomain.ArithmeticOnIntegerBot _ -> `Bot
                      end
                    | `Bot -> `Bot
                    | `Top -> `Top
                  end
                | _ ->
                  let type_size_in_bytes = size_of_type_in_bytes v.vtype in
                  `Lifted type_size_in_bytes
              end
            | _ -> `Top
          end
        in
        (* Map each points-to-set element to its size *)
        let pts_sizes = List.map pts_elems_to_sizes pts_list in
        (* Take the smallest of all sizes that ptr's contents may have *)
        begin match pts_sizes with
          | [] -> `Bot
          | [x] -> x
          | x::xs -> List.fold_left VDQ.ID.join x xs
        end
      | _ ->
        report_oob "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
        `Top

  let get_ptr_deref_type ptr_typ =
    match Cil.unrollType ptr_typ with
    | TPtr (t, _) -> Some t
    | _ -> None

  let eight = Z.of_int 8

  (* TODO: investigate why using PreValueDomain.Offs.to_index instead is too imprecise (to do with the Index case) *)
  let rec offs_to_idx typ offs =
    match offs with
    | `NoOffset -> intdom_of_int 0
    | `Field (field, o) ->
      let bits_offset = Cilfacade.fieldBitsOffsetOnly field in
      let bits_offset = Z.of_int bits_offset in
      (* Interval of floor and ceil division in case bitfield offset. *)
      let bytes_offset = ID.of_interval (Cilfacade.ptrdiff_ikind ()) Z.(fdiv bits_offset eight, cdiv bits_offset eight) in
      let remaining_offset = offs_to_idx field.ftype o in
      add_offsets bytes_offset remaining_offset
    | `Index (x, o) ->
      let typ_size_in_bytes = size_of_type_in_bytes typ in
      let bytes_offset = ID.mul typ_size_in_bytes x in
      let remaining_offset = offs_to_idx typ o in
      add_offsets bytes_offset remaining_offset

  let cil_offs_to_idx man typ offs =
    (* TODO: Some duplication with convert_offset in base.ml, unclear how to immediately get more reuse *)
    let rec convert_offset (ofs: offset) =
      match ofs with
      | NoOffset -> `NoOffset
      | Field (fld, ofs) -> `Field (fld, convert_offset ofs)
      | Index (exp, ofs) when Offset.Index.Exp.is_any exp -> (* special offset added by convertToQueryLval *)
        `Index (ID.top (), convert_offset ofs)
      | Index (exp, ofs) ->
        let i = match man.ask (Queries.EvalInt exp) with
          | `Lifted x -> ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) x (* TODO: proper castkind *)
          | _ -> ID.top_of @@ Cilfacade.ptrdiff_ikind ()
        in
        `Index (i, convert_offset ofs)
    in
    PreValueDomain.Offs.to_index (convert_offset offs)

  let check_unknown_addr_deref man ptr =
    let may_contain_unknown_addr =
      match man.ask (Queries.EvalValue ptr) with
      | a when not (Queries.VD.is_top a) ->
        begin match a with
          | Address a -> ValueDomain.AD.may_be_unknown a
          | _ -> false
        end
      (* Intuition: if ptr evaluates to top, it could potentially evaluate to the unknown address *)
      | _ -> true
    in
    if may_contain_unknown_addr then begin
      report_oob "Pointer %a contains an unknown address. Invalid dereference may occur" d_exp ptr
    end else
      Checks.safe Checks.Category.InvalidMemoryAccess

  let ptr_only_has_str_addr man ptr =
    match man.ask (Queries.EvalValue ptr) with
    | a when not (Queries.VD.is_top a) ->
      begin match a with
        | Address a -> ValueDomain.AD.for_all (fun addr -> match addr with | StrPtr _ -> true | _ -> false) a
        | _ -> false
      end
    (* Intuition: if ptr evaluates to top, it could all sorts of things and not only string addresses *)
    | _ -> false

  let get_addr_offs_from_ad man ptr a =
    match a with
    | a when not (VDQ.AD.is_top a) ->
      begin match get_ptr_deref_type @@ typeOf ptr with
        | Some t ->
          if VDQ.AD.is_empty a then
            ID.top_of @@ Cilfacade.ptrdiff_ikind ()
          else
            (* Get the address offsets of all points-to set elements *)
            let addr_offsets =
              VDQ.AD.filter (function Addr _ -> true | _ -> false) a
              |> VDQ.AD.to_mval
              |> List.map (fun (_, o) -> offs_to_idx t o)
            in
            if List.exists ID.is_bot addr_offsets then
              ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
            else if List.exists (ID.is_top_of (Cilfacade.ptrdiff_ikind ())) addr_offsets then
              ID.top_of @@ Cilfacade.ptrdiff_ikind ()
            else
              begin match addr_offsets with
                | [] -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
                | [x] -> x
                | x :: xs -> List.fold_left ID.join x xs
              end
        | None ->
          M.error "Expression %a doesn't have pointer type" d_exp ptr;
          ID.top_of @@ Cilfacade.ptrdiff_ikind ()
      end
    | _ ->
      ID.top_of @@ Cilfacade.ptrdiff_ikind ()

  let get_addr_offs man ptr =
    get_addr_offs_from_ad man ptr (man.ask (Queries.MayPointTo ptr))

  let report_unknown_object_size access_desc d_exp ptr_exp size_desc =
    report_oob "Size of object %s %a is %s. Memory out-of-bounds access might occur" access_desc d_exp ptr_exp size_desc

  let check_offset_against_object_size access_desc bounds_check man ptr_exp object_size offset =
    let ptrdiff_ikind = Cilfacade.ptrdiff_ikind () in
    let casted_object_size = ID.cast_to ~kind:Internal ptrdiff_ikind object_size in (* TODO: proper castkind *)
    let casted_offset = ID.cast_to ~kind:Internal ptrdiff_ikind offset in (* TODO: proper castkind *)
    begin match bounds_check casted_object_size casted_offset with
      | Some true, _
      | _, Some true ->
        report_oob "Size of object %s %a is %a (in bytes). It is offset by %a (in bytes). Memory out-of-bounds access must occur"
          access_desc d_exp ptr_exp ID.pretty casted_object_size ID.pretty casted_offset
      | Some false, Some false ->
        Checks.safe Checks.Category.InvalidMemoryAccess
      | _ ->
        report_oob "Could not compare size of object %s %a (%a) (in bytes) with offset (%a) (in bytes). Memory out-of-bounds access may occur"
          access_desc d_exp ptr_exp ID.pretty casted_object_size ID.pretty casted_offset
    end

  let check_ptr_deref_access man ptr_exp offs =
    check_unknown_addr_deref man ptr_exp;
    begin match get_size_of_ptr_target man ptr_exp with
      | `Top ->
        report_unknown_object_size "accessed through" d_exp ptr_exp "top"
      | `Bot ->
        report_unknown_object_size "accessed through" d_exp ptr_exp "bot"
      | `Lifted es ->
        let stored_addr_offs = get_addr_offs man ptr_exp in
        let lval_offs =
          match get_ptr_deref_type @@ typeOf ptr_exp with
          | Some t -> cil_offs_to_idx man t offs
          | None -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
        in
        let offs_intdom = add_offsets stored_addr_offs lval_offs in
        check_offset_against_object_size "accessed through" check_deref_offset_bounds man ptr_exp es offs_intdom
    end

  let check_ptr_value_access man ptr_exp addr_offs =
    match get_ptr_deref_type @@ typeOf ptr_exp with
    | Some _ ->
      check_unknown_addr_deref man ptr_exp;
      let ptr_size = get_size_of_ptr_target man ptr_exp in
      begin match ptr_size, addr_offs with
        | `Top, _ ->
          report_unknown_object_size "pointed to by" d_exp ptr_exp "top"
        | `Bot, _ ->
          report_unknown_object_size "pointed to by" d_exp ptr_exp "bot"
        | `Lifted ps, ao ->
          check_offset_against_object_size "pointed to by" check_ptr_offset_bounds man ptr_exp ps ao
      end
    | _ -> M.error "Expression %a is not a pointer" d_exp ptr_exp

  let check_access_for_oob man exp ad =
    let exp = Cil.stripCasts exp in
    match exp with
    (* Actual dereference/access through a pointer. *)
    | AddrOf (Mem e, o) when not (ptr_only_has_str_addr man e) ->
      check_ptr_deref_access man e o
    (* Taking the address of something is not an access itself. *)
    | AddrOf _ ->
      ()
    (* Pointer arithmetic / pointer value access. *)
    | _ when isPointerType (typeOf exp) && not (ptr_only_has_str_addr man exp) ->
      check_ptr_value_access man exp (get_addr_offs_from_ad man exp ad)
    | _ ->
      ()

  (* For memset() and memcpy() *)
  let check_count man fun_name ptr n =
    let ptr_size = get_size_of_ptr_target man ptr in
    let eval_n = man.ask (Queries.EvalInt n) in
    let addr_offs = get_addr_offs man ptr in
    match ptr_size, eval_n with
    | `Top, _ ->
      report_oob "Size of dest %a in function %s is unknown. Memory out-of-bounds access might occur" d_exp ptr fun_name;
    | _, `Top ->
      report_oob "Count parameter, passed to function %s is unknown. Memory out-of-bounds access might occur" fun_name
    | `Bot, _ ->
      report_oob "Size of dest %a in function %s is bottom. Memory out-of-bounds access might occur" d_exp ptr fun_name
    | _, `Bot ->
      report_oob "Count parameter, passed to function %s is bottom" fun_name
    | `Lifted ds, `Lifted en ->
      let casted_ds = ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) ds in (* TODO: proper castkind *)
      let casted_en = ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) en in (* TODO: proper castkind *)
      let casted_ao = ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ()) addr_offs in (* TODO: proper castkind *)
      let dest_size_lt_count = ID.lt casted_ds (ID.add casted_en casted_ao) in
      begin match dest_size_lt_count with
        | Some true ->
          report_oob "Size of %a in function %s is %a (in bytes) with an address offset of %a (in bytes). Count is %a (in bytes). Memory out-of-bounds access must occur" d_exp ptr fun_name ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en
        | Some false ->
          Checks.safe Checks.Category.InvalidMemoryAccess
        | None ->
          report_oob "Could not compare size of dest (%a) with address offset (%a) count (%a) in function %s. Memory out-of-bounds access may occur" ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en fun_name
      end
  let special man (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    let is_arg_accessed_through_pointer arg =
      let read_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = false } arglist in
      let read_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = true } arglist in
      let write_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
      let write_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
      List.mem arg read_shallow_args || List.mem arg read_deep_args || List.mem arg write_shallow_args || List.mem arg write_deep_args
    in
    (* Access events don't preserve whether AddrOf/StartOf arguments were dereferenced by a special access. *)
    List.iter (fun arg ->
        if is_arg_accessed_through_pointer arg then
          match Cil.stripCasts arg with
          | AddrOf lval
          | StartOf lval ->
            let ptr_exp = Lval lval in
            check_ptr_value_access man ptr_exp (get_addr_offs man ptr_exp)
          | _ ->
            ()
      ) arglist;
    (* Check calls to memset and memcpy for out-of-bounds-accesses *)
    match desc.special arglist with
    | Memset { dest; ch; count; } -> check_count man f.vname dest count;
    | Memcpy { dest; src; n = count; } ->
      (check_count man f.vname src count;
       check_count man f.vname dest count;)
    | _ -> man.local

  let startstate v = ()
  let exitstate v = ()

  let event man e oman =
    match e with
    | Events.Access {exp; ad; _} ->
      (* must use original (pre-assign, etc) man queries *)
      check_access_for_oob oman exp ad;
      man.local
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis ~dep:["access"] (module Spec : MCPSpec)
