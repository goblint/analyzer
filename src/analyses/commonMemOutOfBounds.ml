(** MemOutOfBounds-modular value analysis utilities for {!Base} and {!MemOutOfBounds}. *)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AS = AnalysisState
module VDQ = ValueDomainQueries
module ID = IntDomain.IntDomTuple

module CommonFunctions = 
struct
  let size_of_type_in_bytes typ =
    let typ_size_in_bytes = (bitsSizeOf typ) / 8 in
    ID.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int typ_size_in_bytes)

  let eval_ptr_offset_in_binop ctx exp ptr_contents_typ = 
    let eval_offset = ctx.ask (Queries.EvalInt exp) in
    let ptr_contents_typ_size_in_bytes = size_of_type_in_bytes ptr_contents_typ in
    match eval_offset with
    | `Lifted eo ->
      let casted_eo = ID.cast_to (Cilfacade.ptrdiff_ikind ()) eo in
      begin
        try  (ID.mul casted_eo ptr_contents_typ_size_in_bytes)
        with IntDomain.ArithmeticOnIntegerBot _ -> ID.top ()
      end
    | `Top | `Bot -> ID.top ()

  let points_to_heap_only ctx ptr =
    match ctx.ask (Queries.MayPointTo ptr) with
    | a when not (Queries.AD.is_top a)->
      Queries.AD.for_all (function
          | Addr (v, o) -> ctx.ask (Queries.IsHeapVar v)
          | _ -> false
        ) a
    | _ -> false
  let get_size_of_ptr_target ctx ptr =
    if points_to_heap_only ctx ptr then
      (* Ask for BlobSize from the base address (the second component being set to true) in order to avoid BlobSize giving us bot *)
      ctx.ask (Queries.BlobSize {exp = ptr; base_address = true})
    else
      match ctx.ask (Queries.MayPointTo ptr) with
      | a when not (Queries.AD.is_top a) ->
        if M.tracing then M.trace "OOB" "else\n";
        let pts_list = Queries.AD.elements a in
        let pts_elems_to_sizes (addr: Queries.AD.elt) =
          begin match addr with
            | Addr (v, _) ->
              if hasAttribute "goblint_cil_nested" v.vattr then (
                AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
                M.warn "Var %a is potentially accessed out-of-scope. Invalid memory access may occur" CilType.Varinfo.pretty v
              );
              begin match v.vtype with
                | TArray (item_typ, _, _) ->
                  let item_typ_size_in_bytes = size_of_type_in_bytes item_typ in
                  begin match ctx.ask (Queries.EvalLength ptr) with
                    | `Lifted arr_len ->
                      let arr_len_casted = ID.cast_to (Cilfacade.ptrdiff_ikind ()) arr_len in
                      if M.tracing then M.trace "OOB" "arr_len_casted=%a item_typ_size_in_bytes=%a\n" ID.pretty arr_len_casted ID.pretty item_typ_size_in_bytes;
                      begin
                        try `Lifted (ID.mul item_typ_size_in_bytes arr_len_casted)
                        with IntDomain.ArithmeticOnIntegerBot _ -> `Bot
                      end
                    | `Bot -> if M.tracing then M.trace "OOB" "TArray Bot %a\n" d_exp ptr; `Bot
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

        if M.tracing then M.trace "OOB" "ptrs_elements_to_sizes=%a\n" (Pretty.d_list ", " VDQ.ID.pretty) ( pts_sizes);
        begin match pts_sizes with
          | [] -> if M.tracing then M.trace "OOB" "ptr_sizess Bot %a\n" d_exp ptr; `Bot
          | [x] -> x
          | x::xs -> List.fold_left ValueDomainQueries.ID.join x xs
        end
      | _ ->
        (AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
         M.warn "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
         `Top)


  let cil_offs_to_idx ctx typ offs =
    (* TODO: Some duplication with convert_offset in base.ml, unclear how to immediately get more reuse *)
    let rec convert_offset (ofs: offset) =
      match ofs with
      | NoOffset -> `NoOffset
      | Field (fld, ofs) -> `Field (fld, convert_offset ofs)
      | Index (exp, ofs) when CilType.Exp.equal exp Offset.Index.Exp.any -> (* special offset added by convertToQueryLval *)
        `Index (ID.top (), convert_offset ofs)
      | Index (exp, ofs) ->
        let i = match ctx.ask (Queries.EvalInt exp) with
          | `Lifted x -> x
          | _ -> ID.top_of @@ Cilfacade.ptrdiff_ikind ()
        in
        `Index (i, convert_offset ofs)
    in
    PreValueDomain.Offs.to_index (convert_offset offs)

end