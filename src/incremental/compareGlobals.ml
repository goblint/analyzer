open Cil
open MyCFG
open CilMaps
include CompareAST
include CompareCFG

type nodes_diff = {
  unchangedNodes: (node * node) list;
  primObsoleteNodes: node list; (** primary obsolete nodes -> all obsolete nodes are reachable from these *)
}

type unchanged_global = {
  old: global;
  current: global
}
(** For semantically unchanged globals, still keep old and current version of global for resetting current to old. *)

type changed_global = {
  old: global;
  current: global;
  unchangedHeader: bool;
  diff: nodes_diff option
}

type change_info = {
  mutable changed: changed_global list;
  mutable unchanged: unchanged_global list;
  mutable removed: global list;
  mutable added: global list
}

let should_reanalyze (fdec: Cil.fundec) =
  List.mem fdec.svar.vname (GobConfig.get_string_list "incremental.force-reanalyze.funs")

(* If some CFGs of the two functions to be compared are provided, a fine-grained CFG comparison is done that also determines which
 * nodes of the function changed. If on the other hand no CFGs are provided, the "old" AST comparison on the CIL.file is
 * used for functions. Then no information is collected regarding which parts/nodes of the function changed. *)
let eqF (a: Cil.fundec) (b: Cil.fundec) (cfgs : (cfg * (cfg * cfg)) option) (global_function_rename_mapping: method_rename_assumptions) (global_var_rename_mapping: glob_var_rename_assumptions) =
  (* Compares the two varinfo lists, returning as a first element, if the size of the two lists are equal,
   * and as a second a rename_mapping, holding the rename assumptions *)
  let rec rename_mapping_aware_compare (alocals: varinfo list) (blocals: varinfo list) (rename_mapping: string StringMap.t) = match alocals, blocals with
    | [], [] -> true, rename_mapping
    | origLocal :: als, nowLocal :: bls ->
      let new_mapping = StringMap.add origLocal.vname nowLocal.vname rename_mapping in

      (*TODO: maybe optimize this with eq_varinfo*)
      rename_mapping_aware_compare als bls new_mapping
    | _, _ -> false, rename_mapping
  in

  let unchangedHeader, headerRenameMapping, renamesOnSuccessHeader = match cfgs with
    | None -> (
        let headerSizeEqual, headerRenameMapping = rename_mapping_aware_compare a.sformals b.sformals (StringMap.empty) in
        let actHeaderRenameMapping: rename_mapping = (headerRenameMapping, global_function_rename_mapping, global_var_rename_mapping, ([], [])) in

        let (unchangedHeader, (_, _, _, renamesOnSuccessHeader)) = eq_varinfo a.svar b.svar actHeaderRenameMapping &&>>
                                                                   forward_list_equal eq_varinfo a.sformals b.sformals in
        unchangedHeader, headerRenameMapping, renamesOnSuccessHeader
      )
    | Some _ -> (
        let unchangedHeader, headerRenameMapping = eq_varinfo a.svar b.svar emptyRenameMapping &&>>
                                                   forward_list_equal eq_varinfo a.sformals b.sformals in
        let (_, _, _, renamesOnSuccessHeader) = headerRenameMapping in

        (unchangedHeader && is_rename_mapping_empty headerRenameMapping), StringMap.empty, renamesOnSuccessHeader
      )
  in

  let identical, diffOpt, (_, renamed_method_dependencies, renamed_global_vars_dependencies, renamesOnSuccess) =
    if should_reanalyze a then
      false, None, emptyRenameMapping
    else
      (* Here the local variables are checked to be equal *)
      (*flag: when running on cfg, true iff the locals are identical; on ast: if the size of the locals stayed the same*)
      let flag, rename_mapping =
        match cfgs with
        | None -> (
            let sizeEqual, local_rename = rename_mapping_aware_compare a.slocals b.slocals headerRenameMapping in
            sizeEqual, (local_rename, global_function_rename_mapping, global_var_rename_mapping, renamesOnSuccessHeader)
          )
        | Some _ -> (
            let isEqual, rename_mapping = forward_list_equal eq_varinfo a.slocals b.slocals (StringMap.empty, VarinfoMap.empty, VarinfoMap.empty, renamesOnSuccessHeader) in
            isEqual && is_rename_mapping_empty rename_mapping, rename_mapping
          )
      in

      let sameDef = unchangedHeader && flag in
      if not sameDef then
        (false, None, emptyRenameMapping)
      else
        match cfgs with
        | None ->
          let (identical, new_rename_mapping) = eq_block (a.sbody, a) (b.sbody, b) rename_mapping in
          identical, None, new_rename_mapping
        | Some (cfgOld, (cfgNew, cfgNewBack)) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgBidir = struct let prev = cfgNewBack let next = cfgNew end in
          let matches, diffNodes1, updated_rename_mapping = compareFun (module CfgOld) (module CfgNew) a b rename_mapping in
          if diffNodes1 = [] then (true, None, updated_rename_mapping)
          else (false, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1}, updated_rename_mapping)
  in
  identical, unchangedHeader, diffOpt, renamed_method_dependencies, renamed_global_vars_dependencies, renamesOnSuccess
