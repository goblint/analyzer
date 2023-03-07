open GoblintCil
open MyCFG
open CilMaps
include CompareAST
include CompareCFG

module GlobalMap = Map.Make(String)

type global_def = Var of varinfo | Fun of fundec
type global_col = {decls: varinfo option; def: global_def option}

let name_of_global_col gc = match gc.def with
  | Some (Fun f) -> f.svar.vname
  | Some (Var v) -> v.vname
  | None -> match gc.decls with
    | Some v -> v.vname
    | None -> raise (Failure "empty global record")

let compare_global_col gc1 gc2 = compare (name_of_global_col gc1) (name_of_global_col gc2)

module GlobalColMap = Map.Make(
  struct
    type t = global_col
    let compare = compare_global_col
  end)

let name_of_global g = match g with
  | GVar (v,_,_) -> v.vname
  | GFun (f,_) -> f.svar.vname
  | GVarDecl (v,_) -> v.vname
  | _ -> failwith "global constructor not supported"

type nodes_diff = {
  unchangedNodes: (node * node) list;
  primObsoleteNodes: node list; (** primary obsolete nodes -> all obsolete nodes are reachable from these *)
}

type unchanged_global = {
  old: global_col;
  current: global_col
}
(** For semantically unchanged globals, still keep old and current version of global for resetting current to old. *)

type changed_global = {
  old: global_col;
  current: global_col;
  unchangedHeader: bool;
  diff: nodes_diff option
}

module VarinfoSet = Set.Make(CilType.Varinfo)

type change_info = {
  mutable changed: changed_global list;
  mutable unchanged: unchanged_global list;
  mutable removed: global_col list;
  mutable added: global_col list;
  mutable exclude_from_rel_destab: VarinfoSet.t;
  (** Set of functions that are to be force-reanalyzed.
      These functions are additionally included in the [changed] field, among the other changed globals. *)
}

let empty_change_info () : change_info =
  {added = []; removed = []; changed = []; unchanged = []; exclude_from_rel_destab = VarinfoSet.empty}

(* 'ChangedFunHeader' is used for functions whose varinfo or formal parameters changed. 'Changed' is used only for
 * changed functions whose header is unchanged and changed non-function globals *)
type change_status = Unchanged | Changed | ChangedFunHeader of Cil.fundec | ForceReanalyze of Cil.fundec

(** Given a boolean that indicates whether the code object is identical to the previous version, returns the corresponding [change_status]*)
let unchanged_to_change_status = function
  | true -> Unchanged
  | false -> Changed

let empty_rename_mapping: rename_mapping = (StringMap.empty, VarinfoMap.empty, VarinfoMap.empty, ([], []))

let should_reanalyze (fdec: Cil.fundec) =
  List.mem fdec.svar.vname (GobConfig.get_string_list "incremental.force-reanalyze.funs")

(* If some CFGs of the two functions to be compared are provided, a fine-grained CFG comparison is done that also determines which
 * nodes of the function changed. If on the other hand no CFGs are provided, the "old" AST comparison on the CIL.file is
 * used for functions. Then no information is collected regarding which parts/nodes of the function changed. *)
let eqF (old: Cil.fundec) (current: Cil.fundec) (cfgs : (cfg * (cfg * cfg)) option) (global_function_rename_mapping: method_rename_assumptions) (global_var_rename_mapping: glob_var_rename_assumptions) =
  let identical, diffOpt, (_, renamed_method_dependencies, renamed_global_vars_dependencies, renamesOnSuccess) =
    if should_reanalyze current then
      ForceReanalyze current, None, empty_rename_mapping
    else

      let add_locals_to_rename_mapping la lb map =
        try
          List.fold_left (fun map (a, b) -> StringMap.add a.vname b.vname map) map (List.combine la lb)
        with Invalid_argument _ -> map in

      let parameterMapping = add_locals_to_rename_mapping old.sformals current.sformals StringMap.empty in
      let renameMapping = (parameterMapping, global_function_rename_mapping, global_var_rename_mapping, ([], [])) in

      (* compare the function header based on the collected rename assumptions for parameters *)
      let unchangedHeader, renameMapping = eq_varinfo old.svar current.svar ~rename_mapping:renameMapping
                                           &&>> forward_list_equal eq_varinfo old.sformals current.sformals in

      if not unchangedHeader then ChangedFunHeader current, None, empty_rename_mapping
      else
        (* include matching of local variables into rename mapping *)
        let renameMapping = match renameMapping with
          | (pm, gf, gv, re) -> (add_locals_to_rename_mapping old.slocals current.slocals pm, gf, gv, re) in
        let sameLocals, renameMapping = forward_list_equal eq_varinfo old.slocals current.slocals ~rename_mapping:renameMapping in

        if not sameLocals then
          (Changed, None, empty_rename_mapping)
        else
          match cfgs with
          | None ->
            let (identical, new_rename_mapping) = eq_block (old.sbody, old) (current.sbody, current) ~rename_mapping:renameMapping in
            unchanged_to_change_status identical, None, new_rename_mapping
          | Some (cfgOld, (cfgNew, cfgNewBack)) ->
            let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
            let module CfgNew : MyCFG.CfgBidir = struct let prev = cfgNewBack let next = cfgNew end in
            let matches, diffNodes1, updated_rename_mapping = compareFun (module CfgOld) (module CfgNew) old current renameMapping in
            if diffNodes1 = [] then (Unchanged, None, updated_rename_mapping)
            else (Changed, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1}, updated_rename_mapping)
  in
  identical, diffOpt, renamed_method_dependencies, renamed_global_vars_dependencies, renamesOnSuccess
