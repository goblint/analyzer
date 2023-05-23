(** Comparison of CIL files. *)

open GoblintCil
open MyCFG
open CilMaps
include CompareAST
include CompareCFG

module GlobalMap = Map.Make(String)

type global_def = Var of varinfo | Fun of fundec
type global_col = {decls: varinfo option; def: global_def option}

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

let should_reanalyze (fdec: Cil.fundec) =
  List.mem fdec.svar.vname (GobConfig.get_string_list "incremental.force-reanalyze.funs")

(* If some CFGs of the two functions to be compared are provided, a fine-grained CFG comparison is done that also determines which
 * nodes of the function changed. If on the other hand no CFGs are provided, the "old" AST comparison on the CIL.file is
 * used for functions. Then no information is collected regarding which parts/nodes of the function changed. *)
let eqF (old: Cil.fundec) (current: Cil.fundec) (cfgs : (cfg * (cfg * cfg)) option) (global_rename_mapping: method_rename_assumptions) =
  if should_reanalyze current then
    ForceReanalyze current, None
  else
    (* let unchangedHeader = eq_varinfo old.svar current.svar && GobList.equal eq_varinfo old.sformals current.sformals in *)
    let emptyRenameMapping = (StringMap.empty, VarinfoMap.empty) in

    (* Compares the two varinfo lists, returning as a first element, if the size of the two lists are equal,
       and as a second a rename_mapping, holding the rename assumptions *)
    let rec rename_mapping_aware_compare (alocals: varinfo list) (blocals: varinfo list) (rename_mapping: string StringMap.t) = match alocals, blocals with
      | [], [] -> true, rename_mapping
      | origLocal :: als, nowLocal :: bls ->
        let new_mapping = StringMap.add origLocal.vname nowLocal.vname rename_mapping in

        (*TODO: maybe optimize this with eq_varinfo*)
        rename_mapping_aware_compare als bls new_mapping
      | _, _ -> false, rename_mapping
    in

    let unchangedHeader, headerRenameMapping = match cfgs with
      | None -> (
          let headerSizeEqual, headerRenameMapping = rename_mapping_aware_compare old.sformals current.sformals (StringMap.empty) in
          let actHeaderRenameMapping = (headerRenameMapping, global_rename_mapping) in
          eq_varinfo old.svar current.svar ~rename_mapping:actHeaderRenameMapping && GobList.equal (eq_varinfo ~rename_mapping:actHeaderRenameMapping) old.sformals current.sformals, headerRenameMapping
        )
      | Some _ -> (
          eq_varinfo old.svar current.svar ~rename_mapping:emptyRenameMapping && GobList.equal (eq_varinfo ~rename_mapping:emptyRenameMapping) old.sformals current.sformals, StringMap.empty
        )
    in
    if not unchangedHeader then ChangedFunHeader current, None
    else
      (* Here the local variables are checked to be equal *)
      (*flag: when running on cfg, true iff the locals are identical; on ast: if the size of the locals stayed the same*)
      let sameLocals, local_rename =
        match cfgs with
        | None -> rename_mapping_aware_compare old.slocals current.slocals headerRenameMapping
        | Some _ -> GobList.equal (eq_varinfo ~rename_mapping:emptyRenameMapping) old.slocals current.slocals, StringMap.empty
      in
      let rename_mapping: rename_mapping = (local_rename, global_rename_mapping) in

      if not sameLocals then
        (Changed, None)
      else
        match cfgs with
        | None -> unchanged_to_change_status (eq_block (old.sbody, old) (current.sbody, current) ~rename_mapping), None
        | Some (cfgOld, (cfgNew, cfgNewBack)) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgBidir = struct let prev = cfgNewBack let next = cfgNew end in
          let matches, diffNodes1 = compareFun (module CfgOld) (module CfgNew) old current in
          if diffNodes1 = [] then (Unchanged, None)
          else (Changed, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1})

let eq_glob (old: global_col) (current: global_col) (cfgs : (cfg * (cfg * cfg)) option) (global_rename_mapping: method_rename_assumptions) =
  match old.def, current.def with
  | Some (Var x), Some (Var y) -> unchanged_to_change_status (eq_varinfo x y ~rename_mapping:(StringMap.empty, VarinfoMap.empty)), None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | Some (Fun f), Some (Fun g) -> eqF f g cfgs global_rename_mapping
  | None, None -> (match old.decls, current.decls with
      | Some x, Some y -> unchanged_to_change_status (eq_varinfo x y ~rename_mapping:(StringMap.empty, VarinfoMap.empty)), None
      | _, _ -> failwith "should never collect any empty entries in GlobalMap")
  | _, _ -> Changed, None (* it is considered to be changed (not added or removed) because a global collection only exists in the map
                             if there is at least one declaration or definition for this global *)

let compareCilFiles ?(eq=eq_glob) (oldAST: file) (newAST: file) =
  let cfgs = if GobConfig.get_string "incremental.compare" = "cfg"
    then Some Batteries.(CfgTools.getCFG oldAST |> Tuple3.first, CfgTools.getCFG newAST |> Tuple3.get12)
    else None in

  let addGlobal map global  =
    try
      let name, col = match global with
        | GVar (v,_,_) -> v.vname, {decls = None; def = Some (Var v)}
        | GFun (f,_) -> f.svar.vname, {decls = None; def = Some (Fun f)}
        | GVarDecl (v,_) -> v.vname, {decls = Some v; def = None}
        | _ -> raise Not_found in
      let merge_d def1 def2 = match def1, def2 with
        | Some d, None -> Some d
        | None, Some d -> Some d
        | None, None -> None
        | _ -> failwith "there can only be one definition and one declaration per global" in
      let merge_global_col entry = match entry with
        | None -> Some col
        | Some col' -> Some {decls = merge_d col.decls col'.decls; def = merge_d col.def col'.def} in
      GlobalMap.update name merge_global_col map;
    with
      Not_found -> map
  in

  (* Store a map from functionNames in the old file to the function definition*)
  let oldMap = Cil.foldGlobals oldAST addGlobal GlobalMap.empty in
  let newMap = Cil.foldGlobals newAST addGlobal GlobalMap.empty in

  let generate_global_rename_mapping name current_global =
    try
      let old_global = GlobalMap.find name oldMap in
      match old_global.def, current_global.def with
      | Some (Fun f1), Some (Fun f2) ->
        let renamed_params: string StringMap.t = if List.compare_lengths f1.sformals f2.sformals = 0 then
            let mappings = List.combine f1.sformals f2.sformals |>
                           List.filter (fun (original, now) -> not (original.vname = now.vname)) |>
                           List.map (fun (original, now) -> (original.vname, now.vname)) |>
                           List.to_seq
            in
            StringMap.add_seq mappings StringMap.empty
          else StringMap.empty in
        if not (f1.svar.vname = f2.svar.vname) || (StringMap.cardinal renamed_params) > 0 then
          Some (f1.svar, {original_method_name = f1.svar.vname; new_method_name = f2.svar.vname; parameter_renames = renamed_params})
        else None
      | _, _ -> None
    with Not_found -> None
  in

  let global_rename_mapping: method_rename_assumptions = GlobalMap.fold (fun name global_col current_global_rename_mapping ->
      match generate_global_rename_mapping name global_col with
      | Some (funVar, rename_mapping) -> VarinfoMap.add funVar rename_mapping current_global_rename_mapping
      | None -> current_global_rename_mapping
    ) newMap VarinfoMap.empty in

  let changes = empty_change_info () in
  global_typ_acc := [];
  let findChanges map name current_global global_rename_mapping =
    try
      let old_global = GlobalMap.find name map in
      (* Do a (recursive) equal comparison ignoring location information *)
      let change_status, diff = eq old_global current_global cfgs global_rename_mapping in
      let append_to_changed ~unchangedHeader =
        changes.changed <- {current = current_global; old = old_global; unchangedHeader; diff} :: changes.changed
      in
      match change_status with
      | Changed ->
        append_to_changed ~unchangedHeader:true
      | Unchanged -> changes.unchanged <- {current = current_global; old = old_global} :: changes.unchanged
      | ChangedFunHeader f
      | ForceReanalyze f ->
        changes.exclude_from_rel_destab <- VarinfoSet.add f.svar changes.exclude_from_rel_destab;
        append_to_changed ~unchangedHeader:false;
    with Not_found -> changes.added <- current_global::changes.added (* Global could not be found in old map -> added *)
  in

  (*  For each function in the new file, check whether a function with the same name
      already existed in the old version, and whether it is the same function. *)
  GlobalMap.iter (fun name glob_col -> findChanges oldMap name glob_col global_rename_mapping) newMap;

  (* We check whether functions have been added or removed *)
  GlobalMap.iter (fun name glob -> if not (GlobalMap.mem name newMap) then changes.removed <- (glob::changes.removed)) oldMap;
  changes

(** Given an (optional) equality function between [Cil.global]s, an old and a new [Cil.file], this function computes a [change_info],
    which describes which [global]s are changed, unchanged, removed and added.  *)
let compareCilFiles ?eq (oldAST: file) (newAST: file) =
  Timing.wrap "compareCilFiles" (compareCilFiles ?eq oldAST) newAST
