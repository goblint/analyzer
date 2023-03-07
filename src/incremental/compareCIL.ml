open GoblintCil
open MyCFG
include DetectRenamedFunctions
include CompareAST
include CompareCFG
open CilMaps

let eq_glob (old: global_col) (current: global_col) (cfgs : (cfg * (cfg * cfg)) option) =
  let identical, diff, renamesOnSuccess = match old.def, current.def with
    | Some (Var x), Some (Var y) ->
      let identical, (_,_,_,renamesOnSuccess) = eq_varinfo x y ~rename_mapping:empty_rename_mapping in
      unchanged_to_change_status identical, None, renamesOnSuccess (* ignore the init_info - a changed init of a global will lead to a different start state *)
    | Some (Fun f), Some (Fun g) -> (
      let identical, diffOpt, funDep, globVarDep, renamesOnSuccess = CompareGlobals.eqF f g cfgs VarinfoMap.empty VarinfoMap.empty in
      (*Perform renames no matter what.*)
      match identical with
      | Unchanged when not (VarinfoMap.is_empty funDep && areGlobalVarRenameAssumptionsEmpty globVarDep) -> Changed, diffOpt, renamesOnSuccess
      | s -> s, diffOpt, renamesOnSuccess)
    | None, None -> (match old.decls, current.decls with
        | Some x, Some y ->
          let identical, (_,_,_,renamesOnSuccess) = eq_varinfo x y ~rename_mapping:empty_rename_mapping in
          unchanged_to_change_status identical, None, renamesOnSuccess
        | _, _ -> failwith "should never collect any empty entries in GlobalMap")
    | _, _ -> Changed, None, ([], []) (* it is considered to be changed (not added or removed) because a global collection only exists in the map if there is at least one declaration or definition for this global *) in
  performRenames renamesOnSuccess; (* updates enum names and compinfo names and keys that were collected during successful comparisons *)
  identical, diff

let compareCilFiles ?(eq=eq_glob) (oldAST: file) (newAST: file) =
  let cfgs = if GobConfig.get_string "incremental.compare" = "cfg"
    then Some (CfgTools.getCFG oldAST |> fst, CfgTools.getCFG newAST)
    else None in

  let addGlobal map global =
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

  (* Store a map from global names in the old file to the globals declarations and/or definition *)
  let oldMap = Cil.foldGlobals oldAST addGlobal GlobalMap.empty in
  let newMap = Cil.foldGlobals newAST addGlobal GlobalMap.empty in

  let changes = empty_change_info () in
  global_typ_acc := [];

  if GobConfig.get_bool "incremental.detect-renames" then (
    let renameDetectionResults = detectRenamedFunctions oldMap newMap in

    let addToChanges firstPass global status =
      match status with
      | SameName now when firstPass-> changes.unchanged <- {old=global; current=now} :: changes.unchanged
      | Renamed now when firstPass -> changes.unchanged <- {old=global; current=now} :: changes.unchanged
      | Modified (now, unchangedHeader) when firstPass -> changes.changed <- {old=global; current=now; unchangedHeader=unchangedHeader; diff=None} :: changes.changed
      | Created -> changes.added <- global :: changes.added
      | Deleted -> changes.removed <- global :: changes.removed
      | _ -> () in

    GlobalColMap.iter (addToChanges true) renameDetectionResults.statusForOldElem;
    GlobalColMap.iter (addToChanges false) renameDetectionResults.statusForOldElem;

  ) else (
    let findChanges map name current_global =
      try
        let old_global = GlobalMap.find name map in
        let change_status, diff = eq old_global current_global cfgs in
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
          append_to_changed ~unchangedHeader:false
      with Not_found -> changes.added <- current_global::changes.added (* Global could not be found in old map -> added *)
    in

    (* For each function in the new file, check whether a function with the same name
       already existed in the old version, and whether it is the same function. *)
    GlobalMap.iter (fun name glob_col -> findChanges oldMap name glob_col) newMap;
    GlobalMap.iter (fun name glob -> if not (GlobalMap.mem name newMap) then changes.removed <- (glob::changes.removed)) oldMap;
  );
  changes

(** Given an (optional) equality function between [Cil.global]s, an old and a new [Cil.file], this function computes a [change_info],
    which describes which [global]s are changed, unchanged, removed and added.  *)
let compareCilFiles ?eq (oldAST: file) (newAST: file) =
  Timing.wrap "compareCilFiles" (compareCilFiles ?eq oldAST) newAST
