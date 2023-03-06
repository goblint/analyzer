open GoblintCil
open MyCFG
include DetectRenamedFunctions
include CompareAST
include CompareCFG
open CilMaps

let eq_glob (old: global_col) (current: global_col) (cfgs : (cfg * (cfg * cfg)) option) = match old.def, current.def with
  | Some (Var x), Some (Var y) -> unchanged_to_change_status (eq_varinfo x y ~rename_mapping:empty_rename_mapping |> fst), None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | Some (Fun f), Some (Fun g) -> (
      let identical, diffOpt, funDep, globVarDep, renamesOnSuccess = CompareGlobals.eqF f g cfgs VarinfoMap.empty VarinfoMap.empty in
      (*Perform renames no matter what.*)
      let _ = performRenames renamesOnSuccess in
      match identical with
      | Unchanged when VarinfoMap.is_empty funDep && areGlobalVarRenameAssumptionsEmpty globVarDep -> Unchanged, diffOpt
      | _ -> Changed, None)

  | None, None -> (match old.decls, current.decls with
      | Some x, Some y -> unchanged_to_change_status (eq_varinfo x y ~rename_mapping:empty_rename_mapping |> fst), None
      | _, _ -> failwith "should never collect any empty entries in GlobalMap")
  | _, _ -> Changed, None (* it is considered to be changed (not added or removed) because a global collection only exists in the map
                             if there is at least one declaration or definition for this global *)

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
  let findChanges map name current_global =
    try
      if not (GobConfig.get_bool "incremental.detect-renames") then
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
          append_to_changed ~unchangedHeader:false;
    with Not_found -> changes.added <- current_global::changes.added (* Global could not be found in old map -> added *)
  in

  if GobConfig.get_bool "incremental.detect-renames" then (
    let renameDetectionResults = detectRenamedFunctions oldMap newMap in

    if Messages.tracing then
      GlobalColMap.to_seq renameDetectionResults |>
      Seq.iter
        (fun (gT, (functionGlobal, status)) ->
           Messages.trace "compareCIL" "Function status of %s is=" (name_of_global_col gT);
           match status with
           | Unchanged _ ->  Messages.trace "compareCIL" "Same Name\n";
           | Added ->  Messages.trace "compareCIL" "Added\n";
           | Removed ->  Messages.trace "compareCIL" "Removed\n";
           | Changed _ ->  Messages.trace "compareCIL" "Changed\n";
           | UnchangedButRenamed toFrom ->
             match toFrom.def with
             | Some(Fun f) ->  Messages.trace "compareCIL" "Renamed to %s\n" f.svar.vname;
             | Some(Var v) ->  Messages.trace "compareCIL" "Renamed to %s\n" v.vname;
             | _ -> ();
        );

    let unchanged, changed, added, removed = GlobalColMap.fold (fun _ (global, status) (u, c, a, r) ->
        match status with
        | Unchanged now -> (u @ [{old=global; current=now}], c, a, r)
        | UnchangedButRenamed now -> (u @ [{old=global; current=now}], c, a, r)
        | Added -> (u, c, a @ [global], r)
        | Removed -> (u, c, a, r @ [global])
        | Changed (now,unchangedHeader) -> (u, c @ [{old=global; current=now; unchangedHeader=unchangedHeader; diff=None}], a, r)
      ) renameDetectionResults (changes.unchanged, changes.changed, changes.added, changes.removed)
    in

    changes.added <- added;
    changes.removed <- removed;
    changes.changed <- changed;
    changes.unchanged <- unchanged;
  );

  (*  For each function in the new file, check whether a function with the same name
      already existed in the old version, and whether it is the same function. *)
  GlobalMap.iter (fun name glob_col -> findChanges oldMap name glob_col) newMap;

  if not (GobConfig.get_bool "incremental.detect-renames") then (
    GlobalMap.iter (fun name glob -> if not (GlobalMap.mem name newMap) then changes.removed <- (glob::changes.removed)) oldMap;
  );
  changes

(** Given an (optional) equality function between [Cil.global]s, an old and a new [Cil.file], this function computes a [change_info],
    which describes which [global]s are changed, unchanged, removed and added.  *)
let compareCilFiles ?eq (oldAST: file) (newAST: file) =
  Timing.wrap "compareCilFiles" (compareCilFiles ?eq oldAST) newAST
