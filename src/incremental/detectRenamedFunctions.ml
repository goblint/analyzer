open GoblintCil
include CompareGlobals
open CilMaps

let performRenames (renamesOnSuccess: renamesOnSuccess) =
  begin
    let (compinfoRenames, enumRenames) = renamesOnSuccess in
    List.iter (fun (compinfo2, compinfo1) -> compinfo2.cname <- compinfo1.cname; compinfo2.ckey <- compinfo1.ckey) compinfoRenames;
    List.iter (fun (enum2, enum1) -> enum2.ename <- enum1.ename) enumRenames;
  end

let preservesSameNameMatches n_old oldMap n_new newMap = n_old = n_new || (not (GlobalMap.mem n_old newMap) && not (GlobalMap.mem n_new oldMap))

let addToFinalMatchesMapping oV nV final_matches =
  VarinfoMap.add oV nV (fst final_matches), VarinfoMap.add nV oV (snd final_matches)

(* TODO: possibly merge with eq_varinfo, provide only varinfo and mapping from varinfo to global_col *)
(* Compares two varinfos. finalizeOnlyExactMatch=true allows to check a rename assumption and discard the comparison result in case they do not match *)
let compare_varinfo ?(finalizeOnlyExactMatch=false) oV gc_old oldMap nV gc_new newMap change_info final_matches =
  if not (preservesSameNameMatches oV.vname oldMap nV.vname newMap) then
    (* do not allow for matches between differently named variables if one of the variables names exists in both, the new and old file *)
    false, change_info, final_matches
  else (
    (* TODO does the emptyness of the dependencies need to be checked? *)
    let identical, (_, function_dependencies, global_var_dependencies, renamesOnSuccess) = eq_varinfo oV nV ~rename_mapping:empty_rename_mapping in

    if not finalizeOnlyExactMatch || identical then
      performRenames renamesOnSuccess; (* updates enum names and compinfo names and keys that were collected during comparison of this matched function *)
    if identical then (
      change_info.unchanged <- {old = gc_old; current = gc_new} :: change_info.unchanged;
      true, change_info, addToFinalMatchesMapping oV nV final_matches
    ) else if not finalizeOnlyExactMatch then (
      change_info.changed <- {old = gc_old; current = gc_new; unchangedHeader = true; diff = None} :: change_info.changed;
      false, change_info, addToFinalMatchesMapping oV nV final_matches
    ) else
      false, change_info, final_matches
  )
let compare_varinfo_exact = compare_varinfo ~finalizeOnlyExactMatch:true

let get_varinfo gc = match gc.decls, gc.def with
    | _, Some (Var v) -> v
    | _, Some (Fun f) -> f.svar
    | Some v, _ -> v
    | _ -> failwith "A global should have at least a declaration or a definition"
let addNewGlobals name gc_new (change_info, final_matches) =
  if not (VarinfoMap.mem (get_varinfo gc_new) (snd final_matches)) then
    change_info.added <- gc_new :: change_info.added;
  (change_info, final_matches)

let addOldGlobals name gc_old (change_info, final_matches) =
  if not (VarinfoMap.mem (get_varinfo gc_old) (fst final_matches)) then
    change_info.removed <- gc_old :: change_info.removed;
  (change_info, final_matches)

let iname cg = List.mem (name_of_global_col cg) ["main"; "foo"; "bar"]
let inamev v = List.mem v.vname ["main"; "foo"; "bar"]

let detectRenamedFunctions (oldMap : global_col StringMap.t) (newMap : global_col StringMap.t) =
  let extract_fundecs _ gc map = match gc.def with
    | Some (Fun f) -> VarinfoMap.add f.svar f map
    | _ -> map in
  let var_fun_old = GlobalMap.fold extract_fundecs oldMap VarinfoMap.empty in
  let var_fun_new = GlobalMap.fold extract_fundecs newMap VarinfoMap.empty in
  let extract_globs _ gc map =
    let v = get_varinfo gc in
    VarinfoMap.add v gc map in
  let var_glob_old = GlobalMap.fold extract_globs oldMap VarinfoMap.empty in
  let var_glob_new = GlobalMap.fold extract_globs newMap VarinfoMap.empty in
  let empty_rename_assms m = VarinfoMap.for_all (fun vo vn -> vo.vname = vn.vname) m in (* TODO or in final_matches? *)

  let compare_fundec_exact_match f1 f2 change_info final_matches =
    (* check that names of match are each only contained in new or old file *)
    if not (preservesSameNameMatches f1.svar.vname oldMap f2.svar.vname newMap) then (
      false, change_info, final_matches
    ) else
      let doMatch, diff, fun_deps, global_deps, renamesOnSuccess = CompareGlobals.eqF f1 f2 None VarinfoMap.empty VarinfoMap.empty in
      match doMatch with
      | Unchanged when empty_rename_assms (VarinfoMap.filter (fun vo vn -> not (vo.vname = f1.svar.vname && vn.vname = f2.svar.vname)) fun_deps) && empty_rename_assms global_deps ->
        performRenames renamesOnSuccess;
        change_info.unchanged <- {old = VarinfoMap.find f1.svar var_glob_old; current = VarinfoMap.find f2.svar var_glob_new} :: change_info.unchanged;
        let final_matches = addToFinalMatchesMapping f1.svar f2.svar final_matches in
        true, change_info, final_matches
      | Unchanged -> false, change_info, final_matches
      | Changed -> false, change_info, final_matches
      | ChangedFunHeader _ -> false, change_info, final_matches
      | ForceReanalyze _ -> false, change_info, final_matches

  in

  let matchGlobal ~matchVars ~matchFuns name gc_old (change_info, final_matches) =
    try
      let gc_new = StringMap.find name newMap in

      let compare_same_name_fundec_check_contained_renames f1 f2 =
        let doMatch, diff, function_dependencies, global_var_dependencies, renamesOnSuccess = CompareGlobals.eqF f1 f2 None VarinfoMap.empty VarinfoMap.empty in
        performRenames renamesOnSuccess; (* updates enum names and compinfo names and keys that were collected during comparison of this matched function *)
        let funDependenciesMatch, change_info, final_matches = VarinfoMap.fold (fun f_old_var f_new_var (acc, ci, fm) ->
            match VarinfoMap.find_opt f_old_var (fst final_matches) with
            | None ->
              let f_old = VarinfoMap.find f_old_var var_fun_old in
              let f_new = VarinfoMap.find f_new_var var_fun_new in (* TODO: what happens if there exists no fundec for this varinfo? *)
              if acc then
                compare_fundec_exact_match f_old f_new ci fm
              else false, ci, fm
            | Some v -> v = f_new_var, ci, fm) function_dependencies (true, change_info, final_matches) in
        let globalDependenciesMatch, change_info, final_matches = VarinfoMap.fold (fun old_var new_var (acc, ci, fm) ->
            match VarinfoMap.find_opt old_var (fst final_matches) with
            | None ->
              if acc then
                compare_varinfo_exact old_var gc_old oldMap new_var gc_new newMap ci fm
              else false, ci, fm
            | Some v -> v = new_var, ci, fm
          ) global_var_dependencies (true, change_info, final_matches) in
        let dependenciesMatch = funDependenciesMatch && globalDependenciesMatch in
        let append_to_changed ~unchangedHeader ~diff =
          change_info.changed <- {current = gc_new; old = gc_old; unchangedHeader; diff} :: change_info.changed
        in
        (* TODO: merge with no-rename-detection case in compareCIL.compareCilFiles *)
        (match doMatch with
        | Unchanged when dependenciesMatch ->
          change_info.unchanged <- {old = gc_old; current = gc_new} :: change_info.unchanged
        | Unchanged ->
          (* no diff is stored, also when comparing functions based on CFG because currently there is no mechanism to detect which part was affected by the *)
          append_to_changed ~unchangedHeader:true ~diff:None
        | Changed -> append_to_changed ~unchangedHeader:true ~diff:diff
        | _ -> (* this can only be ForceReanalyze or ChangedFunHeader *)
          change_info.exclude_from_rel_destab <- VarinfoSet.add f1.svar change_info.exclude_from_rel_destab;
          append_to_changed ~unchangedHeader:false ~diff:None);
        addToFinalMatchesMapping f1.svar f2.svar final_matches in

      match gc_old.def, gc_new.def with
      | Some (Var v1), Some (Var v2) when matchVars -> let _, ci, fm = compare_varinfo v1 gc_old oldMap v2 gc_new newMap change_info final_matches in ci, fm
      | Some (Fun f1), Some (Fun f2) when matchFuns -> change_info, compare_same_name_fundec_check_contained_renames f1 f2
      | None, None -> (match gc_old.decls, gc_new.decls with
          | Some v1, Some v2 when matchVars -> let _, ci, fm = compare_varinfo v1 gc_old oldMap v2 gc_new newMap change_info final_matches in ci, fm
          | _ -> change_info, final_matches)
      | _ -> change_info, final_matches
    with Not_found -> change_info, final_matches in

  (empty_change_info (), (VarinfoMap.empty, VarinfoMap.empty)) (* change_info and final_matches (bi-directional) is propagated *)
  |> GlobalMap.fold (matchGlobal ~matchVars:true ~matchFuns:false) oldMap
  |> GlobalMap.fold (matchGlobal ~matchVars:false ~matchFuns:true) oldMap
  |> GlobalMap.fold addNewGlobals newMap
  |> GlobalMap.fold addOldGlobals oldMap
