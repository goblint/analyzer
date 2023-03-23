open GoblintCil
open MyCFG
include CompareAST
include CompareCFG
open CilMaps

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

let get_varinfo gc = match gc.decls, gc.def with
  | _, Some (Var v) -> v
  | _, Some (Fun f) -> f.svar
  | Some v, _ -> v
  | _ -> failwith "A global should have at least a declaration or a definition"

let get_fundec gc = match gc.decls, gc.def with
  | _, Some (Fun f) -> f
  | _ -> failwith "Global does not have a function definition"

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

let performRenames (renamesOnSuccess: renamesOnSuccess) =
  begin
    let (compinfoRenames, enumRenames) = renamesOnSuccess in
    List.iter (fun (compinfo2, compinfo1) -> compinfo2.cname <- compinfo1.cname; compinfo2.ckey <- compinfo1.ckey) compinfoRenames;
    List.iter (fun (enum2, enum1) -> enum2.ename <- enum1.ename) enumRenames;
  end

let preservesSameNameMatches n_old oldMap n_new newMap = n_old = n_new || (not (GlobalMap.mem n_old newMap) && not (GlobalMap.mem n_new oldMap))

let addToFinalMatchesMapping oV nV final_matches =
  VarinfoMap.add oV nV (fst final_matches), VarinfoMap.add nV oV (snd final_matches)

let empty_rename_assms m = VarinfoMap.for_all (fun vo vn -> vo.vname = vn.vname) m

(* Compares two varinfos of globals. finalizeOnlyExactMatch=true allows to check a rename assumption and discard the comparison result in case they do not match *)
let eq_glob_var ?(finalizeOnlyExactMatch=false) oV gc_old oldMap nV gc_new newMap change_info final_matches =
  if not (preservesSameNameMatches oV.vname oldMap nV.vname newMap) then
    (* do not allow for matches between differently named variables if one of the variables names exists in both, the new and old file *)
    false, change_info, final_matches
  else (
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
let compare_varinfo_exact = eq_glob_var ~finalizeOnlyExactMatch:true

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
          List.fold_left2 (fun map a b -> StringMap.add a.vname b.vname map) map la lb
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

let eqF_only_consider_exact_match f1 f2 change_info final_matches oldMap newMap var_glob_old var_glob_new =
  (* check that names of match are each only contained in new or old file *)
  if not (preservesSameNameMatches f1.svar.vname oldMap f2.svar.vname newMap) then (
    false, change_info, final_matches
  ) else
    (* the exact comparison is always uses the AST comparison because only when unchanged this match is manifested *)
    let doMatch, diff, fun_deps, global_deps, renamesOnSuccess = eqF f1 f2 None VarinfoMap.empty VarinfoMap.empty in
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

let eqF_check_contained_renames ~renameDetection f1 f2 oldMap newMap cfgs gc_old gc_new (change_info, final_matches) =
  let doMatch, diff, function_dependencies, global_var_dependencies, renamesOnSuccess = eqF f1 f2 cfgs VarinfoMap.empty VarinfoMap.empty in
  performRenames renamesOnSuccess; (* updates enum names and compinfo names and keys that were collected during comparison of this matched function *)

  (* for rename detection, check whether the rename assumptions collected during the function comparison actually match exactly,
     otherwise check that the function comparison was successful without collecting any rename assumptions *)
  let dependenciesMatch, change_info, final_matches =
    if renameDetection then
      let funDependenciesMatch, change_info, final_matches =
        let extract_globs _ gc map =
          let v = get_varinfo gc in
          VarinfoMap.add v gc map in
        let var_glob_old = GlobalMap.fold extract_globs oldMap VarinfoMap.empty in
        let var_glob_new = GlobalMap.fold extract_globs newMap VarinfoMap.empty in
        VarinfoMap.fold (fun f_old_var f_new_var (acc, ci, fm) ->
            match VarinfoMap.find_opt f_old_var (fst fm) with
            | None ->
              let f_old = get_fundec (VarinfoMap.find f_old_var var_glob_old) in
              let f_new = get_fundec (VarinfoMap.find f_new_var var_glob_new) in (* TODO: what happens if there exists no fundec for this varinfo? *)
              if acc then
                eqF_only_consider_exact_match f_old f_new ci fm oldMap newMap var_glob_old var_glob_new
              else false, ci, fm
            | Some v -> v.vid = f_new_var.vid, ci, fm) function_dependencies (true, change_info, final_matches) in
      let globalDependenciesMatch, change_info, final_matches = VarinfoMap.fold (fun old_var new_var (acc, ci, fm) ->
          match VarinfoMap.find_opt old_var (fst fm) with
          | None ->
            if acc then
              compare_varinfo_exact old_var gc_old oldMap new_var gc_new newMap ci fm
            else false, ci, fm
          | Some v -> v.vid = new_var.vid, ci, fm
        ) global_var_dependencies (true, change_info, final_matches) in
      funDependenciesMatch && globalDependenciesMatch, change_info, final_matches
    else
      empty_rename_assms function_dependencies && empty_rename_assms global_var_dependencies, change_info, final_matches in

  let append_to_changed ~unchangedHeader ~diff =
    change_info.changed <- {current = gc_new; old = gc_old; unchangedHeader; diff} :: change_info.changed
  in
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
  change_info, addToFinalMatchesMapping f1.svar f2.svar final_matches

let eq_glob ?(matchVars=true) ?(matchFuns=true) ?(renameDetection=false) oldMap newMap cfgs gc_old gc_new (change_info, final_matches) =
  match gc_old.def, gc_new.def with
  | Some (Var v1), Some (Var v2) when matchVars -> let _, ci, fm = eq_glob_var v1 gc_old oldMap v2 gc_new newMap change_info final_matches in ci, fm
  | Some (Fun f1), Some (Fun f2) when matchFuns ->
    eqF_check_contained_renames ~renameDetection f1 f2 oldMap newMap cfgs gc_old gc_new (change_info, final_matches)
  | None, None -> (match gc_old.decls, gc_new.decls with
      | Some v1, Some v2 when matchVars -> let _, ci, fm = eq_glob_var v1 gc_old oldMap v2 gc_new newMap change_info final_matches in ci, fm
      | _ -> change_info, final_matches (* a global collection should never be empty *))
  (* Without rename detection a global definition or declaration that does not have respective counterpart in the other version is considered to be changed (not added or removed)
     because a global collection only exists in the map if there is at least one declaration or definition for this global.
     For the rename detection they can only be added to changed when the according flag is set, because there would be duplicates when iterating over the globals several times. *)
  | Some (Var _), None
  | None, Some (Var _) -> if matchVars then (
      change_info.changed <- {old = gc_old; current = gc_new; diff = None; unchangedHeader = true} :: change_info.changed;
      change_info, addToFinalMatchesMapping (get_varinfo gc_old) (get_varinfo gc_new) final_matches)
    else
      change_info, final_matches
  | _, _ -> if matchVars && matchFuns then (
      change_info.changed <- {old = gc_old; current = gc_new; diff = None; unchangedHeader = true} :: change_info.changed;
      change_info, addToFinalMatchesMapping (get_varinfo gc_old) (get_varinfo gc_new) final_matches)
    else
      change_info, final_matches

let addNewGlobals name gc_new (change_info, final_matches) =
  if not (VarinfoMap.mem (get_varinfo gc_new) (snd final_matches)) then
    change_info.added <- gc_new :: change_info.added;
  (change_info, final_matches)

let addOldGlobals name gc_old (change_info, final_matches) =
  if not (VarinfoMap.mem (get_varinfo gc_old) (fst final_matches)) then
    change_info.removed <- gc_old :: change_info.removed;
  (change_info, final_matches)

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

  let findChanges ?(matchVars=true) ?(matchFuns=true) ?(renameDetection=false) oldMap newMap cfgs name gc_new (change_info, final_matches) =
    try
      let gc_old = GlobalMap.find name oldMap in
      eq ~matchVars ~matchFuns ~renameDetection oldMap newMap cfgs gc_old gc_new (change_info, final_matches)
    with Not_found ->
      if not renameDetection then
        change_info.added <- gc_new::change_info.added; (* Global could not be found in old map -> added *)
      change_info, final_matches in

  if GobConfig.get_bool "incremental.detect-renames" then (
    let _ =
      (changes, (VarinfoMap.empty, VarinfoMap.empty)) (* change_info and final_matches (bi-directional) is propagated *)
      |> GlobalMap.fold (findChanges ~matchVars:true ~matchFuns:false ~renameDetection:true oldMap newMap cfgs) newMap
      |> GlobalMap.fold (findChanges ~matchVars:false ~matchFuns:true ~renameDetection:true oldMap newMap cfgs) newMap
      |> GlobalMap.fold addNewGlobals newMap
      |> GlobalMap.fold addOldGlobals oldMap in

    ()
  ) else (
    let _ =
      (changes, (VarinfoMap.empty, VarinfoMap.empty)) (* change_info and final_matches (bi-directional) is propagated *)
      |> GlobalMap.fold (findChanges oldMap newMap cfgs) newMap
      |> GlobalMap.fold addOldGlobals oldMap in
    ()
  );
  changes

(** Given an (optional) equality function between [Cil.global]s, an old and a new [Cil.file], this function computes a [change_info],
    which describes which [global]s are changed, unchanged, removed and added.  *)
let compareCilFiles ?eq (oldAST: file) (newAST: file) =
  Timing.wrap "compareCilFiles" (compareCilFiles ?eq oldAST) newAST
