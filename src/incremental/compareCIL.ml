open GoblintCil
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

module VarinfoSet = Set.Make(CilType.Varinfo)

type change_info = {
  mutable changed: changed_global list;
  mutable unchanged: unchanged_global list;
  mutable removed: global list;
  mutable added: global list;
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
          eq_varinfo old.svar current.svar actHeaderRenameMapping && GobList.equal (eq_varinfo2 actHeaderRenameMapping) old.sformals current.sformals, headerRenameMapping
        )
      | Some _ -> (
          eq_varinfo old.svar current.svar emptyRenameMapping && GobList.equal (eq_varinfo2 emptyRenameMapping) old.sformals current.sformals, StringMap.empty
        )
    in
    if not unchangedHeader then ChangedFunHeader current, None
    else
      (* Here the local variables are checked to be equal *)
      (*flag: when running on cfg, true iff the locals are identical; on ast: if the size of the locals stayed the same*)
      let sameLocals, local_rename =
        match cfgs with
        | None -> rename_mapping_aware_compare old.slocals current.slocals headerRenameMapping
        | Some _ -> GobList.equal (eq_varinfo2 emptyRenameMapping) old.slocals current.slocals, StringMap.empty
      in
      let rename_mapping: rename_mapping = (local_rename, global_rename_mapping) in

      if not sameLocals then
        (Changed, None)
      else
        match cfgs with
        | None -> unchanged_to_change_status (eq_block (old.sbody, old) (current.sbody, current) rename_mapping), None
        | Some (cfgOld, (cfgNew, cfgNewBack)) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgBidir = struct let prev = cfgNewBack let next = cfgNew end in
          let matches, diffNodes1 = compareFun (module CfgOld) (module CfgNew) old current in
          if diffNodes1 = [] then (Unchanged, None)
          else (Changed, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1})

let eq_glob (old: global) (current: global) (cfgs : (cfg * (cfg * cfg)) option) (global_rename_mapping: method_rename_assumptions) = match old, current with
  | GFun (f,_), GFun (g,_) -> eqF f g cfgs global_rename_mapping
  | GVar (x, init_x, _), GVar (y, init_y, _) -> unchanged_to_change_status (eq_varinfo x y (StringMap.empty, VarinfoMap.empty)), None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> unchanged_to_change_status (eq_varinfo x y (StringMap.empty, VarinfoMap.empty)), None
  | _ -> ignore @@ Pretty.printf "Not comparable: %a and %a\n" Cil.d_global old Cil.d_global current; Changed, None

let compareCilFiles ?(eq=eq_glob) (oldAST: file) (newAST: file) =
  let cfgs = if GobConfig.get_string "incremental.compare" = "cfg"
    then Some (CfgTools.getCFG oldAST |> fst, CfgTools.getCFG newAST)
    else None in

  let generate_global_rename_mapping map global =
    try
      let ident = identifier_of_global global in
      let old_global = GlobalMap.find ident map in

      match old_global, global with
      | GFun(f, _), GFun (g, _) ->
        let renamed_params: string StringMap.t = if (List.length f.sformals) = (List.length g.sformals) then
            let mappings = List.combine f.sformals g.sformals |>
                           List.filter (fun (original, now) -> not (original.vname = now.vname)) |>
                           List.map (fun (original, now) -> (original.vname, now.vname)) |>
                           List.to_seq
            in

            StringMap.add_seq mappings StringMap.empty
          else StringMap.empty in

        if not (f.svar.vname = g.svar.vname) || (StringMap.cardinal renamed_params) > 0 then
          Some (f.svar, {original_method_name=f.svar.vname; new_method_name=g.svar.vname; parameter_renames=renamed_params})
        else None
      | _, _ -> None
    with Not_found -> None
  in

  let addGlobal map global  =
    try
      let gid = identifier_of_global global in
      let gid_to_string gid = match gid.global_t with
        | Var -> "Var " ^ gid.name
        | Decl -> "Decl " ^ gid.name
        | Fun -> "Fun " ^ gid.name in
      if GlobalMap.mem gid map then failwith ("Duplicate global identifier: " ^ gid_to_string gid) else GlobalMap.add gid global map
    with
      Not_found -> map
  in

  let changes = empty_change_info () in
  global_typ_acc := [];
  let findChanges map global global_rename_mapping =
    try
      let ident = identifier_of_global global in
      let old_global = GlobalMap.find ident map in
      (* Do a (recursive) equal comparison ignoring location information *)
      let change_status, diff = eq old_global global cfgs global_rename_mapping in
      let append_to_changed ~unchangedHeader =
        changes.changed <- {current = global; old = old_global; unchangedHeader; diff} :: changes.changed
      in
      match change_status with
      | Changed ->
        append_to_changed ~unchangedHeader:true
      | Unchanged -> changes.unchanged <- {current = global; old = old_global} :: changes.unchanged
      | ChangedFunHeader f
      | ForceReanalyze f ->
        changes.exclude_from_rel_destab <- VarinfoSet.add f.svar changes.exclude_from_rel_destab;
        append_to_changed ~unchangedHeader:false;
    with Not_found -> () (* Global was no variable or function, it does not belong into the map *)
  in
  let checkExists map global =
    match identifier_of_global global with
    | name -> GlobalMap.mem name map
    | exception Not_found -> true (* return true, so isn't considered a change *)
  in
  (* Store a map from functionNames in the old file to the function definition*)
  let oldMap = Cil.foldGlobals oldAST addGlobal GlobalMap.empty in
  let newMap = Cil.foldGlobals newAST addGlobal GlobalMap.empty in

  let global_rename_mapping: method_rename_assumptions = Cil.foldGlobals newAST (fun (current_global_rename_mapping: method_rename_assumption VarinfoMap.t) global ->
      match generate_global_rename_mapping oldMap global with
      | Some (funVar, rename_mapping) -> VarinfoMap.add funVar rename_mapping current_global_rename_mapping
      | None -> current_global_rename_mapping
    ) VarinfoMap.empty
  in

  (*  For each function in the new file, check whether a function with the same name
      already existed in the old version, and whether it is the same function. *)
  Cil.iterGlobals newAST
    (fun glob -> findChanges oldMap glob global_rename_mapping);

  (* We check whether functions have been added or removed *)
  Cil.iterGlobals newAST (fun glob -> if not (checkExists oldMap glob) then changes.added <- (glob::changes.added));
  Cil.iterGlobals oldAST (fun glob -> if not (checkExists newMap glob) then changes.removed <- (glob::changes.removed));
  changes

(** Given an (optional) equality function between [Cil.global]s, an old and a new [Cil.file], this function computes a [change_info],
    which describes which [global]s are changed, unchanged, removed and added.  *)
let compareCilFiles ?eq (oldAST: file) (newAST: file) =
  Timing.wrap "compareCilFiles" (compareCilFiles ?eq oldAST) newAST
