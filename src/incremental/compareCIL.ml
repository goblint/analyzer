open Cil
open MyCFG
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

let empty_change_info () : change_info = {added = []; removed = []; changed = []; unchanged = []}

let should_reanalyze (fdec: Cil.fundec) =
  List.mem fdec.svar.vname (GobConfig.get_string_list "incremental.force-reanalyze.funs")

(* If some CFGs of the two functions to be compared are provided, a fine-grained CFG comparison is done that also determines which
 * nodes of the function changed. If on the other hand no CFGs are provided, the "old" AST comparison on the CIL.file is
 * used for functions. Then no information is collected regarding which parts/nodes of the function changed. *)
let eqF (a: Cil.fundec) (b: Cil.fundec) (cfgs : (cfg * (cfg * cfg)) option) (global_rename_mapping: method_rename_assumptions) =
  let local_rename_map: (string, string) Hashtbl.t = Hashtbl.create (List.length a.slocals) in

  if (List.length a.slocals) = (List.length b.slocals) then
    List.combine a.slocals b.slocals |>
      List.map (fun x -> match x with (a, b) -> (a.vname, b.vname)) |>
      List.iter (fun pair -> match pair with (a, b) -> Hashtbl.add local_rename_map a b);


  (* Compares the two varinfo lists, returning as a first element, if the size of the two lists are equal,
   * and as a second a rename_mapping, holding the rename assumptions *)
  let rec rename_mapping_aware_compare (alocals: varinfo list) (blocals: varinfo list) (rename_mapping: string StringMap.t) = match alocals, blocals with
        | [], [] -> true, rename_mapping
        | origLocal :: als, nowLocal :: bls ->
          let new_mapping = if origLocal.vname <> nowLocal.vname then StringMap.add origLocal.vname nowLocal.vname rename_mapping else rename_mapping in

          (*TODO: maybe optimize this with eq_varinfo*)
          rename_mapping_aware_compare als bls new_mapping
        | _, _ -> false, rename_mapping
        in

  let headerSizeEqual, headerRenameMapping = rename_mapping_aware_compare a.sformals b.sformals (StringMap.empty) in
  let actHeaderRenameMapping = (headerRenameMapping, global_rename_mapping) in

  let unchangedHeader = eq_varinfo a.svar b.svar actHeaderRenameMapping &&>> forward_list_equal eq_varinfo a.sformals b.sformals in
  let identical, diffOpt =
    if should_reanalyze a then
      false, None
    else
      (* Here the local variables are checked to be equal *)
      let sizeEqual, local_rename = rename_mapping_aware_compare a.slocals b.slocals headerRenameMapping in
      let rename_mapping: rename_mapping = (local_rename, global_rename_mapping) in

      let sameDef = unchangedHeader &&> sizeEqual |> fst in
      if not sameDef then
        (false, None)
      else
        match cfgs with
        | None -> eq_block (a.sbody, a) (b.sbody, b) rename_mapping |> fst, None
        | Some (cfgOld, (cfgNew, cfgNewBack)) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgBidir = struct let prev = cfgNewBack let next = cfgNew end in
          let matches, diffNodes1 = compareFun (module CfgOld) (module CfgNew) a b in
          if diffNodes1 = [] then (true, None)
          else (false, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1})
  in
  identical, unchangedHeader, diffOpt

let eq_glob (a: global) (b: global) (cfgs : (cfg * (cfg * cfg)) option) (global_rename_mapping: method_rename_assumptions) = match a, b with
  | GFun (f,_), GFun (g,_) ->
    let identical, unchangedHeader, diffOpt = eqF f g cfgs global_rename_mapping in

    identical, unchangedHeader |> fst, diffOpt
  | GVar (x, init_x, _), GVar (y, init_y, _) -> eq_varinfo x y (StringMap.empty, StringMap.empty) |> fst, false, None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> eq_varinfo x y (StringMap.empty, StringMap.empty) |> fst, false, None
  | _ -> ignore @@ Pretty.printf "Not comparable: %a and %a\n" Cil.d_global a Cil.d_global b; false, false, None

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
            List.combine f.sformals g.sformals |>
            List.filter (fun (original, now) -> not (original.vname = now.vname)) |>
            List.map (fun (original, now) -> (original.vname, now.vname)) |>
            (fun list ->
              List.fold_left (fun map mapping -> StringMap.add (fst mapping) (snd mapping) map) StringMap.empty list
            )
          else StringMap.empty in

          if not (f.svar.vname = g.svar.vname) || not (StringMap.is_empty renamed_params) then
            Some {original_method_name=f.svar.vname; new_method_name=g.svar.vname; parameter_renames=renamed_params}
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
      let identical, unchangedHeader, diff = eq old_global global cfgs global_rename_mapping in
      if identical
      then changes.unchanged <- {current = global; old = old_global} :: changes.unchanged
      else changes.changed <- {current = global; old = old_global; unchangedHeader; diff} :: changes.changed
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

  let global_rename_mapping: method_rename_assumptions = Cil.foldGlobals newAST (fun (current_global_rename_mapping: method_rename_assumption list) global ->
    match generate_global_rename_mapping oldMap global with
      | Some rename_mapping -> current_global_rename_mapping @ [rename_mapping]
      | None -> current_global_rename_mapping
  ) [] |>
    (fun mappings ->
      List.fold_left (fun map mapping -> StringMap.add mapping.original_method_name mapping map) StringMap.empty mappings
    ) in

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
let compareCilFiles ?(eq=eq_glob) (oldAST: file) (newAST: file) =
  Stats.time "compareCilFiles" (compareCilFiles ~eq oldAST) newAST
