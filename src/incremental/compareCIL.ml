open Cil
open MyCFG
include CompareAST
include CompareCFG

type nodes_diff = {
  unchangedNodes: (node * node) list;
  primObsoleteNodes: node list; (** primary obsolete nodes -> all obsolete nodes are reachable from these *)
  primNewNodes: node list (** primary new nodes -> all differing nodes in the new CFG are reachable from these *)
}

type changed_global = {
  old: global;
  current: global;
  unchangedHeader: bool;
  diff: nodes_diff option
}

module VarinfoSet = Set.Make(CilType.Varinfo)

type change_info = {
  mutable changed: changed_global list;
  mutable unchanged: global list;
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
let eqF (old: Cil.fundec) (current: Cil.fundec) (cfgs : (cfg * cfg) option) =
  if should_reanalyze current then
    ForceReanalyze current, None
  else
    let unchangedHeader = eq_varinfo old.svar current.svar && GobList.equal eq_varinfo old.sformals current.sformals in
    if not unchangedHeader then ChangedFunHeader current, None
    else
      let sameLocals = GobList.equal eq_varinfo old.slocals current.slocals in
      if not sameLocals then
        (Changed, None)
      else
        match cfgs with
        | None -> unchanged_to_change_status (eq_block (old.sbody, old) (current.sbody, current)), None
        | Some (cfgOld, cfgNew) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgForward = struct let next = cfgNew end in
          let matches, diffNodes1, diffNodes2 = compareFun (module CfgOld) (module CfgNew) old current in
          if diffNodes1 = [] && diffNodes2 = [] then (Changed, None)
          else (Changed, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1; primNewNodes = diffNodes2})

let eq_glob (old: global) (current: global) (cfgs : (cfg * cfg) option) = match old, current with
  | GFun (f,_), GFun (g,_) -> eqF f g cfgs
  | GVar (x, init_x, _), GVar (y, init_y, _) -> unchanged_to_change_status (eq_varinfo x y), None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> unchanged_to_change_status (eq_varinfo x y), None
  | _ -> ignore @@ Pretty.printf "Not comparable: %a and %a\n" Cil.d_global old Cil.d_global current; Changed, None

let compareCilFiles ?(eq=eq_glob) (oldAST: file) (newAST: file) =
  let cfgs = if GobConfig.get_string "incremental.compare" = "cfg"
    then Some (CfgTools.getCFG oldAST |> fst, CfgTools.getCFG newAST |> fst)
    else None in

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
  let checkUnchanged map global =
    try
      let ident = identifier_of_global global in
      let old_global = GlobalMap.find ident map in
      (* Do a (recursive) equal comparison ignoring location information *)
      let change_status, diff = eq old_global global cfgs in
      let append_to_changed (unchangedHeader) =
        changes.changed <- {current = global; old = old_global; unchangedHeader; diff} :: changes.changed
      in
      match change_status with
      | ChangedFunHeader f ->
        changes.exclude_from_rel_destab <- VarinfoSet.add f.svar changes.exclude_from_rel_destab;
        append_to_changed false
      | Changed ->
        append_to_changed true
      | Unchanged -> changes.unchanged <- global :: changes.unchanged
      | ForceReanalyze f ->
        changes.exclude_from_rel_destab <- VarinfoSet.add f.svar changes.exclude_from_rel_destab;
        append_to_changed false;
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
  (*  For each function in the new file, check whether a function with the same name
      already existed in the old version, and whether it is the same function. *)
  Cil.iterGlobals newAST
    (fun glob -> checkUnchanged oldMap glob);

  (* We check whether functions have been added or removed *)
  Cil.iterGlobals newAST (fun glob -> if not (checkExists oldMap glob) then changes.added <- (glob::changes.added));
  Cil.iterGlobals oldAST (fun glob -> if not (checkExists newMap glob) then changes.removed <- (glob::changes.removed));
  changes
