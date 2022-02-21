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

  (* Set of functions that are to be force-reanalyzed.
     These functions are additionally to be included in the [changed] field *)
  mutable force_reanalyze: VarinfoSet.t;
}

let empty_change_info () : change_info =
  {added = []; removed = []; changed = []; unchanged = []; force_reanalyze = VarinfoSet.empty}

type change_status = Unchanged | Changed | ForceReanalyze of Cil.fundec

(** Give a boolean that indicates whether the code object is identical to the previous version, returns the corresponding [change_status]*)
let unchanged_to_change_status = function
  | true -> Unchanged
  | false -> Changed

(* If some CFGs of the two functions to be compared are provided, a fine-grained CFG comparison is done that also determines which
 * nodes of the function changed. If on the other hand no CFGs are provided, the "old" AST comparison on the CIL.file is
 * used for functions. Then no information is collected regarding which parts/nodes of the function changed. *)
let eqF (old: Cil.fundec) (current: Cil.fundec) (cfgs : (cfg * cfg) option) =
  let unchangedHeader =
    try
      eq_varinfo old.svar current.svar &&
      List.for_all2 eq_varinfo old.sformals current.sformals
    with Invalid_argument _ -> false in
  let change_status, diffOpt =
    if List.mem current.svar.vname (GobConfig.get_string_list "incremental.force-reanalyze.funs") then
      ForceReanalyze current, None
    else
      try
        let sameDef = unchangedHeader && List.for_all2 eq_varinfo old.slocals current.slocals in
        match cfgs with
        | None -> unchanged_to_change_status (sameDef && eq_block (old.sbody, old) (current.sbody, current)), None
        | Some (cfgOld, cfgNew) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgForward = struct let next = cfgNew end in
          let matches, diffNodes1, diffNodes2 = compareFun (module CfgOld) (module CfgNew) old current in
          if not sameDef then (Changed, None)
          else if diffNodes1 = [] && diffNodes2 = [] then (Changed, None)
          else (Changed, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1; primNewNodes = diffNodes2})
      with Invalid_argument _ -> (* The combine failed because the lists have differend length *)
        Changed, None in
  change_status, unchangedHeader, diffOpt

let eq_glob (old: global) (current: global) (cfgs : (cfg * cfg) option) = match old, current with
  | GFun (f,_), GFun (g,_) -> eqF f g cfgs
  | GVar (x, init_x, _), GVar (y, init_y, _) -> unchanged_to_change_status (eq_varinfo x y), false, None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> unchanged_to_change_status (eq_varinfo x y), false, None
  | _ -> ignore @@ Pretty.printf "Not comparable: %a and %a\n" Cil.d_global old Cil.d_global current; Changed, false, None

let compareCilFiles (oldAST: file) (newAST: file) =
  let cfgs = if GobConfig.get_string "incremental.compare" = "cfg"
    then Some (CfgTools.getCFG oldAST |> fst, CfgTools.getCFG newAST |> fst)
    else None in

  let addGlobal map global  =
    try
      GlobalMap.add (identifier_of_global global) global map
    with
      NoGlobalIdentifier _ -> map
  in
  let changes = empty_change_info () in
  global_typ_acc := [];
  let checkUnchanged map global =
    try
      let ident = identifier_of_global global in
      (try
         let old_global = GlobalMap.find ident map in
         (* Do a (recursive) equal comparison ignoring location information *)
         let change_status, unchangedHeader, diff = eq_glob old_global global cfgs in
         let append_to_changed () =
           changes.changed <- {current = global; old = old_global; unchangedHeader; diff} :: changes.changed
         in
         match change_status with
         | Changed -> append_to_changed ()
         | Unchanged -> changes.unchanged <- global :: changes.unchanged
         | ForceReanalyze f ->
           changes.force_reanalyze <- VarinfoSet.add f.svar changes.force_reanalyze;
           append_to_changed ();

       with Not_found -> ())
    with NoGlobalIdentifier _ -> () (* Global was no variable or function, it does not belong into the map *)  in
  let checkExists map global =
    let name = identifier_of_global global in
    GlobalMap.mem name map
  in
  (* Store a map from functionNames in the old file to the function definition*)
  let oldMap = Cil.foldGlobals oldAST addGlobal GlobalMap.empty in
  let newMap = Cil.foldGlobals newAST addGlobal GlobalMap.empty in
  (*  For each function in the new file, check whether a function with the same name
      already existed in the old version, and whether it is the same function. *)
  Cil.iterGlobals newAST
    (fun glob -> checkUnchanged oldMap glob);

  (* We check whether functions have been added or removed *)
  Cil.iterGlobals newAST (fun glob -> try if not (checkExists oldMap glob) then changes.added <- (glob::changes.added) with NoGlobalIdentifier _ -> ());
  Cil.iterGlobals oldAST (fun glob -> try if not (checkExists newMap glob) then changes.removed <- (glob::changes.removed) with NoGlobalIdentifier _ -> ());
  changes
