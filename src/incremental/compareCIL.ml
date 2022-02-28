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

type change_info = {
  mutable changed: changed_global list;
  mutable unchanged: global list;
  mutable removed: global list;
  mutable added: global list
}

let empty_change_info () : change_info = {added = []; removed = []; changed = []; unchanged = []}

let should_reanalyze (fdec: Cil.fundec) =
  List.mem fdec.svar.vname (GobConfig.get_string_list "incremental.force-reanalyze.funs")

(* If some CFGs of the two functions to be compared are provided, a fine-grained CFG comparison is done that also determines which
 * nodes of the function changed. If on the other hand no CFGs are provided, the "old" AST comparison on the CIL.file is
 * used for functions. Then no information is collected regarding which parts/nodes of the function changed. *)
let eqF (a: Cil.fundec) (b: Cil.fundec) (cfgs : (cfg * cfg) option) =
  let unchangedHeader =
    try
      eq_varinfo a.svar b.svar &&
      List.for_all2 eq_varinfo a.sformals b.sformals
    with Invalid_argument _ -> false in
  let identical, diffOpt =
    if should_reanalyze a then
      false, None
    else
      try
        let sameDef = unchangedHeader && List.for_all2 eq_varinfo a.slocals b.slocals in
        match cfgs with
        | None -> sameDef && eq_block (a.sbody, a) (b.sbody, b), None
        | Some (cfgOld, cfgNew) ->
          let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
          let module CfgNew : MyCFG.CfgForward = struct let next = cfgNew end in
          let matches, diffNodes1, diffNodes2 = compareFun (module CfgOld) (module CfgNew) a b in
          if not sameDef then (false, None)
          else if diffNodes1 = [] && diffNodes2 = [] then (true, None)
          else (false, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1; primNewNodes = diffNodes2})
      with Invalid_argument _ -> (* The combine failed because the lists have differend length *)
        false, None in
  identical, unchangedHeader, diffOpt

let eq_glob (a: global) (b: global) (cfgs : (cfg * cfg) option) = match a, b with
  | GFun (f,_), GFun (g,_) -> eqF f g cfgs
  | GVar (x, init_x, _), GVar (y, init_y, _) -> eq_varinfo x y, false, None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> eq_varinfo x y, false, None
  | _ -> ignore @@ Pretty.printf "Not comparable: %a and %a\n" Cil.d_global a Cil.d_global b; false, false, None

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
        | Fun -> "Fun " ^ gid.name
        | _ -> raise (NoGlobalIdentifier global) in
      if GlobalMap.mem gid map then failwith ("Duplicate global identifier: " ^ gid_to_string gid) else GlobalMap.add gid global map
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
         let identical, unchangedHeader, diff = eq old_global global cfgs in
         if identical
         then changes.unchanged <- global :: changes.unchanged
         else changes.changed <- {current = global; old = old_global; unchangedHeader; diff} :: changes.changed
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
