open MyCFG
open Queue
open Cil
include CompareAST

type nodes_diff = {
  unchangedNodes: (node * node) list;
  primObsoleteNodes: node list;
  primNewNodes: node list
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

let eq_node (x, fun1) (y, fun2) =
  match x,y with
  | Statement s1, Statement s2 -> (try eq_stmt ~cfg_comp:true (s1, fun1) (s2, fun2) with _ -> false)
  | Function f1, Function f2 -> eq_varinfo f1.svar f2.svar
  | FunctionEntry f1, FunctionEntry f2 -> eq_varinfo f1.svar f2.svar
  | _ -> false

let eq_edge x y = match x, y with
  | Assign (lv1, rv1), Assign (lv2, rv2) -> eq_lval lv1 lv2 && eq_exp rv1 rv2
  | Proc (None,f1,ars1), Proc (None,f2,ars2) -> eq_exp f1 f2 && eq_list eq_exp ars1 ars2
  | Proc (Some r1,f1,ars1), Proc (Some r2,f2,ars2) ->
      eq_lval r1 r2 && eq_exp f1 f2 && eq_list eq_exp ars1 ars2
  | Entry f1, Entry f2 -> eq_varinfo f1.svar f2.svar
  | Ret (None,fd1), Ret (None,fd2) -> eq_varinfo fd1.svar fd2.svar
  | Ret (Some r1,fd1), Ret (Some r2,fd2) -> eq_exp r1 r2 && eq_varinfo fd1.svar fd2.svar
  | Test (p1,b1), Test (p2,b2) -> eq_exp p1 p2 && b1 = b2
  | ASM _, ASM _ -> false
  | Skip, Skip -> true
  | VDecl v1, VDecl v2 -> eq_varinfo v1 v2
  | SelfLoop, SelfLoop -> true
  | _ -> false

(* The order of the edges in the list is relevant. Therefore compare
them one to one without sorting first*)
let eq_edge_list xs ys = eq_list eq_edge xs ys

let to_edge_list ls = List.map (fun (loc, edge) -> edge) ls

let compareCfgs (module Cfg1 : CfgForward) (module Cfg2 : CfgForward) fun1 fun2 =
  let diff = Hashtbl.create 113 in
  let same = Hashtbl.create 113 in
  let waitingList : (node * node) t = Queue.create () in

  let rec compareNext () =
    if Queue.is_empty waitingList then ()
    else
      let fromNode1, fromNode2 = Queue.take waitingList in
      let outList1 = Cfg1.next fromNode1 in
      let outList2 = Cfg2.next fromNode2 in

      let findEquiv (edgeList1, toNode1) =
        let rec aux remSuc = match remSuc with
          | [] -> Hashtbl.replace diff toNode1 ()
          | (locEdgeList2, toNode2)::remSuc' ->
              let edgeList2 = to_edge_list locEdgeList2 in
              if eq_node (toNode1, fun1) (toNode2, fun2) && eq_edge_list edgeList1 edgeList2 then
                begin
                  let notInSame, matchedAlready =
                    Hashtbl.fold (fun (toNode1', toNode2') v (ni, ma) ->
                      let n1equal = Node.equal toNode1 toNode1' in
                      let n2equal = Node.equal toNode2 toNode2' in
                      (ni && not (n1equal && n2equal), ma || (n1equal && not n2equal))) same (true, false) in
                  if matchedAlready then Hashtbl.replace diff toNode1 ()
                  else Hashtbl.replace same (toNode1, toNode2) ();
                  if notInSame then Queue.add (toNode1, toNode2) waitingList;
                  Hashtbl.replace same (toNode1, toNode2) ()
                end
              else aux remSuc' in
        aux outList2 in
      let iterOuts (locEdgeList1, toNode1) =
        let edgeList1 = to_edge_list locEdgeList1 in
        (* Differentiate between a possibly duplicate Test(1,false) edge and a single occurence. In the first
        case the edge is directly added to the diff set to avoid undetected ambiguities during the recursive
        call. *)
        let testFalseEdge edge = match edge with
          | Test (p,b) -> p = Cil.one && b = false
          | _ -> false in
        let posAmbigEdge edgeList = let findTestFalseEdge (ll,_) = testFalseEdge (snd (List.hd ll)) in
          let numDuplicates l = List.length (List.find_all findTestFalseEdge l) in
          testFalseEdge (List.hd edgeList) && (numDuplicates outList1 > 1 || numDuplicates outList2 > 1) in
        if posAmbigEdge edgeList1
          then Hashtbl.replace diff toNode1 ()
          else findEquiv (edgeList1, toNode1) in
    List.iter iterOuts outList1; compareNext () in

  let entryNode1, entryNode2 = (FunctionEntry fun1, FunctionEntry fun2) in
  Queue.push (entryNode1,entryNode2) waitingList; compareNext (); (same, diff)

let reexamine f1 f2 (same : ((node * node), unit) Hashtbl.t) (diffNodes1 : (node,unit) Hashtbl.t) (module Cfg1 : CfgForward) (module Cfg2 : CfgForward) =
  Hashtbl.filter_map_inplace (fun (n1,n2) _ -> if Hashtbl.mem diffNodes1 n1 then None else Some ()) same;
  Hashtbl.add same (FunctionEntry f1, FunctionEntry f2) ();
  let vis = Hashtbl.create 103 in
  let diffNodes2 = Hashtbl.create 103 in

  let asSndInSame k = Hashtbl.fold (fun (n1,n2) _ acc -> acc || Node.equal n2 k) same false in
  let rec refine_same k =
    if Hashtbl.mem vis k then ()
    else begin
      Hashtbl.add vis k ();
      if asSndInSame k then
        Hashtbl.filter_map_inplace (fun (n1,n2) _ -> if Node.equal n2 k then (Hashtbl.replace diffNodes1 n1 (); Hashtbl.replace diffNodes2 n2 (); None) else Some ()) same
      else dfs2 k refine_same end
  and classify_prim_new k =
    if Hashtbl.mem vis k then ()
    else begin
      Hashtbl.replace vis k ();
      if asSndInSame k then dfs2 k classify_prim_new
      else (Hashtbl.add diffNodes2 k (); Hashtbl.clear vis; dfs2 k refine_same) end
  and dfs2 node f =
    let succ = List.map snd (Cfg2.next node) in
    List.iter f succ in
  dfs2 (FunctionEntry f2) classify_prim_new;
  (Hashtbl.to_seq_keys same, Hashtbl.to_seq_keys diffNodes1, Hashtbl.to_seq_keys diffNodes2)

let compareFun (module Cfg1 : CfgForward) (module Cfg2 : CfgForward) fun1 fun2 =
  let same, diff = compareCfgs (module Cfg1) (module Cfg2) fun1 fun2 in
  let unchanged, diffNodes1, diffNodes2 = reexamine fun1 fun2 same diff (module Cfg1) (module Cfg2) in
  List.of_seq unchanged, List.of_seq diffNodes1, List.of_seq diffNodes2

let eqF (a: Cil.fundec) (b: Cil.fundec) (cfgs : (cfg * cfg) option) =
  let unchangedHeader =
    try
      eq_varinfo a.svar b.svar &&
      List.for_all (fun (x, y) -> eq_varinfo x y) (List.combine a.sformals b.sformals)
    with Invalid_argument _ -> false in
  let identical, diffOpt =
    try
      let sameDef = unchangedHeader && List.for_all (fun (x, y) -> eq_varinfo x y) (List.combine a.slocals b.slocals) in
      match cfgs with
      | None -> sameDef && eq_block (a.sbody, a) (b.sbody, b), None
      | Some (cfgOld, cfgNew) ->
        let module CfgOld : MyCFG.CfgForward = struct let next = cfgOld end in
        let module CfgNew : MyCFG.CfgForward = struct let next = cfgNew end in
        let matches, diffNodes1, diffNodes2 = compareFun (module CfgOld) (module CfgNew) a b in
        if not sameDef then (false, None)
        else if List.length diffNodes1 = 0 && List.length diffNodes2 = 0 then (true, None)
        else (false, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1; primNewNodes = diffNodes2})
    with Invalid_argument _ -> (* The combine failed because the lists have differend length *)
      false, None in
  identical, unchangedHeader, diffOpt

let eq_glob (a: global) (b: global) (cfgs : (cfg * cfg) option) = match a, b with
  | GFun (f,_), GFun (g,_) -> eqF f g cfgs
  | GVar (x, init_x, _), GVar (y, init_y, _) -> eq_varinfo x y, false, None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> eq_varinfo x y, false, None
  | _ -> print_endline @@ "Not comparable: " ^ (Pretty.sprint ~width:100 (Cil.d_global () a)) ^ " and " ^ (Pretty.sprint ~width:100 (Cil.d_global () a)); false, false, None

let compareCilFiles (oldAST: file) (newAST: file) =
  let cfgs = if GobConfig.get_bool "incremental.within_functions"
    then Some (CfgTools.getCFG oldAST |> fst, CfgTools.getCFG newAST |> fst)
    else None in

  let addGlobal map global  =
    try
      GlobalMap.add (identifier_of_global global) global map
    with
      e -> map
  in
  let changes = empty_change_info () in
  global_typ_acc := [];
  let checkUnchanged map global =
    try
      let ident = identifier_of_global global in
      (try
         let old_global = GlobalMap.find ident map in
         (* Do a (recursive) equal comparision ignoring location information *)
         let identical, unchangedHeader, diff = eq_glob old_global global cfgs in
         if identical
         then changes.unchanged <- global :: changes.unchanged
         else changes.changed <- {current = global; old = old_global; unchangedHeader = unchangedHeader; diff = diff} :: changes.changed
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
  Cil.iterGlobals newAST (fun glob -> try if not (checkExists oldMap glob) then changes.added <- (glob::changes.added) with e -> ());
  Cil.iterGlobals oldAST (fun glob -> try if not (checkExists newMap glob) then changes.removed <- (glob::changes.removed) with e -> ());
  changes
