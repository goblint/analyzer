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

let eq_varinfo' (a: varinfo) (b: varinfo) = a.vname = b.vname && eq_typ a.vtype b.vtype && eq_list eq_attribute a.vattr b.vattr &&
                                           a.vstorage = b.vstorage && a.vglob = b.vglob

let eq_instr' (a: instr) (b: instr) = match a, b with
  | Set (lv1, exp1, _l1), Set (lv2, exp2, _l2) -> eq_lval lv1 lv2 && eq_exp exp1 exp2
  | Call (Some lv1, f1, args1, _l1), Call (Some lv2, f2, args2, _l2) -> eq_lval lv1 lv2 && eq_exp f1 f2 && eq_list eq_exp args1 args2
  | Call (None, f1, args1, _l1), Call (None, f2, args2, _l2) -> eq_exp f1 f2 && eq_list eq_exp args1 args2
  | Asm (attr1, tmp1, ci1, dj1, rk1, l1), Asm (attr2, tmp2, ci2, dj2, rk2, l2) ->
      eq_list String.equal tmp1 tmp2 && eq_list(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2
      && eq_lval z1 z2) ci1 ci2 && eq_list(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2
      && eq_exp z1 z2) dj1 dj2 && eq_list String.equal rk1 rk2(* ignore attributes and locations *)
  | VarDecl (v1, _l1), VarDecl (v2, _l2) -> eq_varinfo' v1 v2
  | _, _ -> false

(* in contrast to the similar method eq_stmtkind in CompareAST, this method does not compare the inner body,
   that is sub blocks, of if and switch statements *)
let eq_stmtkind' ((a, af): stmtkind * fundec) ((b, bf): stmtkind * fundec) =
  let eq_block' = fun x y -> eq_block (x, af) (y, bf) in
  match a, b with
  | Instr is1, Instr is2 -> eq_list eq_instr' is1 is2
  | Return (Some exp1, _l1), Return (Some exp2, _l2) -> eq_exp exp1 exp2
  | Return (None, _l1), Return (None, _l2) -> true
  | Return _, Return _ -> false
  | Goto (st1, _l1), Goto (st2, _l2) -> eq_stmt_with_location (!st1, af) (!st2, bf)
  | Break _, Break _ -> true
  | Continue _, Continue _ -> true
  | If (exp1, then1, else1, _l1), If (exp2, then2, else2, _l2) ->
      eq_exp exp1 exp2 (* && eq_block' then1 then2 && eq_block' else1 else2 *)
  | Switch (exp1, block1, stmts1, _l1), Switch (exp2, block2, stmts2, _l2) ->
      eq_exp exp1 exp2 (* && eq_block' block1 block2 && eq_list (fun a b -> eq_stmt (a,af) (b,bf)) stmts1 stmts2 *)
  | Loop (block1, _l1, _con1, _br1), Loop (block2, _l2, _con2, _br2) -> true (* eq_block' block1 block2 *)
  | Block block1, Block block2 -> eq_block' block1 block2
  | TryFinally (tryBlock1, finallyBlock1, _l1), TryFinally (tryBlock2, finallyBlock2, _l2) -> assert false
      (* eq_block' tryBlock1 tryBlock2 && eq_block' finallyBlock1 finallyBlock2 *)
  | TryExcept (tryBlock1, exn1, exceptBlock1, _l1), TryExcept (tryBlock2, exn2, exceptBlock2, _l2) -> assert false
      (* eq_block' tryBlock1 tryBlock2 && eq_block' exceptBlock1 exceptBlock2 *)
  | _, _ -> false

let eq_stmt' ((a, af): stmt * fundec) ((b, bf): stmt * fundec) =
  (* catch Invalid Argument exception which is thrown by List.combine if the label lists are of different length *)
  try List.for_all (fun (x,y) -> eq_label x y) (List.combine a.labels b.labels)
    && eq_stmtkind' (a.skind, af) (b.skind, bf)
  with Invalid_argument _ -> false

let eq_node (x, fun1) (y, fun2) =
  match x,y with
  | Statement s1, Statement s2 -> eq_stmt' (s1, fun1) (s2, fun2)
  | Function f1, Function f2 -> eq_varinfo' f1.svar f2.svar
  | FunctionEntry f1, FunctionEntry f2 -> eq_varinfo' f1.svar f2.svar
  | _ -> false

let eq_edge x y = match x, y with
  | Assign (lv1, rv1), Assign (lv2, rv2) -> eq_lval lv1 lv2 && eq_exp rv1 rv2
  | Proc (None,f1,ars1), Proc (None,f2,ars2) -> eq_exp f1 f2 && eq_list eq_exp ars1 ars2
  | Proc (Some r1,f1,ars1), Proc (Some r2,f2,ars2) ->
      eq_lval r1 r2 && eq_exp f1 f2 && eq_list eq_exp ars1 ars2
  | Entry f1, Entry f2 -> eq_varinfo' f1.svar f2.svar
  | Ret (None,fd1), Ret (None,fd2) -> eq_varinfo' fd1.svar fd2.svar
  | Ret (Some r1,fd1), Ret (Some r2,fd2) -> eq_exp r1 r2 && eq_varinfo' fd1.svar fd2.svar
  | Test (p1,b1), Test (p2,b2) -> eq_exp p1 p2 && b1 = b2
  | ASM _, ASM _ -> false
  | Skip, Skip -> true
  | VDecl v1, VDecl v2 -> eq_varinfo' v1 v2
  | SelfLoop, SelfLoop -> true
  | _ -> false

(* The order of the edges in the list is relevant. Therefor compare
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

let reexamine f1 f2 same diff (module Cfg1 : CfgForward) (module Cfg2 : CfgForward) =
  let module NodeSet = Set.Make(Node) in
  let module NodeNodeSet = Set.Make (struct type t = node * node let compare = compare end) in
  let diffNodes1 = Hashtbl.fold (fun n _ acc -> NodeSet.add n acc) diff NodeSet.empty in
  let sameNodes =
    let notInDiff = Hashtbl.fold (fun (n1,n2) _ acc -> if NodeSet.mem n1 diffNodes1 then acc else NodeNodeSet.add (n1,n2) acc) same NodeNodeSet.empty in
    NodeNodeSet.add (FunctionEntry f1, FunctionEntry f2) notInDiff in

  let diffNodes2, _ =
    let rec dfs2 node (diffNodes2, vis) =
      let classify k (diffNodes2, vis) =
        if NodeSet.mem k vis then (diffNodes2, vis)
        else let vis' = NodeSet.add k vis in
        if NodeNodeSet.exists (fun (_,n2) -> Node.equal n2 k) sameNodes then dfs2 k (diffNodes2, vis')
        else (NodeSet.add k diffNodes2, vis') in
      let succ = List.map snd (Cfg2.next node) in
      List.fold_right classify succ (diffNodes2, vis) in
    dfs2 (FunctionEntry f2) (NodeSet.empty, NodeSet.empty) in

  let sameNodes', diffNodes1', diffNodes2', _ =
    let rec dfs2 node (sameNodes', diffNodes1', diffNodes2', vis) =
      let classify k (sameNodes', diffNodes1', diffNodes2', vis) =
        if NodeSet.mem k vis then (sameNodes', diffNodes1', diffNodes2', vis)
        else let vis' = NodeSet.add k vis in
        if NodeNodeSet.exists (fun (n1,n2) -> Node.equal n2 k) sameNodes
        then let rsn, rd1, rd2 = NodeNodeSet.fold (fun (n1,n2) (rsn, rd1, rd2) -> if Node.equal n2 k then (NodeNodeSet.remove (n1,n2) rsn, NodeSet.add n1 rd1, NodeSet.add k rd2) else (rsn, rd1,rd2)) sameNodes (sameNodes', diffNodes1', diffNodes2') in
         (rsn, rd1, rd2, vis')
        else dfs2 k (sameNodes', diffNodes1', diffNodes2', vis') in
      let succ = List.map snd (Cfg2.next node) in
      List.fold_right classify succ (sameNodes', diffNodes1', diffNodes2', vis) in
    NodeSet.fold dfs2 diffNodes2 (sameNodes, diffNodes1, diffNodes2, NodeSet.empty) in

  (NodeNodeSet.elements sameNodes', NodeSet.elements diffNodes1', NodeSet.elements diffNodes2')

let compareFun (module Cfg1 : CfgForward) (module Cfg2 : CfgForward) fun1 fun2 =
  let same, diff = compareCfgs (module Cfg1) (module Cfg2) fun1 fun2 in
  let unchanged, diffNodes1, diffNodes2 = reexamine fun1 fun2 same diff (module Cfg1) (module Cfg2) in
  unchanged, diffNodes1, diffNodes2

let eqF' (a: Cil.fundec) (module Cfg1 : MyCFG.CfgForward) (b: Cil.fundec) (module Cfg2 : MyCFG.CfgForward) =
  let unchangedHeader =
    try
      eq_varinfo a.svar b.svar &&
      List.for_all (fun (x, y) -> eq_varinfo x y) (List.combine a.sformals b.sformals)
    with Invalid_argument _ -> false in
  let identical, diffOpt =
    try
      let sameDef = unchangedHeader && List.for_all (fun (x, y) -> eq_varinfo x y) (List.combine a.slocals b.slocals) in
      let matches, diffNodes1, diffNodes2 = compareFun (module Cfg1) (module Cfg2) a b in
      if not sameDef then (false, None)
      else if List.length diffNodes1 = 0 && List.length diffNodes2 = 0 then (true, None)
      else (false, Some {unchangedNodes = matches; primObsoleteNodes = diffNodes1; primNewNodes = diffNodes2})
    with Invalid_argument _ -> (* The combine failed because the lists have differend length *)
      false, None in
  identical, unchangedHeader, diffOpt

let eq_glob' (a: global) (module Cfg1 : MyCFG.CfgForward) (b: global) (module Cfg2 : MyCFG.CfgForward) = match a, b with
  | GFun (f,_), GFun (g,_) -> if GobConfig.get_bool "incremental.within_functions" then eqF' f (module Cfg1) g (module Cfg2) else eqF f g
  | GVar (x, init_x, _), GVar (y, init_y, _) -> eq_varinfo x y, false, None (* ignore the init_info - a changed init of a global will lead to a different start state *)
  | GVarDecl (x, _), GVarDecl (y, _) -> eq_varinfo x y, false, None
  | _ -> print_endline @@ "Not comparable: " ^ (Pretty.sprint ~width:100 (Cil.d_global () a)) ^ " and " ^ (Pretty.sprint ~width:100 (Cil.d_global () a)); false, false, None

let compareCilFiles (oldAST: file) (newAST: file) =
  let oldCfg, _ = CfgTools.getCFG oldAST in
  let newCfg, _ = CfgTools.getCFG newAST in

  let module OldCfg: MyCFG.CfgForward = struct let next = oldCfg end in
  let module NewCfg: MyCFG.CfgForward = struct let next = newCfg end in

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
         let identical, unchangedHeader, diff = eq_glob' old_global (module OldCfg) global (module NewCfg) in
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
