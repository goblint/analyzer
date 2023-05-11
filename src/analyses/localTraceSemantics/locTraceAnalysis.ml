open Prelude.Ana
open Analyses
open LocalTraces
open AuxiliaryClasses
open PriorityCalc
open PostSolvingFlag
open HelperFunctions
(* Analysis framework for local traces *)
module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec
  module D = GraphSet

  module C = Lattice.Unit 

  (* side effect components *)
  module V = SideEffectDomain

  module G = GraphSet


  (* Creates a sequence of assignments where custom local variables are generated.
     This helper-function utilizes modules D and V, therefore it is not moved into 'locTraceHelper.ml' *)
  let rec create_local_assignments graphList globals ctx =
    let rec loop global graphList acc =
      match graphList with 
      |  graph::gs ->
        let lastNode = LocalTrace.get_last_node graph
        in
        let localGlobalVar = customVinfoStore#getLocalVarinfo (make_local_global_varinfo global) global.vtype

        in 
        let lockVarinfo = customVinfoStore#getGlobalVarinfo "pthread_mutex_lock" 
        in
        let unlockVarinfo = customVinfoStore#getGlobalVarinfo "pthread_mutex_unlock"
        in
        let customMutex = customVinfoStore#getGlobalVarinfo (make_mutex_varinfo global)
        in
        let lockingLabel:Edge.t = Proc(None, Lval(Var(lockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
        in
        let lvalLockedLs = VarinfoSet.add customMutex lastNode.lockSet
        in
        let lockedNode = {programPoint=lastNode.programPoint;sigma=lastNode.sigma; id=(idGenerator#getID lastNode (EdgeImpl.convert_edge lockingLabel) lastNode.programPoint lastNode.sigma lastNode.tid lvalLockedLs);tid=lastNode.tid;lockSet=lvalLockedLs}
        in
        let lockingEdge = (lastNode,EdgeImpl.convert_edge lockingLabel,lockedNode)
        in
        let myTmp:V.t = Mutex(customMutex)
        in
        let allUnlockingTraces = ctx.global myTmp
        in 
        let firstNode = LocalTrace.get_first_node graph
        in
        let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(customMutex),lockedNode)
        in
        let lockedGraph = LocalTrace.extend_by_gEdge graph lockingEdge
        in 
        let firstLockedGraph = if (NodeImpl.equal firstNode lastNode) then lockedGraph
          else if (LocalTrace.exists_unlock_mutex graph customMutex)
          then add_dependency_from_last_unlock lockedGraph customMutex 
          else if not (LocalTrace.exists_lock_mutex graph customMutex) then  LocalTrace.extend_by_gEdge lockedGraph firstLockEdge
          else lockedGraph (* This case happens if lock(m) is executed consecutively. Should I support that? *)
        in
        print_string ("firstLockedGraph="^(LocalTrace.show firstLockedGraph)^"\n");
        let lockedGraphList = 
          firstLockedGraph::(mutexLock_join allUnlockingTraces lockedGraph lastNode lockingLabel lockedNode customMutex)
        in
        let graphList = List.fold (fun resultGraphList lockedGraph -> 
            let sigmaList,success_inner, _ = eval_wrapper lockedNode.sigma localGlobalVar (Lval(Var(global),NoOffset)) lockedGraph lockedNode true
            in
            (List.fold (fun sigma_graphList evaluated -> 
                 let assignedNode = {programPoint=ctx.prev_node;sigma=evaluated;id=(idGenerator#getID lockedNode 
                                                                                      (Assign((Var(localGlobalVar),NoOffset),Lval(Var(global), NoOffset))) ctx.prev_node evaluated lastNode.tid lvalLockedLs);tid=lastNode.tid;lockSet=lvalLockedLs}
                 in
                 let assigningEdge:(node * CustomEdge.t * node) =  (lockedNode, (Assign((Var(localGlobalVar),NoOffset),Lval(Var(global), NoOffset))), assignedNode)
                 in
                 let assignedGraph = LocalTrace.extend_by_gEdge lockedGraph assigningEdge
                 in
                 let unlockingLabel:CustomEdge.t = Proc(None, Lval(Var(unlockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
                 in
                 let unlockedNode = {programPoint=ctx.prev_node;sigma=evaluated;id=(idGenerator#getID assignedNode unlockingLabel ctx.prev_node evaluated lastNode.tid lastNode.lockSet);tid=lastNode.tid;lockSet=lastNode.lockSet}
                 in
                 let unlockingEdge = (assignedNode, unlockingLabel, unlockedNode)
                 in
                 let unlockingGraph = LocalTrace.extend_by_gEdge assignedGraph unlockingEdge
                 in
                 let ctxGlobalTid = ctx.global myTmp
                 in
                 ctx.sideg myTmp (D.add unlockingGraph ctxGlobalTid);
                 unlockingGraph::sigma_graphList
               ) [] sigmaList)@resultGraphList

          ) [] lockedGraphList
        in
        graphList@acc
      | [] -> acc
    in
    match globals with global::xs -> 
      create_local_assignments (loop global graphList []) xs ctx
                     | [] -> graphList


  let context fundec l =
    ()

  let name () = "localTraces"

  (* start state is a set of one empty graph *)
  let startstate v = let g = D.empty () in let tmp = D.add LocTraceGraph.empty g
    in if D.is_empty tmp then tmp else tmp


  let exitstate = startstate

  (* ASSIGN helper functions *)
  (* perform assign-effect on given node *)
  let assign_on_node graph ctx lval rval {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} = 
    (match lval with (Var x, _) -> 
       let lockVarinfo = customVinfoStore#getGlobalVarinfo "pthread_mutex_lock" 
       in
       let unlockVarinfo = customVinfoStore#getGlobalVarinfo "pthread_mutex_unlock"
       in
       let rvalGlobals = get_all_globals rval VarinfoSet.empty
       in print_string ("in assign, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
       let someTmp = create_local_assignments [graph] (VarinfoSet.to_list rvalGlobals) ctx
       in
       let resultList = List.fold (
           fun resultList_outter graph_outter ->
             (
               let lastNode = LocalTrace.get_last_node graph_outter
               in
               let sigmaList,success_inner, newExp = eval_wrapper lastNode.sigma x rval  graph_outter lastNode true in 
               if not success_inner then (print_string "assignment did not succeed!\n"; 
                                          [LocalTrace.extend_by_gEdge graph_outter (lastNode,Assign(lval, newExp),{programPoint=LocalTrace.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty})] )
               else
                 (List.fold ( fun outerGraphList evaluated ->
                      let newSigma = remove_global_locals_sigma evaluated (VarinfoSet.to_list rvalGlobals)
                      in
                      if x.vglob then (
                        let customMutex = customVinfoStore#getGlobalVarinfo (make_mutex_varinfo x)
                        in
                        let lockingLabel:Edge.t = Proc(None, Lval(Var(lockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
                        in
                        let lvalLockedLs = VarinfoSet.add customMutex ls
                        in
                        let lockedNode = {programPoint=programPoint;sigma=lastNode.sigma; id=(idGenerator#getID lastNode (EdgeImpl.convert_edge lockingLabel) programPoint sigma tid lvalLockedLs);tid=tid;lockSet=lvalLockedLs}
                        in
                        let lockingEdge = (lastNode,EdgeImpl.convert_edge lockingLabel,lockedNode)
                        in
                        let lockedGraph = LocalTrace.extend_by_gEdge graph_outter lockingEdge  
                        in
                        let myTmp:V.t = Mutex(customMutex)
                        in
                        let allUnlockingTraces = ctx.global myTmp
                        in 
                        let firstNode = LocalTrace.get_first_node graph_outter
                        in
                        print_string ("In assign, firstNode is "^(NodeImpl.show firstNode)^" for graph_outter "^(LocalTrace.show graph_outter)^"\n");
                        let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(customMutex),lockedNode)
                        in
                        let firstLockedGraph = if (NodeImpl.equal firstNode lastNode) then lockedGraph
                          else if (LocalTrace.exists_unlock_mutex graph_outter customMutex)
                          then add_dependency_from_last_unlock lockedGraph customMutex 
                          else if not (LocalTrace.exists_lock_mutex graph_outter customMutex) then LocalTrace.extend_by_gEdge lockedGraph firstLockEdge
                          else lockedGraph
                        in 
                        print_string ("firstLockedGraph="^(LocalTrace.show firstLockedGraph)^"\n");
                        let lockedGraphList = 
                          firstLockedGraph::(mutexLock_join allUnlockingTraces lockedGraph lastNode lockingLabel lockedNode customMutex)
                        in
                        let graphList = List.fold (fun resultGraphList lockedGraph -> 
                            let assignedNode = {programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID lockedNode 
                                                                                           (Assign(lval,newExp)) ctx.node newSigma tid lvalLockedLs);tid=tid;lockSet=lvalLockedLs}
                            in
                            let assigningEdge:(node * CustomEdge.t * node) = 
                              (lockedNode, (Assign(lval,newExp)), assignedNode)
                            in
                            let assignedGraph = LocalTrace.extend_by_gEdge lockedGraph assigningEdge
                            in
                            let unlockingLabel:CustomEdge.t = Proc(None, Lval(Var(unlockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
                            in
                            let unlockedNode = {programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID assignedNode unlockingLabel ctx.node newSigma tid ls);tid=tid;lockSet=ls}
                            in
                            let unlockingEdge = (assignedNode, unlockingLabel, unlockedNode)
                            in
                            let unlockingGraph = LocalTrace.extend_by_gEdge assignedGraph unlockingEdge
                            in
                            let ctxGlobalTid = ctx.global myTmp
                            in
                            ctx.sideg myTmp (D.add unlockingGraph ctxGlobalTid);
                            unlockingGraph::resultGraphList

                          ) [] lockedGraphList
                        in
                        graphList@outerGraphList
                      ) else(
                        let (myEdge:(node * CustomEdge.t * node)) =
                          (lastNode, (Assign(lval,newExp)),{programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID lastNode (Assign(lval,newExp)) ctx.node newSigma tid ls);tid=tid;lockSet=ls})
                        in
                        print_string ("assignment succeeded so we add the edge "^(LocalTrace.show_edge myEdge)^"\n");
                        (LocalTrace.extend_by_gEdge graph_outter myEdge)::outerGraphList )
                    ) [] sigmaList))@resultList_outter
         ) [] someTmp
       in
       resultList
                   | _ -> Printf.printf "This type of assignment is not supported in assign_on_node\n"; exit 0

    )

  (* iterate over the graphs in previous state *)
  let assign_fold_graphSet graph (lval, rval, ctx,set_acc) =
    let lastNode = 
      if (LocTraceGraph.is_empty graph)&&(match lval with (Var x, _) -> x.vglob | _ -> false) then
        (
          tidRecord#addTID 1;
          {programPoint=ctx.prev_node;sigma=SigmaMap.empty;id=idGenerator#increment();tid=1;lockSet=VarinfoSet.empty}
        )
      else LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      (if Node.equal lastNode.programPoint LocalTrace.error_node then (print_string ("In assign, we have a trace that does not end in previous node:
    "^(LocalTrace.show graph)^"\n"); set_acc) else 
         let result_graphList = 
           assign_on_node (if LocTraceGraph.is_empty graph then (LocTraceGraph.add_vertex graph lastNode) else graph) ctx lval rval lastNode
         in
         List.fold (fun set_fold result_graph -> if (LocalTrace.equal graph result_graph)  then set_acc else D.add result_graph set_fold) set_acc result_graphList
      )
    in
    (lval, rval, ctx, new_set)

  let assign ctx (lval:lval) (rval:exp) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect assign was invoked with lval "^(CilType.Lval.show lval)^" and rval "^(CilType.Exp.show rval)^" and ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^"\n");
    print_string("in assign, ctx.local=["^(D.fold (fun graph acc -> (LocalTrace.show graph)^"\n"^acc) ctx.local "")^"]\n");
    let _, _, _, result =
      D.fold assign_fold_graphSet ctx.local (lval, rval, ctx, D.empty ())
    in result

  (* BRANCH helper functions *)
  (* perform branch-effect on given node *)
  let branch_on_node graph ctx exp tv {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
    let rvalGlobals = get_all_globals exp VarinfoSet.empty
    in print_string ("in branch, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
    let someTmp = create_local_assignments [graph] (VarinfoSet.to_list rvalGlobals) ctx
    in
    List.fold (fun resultList_outter graph_outter -> 
        (
          let lastNode = LocalTrace.get_last_node graph_outter
          in
          let branch_sigma = SigmaMap.add LocalTrace.branch_vinfo Error lastNode.sigma 
          in
          let sigmaList,success, newExp = eval_wrapper branch_sigma LocalTrace.branch_vinfo exp graph_outter lastNode tv
          in
          List.map (fun sigma_map ->
              let result_as_int = match (SigmaMap.find_default Error LocalTrace.branch_vinfo sigma_map) with
                  Int(i1,i2,_) -> print_string ("in branch, the result is ["^(Big_int_Z.string_of_big_int i1)^";"^(Big_int_Z.string_of_big_int i2)^"]");
                  if (Big_int_Z.int_of_big_int i1 == 0)&&(Big_int_Z.int_of_big_int i2 == 0) then 0 
                  else if (Big_int_Z.int_of_big_int i1 > 0)||(Big_int_Z.int_of_big_int i2 < 0) then 1
                  else -1
                |_ -> -1
              in
              let sigmaNew = remove_global_locals_sigma (SigmaMap.remove LocalTrace.branch_vinfo (NodeImpl.destruct_add_sigma lastNode.sigma sigma_map)) (VarinfoSet.to_list rvalGlobals)
              in
              print_string ("result_as_int: "^(string_of_int result_as_int)^"\n");
              let myEdge:node*CustomEdge.t*node = (lastNode, Test(newExp, tv),
                                                   {programPoint=ctx.node;sigma=sigmaNew;id=(idGenerator#getID lastNode (Test(newExp, tv)) ctx.node sigmaNew tid ls);tid=tid;lockSet=ls})
              in
              print_string ("success="^(string_of_bool success)^", tv="^(string_of_bool tv)^", result_as_int="^(string_of_int result_as_int)^"\nand possible edge="^(LocalTrace.show_edge myEdge)^"\n");
              let result_graph = if success&&((tv=true && result_as_int = 1)||(tv=false&&result_as_int=0)) 
                then LocalTrace.extend_by_gEdge graph_outter myEdge else (print_string "no edge added for current sigma in branch\n";graph)
              in
              result_graph
            ) sigmaList
        )@resultList_outter
      ) [] someTmp

  (* iterate over the graphs in previous state *)
  let branch_fold_graphSet graph (exp, tv, ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set =
      (if Node.equal lastNode.programPoint LocalTrace.error_node then (print_string ("In branch, we have a trace that does not end in previous node:
  "^(LocalTrace.show graph)^"\n"); set_acc) else
         let result_graphList = 
           branch_on_node graph ctx exp tv lastNode
         in
         List.fold (fun set_fold result_graph -> if (LocalTrace.equal graph result_graph) then set_fold else D.add result_graph set_fold) set_acc result_graphList
      )
    in
    (exp, tv, ctx, new_set)

  let branch ctx (exp:exp) (tv:bool) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect branch was invoked with exp="^(CilType.Exp.show exp)^" and tv="^(string_of_bool tv)^" and ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^"\n");
    let _, _, _, result =
      D.fold branch_fold_graphSet ctx.local (exp, tv, ctx, D.empty ())
    in result

  (* BODY helper functions *)
  (* perform body-effect on given node *)
  let body_on_node graph ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
    let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
    in
    let result_graph = LocalTrace.extend_by_gEdge graph myEdge
    in
    result_graph

  (* iterate over the graphs in previous state *)
  let body_fold_graphSet graph (ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = (if Node.equal lastNode.programPoint LocalTrace.error_node then (print_string ("In body, we have a trace that does not end in previous node:
    "^(LocalTrace.show graph)^"\n");if LocTraceGraph.is_empty graph 
                                                                                    then(
                                                                                      let first_ID = idGenerator#increment()
                                                                                      in
                                                                                      let second_ID = idGenerator#increment()
                                                                                      in
                                                                                      tidRecord#addTID first_ID;
                                                                                      D.add (LocalTrace.extend_by_gEdge graph ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id= first_ID;tid=first_ID;lockSet=VarinfoSet.empty},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=SigmaMap.empty; id= second_ID;tid=first_ID;lockSet=VarinfoSet.empty})) set_acc)
                                                                                    else set_acc) else
                     let result_graph = 
                       body_on_node graph ctx lastNode
                     in
                     D.add result_graph set_acc)
    in
    (ctx, new_set)

  let body ctx (f:fundec) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect body was invoked with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^"\n");
    let _, result =
      D.fold body_fold_graphSet ctx.local (ctx, D.empty ())
    in
    print_string ("Resulting state of body: "^(D.fold (fun foldGraph acc -> (LocalTrace.show foldGraph)^", "^acc) result "")^"\n");result

  (* RETURN helper functions *)
  (* perform return-effect on given node *)
  let return_on_node graph ctx exp {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} (f:fundec) =
    let result_graphList =
      match exp with 
      | None ->  let myEdge =  print_string "In return case None\n";
                   ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
        in
        let result_graph =LocalTrace.extend_by_gEdge graph myEdge
        in
        (
          match ctx.ask CurrentThreadId with
          | `Lifted tid_lifted when ThreadReturn.is_current (Analyses.ask_of_ctx ctx) -> print_string("In return, I reached ThreadReturn\n");
            let myTmp:V.t =ThreadID(tid)
            in
            let ctxGlobalTid = ctx.global myTmp
            in
            ctx.sideg myTmp (D.add result_graph ctxGlobalTid)
          | _ -> () 
        );
        [result_graph]
      | Some(ret_exp) -> (  ( print_string ("return expression: "^(CilType.Exp.show ret_exp)^"\n");
                              match ret_exp with
                              | CastE(TPtr(TVoid(_), attrList2),Const(CInt(cilint,IInt,_))) ->
                                let myEdge =
                                  if Cilint.is_zero_cilint cilint then (
                                    {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls}) 
                                  else (print_string "In return, unsupported expression\n"; exit 0)
                                in
                                let result_graph = LocalTrace.extend_by_gEdge graph myEdge
                                in
                                (
                                  match ctx.ask CurrentThreadId with
                                  | `Lifted tid_lifted when ThreadReturn.is_current (Analyses.ask_of_ctx ctx) -> print_string("In return, I reached ThreadReturn\n");
                                    let myTmp:V.t = ThreadID(tid)
                                    in
                                    let ctxGlobalTid = ctx.global myTmp
                                    in
                                    ctx.sideg myTmp (D.add result_graph ctxGlobalTid)
                                  | _ -> () 
                                );
                                [result_graph]
                              | _ -> 
                                let rvalGlobals = get_all_globals ret_exp VarinfoSet.empty
                                in print_string ("in return, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
                                let someTmp = create_local_assignments [graph] (VarinfoSet.to_list rvalGlobals) ctx
                                in
                                let outterResultList = List.fold ( fun resultList_outter graph_outter ->
                                    (
                                      let lastNode = LocalTrace.get_last_node graph_outter
                                      in  
                                      let resultList, success, newExp = eval_wrapper lastNode.sigma LocalTrace.return_vinfo ret_exp graph_outter lastNode true
                                      in
                                      List.map (fun sigma_map -> 
                                          let newSigma = remove_global_locals_sigma sigma_map (VarinfoSet.to_list rvalGlobals)
                                          in
                                          let myEdge: node*CustomEdge.t*node =
                                            if success = false then (print_string "Error: Evaluation of return expression was unsuccessful\n"; exit 0)
                                            else (lastNode,Ret(Some(newExp),f),{programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID lastNode (Ret(Some(newExp),f)) ctx.node newSigma tid ls);tid=tid;lockSet=ls})
                                          in
                                          let result_graph =
                                            LocalTrace.extend_by_gEdge graph_outter myEdge
                                          in
                                          (
                                            match ctx.ask CurrentThreadId with
                                            | `Lifted tid_lifted when ThreadReturn.is_current (Analyses.ask_of_ctx ctx) -> print_string("In return, I reached ThreadReturn\n");
                                              let myTmp:V.t = ThreadID(tid)
                                              in
                                              let ctxGlobalTid = ctx.global myTmp
                                              in
                                              ctx.sideg myTmp (D.add result_graph ctxGlobalTid)
                                            | _ -> () 
                                          );
                                          result_graph
                                        ) resultList )@resultList_outter

                                  ) [] someTmp
                                in outterResultList

                            )
                         )
    in
    result_graphList

  (* iterate over the graphs in previous state *)
  let return_fold_graphSet graph (f, exp, ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTrace.error_node then (print_string ("In return, we have a trace that does not end in previous node:
  "^(LocalTrace.show graph)^"\n"); set_acc) else
        let result_graphList = 
          return_on_node graph ctx exp lastNode f
        in
        List.fold (fun set_fold result_graph -> D.add result_graph set_fold) set_acc result_graphList

    in
    (f, exp, ctx, new_set)

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect return was invoked with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^"\n");
    let _, _, _, result = D.fold return_fold_graphSet ctx.local (f, exp, ctx, D.empty ())
    in 
    result

  (* SPECIAL helper functions *)
  (* perform special-effect on given node *)
  let special_on_node graph ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} f arglist lval =
    print_string ("in special, we have ctx.edge="^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^"\n");
    let desc = LibraryFunctions.find f in
    match desc.special arglist, f.vname with

    | Lock {lock = lockExp;_}, _ ->
      ( let mutex_vinfo =
          print_string ("in special, we found Lock with lockExp="^(CilType.Exp.show lockExp)^"\n");
          match lockExp with AddrOf(Var(vinfo), _) -> vinfo
                           | _ -> Printf.printf "Expression in pthread_mutex_lock is not supported\n"; exit 0
        in
        let myTmp:V.t = Mutex(mutex_vinfo)
        in
        let allUnlockingTraces = ctx.global myTmp
        in
        print_string("in special, Lock, allUnlockingTraces= ["^(D.fold (fun graph_fold s_fold -> s_fold^(LocalTrace.show graph_fold)^"\n") allUnlockingTraces "")^"]\n");
        let firstNode = LocalTrace.get_first_node graph
        in
        print_string ("In special, firstNode is "^(NodeImpl.show firstNode)^"\n");
        let lockedNode = {programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid (VarinfoSet.add mutex_vinfo ls));tid=tid;lockSet=(VarinfoSet.add mutex_vinfo ls)}
        in
        let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(mutex_vinfo),lockedNode)
        in
        let lockingEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),lockedNode)
        in
        let lockedGraph = LocalTrace.extend_by_gEdge graph lockingEdge
        in
        (* First lock is only added, if this is the first lock *)
        let firstLockedGraph = if LocalTrace.exists_unlock_mutex graph mutex_vinfo then
            add_dependency_from_last_unlock lockedGraph mutex_vinfo 
          else if not (LocalTrace.exists_lock_mutex graph mutex_vinfo) then 
            ( 
              LocalTrace.extend_by_gEdge lockedGraph firstLockEdge)
          else lockedGraph
        in
        (* we create both: merged traces and trace that assumes first lock *)
        firstLockedGraph::(mutexLock_join allUnlockingTraces lockedGraph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} ctx.edge lockedNode mutex_vinfo)
      )

    | Unlock(unlockExp), _ ->
      (
        let mutex_vinfo =
          match unlockExp with AddrOf(Var(vinfo), _) -> vinfo
                             | _ -> Printf.printf "Expression in pthread_mutex_lock is not supported\n"; exit 0
        in
        print_string ("in special, we found Unlock with mutex_vinfo="^(CilType.Varinfo.show mutex_vinfo)^"\n");
        let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid (VarinfoSet.remove mutex_vinfo ls));tid=tid;lockSet=(VarinfoSet.remove mutex_vinfo ls)})
        in
        let result_graph = LocalTrace.extend_by_gEdge graph myEdge
        in
        let myTmp:V.t = Mutex(mutex_vinfo)
        in
        let ctxGlobalTid = ctx.global myTmp
        in
        ctx.sideg myTmp (D.add result_graph ctxGlobalTid);
        [result_graph])

    | ThreadJoin { thread = tidExp; ret_var }, _ ->
      let myTmp = (print_string ("We found a pthread_join in special with tidExp="^(CilType.Exp.show tidExp)^" and ret_var="^(CilType.Exp.show ret_var)^"\n"); 
                   let special_varinfo = makeVarinfo false "__goblint__traces__special" (TInt(IInt,[]))
                   in
                   let tidSigmaList, success, newExp = eval_wrapper sigma special_varinfo tidExp graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} true
                   in if not success then (Printf.printf "Error: could not evaluate argument of pthread_join in special\n"; exit 0);
                   List.fold (fun graphList tidSigma -> 
                       let tidJoin = 
                         match SigmaMap.find special_varinfo tidSigma with
                           Int(l,u,_) -> if l = u then Big_int_Z.int_of_big_int l (* TODO iterate over interval or pick a few values *) else (Printf.printf "Intervals for pthread_join is not supported in special\n"; exit 0)
                         | ThreadID(tid_find) -> tid_find
                         | _ -> Printf.printf "Error: wrong type of argument of pthread_join in special\n"; exit 0
                       in
                       if LocalTrace.is_already_joined tidJoin graph then (
                         Messages.warn "ThreadJoin on already joined Thread-ID";
                         omitPostSolving#setFlag ();
                         print_string ("ThreadJoin did not succeed due to already joined TID for graph: \n"^(LocalTrace.show graph)^"\nand tidJoin: "^(string_of_int tidJoin)^"\n");
                         let graph_error_edge = LocalTrace.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTrace.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty}) 
                         in
                         graph_error_edge::graphList
                       )
                       else 
                         let myTmp:V.t = ThreadID(tidJoin)
                         in
                         let endingTraces = graphSet_to_list (ctx.global myTmp)
                         in
                         print_string("in special, ending traces: ["^(List.fold (fun acc g -> acc^(LocalTrace.show g)) "" endingTraces)^"]\n");
                         let joinableTraces = find_joinable_traces endingTraces graph tid
                         in
                         if not (tidRecord#existsTID tidJoin) then (
                           Messages.warn "ThreadJoin on non-existent Thread-ID";
                           omitPostSolving#setFlag ();
                           let graph_error_edge = LocalTrace.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTrace.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty}) 
                           in
                           print_string("In ThreadJoin, we add an error-trace with endingTraces:\n
["^(List.fold (fun s_fold graph_fold -> (LocalTrace.show graph_fold)^";\n"^s_fold) "" endingTraces)^"]\n");
                           graph_error_edge::graphList
                         )
                         else if List.is_empty joinableTraces then 
                           (* we cannot join *)
                           graphList
                         else 
                           (
                             print_string ("in special, joinable traces for tidJoin "^(string_of_int tidJoin)^" are: ["^(List.fold (fun acc g -> acc^(LocalTrace.show g)) "" joinableTraces)^"]\n");
                             List.fold (
                               fun list_fold trace_fold -> let tmp_graph = LocalTrace.merge_graphs graph trace_fold
                                 in
                                 let newVarinfoSet = VarinfoSet.union ls (LocalTrace.get_last_node trace_fold).lockSet
                                 in
                                 let destination_node = {programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid newVarinfoSet);tid=tid;lockSet=newVarinfoSet}
                                 in
                                 print_string ("In ThreadJoin, destination_node="^(NodeImpl.show destination_node)^"\n");
                                 let tmp_graph_edge1 = LocalTrace.extend_by_gEdge tmp_graph (LocalTrace.get_last_node trace_fold, EdgeImpl.convert_edge ctx.edge,destination_node)
                                 in
                                 let tmp_graph_edge2 = LocalTrace.extend_by_gEdge tmp_graph_edge1 ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,destination_node)
                                 in
                                 print_string ("tmp_graph_edge2 = "^(LocalTrace.show tmp_graph_edge2)^"\n");
                                 ((tmp_graph_edge2)::list_fold)
                             ) [] joinableTraces)@graphList
                     ) [] tidSigmaList)
      in 
      print_string("Before ThreadJoin, we had:\n
"^(LocalTrace.show graph)^"\n
and after, we have:
"^(List.fold (fun s_fold g_fold -> (LocalTrace.show g_fold)^";\n"^s_fold) "" myTmp)^" \n");
      myTmp

    | ThreadCreate {thread = tidExp;start_routine=start_routine;arg=arg_create}, _ ->  print_string ("We found a pthread_create in special\n"); [graph]

    | ThreadExit _, _ -> print_string "In special, I reached ThreadExit\n"; 
      let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
      in
      let result_graph = LocalTrace.extend_by_gEdge graph myEdge
      in
      let myTmp:V.t = ThreadID(tid)
      in
      let ctxGlobalTid = ctx.global myTmp
      in
      ctx.sideg myTmp (D.add result_graph ctxGlobalTid); [result_graph]

    | _ -> 
      if String.equal f.vname "pthread_mutex_destroy" 
      then (print_string ("In special, we found pthread_mutex_destroy 
    with arglist: "^(List.fold (fun acc_fold arg_fold -> (CilType.Exp.show arg_fold)^"; "^acc_fold) "" arglist)^"\n");
            match arglist with [AddrOf(Var(argVinfo),_)] -> if VarinfoSet.mem argVinfo ls 
              then ( Messages.warn "mutex_destroy on locked mutex";
                     omitPostSolving#setFlag ();
                     let graph_error_edge = LocalTrace.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTrace.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty})
                     in
                     [graph_error_edge]) 
              else (let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
                    in
                    let result_graph = LocalTrace.extend_by_gEdge graph myEdge
                    in
                    [result_graph])
                             | _ -> Printf.printf "Error: wrong amount of arguments for pthread_mutex_destroy in special\n"; exit 0)

      else if String.equal f.vname "rand" then (
        match lval with 
        | Some(Var(var), NoOffset) ->
          let randomValue = Big_int_Z.big_int_of_int (randomIntGenerator#getRandomValueFullCInt (LocalTrace.hash graph) var )
          in
          let newSigma = SigmaMap.add var (Int(randomValue, randomValue, IInt)) sigma
          in
          let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,
                        {programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid ls);tid=tid;lockSet=ls})
          in
          let result_graph = LocalTrace.extend_by_gEdge graph myEdge
          in
          [result_graph]
        | _ -> print_string "In special, lval for rand is not suitable\n"; exit 0
      )
      else
        (print_string ("This edge is not one of my considered special functions\n"); 
         let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
         in
         let result_graph = LocalTrace.extend_by_gEdge graph myEdge
         in
         [result_graph])

  (* iterate over the graphs in previous state *)
  let special_fold_graphSet graph (lval, f, arglist, ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set =
      if Node.equal lastNode.programPoint LocalTrace.error_node then (print_string ("In special, we have a trace that does not end in previous node:
    "^(LocalTrace.show graph)^"\n"); set_acc) else 
        let result_graph = 
          special_on_node graph ctx lastNode f arglist lval
        in
        List.fold (fun set_fold graph_fold -> D.add graph_fold set_fold) set_acc result_graph
    in
    (lval, f, arglist, ctx, new_set)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect special was invoked with ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^" and ctx.edge "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^" und f "^(CilType.Varinfo.show f)^"\n");
    let _, _, _, _, result =   D.fold special_fold_graphSet ctx.local (lval, f, arglist, ctx, D.empty ())
    in result

  (* ENTER helper functions *)
  (* From a list of sigmas and a list of possible values for varinfo, generate each combination of sigma containing varinfo *)
  let construct_sigma_combinations varinfo varDomList sigmaList =
    let rec loop varDomList_loop sigAcc =
      match varDomList_loop with x::xs -> loop xs ((List.map (fun sigma -> (SigmaMap.add varinfo x sigma)) sigmaList)@sigAcc)
                               | [] -> sigAcc
    in
    loop varDomList []

  (* perform enter-effect on given node *)
  let enter_on_node graph ctx f args {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
    let rvalGlobals = List.fold (fun ls_fold arg_fold -> get_all_globals arg_fold ls_fold) VarinfoSet.empty args
    in
    print_string ("in enter, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
    let someTmp = create_local_assignments [graph] (VarinfoSet.to_list rvalGlobals) ctx
    in
    (* print_string ("someTmp: "^(List.fold (fun s_acc graph_fold -> (LocalTrace.show graph_fold)^"; "^s_acc) "" someTmp)^"\n"); *)
    let outterResultList = List.fold ( fun resultList_outter graph_outter ->
        (
          let lastNode = LocalTrace.get_last_node graph_outter
          in
          let sigma_formalList, _, newExpList = List.fold ( 
              fun (sigAcc, formalExp, expListAcc) formal -> (match formalExp with 
                  | x::xs -> (
                      let resultList, success, newExp = eval_wrapper lastNode.sigma formal x graph_outter lastNode true
                      in if success = true 
                      then (
                        let varDomainList = List.fold (fun varDomList_fold sigma_fold -> (SigmaMap.find formal sigma_fold)::varDomList_fold) [] resultList  
                        in
                        (construct_sigma_combinations formal varDomainList sigAcc) , xs, newExp::expListAcc) 
                      else (sigAcc, xs, newExp::expListAcc)
                    )
                  | [] -> Printf.printf "Fatal error: missing expression for formals in enter\n"; exit 0)
            ) ([SigmaMap.empty], args, []) f.sformals
          in
          print_string ("sigma_formalList={"^(List.fold (fun s_fold sigma_fold -> (NodeImpl.show_sigma sigma_fold)^";"^s_fold) "" sigma_formalList)^"}\n");
          List.map (fun sigma_map ->
              print_string ("in enter_on_node, sigma_map="^(NodeImpl.show_sigma sigma_map)^"\n");
              let newEdgeLabel:CustomEdge.t = match ctx.edge with Proc(lvalOp, fexp, _) -> print_string ("Edge is a Proc("^(CilType.Exp.show fexp)^") \n"); 
                Proc(lvalOp, fexp, List.rev newExpList)
                                                                | Skip -> Skip (* in case of main function *)
                                                                | _ -> print_string ("Error: in enter, the edge label is not a function call (or skip); ctx.edge="^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^"\n"); exit 0
              in
              let myEdge = (lastNode, newEdgeLabel,
                            {programPoint=(FunctionEntry(f));sigma= (remove_global_locals_sigma sigma_map (VarinfoSet.to_list rvalGlobals));id=(idGenerator#getID lastNode newEdgeLabel (FunctionEntry(f)) sigma_map tid ls);tid=tid;lockSet=ls})
              in
              LocalTrace.extend_by_gEdge graph_outter myEdge
            ) sigma_formalList)@resultList_outter
      ) [] someTmp
    in
    outterResultList



  (* iterate over the graphs in previous state *)
  let enter_fold_graphSet graph (f, args, ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTrace.error_node then (print_string ("In enter, we have a trace that does not end in previous node:
  "^(LocalTrace.show graph)^"\n"); set_acc) else
        let result_graphList =
          enter_on_node graph ctx f args lastNode
        in
        List.fold (fun set_fold result_graph -> D.add result_graph set_fold) set_acc result_graphList
    in
    (f, args,ctx, new_set)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect enter was invoked with function "^(CilType.Fundec.show f)^" with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^", edge label "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^"
  with formals "^(List.fold (fun s sformal -> s^", "^(CilType.Varinfo.show sformal)) "" f.sformals)^" and arguments "^(List.fold (fun s exp -> s^", "^(CilType.Exp.show exp)) "" args)^"\n");
    if D.is_empty ctx.local then (
      let first_ID = idGenerator#increment()
      in
      let second_ID = idGenerator#increment()
      in
      [ctx.local, D.add (LocalTrace.extend_by_gEdge (LocTraceGraph.empty) ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id=first_ID;tid=first_ID;lockSet=VarinfoSet.empty}, (EdgeImpl.convert_edge ctx.edge), {programPoint=(FunctionEntry(f));sigma=SigmaMap.empty;id= second_ID;tid=first_ID;lockSet=VarinfoSet.empty})) (D.empty ())] )
    else
      let _, _, _, result = print_string ("In enter, neuer state wird erstellt\n mit ctx.local: "^(D.show ctx.local)^" und |ctx.local| = "^(string_of_int (D.cardinal ctx.local))^"\n"); 
        D.fold enter_fold_graphSet ctx.local (f, args, ctx, D.empty ())
      in
      [ctx.local, result]  

  (* COMBINE helper functions *)
  (* perform combine-effect on node *)
  let combine_on_node args ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} {programPoint=progP_returning;id=id_returning;sigma=sigma_returning;tid=tid_returning;lockSet=ls_returning} callee_local lval graph =
    print_string ("In combine, calling node="^(NodeImpl.show {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls})^"\n");
    let rvalGlobals = List.fold (fun ls_fold arg_fold -> get_all_globals arg_fold ls_fold) VarinfoSet.empty args
    in
    print_string ("in combine, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
    let newSigma = remove_global_locals_sigma sigma (VarinfoSet.to_list rvalGlobals)
    in
    if tid != tid_returning then (Printf.printf "TIDs from current node and found returning node are different in combine\n"; exit 0);
    let (myEdge:(node * CustomEdge.t * node)) = 
      (match lval with None -> 
         {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Skip,
         {programPoint=ctx.node;sigma=newSigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning; lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid_returning (VarinfoSet.union ls_returning ls));tid=tid_returning;lockSet=(VarinfoSet.union ls_returning ls)}
                     |Some (Var x, y) ->  if x.vglob 
                       then 
                         ({programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Assign((Var(x), y), 
                                                                                                                                             (Lval(Var(LocalTrace.return_vinfo),NoOffset))),{programPoint=ctx.node;sigma=newSigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid_returning (VarinfoSet.union ls_returning ls));tid=tid_returning;lockSet=(VarinfoSet.union ls_returning ls)}) 
                       else
                         (let return_value = SigmaMap.find LocalTrace.return_vinfo sigma_returning
                          in if equal_varDomain return_value Error then (print_string "In combine, a returning Error is assigned to some lval, this is not supported\n";exit 0) else
                            let result_sigma = SigmaMap.add x return_value newSigma
                            in {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Skip,{programPoint=ctx.node;sigma=result_sigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node result_sigma tid_returning (VarinfoSet.union ls_returning ls));tid=tid_returning; lockSet=(VarinfoSet.union ls_returning ls)})
                     | _ -> Printf.printf "Invalid Lval format in combine\n"; exit 0)
    in
    let result_graph = LocalTrace.extend_by_gEdge graph myEdge
    in
    result_graph


  (* iterate over the graphs in previous state *)
  let combine_fold_graphSet graph (args, lval, callee_local, ctx,set_acc) =
    (* wir müssen schauen, ob ein returnender Knoten für irgendein ctx.prev_node ein last node ist*)
    let lastGraphNode = LocalTrace.get_last_node graph 
    in if Node.equal lastGraphNode.programPoint LocalTrace.error_node then (print_string "In combine, we have an empty graph\n"; (args, lval, callee_local, ctx,set_acc)) 
    else (
      let currentNode = 
        LocalTrace.find_calling_node lastGraphNode graph ctx.prev_node
      in
      let new_set = 
        (if Node.equal currentNode.programPoint LocalTrace.error_node then (print_string ("In combine, we could not find the calling node:
      "^(LocalTrace.show graph)^"\n"); set_acc) else
           let result_graph= 
             combine_on_node args ctx currentNode lastGraphNode callee_local lval graph
           in
           D.add result_graph set_acc)
      in
      (args, lval, callee_local, ctx, new_set))

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect combine was invoked with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^", edge label "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^"
    and lval "^(match lval with None -> "None" | Some(l) -> CilType.Lval.show l)^" and fexp "^(CilType.Exp.show fexp)^"\n");
    let _,_,_,_, result=
      D.fold combine_fold_graphSet callee_local (args, lval, callee_local, ctx, D.empty ())
    in result

  (* THREADENTER helper functions *)
  (* perform threadenter-effect on node *)
  let threadenter_on_node graph ctx f args {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
    match Cilfacade.find_varinfo_fundec f with
    | fd -> print_string ("fd.sformals ={"^(List.fold (fun acc_fold formal_fold -> acc_fold^(CilType.Varinfo.show formal_fold)^"; ") "" fd.sformals )^"}\n");
      ( let sigma_formals, _ = 
          List.fold (
            fun (sigAcc, formalExp) formal -> (match formalExp with 
                | CastE(TPtr(TVoid(_), attrList2),CastE(TPtr(TVoid(_), _),Const(CInt(cilint,IInt,_))))::xs -> if Cilint.is_zero_cilint cilint 
                  then (SigmaMap.empty, xs) 
                  else (print_string "Inputs for threads is not yet supported\n"; exit 0)
                | x::xs -> (print_string "Inputs for threads is not yet supported\n"; exit 0)
                | [] -> Printf.printf "Fatal error: missing expression for formals in enter\n"; exit 0)
          ) (sigma, args) fd.sformals 
        in print_string ("sigma_formals: "^(NodeImpl.show_sigma sigma_formals)^"\n");
        let new_id = idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma_formals id VarinfoSet.empty
        in
        (* create dependency is just the create edge itself *)
        let myEdge = {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=(FunctionEntry(fd));sigma=sigma_formals;id=new_id;tid=id;lockSet=VarinfoSet.empty}
        in
        let result_graph = print_string ("In threadenter, we add the edge "^(LocalTrace.show_edge myEdge)^"\n
  to the graph:"^(LocalTrace.show graph)^"\n"); LocalTrace.extend_by_gEdge graph myEdge
        in
        result_graph
      )
    | exception Not_found -> Printf.printf "Error: function does not exist, in threadenter\n"; exit 0


  (* iterate over the graphs in previous state *)
  let threadenter_fold_graphSet graph (args, f, ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTrace.error_node 
      then (print_string ("In threadenter, we have a trace that does not end in previous node:
   "^(LocalTrace.show graph)^"\n"); set_acc) else(
        let result_graph = threadenter_on_node graph ctx f args lastNode
        in
        D.add result_graph set_acc)
    in
    (args, f, ctx, new_set)

  let threadenter ctx lval f (args:exp list) = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect threadenter was invoked with ctx.prev_node "^(Node.show ctx.prev_node)^", ctx.edge "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^" and ctx.node "^(Node.show ctx.node)^"
    , lval "^(match lval with None -> "None" |Some(l) -> CilType.Lval.show l)^"
    \nand args={"^(List.fold (fun acc_fold exp_fold -> acc_fold^(CilType.Exp.show exp_fold)^"; ") "" args)^"}\n");
    let _, _, _, result = D.fold threadenter_fold_graphSet (ctx.local) (args, f, ctx, D.empty())
    in
    [result]

  (* THREADSPAWN helper functions *)
  (* perform threadspawn-effect on given node *)
  let threadspawn_on_node graph ctx lval {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
    (* convention: TID of new thread is always the ID of creating node *)
    tidRecord#addTID id;
    let myEdge =  (match lval with None ->
        {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls}
                                 | Some(Mem(CastE(TPtr(TNamed(tInfo, tAttr), ptrAttr), AddrOf(Var(lvalVinfo),_))), offset) -> (
                                     if String.equal tInfo.tname "pthread_t" 
                                     then (print_string ("input in threadspawn is pthread_t\n");
                                           let result_sigma = SigmaMap.add lvalVinfo (ThreadID(id)) sigma
                                           in 
                                           {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=result_sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node result_sigma tid ls);tid=tid;lockSet=ls}  
                                          )
                                     else (Printf.printf "Unsupported argument in threadspawn\n"; exit 0))
                                 | Some(lvalOption) -> (print_string ("Unsupported argument in threadspawn with "^(CilType.Lval.show lvalOption)^"\n"); exit 0)
      )
    in
    let result_graph = 
      print_string ("In threadspawn, we add the edge "^(LocalTrace.show_edge myEdge)^"\n
    to the graph:"^(LocalTrace.show graph)^"\n"); LocalTrace.extend_by_gEdge graph myEdge
    in
    result_graph

  (* iterate over the graphs in previous state *)
  let threadspawn_fold_graphSet graph (lval, ctx,set_acc) =
    let lastNode = LocalTrace.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTrace.error_node 
      then (print_string ("In threadspawn, we have a trace that does not end in previous node:\n
     "^(LocalTrace.show graph)^"\n"); set_acc) else(
        let result_graph = threadspawn_on_node graph ctx lval lastNode
        in
        D.add result_graph set_acc)
    in
    (lval, ctx, new_set)

  let threadspawn ctx lval f args fctx = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect threadspawn was invoked with ctx.prev_node "^(Node.show ctx.prev_node)^", ctx.edge "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^" and ctx.node "^(Node.show ctx.node)^"\n");
    let _, _, result = D.fold threadspawn_fold_graphSet (ctx.local) (lval, ctx, D.empty())
    in
    result
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)