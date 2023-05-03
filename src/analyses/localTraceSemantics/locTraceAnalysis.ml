open Prelude.Ana
open Analyses
open LocTraceDS
open PriorityCalc
open PostSolvingFlag

(* Custom exceptions for error messaging *)
exception Overflow_addition_Int
exception Underflow_subtraction_Int

(* Constants *)
let intMax = 2147483647
let intMin = -2147483648


(* Analysis framework for local traces *)
module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec
  module D = GraphSet

  module C = Lattice.Unit 

  (* side effect components *)
  module V = SideEffectDomain

  module G = GraphSet

  let context fundec l =
    ()

  let name () = "localTraces"

  (* start state is a set of one empty graph *)
  let startstate v = let g = D.empty () in let tmp = D.add LocTraceGraph.empty g
    in if D.is_empty tmp then tmp else tmp


  let exitstate = startstate

  let add_dependency_from_last_unlock graph mutexVinfo = 
    let lastNode = LocalTraces.get_last_node graph
    in 
    let lastUnlockingNode = LocalTraces.get_last_unlocking_node graph mutexVinfo
    in
    if not (Node.equal lastUnlockingNode.programPoint LocalTraces.error_node)
    then
      (
        (* let allPredecessorEdges = LocalTraces.get_predecessors_edges graph lastNode
           in if (List.exists ( fun (edge: node * CustomEdge.t * node) ->
            match edge with (_, Proc(_, Lval(Var(fvinfo), NoOffset), [AddrOf(Var(fMutex), _)]),_) -> 
              (String.equal fvinfo.vname "pthread_mutex_unlock") && (String.equal mutexVinfo.vname fMutex.vname)
                          | _ ->  false
           ) allPredecessorEdges) 
           then (
           graph
           (* print_string ("Unsupported: For now, lock("^(CilType.Varinfo.show mutexVinfo)^") directly succeeding unlock("^(CilType.Varinfo.show mutexVinfo)^") is not supported"); exit 0 *)
           )
           else ( *)
        (* all checks passed *)
        let depEdge:LocTraceGraph.edge = (lastUnlockingNode,DepMutex (mutexVinfo),lastNode)
        in
        LocalTraces.extend_by_gEdge graph depEdge
        (* ) *)
      )
    else
      (print_string ("Error: graph has no last unlocking node for mutex "^(CilType.Varinfo.show mutexVinfo)^", graph:\n"^(LocalTraces.show graph)^"\n"); exit 0)


  (* functions for join-check *)
  (* symmetric prefix
     This checks whether two graphs are equivalent ending in prefixNode *)
  let is_trace_joinable_symmetric candidate graph prefixNode = 
    let rec inner_loop edgeList =
      match edgeList with (pred_node,_,_)::xs -> if loop pred_node then inner_loop xs else (print_string("loop in inner_loop resulted in false\n"); false)
                        | [] -> true
    and
      loop current_prefix =
      let candidate_pred_edges = LocalTraces.get_predecessors_edges candidate current_prefix
      in let graph_pred_edges = LocalTraces.get_predecessors_edges graph current_prefix
      in
      if not (LocalTraces.equal_edge_lists candidate_pred_edges graph_pred_edges) 
      then (
        false)
      else (
        (LocalTraces.check_no_multiple_depMutexes 
           (LocalTraces.merge_edge_lists (LocalTraces.get_successors_edges candidate current_prefix) (LocalTraces.get_successors_edges graph current_prefix)))
        &&
        (inner_loop candidate_pred_edges))
    in
    loop prefixNode

  (* Checks prefix of two graphs assuming that graph is the creator of candidate *)
  let is_trace_joinable candidate graph creatorTID = 
    let create_node = LocalTraces.find_creating_node (LocalTraces.get_last_node candidate) candidate
    in
    if not (LocalTraces.exists_node graph create_node) then (
      print_string ("create_node does not exists in creator-trace with\ncreate_node="^(NodeImpl.show create_node)^"\ngraph="^(LocalTraces.show graph)^"\n");
      false) else
      (
        is_trace_joinable_symmetric candidate graph create_node
      )

  (* Computes a set of traces that have the same prefix as graph *)
  let rec find_joinable_traces candidates graph creatorTID  = 
    match candidates with
      x::xs -> if is_trace_joinable x graph creatorTID  then x::(find_joinable_traces xs graph creatorTID) else find_joinable_traces xs graph creatorTID
    | [] -> []

  (* helper functions for mutexLock_join*)
  (* Checks whether the locking node of graph is contained in candidate 
     or the unlocking node of candidate is already contained in graph *)
  let check_exists_unlock_lock candidate graph = 
    (LocalTraces.exists_node graph (LocalTraces.get_last_node candidate)) || (LocalTraces.exists_node candidate (LocalTraces.get_last_node graph))

  (* Checks symmetrically whether candidate and graph have the same prefix while determining the prefix node *)
  let check_prefix candidate graph = 
    let prefixNode = LocalTraces.get_recent_divergent_node candidate graph
    in 
    print_string ("in check_prefix, our prefixNode is "^(NodeImpl.show prefixNode)^"\n");
    if Node.equal prefixNode.programPoint LocalTraces.error_node then 
      (print_string "in check_prefix, we got an error_node\n";
       false)
    else
      is_trace_joinable_symmetric candidate graph prefixNode

  (* Checks whether the lock sets are disjoint *)
  let check_compatible_lockSets lastCandidateNode lastGraphNode =
    VarinfoSet.is_empty (VarinfoSet.inter lastCandidateNode.lockSet lastGraphNode.lockSet)

  (* Helper function for merging of graphs wrt the mutex *)
  let mutexLock_join_helper customGraph candidate mutex_vinfo ctxEdge lockingNode prevNode  =
    let lastCandidateNode = LocalTraces.get_last_node candidate
    in
    let lastGraphNode = LocalTraces.get_last_node customGraph
    in
    (* perform all pre-checks for merging *)
    if  
      (not (check_exists_unlock_lock candidate customGraph ))&&
      (check_prefix candidate customGraph )
      &&(check_compatible_lockSets lastCandidateNode lastGraphNode) 
    then (
      print_string "mutexLock_join passed all checks\n";
      let merged_graph = LocalTraces.merge_graphs customGraph candidate
      in
      (* add unlock edge and add dependency edge from candidate end to graph end*)
      let depEdge : node * CustomEdge.t * node = (lastCandidateNode, DepMutex(mutex_vinfo), lockingNode)
      in
      let result_graph = LocalTraces.extend_by_gEdge merged_graph depEdge
      in
      (* let myEdge = (prevNode,(EdgeImpl.convert_edge ctxEdge),lockingNode)
         in *)
      (* perform post-check and reject if merged graph did not pass *)
      if LocalTraces.is_valid_merged_graph result_graph then [result_graph]  
      else (print_string ("result_graph is not valid\nresult_graph:"^(LocalTraces.show result_graph)^"\n"); [])
    )
    else  [] 

  (* Merges all compatible candidates per mutex lock *)
  let mutexLock_join candidates graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} ctxEdge lockingNode mutex_vinfo =
    print_string ("mutexLock_join was invoked with |candidates| = "^(string_of_int (D.cardinal candidates))^"\n");
    let rec loop candidateList graphList =
      match candidateList with candidate::xs -> 
        let result_graph = mutexLock_join_helper graph candidate mutex_vinfo ctxEdge lockingNode {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}
        in
        let result_list = 
          result_graph@graphList
        in
        loop xs result_list
                             | [] -> graphList
    in
    loop (graphSet_to_list candidates) []

  (* evaluates global variables *)
  let rec eval_global var graph node  = 
    let result_node, result_edge = LocalTraces.find_globvar_assign_node var graph node in 
    (match result_edge with 
       (Assign(_, edgeExp)) -> (
         let custom_glob_vinfo = makeVarinfo false "__goblint__traces__custom_nonglobal" (TInt(IInt,[]))
         in
         let tmp_sigma_global,otherValues, _ = eval result_node.sigma custom_glob_vinfo edgeExp graph result_node true (* truth value should not matter here *)
         in 
         let tmp = SigmaMap.find custom_glob_vinfo tmp_sigma_global
         in print_string ("eval_global evaluated to "^(show_varDomain tmp)^"\n");
         (tmp ,true,SigmaMap.empty, otherValues))
     | _ -> Printf.printf "Assignment to global variable was not found\n"; exit 0 )

  and 
    (* Evaluates the effects of an assignment to sigma *)
    eval sigOld vinfo (rval: exp) graph node tv =
    let nopVal sigEnhanced newExp = (Int((Big_int_Z.big_int_of_int (-13)), (Big_int_Z.big_int_of_int (-13)),IInt), false, sigEnhanced, [], newExp)

    (* returns a function which calculates [l1, u1] OP [l2, u2]*)
    in let get_binop_int op ik = 
         if not (CilType.Ikind.equal ik IInt) then (Printf.printf "This type of assignment is not supported in get_binop_int\n"; exit 0) else 
           (match op with 
            | PlusA -> 
              fun x1 x2 -> (match (x1,x2) with 
                    (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0); 
                    (if (Big_int_Z.add_big_int u1 u2 > Big_int_Z.big_int_of_int intMax) then raise Overflow_addition_Int else Int(Big_int_Z.add_big_int l1 l2, Big_int_Z.add_big_int u1 u2, ik))
                  | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

            | MinusA -> 
              fun x1 x2 -> (match (x1,x2) with (* Minus sollte negieren dann addieren sein, sonst inkorrekt!! *)
                    (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0);
                    (let neg_second_lower = Big_int_Z.minus_big_int u2
                     in let neg_second_upper = Big_int_Z.minus_big_int l2
                     in print_string("get_binop_int with MinusA: l1="^(Big_int_Z.string_of_big_int l1)^", u1="^(Big_int_Z.string_of_big_int u1)^", neg_second_lower="^(Big_int_Z.string_of_big_int neg_second_lower)^", neg_second_upper="^(Big_int_Z.string_of_big_int neg_second_upper)^"\n"); 
                     if (Big_int_Z.add_big_int l1 neg_second_lower < Big_int_Z.big_int_of_int intMin) then raise Underflow_subtraction_Int else Int(Big_int_Z.add_big_int l1 neg_second_lower, Big_int_Z.add_big_int u1 neg_second_upper, ik))
                  | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

            | Lt -> 
              fun x1 x2 -> (match (x1,x2) with 
                  | (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0);
                    (if Big_int_Z.lt_big_int u1 l2 then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, ik) )
                     else if Big_int_Z.le_big_int u2 l1 then (Int(Big_int_Z.zero_big_int,Big_int_Z.zero_big_int, ik))
                     else (print_string "Overlapping Lt is not supported\n"; exit 1)
                    )

                  | _,_ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0 )
            (* | Div -> fun x1 x2 -> (match (x1,x2) with 
               (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if l2 <= Big_int_Z.zero_big_int && u2 >= Big_int_Z.zero_big_int then raise Division_by_zero_Int else (Printf.printf "This type of assignment is not supported - as I do not allow Division yet\n"; exit 0)
               | _,_ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0) *)
            | _ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0)

    in
    let rec eval_helper subexp currentSigEnhanced =
      (match subexp with

       | Const(CInt(c, ik, s)) -> (match ik with
           | IInt -> if c < Big_int_Z.big_int_of_int intMin 
             then (Int (Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int intMin, ik), true,currentSigEnhanced, [], Const(CInt(c, ik, s))) 
             else if c > Big_int_Z.big_int_of_int intMax 
             then (Int (Big_int_Z.big_int_of_int intMax,Big_int_Z.big_int_of_int intMax, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s))) 
             else (Int (c,c, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s)))
           | IUInt -> if c < Big_int_Z.zero_big_int 
             then (Int (Big_int_Z.zero_big_int,Big_int_Z.zero_big_int, ik), true,currentSigEnhanced, [], Const(CInt(c, ik, s))) 
             else if c > Big_int_Z.big_int_of_int intMax 
             then (Int (Big_int_Z.big_int_of_int intMax,Big_int_Z.big_int_of_int intMax, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s))) 
             else (Int (c,c, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s)))
           | IShort -> print_string "in eval_helper Const, type IShort is not supported. But we continue with evaluation because this could be some pthread.h initializations\n";
             (Int (c,c, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s)))
           | _ ->  print_string ("This type of assignment is not supported in eval_helper Const, ik="^(CilType.Ikind.show ik)^"\n"); exit 0 )

       (* The only case where I need to evaluate a global *)
       | Lval(Var(var), NoOffset) -> if var.vglob = true 
         then (
           let localGlobalVinfo = (customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype)
           in
           if SigmaMap.mem localGlobalVinfo sigOld then
             (

               match SigmaMap.find localGlobalVinfo sigOld with
               | Int(l,u,k) -> if l = u then (Int(l,u,k), true, currentSigEnhanced, [], Lval(Var(localGlobalVinfo), NoOffset)) else 
                   (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (currentSigEnhanced),[], Lval(Var(localGlobalVinfo), NoOffset))
               | rest -> (rest, true,currentSigEnhanced,[], Lval(Var(localGlobalVinfo), NoOffset)) 
             )
           else
             (
               let a, b, c, d = eval_global var graph node
               in (a, b, c, d, Lval(Var(localGlobalVinfo), NoOffset)))
         )
         else
           (if SigmaMap.mem var sigOld then (
               match SigmaMap.find var sigOld with
               | Int(l,u,k) -> if l = u then (Int(l,u,k), true, currentSigEnhanced, [], Lval(Var(var), NoOffset)) else 
                   (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (currentSigEnhanced),[], Lval(Var(var), NoOffset))
               | rest -> (rest, true,currentSigEnhanced,[], Lval(Var(var), NoOffset)) 
             )
            else if SigmaMap.mem var currentSigEnhanced then (
              match SigmaMap.find var currentSigEnhanced with
              | Int(l,u,k) -> if l = u then (Int(l,u,k), true,currentSigEnhanced, [], Lval(Var(var), NoOffset)) else 
                  (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (currentSigEnhanced),[], Lval(Var(var), NoOffset))
              | rest -> (rest, true,currentSigEnhanced,[], Lval(Var(var), NoOffset)) 
            )
            else (print_string ("var="^(CilType.Varinfo.show var)^" not found in sigOld="^(NodeImpl.show_sigma sigOld)^"\nThis means we choose a value for this trace\n");
                if not tv then (
                  (Int(Big_int_Z.zero_big_int, Big_int_Z.zero_big_int, IInt), true, SigmaMap.add var (Int(Big_int_Z.zero_big_int, Big_int_Z.zero_big_int, IInt)) currentSigEnhanced,
                [], Lval(Var(var), NoOffset))
                )
                else
                  let randomNr = randomIntGenerator#getRandomValue (LocalTraces.hash graph) var 
                  in
                  let randomVd = Int((Big_int_Z.big_int_of_int (randomNr)), (Big_int_Z.big_int_of_int (randomNr)),IInt)
                  in
                  (randomVd, true, SigmaMap.add var randomVd (currentSigEnhanced), [(var, (Int((Big_int_Z.big_int_of_int (randomNr+1)), (Big_int_Z.big_int_of_int (randomNr+1)),IInt)));
                                                                                    (var, (Int((Big_int_Z.big_int_of_int (randomNr+2)), (Big_int_Z.big_int_of_int (randomNr+2)),IInt)));
                                                                                    (var, (Int((Big_int_Z.big_int_of_int (randomNr+3)), (Big_int_Z.big_int_of_int (randomNr+3)),IInt)))
                                                                                   ],
                   Lval(Var(var), NoOffset)
                  )))

       (* | AddrOf (Var(v), NoOffset) ->  if v.vglob 
          then (Address(v), true, currentSigEnhanced, [], AddrOf (Var(customVinfoStore#getLocalVarinfo (make_local_global_varinfo v) v.vtype), NoOffset)) 
          else (Address(v), true, currentSigEnhanced, [], AddrOf (Var(v), NoOffset)) *)

       (* unop expressions *)
       (* for type Integer *)
       | UnOp(Neg, unopExp, TInt(unopIk, attr)) -> 
         (match eval_helper unopExp currentSigEnhanced with (Int(l,u,ik), true, sigEnhanced, otherValues, newUnopExp) ->
            (match ik with 
             |IInt -> 
               if CilType.Ikind.equal unopIk IInt then( 
                 let negLowerBound = (if Big_int_Z.minus_big_int u < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
                                      else if Big_int_Z.minus_big_int u > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int u )
                 in
                 let negUpperBound = (if Big_int_Z.minus_big_int l < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
                                      else if Big_int_Z.minus_big_int l > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int l )
                 in
                 (Int (negLowerBound, negUpperBound, unopIk), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, UnOp(Neg, newUnopExp, TInt(unopIk, attr)))) 
               else (Printf.printf "This type of assignment is not supported in eval_helper UnOp\n"; exit 0)
             | _ -> Printf.printf "This type of assignment is not supported in eval_helper UnOp\n"; exit 0)
                                                          |(_, false, sigEnhanced, _, newUnopExp) -> print_string "nopVal created at unop Neg for Int\n";
                                                            nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) (UnOp(Neg, newUnopExp, TInt(unopIk, attr)))
                                                          |(_, _,_,_, _) -> Printf.printf "This type of assignment is not supported in eval_helper in UnOp\n"; exit 0) 

       (* binop expressions *)
       (* Lt could be a special case since it has enhancements on sigma *)
       (* in var1 < var2 case, I have not yet managed boundary cases, so here are definitely some bugs *)
       | BinOp(Lt, Lval(Var(var1), NoOffset),Lval(Var(var2), NoOffset),TInt(biopIk, attr)) ->(
           let newVar1 = if var1.vglob then customVinfoStore#getLocalVarinfo (make_local_global_varinfo var1) var1.vtype else var1
           in
           let newVar2 = if var2.vglob then customVinfoStore#getLocalVarinfo (make_local_global_varinfo var2) var2.vtype else var2
           in
           let newExpr = BinOp(Lt, Lval(Var(newVar1), NoOffset),Lval(Var(newVar2), NoOffset),TInt(biopIk, attr)) 
           in
           (* If one of var is global and the corresponding newVar is not in sigma, then this is not intended *)
           if ((var1.vglob) && (not (SigmaMap.mem newVar1 sigOld))) || ((var2.vglob) && (not (SigmaMap.mem newVar2 sigOld))) 
           then (print_string ("Error: there is a global in expression 'var1 < var2' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0)
           else if ((SigmaMap.mem newVar1 sigOld))&&((SigmaMap.mem newVar2 sigOld))
           then (
             let vd1 = (SigmaMap.find newVar1 sigOld)
             in 
             let vd2 = (SigmaMap.find newVar2 sigOld)
             in
             match vd1,vd2 with
             | (Int(l1,u1,k1)), (Int(l2,u2,k2)) -> 
               if not (CilType.Ikind.equal k1 k2) then (Printf.printf "This type of assignment is not supported in eval_helper in BinOp(var, var)\n"; exit 0);
               if (u1 < l2) || (u2 <= l1) then ((get_binop_int Lt biopIk) (Int(l1, u1, k1)) (Int(l2, u2, k2)), true , currentSigEnhanced,[], newExpr) 
               else
                 (* overlap split *)
                 (print_string "Overlapping intervals are not supported."; exit 0)
             | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, var)\n"; exit 0
           )
           else if SigmaMap.mem newVar1 sigOld then
             (print_string "nopVal created at binop Lt of two variables. second is unknown\n";
              match SigmaMap.find newVar1 sigOld with 
              | Int(l1,u1,k1) -> if tv then (
                  (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true, SigmaMap.add newVar2 (Int(Big_int_Z.add_int_big_int 1 u1,Big_int_Z.big_int_of_int intMax, k1)) currentSigEnhanced, [], newExpr)
                )
                else (
                  (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k1), true, SigmaMap.add newVar2 (Int(Big_int_Z.big_int_of_int intMin,l1, k1)) currentSigEnhanced, [], newExpr)
                )
              | _ -> print_string "In expresion v1 < v2, v1 has unexpected type"; exit 0
             )
           else if SigmaMap.mem newVar2 sigOld then
             (print_string "nopVal created at binop Lt of two variables. first is unknown\n";
              match SigmaMap.find newVar2 sigOld with 
              | Int(l2,u2,k2) -> if tv then (
                  (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k2), true, SigmaMap.add newVar1 (Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.sub_big_int l2 (Big_int_Z.big_int_of_int 1), k2)) currentSigEnhanced, [], newExpr)
                )
                else (
                  (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k2), true, SigmaMap.add newVar1 (Int(u2,Big_int_Z.big_int_of_int (intMax), k2)) currentSigEnhanced, [], newExpr)
                )
              | _ -> print_string "In expresion v1 < v2, v1 has unexpected type"; exit 0
             )
           else            
             (print_string "nopVal created at binop Lt of two variables. both are unknown\n";
              if tv then (
                let newOtherValues = [(newVar1, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int (-1), IInt));
                                      (newVar1, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int (-2), IInt));
                                      (newVar1, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int 1, IInt));
                                      (newVar1, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int 2, IInt))
                                     ]
                in 
                (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, IInt), true, SigmaMap.add newVar2 (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int intMax, IInt)) (SigmaMap.add newVar1 (Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int 0, IInt)) currentSigEnhanced), newOtherValues, newExpr)
              )
              else(
                let newOtherValues = [(newVar2, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int (-1), IInt));
                                      (newVar2, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int (-2), IInt));
                                      (newVar2, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int 1, IInt));
                                      (newVar2, Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int 2, IInt))
                                     ]
                in 
                (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, IInt), true, SigmaMap.add newVar2 (Int(Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int 0, IInt)) (SigmaMap.add newVar1 (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int intMax, IInt)) currentSigEnhanced), newOtherValues, newExpr)
              )
             )
         )

       | BinOp(Lt, binopExp1,Lval(Var(var), NoOffset),TInt(biopIk, attr)) ->(
           let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype else var
           in
           if ((var.vglob) && (not (SigmaMap.mem newVar sigOld))) 
           then (print_string ("Error: there is a global in expression 'expr < var' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
           match eval_helper binopExp1 currentSigEnhanced with 
           | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp1) -> (
               let newExpr = BinOp(Lt, newBinOpExp1,Lval(Var(newVar), NoOffset),TInt(biopIk, attr))
               in
               if SigmaMap.mem newVar sigEnhanced then
                 (match SigmaMap.find newVar sigEnhanced with Int(lVar, uVar, kVar) -> 
                    if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
                    if uVar <= l || u < lVar 
                    then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
                    else (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
                                                            | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
               else if SigmaMap.mem newVar sigOld then
                 (match SigmaMap.find newVar sigOld with Int(lVar, uVar, kVar) -> 
                    if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
                    if uVar <= l || u < lVar 
                    then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
                    else (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
                                                       | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
               else ( 
                 if var.vglob then (print_string("Error: there is a global in expression 'exp < var' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
                 if tv then
                   let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(Big_int_Z.add_big_int u (Big_int_Z.big_int_of_int 1), Big_int_Z.big_int_of_int intMax, k)) (SigmaMap.empty))
                   in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
                 else 
                   let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(Big_int_Z.big_int_of_int intMin, u, k)) (SigmaMap.empty))
                   in (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
               )
             )
           | (_,false, sigEnhanced,_, newBinOpExp1) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newBinOpExp1
           | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0
         )

       | BinOp(Lt, Lval(Var(var), NoOffset), binopExp2,TInt(biopIk, attr)) -> (
           let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype else var
           in
           if ((var.vglob) && (not (SigmaMap.mem newVar sigOld))) 
           then (print_string ("Error: there is a global in expression 'var < expr' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
           match eval_helper binopExp2 currentSigEnhanced with 
           | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp2) -> (
               let newExpr = BinOp(Lt, Lval(Var(newVar), NoOffset), newBinOpExp2,TInt(biopIk, attr))
               in
               if SigmaMap.mem newVar sigEnhanced then 
                 (match SigmaMap.find newVar sigEnhanced with Int(lVar, uVar, kVar) -> 
                    if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), unequal Ikind in sigEnhanced\n"; exit 0);
                    if uVar < l || u <= lVar 
                    then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
                    else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
                                                            | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), var is not an Integer in sigEnhanced\n"; exit 0)
               else if SigmaMap.mem newVar sigOld then
                 (match SigmaMap.find newVar sigOld with Int(lVar, uVar, kVar) -> 
                    if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), unequal IKind in sigOld\n"; exit 0);
                    if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
                    else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
                                                       | other -> print_string ("This type of assignment is not supported in eval_helper BinOp(var, exp), var="^(CilType.Varinfo.show newVar)^" is not an Integer in sigOld:"^(show_varDomain other)^"\n"); exit 0)
               else ( 
                 if var.vglob then (print_string("Error: there is a global in expression 'var < exp' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
                 if tv then
                   let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(Big_int_Z.big_int_of_int intMin, Big_int_Z.sub_big_int l (Big_int_Z.big_int_of_int 1), k)) (SigmaMap.empty))
                   in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
                 else 
                   let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(u, Big_int_Z.big_int_of_int intMax, k )) (SigmaMap.empty))
                   in (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
               ))
           | (_,false, sigEnhanced, _, newExp) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newExp
           | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), exp does not evaluate properly\n"; exit 0)

       (* for type Integer *)
       | BinOp(op, binopExp1, binopExp2,TInt(biopIk, attr)) ->
         let (value1, success1, sigEnhanced1, otherValues1, newBinOpExp1) = eval_helper binopExp1 currentSigEnhanced
         in
         let mergedEnhancedSigma = (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced1)
         in
         (* print_string ("in BinOp(exp, exp) we have sigEnhanced1=["^(NodeImpl.show_sigma sigEnhanced1)^"]\n
            and mergedEnhancedSigma=["^(NodeImpl.show_sigma mergedEnhancedSigma)^"]\n"); *)
         (match ((value1, success1, sigEnhanced1, otherValues1), eval_helper binopExp2 mergedEnhancedSigma) with 
          | ((Int(l1,u1, ik1), true,sigEnhanced1,otherValues1),(Int(l2,u2, ik2), true,sigEnhanced2,otherValues2, newBinOpExp2)) -> 
            let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr))
            in
            if CilType.Ikind.equal ik1 ik2 then 
              ((get_binop_int op biopIk) (Int(l1,u1, ik1)) (Int(l2,u2, ik2)), true, NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2, otherValues1@otherValues2, newExpr) 
            else (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, exp)\n"; exit 0)
          | ((_,_,sigEnhanced1,_), (_,false, sigEnhanced2,_, newBinOpExp2)) -> 
            let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr))
            in
            print_string "nopVal created at binop for Integer 1\n";nopVal (NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2) newExpr
          | ((_,false, sigEnhanced1,_), (_,_,sigEnhanced2,_, newBinOpExp2)) -> 
            let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr))
            in
            print_string "nopVal created at binop for Integer 2\n";nopVal (NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2) newExpr
          |(_, _) -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, exp)\n"; exit 0) 

       | otherExp -> print_string ("This type of assignment is not supported in eval_helper, subexp="^(CilType.Exp.show subexp)^". 
Evaluation continues because this could be some pthread.h initializations\n"); nopVal currentSigEnhanced otherExp)
      (* sigNew will collect all enhancements and then we need to apply sigOld (+) sigEnhancedEffects *)
    in let (result,success,sigEnhancedEffects, otherValues, newExpr) = eval_helper rval SigmaMap.empty
    in
    (* otherValues needs to be propagated still, e.g. global = x < 3 still poses new information on x *)
    let sigNew = NodeImpl.destruct_add_sigma sigOld sigEnhancedEffects
    in if vinfo.vglob = true then (SigmaMap.remove vinfo sigNew, otherValues, newExpr) else
    if success then (SigmaMap.add vinfo result sigNew, otherValues, newExpr)  else (print_string "Eval could not evaluate expression\n"; exit 0)

  (* Catches exception in eval function and produces warnings *)
  let eval_catch_exceptions sigOld vinfo rval graph node tv =
    try (eval sigOld vinfo rval graph node tv, true) with 
    | Overflow_addition_Int -> Messages.warn "Contains a trace with overflow of Integer addition";
      omitPostSolving#setFlag ();
      ((SigmaMap.add vinfo Error sigOld, [], rval) ,false)
    | Underflow_subtraction_Int -> Messages.warn "Contains a trace with underflow of Integer subtraction";
      omitPostSolving#setFlag ();
      ((SigmaMap.add vinfo Error sigOld, [], rval) ,false)

  (* Manages other generated values and return a set of sigmas *)
  let eval_wrapper sigOld vinfo rval graph node tv =
    let rec iter_otherValues doneValues workList sigmaList =
      match workList with 
        (var,vd)::xs -> if List.mem (var,vd) doneValues then iter_otherValues doneValues xs sigmaList
        else (let sigTmp = SigmaMap.add var vd sigOld
              in 
              let (anotherSig, moreValues, _), success_other = eval_catch_exceptions sigTmp vinfo rval graph node tv
              in 
              let newWorkList = List.fold (fun acc value -> if List.mem value acc then acc else value::acc) workList moreValues
              in iter_otherValues ((var,vd)::doneValues) newWorkList (if (List.exists (fun sigma_exists -> NodeImpl.equal_sigma sigma_exists anotherSig) sigmaList) then sigmaList else (anotherSig::sigmaList)))
      | [] -> sigmaList
    in
    let (sigNew,otherValues, newExpr), success = eval_catch_exceptions sigOld vinfo rval graph node tv
    in 
    let allSigmas = iter_otherValues [] otherValues [sigNew]
    in
    allSigmas, success, newExpr

  (* Collects all varinfos of globals in an expression *)
  let rec get_all_globals (expr:exp) (acc:VarinfoSet.t) =
    match expr with 
    | Const(CInt(c, ik, _)) -> acc
    | Lval(Var(var), NoOffset) -> if var.vglob then VarinfoSet.add var acc else acc
    | AddrOf (Var(v), NoOffset) -> if v.vglob then VarinfoSet.add v acc else acc
    | UnOp(Neg, unopExp, TInt(unopIk, _)) -> get_all_globals unopExp acc
    | BinOp(PlusA, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
    | BinOp(MinusA, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
    | BinOp(Lt, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
    | BinOp(Div, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
    | _ -> acc

  (* Reoves all custom locals for globals from the sigma *)
  let rec remove_global_locals_sigma sigma globalList =
    match globalList with x::xs -> remove_global_locals_sigma (SigmaMap.remove (customVinfoStore#getGlobalVarinfo (make_local_global_varinfo x)) sigma) xs
                        | [] -> sigma

  (* Creates a sequence of assignments where custom local variables are generated.
     These read the value of the corresponding global *)
  let rec create_local_assignments graphList globals ctx =
    let rec loop global graphList acc =
      match graphList with 
      |  graph::gs ->
        let lastNode = LocalTraces.get_last_node graph
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
        let firstNode = LocalTraces.get_first_node graph
        in
        let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(customMutex),lockedNode)
        in
        let lockedGraph = LocalTraces.extend_by_gEdge graph lockingEdge
        in 
        let firstLockedGraph = if (NodeImpl.equal firstNode lastNode) then lockedGraph
          else if (LocalTraces.exists_unlock_mutex graph customMutex)
          then add_dependency_from_last_unlock lockedGraph customMutex 
          else if not (LocalTraces.exists_lock_mutex graph customMutex) then  LocalTraces.extend_by_gEdge lockedGraph firstLockEdge
          else lockedGraph (* This case happens if lock(m) is executed consecutively. Should I support that? *)
        in
        print_string ("firstLockedGraph="^(LocalTraces.show firstLockedGraph)^"\n");
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
                 let assignedGraph = LocalTraces.extend_by_gEdge lockedGraph assigningEdge
                 in
                 let unlockingLabel:CustomEdge.t = Proc(None, Lval(Var(unlockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
                 in
                 let unlockedNode = {programPoint=ctx.prev_node;sigma=evaluated;id=(idGenerator#getID assignedNode unlockingLabel ctx.prev_node evaluated lastNode.tid lastNode.lockSet);tid=lastNode.tid;lockSet=lastNode.lockSet}
                 in
                 let unlockingEdge = (assignedNode, unlockingLabel, unlockedNode)
                 in
                 let unlockingGraph = LocalTraces.extend_by_gEdge assignedGraph unlockingEdge
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
               let lastNode = LocalTraces.get_last_node graph_outter
               in
               let sigmaList,success_inner, newExp = eval_wrapper lastNode.sigma x rval  graph_outter lastNode true in 
               if not success_inner then (print_string "assignment did not succeed!\n"; 
                                          [LocalTraces.extend_by_gEdge graph_outter (lastNode,Assign(lval, newExp),{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty})] )
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
                        let lockedGraph = LocalTraces.extend_by_gEdge graph_outter lockingEdge  
                        in
                        let myTmp:V.t = Mutex(customMutex)
                        in
                        let allUnlockingTraces = ctx.global myTmp
                        in 
                        let firstNode = LocalTraces.get_first_node graph_outter
                        in
                        print_string ("In assign, firstNode is "^(NodeImpl.show firstNode)^" for graph_outter "^(LocalTraces.show graph_outter)^"\n");
                        let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(customMutex),lockedNode)
                        in
                        let firstLockedGraph = if (NodeImpl.equal firstNode lastNode) then lockedGraph
                          else if (LocalTraces.exists_unlock_mutex graph_outter customMutex)
                          then add_dependency_from_last_unlock lockedGraph customMutex 
                          else if not (LocalTraces.exists_lock_mutex graph_outter customMutex) then LocalTraces.extend_by_gEdge lockedGraph firstLockEdge
                          else lockedGraph
                        in 
                        print_string ("firstLockedGraph="^(LocalTraces.show firstLockedGraph)^"\n");
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
                            let assignedGraph = LocalTraces.extend_by_gEdge lockedGraph assigningEdge
                            in
                            let unlockingLabel:CustomEdge.t = Proc(None, Lval(Var(unlockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
                            in
                            let unlockedNode = {programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID assignedNode unlockingLabel ctx.node newSigma tid ls);tid=tid;lockSet=ls}
                            in
                            let unlockingEdge = (assignedNode, unlockingLabel, unlockedNode)
                            in
                            let unlockingGraph = LocalTraces.extend_by_gEdge assignedGraph unlockingEdge
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
                        print_string ("assignment succeeded so we add the edge "^(LocalTraces.show_edge myEdge)^"\n");
                        (LocalTraces.extend_by_gEdge graph_outter myEdge)::outerGraphList )
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
      else LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      (if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In assign, we have a trace that does not end in previous node:
    "^(LocalTraces.show graph)^"\n"); set_acc) else 
         let result_graphList = 
           assign_on_node (if LocTraceGraph.is_empty graph then (LocTraceGraph.add_vertex graph lastNode) else graph) ctx lval rval lastNode
         in
         List.fold (fun set_fold result_graph -> if (LocalTraces.equal graph result_graph)  then set_acc else D.add result_graph set_fold) set_acc result_graphList
      )
    in
    (lval, rval, ctx, new_set)

  let assign ctx (lval:lval) (rval:exp) : D.t = 
    predominatorRegistration#update ctx.prev_node ctx.node;
    print_string ("Edge effect assign was invoked with lval "^(CilType.Lval.show lval)^" and rval "^(CilType.Exp.show rval)^" and ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^"\n");
    print_string("in assign, ctx.local=["^(D.fold (fun graph acc -> (LocalTraces.show graph)^"\n"^acc) ctx.local "")^"]\n");
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
          let lastNode = LocalTraces.get_last_node graph_outter
          in
          let branch_sigma = SigmaMap.add LocalTraces.branch_vinfo Error lastNode.sigma 
          in
          let sigmaList,success, newExp = eval_wrapper branch_sigma LocalTraces.branch_vinfo exp graph_outter lastNode tv
          in
          List.map (fun sigma_map ->
              let result_as_int = match (SigmaMap.find_default Error LocalTraces.branch_vinfo sigma_map) with
                  Int(i1,i2,_) -> print_string ("in branch, the result is ["^(Big_int_Z.string_of_big_int i1)^";"^(Big_int_Z.string_of_big_int i2)^"]");
                  if (Big_int_Z.int_of_big_int i1 == 0)&&(Big_int_Z.int_of_big_int i2 == 0) then 0 
                  else if (Big_int_Z.int_of_big_int i1 > 0)||(Big_int_Z.int_of_big_int i2 < 0) then 1
                  else -1
                |_ -> -1
              in
              let sigmaNew = remove_global_locals_sigma (SigmaMap.remove LocalTraces.branch_vinfo (NodeImpl.destruct_add_sigma lastNode.sigma sigma_map)) (VarinfoSet.to_list rvalGlobals)
              in
              print_string ("result_as_int: "^(string_of_int result_as_int)^"\n");
              let myEdge:node*CustomEdge.t*node = (lastNode, Test(newExp, tv),
                                                   {programPoint=ctx.node;sigma=sigmaNew;id=(idGenerator#getID lastNode (Test(newExp, tv)) ctx.node sigmaNew tid ls);tid=tid;lockSet=ls})
              in
              print_string ("success="^(string_of_bool success)^", tv="^(string_of_bool tv)^", result_as_int="^(string_of_int result_as_int)^"\nand possible edge="^(LocalTraces.show_edge myEdge)^"\n");
              let result_graph = if success&&((tv=true && result_as_int = 1)||(tv=false&&result_as_int=0)) 
                then LocalTraces.extend_by_gEdge graph_outter myEdge else (print_string "no edge added for current sigma in branch\n";graph)
              in
              result_graph
            ) sigmaList
        )@resultList_outter
      ) [] someTmp

  (* iterate over the graphs in previous state *)
  let branch_fold_graphSet graph (exp, tv, ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set =
      (if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In branch, we have a trace that does not end in previous node:
  "^(LocalTraces.show graph)^"\n"); set_acc) else
         let result_graphList = 
           branch_on_node graph ctx exp tv lastNode
         in
         List.fold (fun set_fold result_graph -> if (LocalTraces.equal graph result_graph) then set_fold else D.add result_graph set_fold) set_acc result_graphList
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
    let result_graph = LocalTraces.extend_by_gEdge graph myEdge
    in
    result_graph

  (* iterate over the graphs in previous state *)
  let body_fold_graphSet graph (ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = (if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In body, we have a trace that does not end in previous node:
    "^(LocalTraces.show graph)^"\n");if LocTraceGraph.is_empty graph 
                                                                                    then(
                                                                                      let first_ID = idGenerator#increment()
                                                                                      in
                                                                                      let second_ID = idGenerator#increment()
                                                                                      in
                                                                                      tidRecord#addTID first_ID;
                                                                                      D.add (LocalTraces.extend_by_gEdge graph ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id= first_ID;tid=first_ID;lockSet=VarinfoSet.empty},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=SigmaMap.empty; id= second_ID;tid=first_ID;lockSet=VarinfoSet.empty})) set_acc)
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
    print_string ("Resulting state of body: "^(D.fold (fun foldGraph acc -> (LocalTraces.show foldGraph)^", "^acc) result "")^"\n");result

  (* RETURN helper functions *)
  (* perform return-effect on given node *)
  let return_on_node graph ctx exp {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} (f:fundec) =
    let result_graphList =
      match exp with 
      | None ->  let myEdge =  print_string "In return case None\n";
                   ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
        in
        let result_graph =LocalTraces.extend_by_gEdge graph myEdge
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
                                let result_graph = LocalTraces.extend_by_gEdge graph myEdge
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
                                      let lastNode = LocalTraces.get_last_node graph_outter
                                      in  
                                      let resultList, success, newExp = eval_wrapper lastNode.sigma LocalTraces.return_vinfo ret_exp graph_outter lastNode true
                                      in
                                      List.map (fun sigma_map -> 
                                          let newSigma = remove_global_locals_sigma sigma_map (VarinfoSet.to_list rvalGlobals)
                                          in
                                          let myEdge: node*CustomEdge.t*node =
                                            if success = false then (print_string "Error: Evaluation of return expression was unsuccessful\n"; exit 0)
                                            else (lastNode,Ret(Some(newExp),f),{programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID lastNode (Ret(Some(newExp),f)) ctx.node newSigma tid ls);tid=tid;lockSet=ls})
                                          in
                                          let result_graph =
                                            LocalTraces.extend_by_gEdge graph_outter myEdge
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
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In return, we have a trace that does not end in previous node:
  "^(LocalTraces.show graph)^"\n"); set_acc) else
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
        print_string("in special, Lock, allUnlockingTraces= ["^(D.fold (fun graph_fold s_fold -> s_fold^(LocalTraces.show graph_fold)^"\n") allUnlockingTraces "")^"]\n");
        let firstNode = LocalTraces.get_first_node graph
        in
        print_string ("In special, firstNode is "^(NodeImpl.show firstNode)^"\n");
        let lockedNode = {programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid (VarinfoSet.add mutex_vinfo ls));tid=tid;lockSet=(VarinfoSet.add mutex_vinfo ls)}
        in
        let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(mutex_vinfo),lockedNode)
        in
        let lockingEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),lockedNode)
        in
        let lockedGraph = LocalTraces.extend_by_gEdge graph lockingEdge
        in
        (* First lock is only added, if this is the first lock *)
        let firstLockedGraph = if LocalTraces.exists_unlock_mutex graph mutex_vinfo then
            add_dependency_from_last_unlock lockedGraph mutex_vinfo 
          else if not (LocalTraces.exists_lock_mutex graph mutex_vinfo) then 
            ( 
              LocalTraces.extend_by_gEdge lockedGraph firstLockEdge)
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
        let result_graph = LocalTraces.extend_by_gEdge graph myEdge
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
                       if LocalTraces.is_already_joined tidJoin graph then (
                         Messages.warn "ThreadJoin on already joined Thread-ID";
                         omitPostSolving#setFlag ();
                         print_string ("ThreadJoin did not succeed due to already joined TID for graph: \n"^(LocalTraces.show graph)^"\nand tidJoin: "^(string_of_int tidJoin)^"\n");
                         let graph_error_edge = LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty}) 
                         in
                         graph_error_edge::graphList
                       )
                       else 
                         let myTmp:V.t = ThreadID(tidJoin)
                         in
                         let endingTraces = graphSet_to_list (ctx.global myTmp)
                         in
                         print_string("in special, ending traces: ["^(List.fold (fun acc g -> acc^(LocalTraces.show g)) "" endingTraces)^"]\n");
                         let joinableTraces = find_joinable_traces endingTraces graph tid
                         in
                         if not (tidRecord#existsTID tidJoin) then (
                           Messages.warn "ThreadJoin on non-existent Thread-ID";
                           omitPostSolving#setFlag ();
                           let graph_error_edge = LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty}) 
                           in
                           print_string("In ThreadJoin, we add an error-trace with endingTraces:\n
["^(List.fold (fun s_fold graph_fold -> (LocalTraces.show graph_fold)^";\n"^s_fold) "" endingTraces)^"]\n");
                           graph_error_edge::graphList
                         )
                         else if List.is_empty joinableTraces then 
                           (* we cannot join *)
                           graphList
                         else 
                           (
                             print_string ("in special, joinable traces for tidJoin "^(string_of_int tidJoin)^" are: ["^(List.fold (fun acc g -> acc^(LocalTraces.show g)) "" joinableTraces)^"]\n");
                             List.fold (
                               fun list_fold trace_fold -> let tmp_graph = LocalTraces.merge_graphs graph trace_fold
                                 in
                                 let newVarinfoSet = VarinfoSet.union ls (LocalTraces.get_last_node trace_fold).lockSet
                                 in
                                 let destination_node = {programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid newVarinfoSet);tid=tid;lockSet=newVarinfoSet}
                                 in
                                 print_string ("In ThreadJoin, destination_node="^(NodeImpl.show destination_node)^"\n");
                                 let tmp_graph_edge1 = LocalTraces.extend_by_gEdge tmp_graph (LocalTraces.get_last_node trace_fold, EdgeImpl.convert_edge ctx.edge,destination_node)
                                 in
                                 let tmp_graph_edge2 = LocalTraces.extend_by_gEdge tmp_graph_edge1 ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,destination_node)
                                 in
                                 print_string ("tmp_graph_edge2 = "^(LocalTraces.show tmp_graph_edge2)^"\n");
                                 ((tmp_graph_edge2)::list_fold)
                             ) [] joinableTraces)@graphList
                     ) [] tidSigmaList)
      in 
      print_string("Before ThreadJoin, we had:\n
"^(LocalTraces.show graph)^"\n
and after, we have:
"^(List.fold (fun s_fold g_fold -> (LocalTraces.show g_fold)^";\n"^s_fold) "" myTmp)^" \n");
      myTmp

    | ThreadCreate {thread = tidExp;start_routine=start_routine;arg=arg_create}, _ ->  print_string ("We found a pthread_create in special\n"); [graph]

    | ThreadExit _, _ -> print_string "In special, I reached ThreadExit\n"; 
      let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
      in
      let result_graph = LocalTraces.extend_by_gEdge graph myEdge
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
                     let graph_error_edge = LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty})
                     in
                     [graph_error_edge]) 
              else (let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
                    in
                    let result_graph = LocalTraces.extend_by_gEdge graph myEdge
                    in
                    [result_graph])
                             | _ -> Printf.printf "Error: wrong amount of arguments for pthread_mutex_destroy in special\n"; exit 0)

      else if String.equal f.vname "rand" then (
        match lval with 
        | Some(Var(var), NoOffset) ->
          let randomValue = Big_int_Z.big_int_of_int (randomIntGenerator#getRandomValueFullCInt (LocalTraces.hash graph) var )
          in
          let newSigma = SigmaMap.add var (Int(randomValue, randomValue, IInt)) sigma
          in
          let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,
                        {programPoint=ctx.node;sigma=newSigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid ls);tid=tid;lockSet=ls})
          in
          let result_graph = LocalTraces.extend_by_gEdge graph myEdge
          in
          [result_graph]
        | _ -> print_string "In special, lval for rand is not suitable\n"; exit 0
      )
      else
        (print_string ("This edge is not one of my considered special functions\n"); 
         let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls})
         in
         let result_graph = LocalTraces.extend_by_gEdge graph myEdge
         in
         [result_graph])

  (* iterate over the graphs in previous state *)
  let special_fold_graphSet graph (lval, f, arglist, ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set =
      if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In special, we have a trace that does not end in previous node:
    "^(LocalTraces.show graph)^"\n"); set_acc) else 
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
    (* print_string ("someTmp: "^(List.fold (fun s_acc graph_fold -> (LocalTraces.show graph_fold)^"; "^s_acc) "" someTmp)^"\n"); *)
    let outterResultList = List.fold ( fun resultList_outter graph_outter ->
        (
          let lastNode = LocalTraces.get_last_node graph_outter
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
              LocalTraces.extend_by_gEdge graph_outter myEdge
            ) sigma_formalList)@resultList_outter
      ) [] someTmp
    in
    outterResultList



  (* iterate over the graphs in previous state *)
  let enter_fold_graphSet graph (f, args, ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In enter, we have a trace that does not end in previous node:
  "^(LocalTraces.show graph)^"\n"); set_acc) else
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
      [ctx.local, D.add (LocalTraces.extend_by_gEdge (LocTraceGraph.empty) ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id=first_ID;tid=first_ID;lockSet=VarinfoSet.empty}, (EdgeImpl.convert_edge ctx.edge), {programPoint=(FunctionEntry(f));sigma=SigmaMap.empty;id= second_ID;tid=first_ID;lockSet=VarinfoSet.empty})) (D.empty ())] )
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
                                                                                                                                             (Lval(Var(LocalTraces.return_vinfo),NoOffset))),{programPoint=ctx.node;sigma=newSigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid_returning (VarinfoSet.union ls_returning ls));tid=tid_returning;lockSet=(VarinfoSet.union ls_returning ls)}) 
                       else
                         (let return_value = SigmaMap.find LocalTraces.return_vinfo sigma_returning
                          in if equal_varDomain return_value Error then (print_string "In combine, a returning Error is assigned to some lval, this is not supported\n";exit 0) else
                            let result_sigma = SigmaMap.add x return_value newSigma
                            in {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Skip,{programPoint=ctx.node;sigma=result_sigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node result_sigma tid_returning (VarinfoSet.union ls_returning ls));tid=tid_returning; lockSet=(VarinfoSet.union ls_returning ls)})
                     | _ -> Printf.printf "Invalid Lval format in combine\n"; exit 0)
    in
    let result_graph = LocalTraces.extend_by_gEdge graph myEdge
    in
    result_graph


  (* iterate over the graphs in previous state *)
  let combine_fold_graphSet graph (args, lval, callee_local, ctx,set_acc) =
    (* wir müssen schauen, ob ein returnender Knoten für irgendein ctx.prev_node ein last node ist*)
    let lastGraphNode = LocalTraces.get_last_node graph 
    in if Node.equal lastGraphNode.programPoint LocalTraces.error_node then (print_string "In combine, we have an empty graph\n"; (args, lval, callee_local, ctx,set_acc)) 
    else (
      let currentNode = 
        LocalTraces.find_calling_node lastGraphNode graph ctx.prev_node
      in
      let new_set = 
        (if Node.equal currentNode.programPoint LocalTraces.error_node then (print_string ("In combine, we could not find the calling node:
      "^(LocalTraces.show graph)^"\n"); set_acc) else
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
        let result_graph = print_string ("In threadenter, we add the edge "^(LocalTraces.show_edge myEdge)^"\n
  to the graph:"^(LocalTraces.show graph)^"\n"); LocalTraces.extend_by_gEdge graph myEdge
        in
        result_graph
      )
    | exception Not_found -> Printf.printf "Error: function does not exist, in threadenter\n"; exit 0


  (* iterate over the graphs in previous state *)
  let threadenter_fold_graphSet graph (args, f, ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTraces.error_node 
      then (print_string ("In threadenter, we have a trace that does not end in previous node:
   "^(LocalTraces.show graph)^"\n"); set_acc) else(
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
      print_string ("In threadspawn, we add the edge "^(LocalTraces.show_edge myEdge)^"\n
    to the graph:"^(LocalTraces.show graph)^"\n"); LocalTraces.extend_by_gEdge graph myEdge
    in
    result_graph

  (* iterate over the graphs in previous state *)
  let threadspawn_fold_graphSet graph (lval, ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
    in
    let new_set = 
      if Node.equal lastNode.programPoint LocalTraces.error_node 
      then (print_string ("In threadspawn, we have a trace that does not end in previous node:\n
     "^(LocalTraces.show graph)^"\n"); set_acc) else(
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