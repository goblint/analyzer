open Prelude.Ana
open Analyses
open LocalTraces
open PostSolvingFlag
open AuxiliaryClasses

(* Custom exceptions for error messaging *)
exception Overflow_addition_Int
exception Underflow_subtraction_Int

(* Constants *)
let intMax = 2147483647
let intMin = -2147483648


let add_dependency_from_last_unlock graph mutexVinfo = 
  let lastNode = LocalTrace.get_last_node graph
  in 
  let lastUnlockingNode = LocalTrace.get_last_unlocking_node graph mutexVinfo
  in
  if not (Node.equal lastUnlockingNode.programPoint LocalTrace.error_node)
  then
    (
      (* let allPredecessorEdges = LocalTrace.get_predecessors_edges graph lastNode
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
      LocalTrace.extend_by_gEdge graph depEdge
      (* ) *)
    )
  else
    (print_string ("Error: graph has no last unlocking node for mutex "^(CilType.Varinfo.show mutexVinfo)^", graph:\n"^(LocalTrace.show graph)^"\n"); exit 0)


(* functions for join-check *)
(* symmetric prefix
   This checks whether two graphs are equivalent ending in prefixNode *)
let is_trace_joinable_symmetric candidate graph prefixNode = 
  let rec inner_loop edgeList =
    match edgeList with (pred_node,_,_)::xs -> if loop pred_node then inner_loop xs else (print_string("loop in inner_loop resulted in false\n"); false)
                      | [] -> true
  and
    loop current_prefix =
    let candidate_pred_edges = LocalTrace.get_predecessors_edges candidate current_prefix
    in let graph_pred_edges = LocalTrace.get_predecessors_edges graph current_prefix
    in
    if not (LocalTrace.equal_edge_lists candidate_pred_edges graph_pred_edges) 
    then (
      false)
    else (
      (LocalTrace.check_no_multiple_depMutexes 
         (LocalTrace.merge_edge_lists (LocalTrace.get_successors_edges candidate current_prefix) (LocalTrace.get_successors_edges graph current_prefix)))
      &&
      (inner_loop candidate_pred_edges))
  in
  loop prefixNode

(* Checks prefix of two graphs assuming that graph is the creator of candidate *)
let is_trace_joinable candidate graph creatorTID = 
  let create_node = LocalTrace.find_creating_node (LocalTrace.get_last_node candidate) candidate
  in
  if not (LocalTrace.exists_node graph create_node) then (
    print_string ("create_node does not exists in creator-trace with\ncreate_node="^(NodeImpl.show create_node)^"\ngraph="^(LocalTrace.show graph)^"\n");
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
  (LocalTrace.exists_node graph (LocalTrace.get_last_node candidate)) || (LocalTrace.exists_node candidate (LocalTrace.get_last_node graph))

(* Checks symmetrically whether candidate and graph have the same prefix while determining the prefix node *)
let check_prefix candidate graph = 
  let prefixNode = LocalTrace.get_recent_divergent_node candidate graph
  in 
  print_string ("in check_prefix, our prefixNode is "^(NodeImpl.show prefixNode)^"\n");
  if Node.equal prefixNode.programPoint LocalTrace.error_node then 
    (print_string "in check_prefix, we got an error_node\n";
     false)
  else
    is_trace_joinable_symmetric candidate graph prefixNode

(* Checks whether the lock sets are disjoint *)
let check_compatible_lockSets lastCandidateNode lastGraphNode =
  VarinfoSet.is_empty (VarinfoSet.inter lastCandidateNode.lockSet lastGraphNode.lockSet)

(* Helper function for merging of graphs wrt the mutex *)
let mutexLock_join_helper customGraph candidate mutex_vinfo ctxEdge lockingNode prevNode  =
  let lastCandidateNode = LocalTrace.get_last_node candidate
  in
  let lastGraphNode = LocalTrace.get_last_node customGraph
  in
  (* perform all pre-checks for merging *)
  if  
    (not (check_exists_unlock_lock candidate customGraph ))&&
    (check_prefix candidate customGraph )
    &&(check_compatible_lockSets lastCandidateNode lastGraphNode) 
  then (
    print_string "mutexLock_join passed all checks\n";
    let merged_graph = LocalTrace.merge_graphs customGraph candidate
    in
    (* add unlock edge and add dependency edge from candidate end to graph end*)
    let depEdge : node * CustomEdge.t * node = (lastCandidateNode, DepMutex(mutex_vinfo), lockingNode)
    in
    let result_graph = LocalTrace.extend_by_gEdge merged_graph depEdge
    in
    (* let myEdge = (prevNode,(EdgeImpl.convert_edge ctxEdge),lockingNode)
       in *)
    (* perform post-check and reject if merged graph did not pass *)
    if LocalTrace.is_valid_merged_graph result_graph then [result_graph]  
    else (print_string ("result_graph is not valid\nresult_graph:"^(LocalTrace.show result_graph)^"\n"); [])
  )
  else  [] 

(* Merges all compatible candidates per mutex lock *)
let mutexLock_join candidates graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} ctxEdge lockingNode mutex_vinfo =
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
  let result_node, result_edge = LocalTrace.find_globvar_assign_node var graph node in 
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
                  let randomNr = randomIntGenerator#getRandomValue (LocalTrace.hash graph) var 
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
                 let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(Big_int_Z.big_int_of_int intMin, l, k)) (SigmaMap.empty))
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
