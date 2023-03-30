open Prelude.Ana
open Analyses
open LocTraceDS
open PriorityCalc
  
(* Custom exceptions for eval-function *)
exception Division_by_zero_Int
exception Overflow_addition_Int
exception Overflow_multiplication_Int
exception Underflow_multiplication_Int
exception Underflow_subtraction_Int
exception Join_Nonexistent_Tid

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
let startstate v = let g = D.empty () in let tmp = Printf.printf "Leerer Graph wird erstellt\n";D.add LocTraceGraph.empty g
in if D.is_empty tmp then tmp else tmp


let exitstate = startstate

(* functions for join-check *)
 (* symmetric prefix *)
 let is_trace_joinable_symmetric candidate graph prefixNode = 
  (* TODO vlt muss ich hier mit Ausnahme von prefixNode auch die Gleichheit der Successors überprüfen *)
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
    (* print_string ("prefix-check failed: predecessor lists of creator-trace and thread-trace are different;\ncurrent_prefix="^(NodeImpl.show current_prefix)^"\n
 graph_pred_edges=["^(List.fold (fun s_fold edge_fold -> (LocalTraces.show_edge edge_fold)^", "^s_fold) "" graph_pred_edges)^"]\n
 candidate_pred_edges=["^(List.fold (fun s_fold edge_fold -> (LocalTraces.show_edge edge_fold)^", "^s_fold) "" candidate_pred_edges)^"]\n");  *)
 false)
 else (
  (* if it's the prefixNode, then we do not check the successors, since this would definitely fail *)
  if NodeImpl.equal current_prefix prefixNode then
inner_loop candidate_pred_edges
  else
    (let candidate_succ_edges = LocalTraces.get_successors_edges candidate current_prefix
  in let graph_succ_edges = LocalTraces.get_successors_edges graph current_prefix
in
if not (LocalTraces.equal_edge_lists candidate_succ_edges graph_succ_edges) then false else inner_loop candidate_pred_edges
 ))
in
loop prefixNode
 
(* prefix check where graph created candidate *)
let is_trace_joinable candidate graph creatorTID = 
 let create_node = LocalTraces.find_creating_node (LocalTraces.get_last_node candidate) candidate
in
if not (LocalTraces.exists_node graph create_node) then (print_string ("create_node does not exists in creator-trace with\ncreate_node="^(NodeImpl.show create_node)^"\ngraph="^(LocalTraces.show graph)^"\n"); false) else
 (
   is_trace_joinable_symmetric candidate graph create_node
 )


let rec find_joinable_traces candidates graph creatorTID  = 
 match candidates with
 x::xs -> if is_trace_joinable x graph creatorTID  then x::(find_joinable_traces xs graph creatorTID) else find_joinable_traces xs graph creatorTID
   | [] -> []

   (* helper functions for mutexLock_join*)
   let check_exists_unlock_lock candidate graph = 
     (LocalTraces.exists_node graph (LocalTraces.get_last_node candidate)) || (LocalTraces.exists_node candidate (LocalTraces.get_last_node graph))

   let check_prefix candidate graph = 
     let prefixNode = LocalTraces.get_recent_divergent_node candidate graph
   in 
   print_string ("in check_prefix, our prefixNode is "^(NodeImpl.show prefixNode)^"\n");
   if Node.equal prefixNode.programPoint LocalTraces.error_node then 
     (print_string "in check_prefix, we got an error_node\n";
   false)
   else
   is_trace_joinable_symmetric candidate graph prefixNode

   (* checks whether the lockSets are disjoint *)
 let check_compatible_lockSets lastCandidateNode lastGraphNode =
 LockSet.is_empty (LockSet.inter lastCandidateNode.lockSet lastGraphNode.lockSet)

let mutexLock_join_helper customGraph graph candidate mutex_vinfo ctxEdge lockingNode prevNode  =
  let lastCandidateNode = LocalTraces.get_last_node candidate
in
let lastGraphNode = LocalTraces.get_last_node customGraph
in
(* print_string ("customGraph="^(LocalTraces.show customGraph)^"\n");
print_string("candidate="^(LocalTraces.show candidate)^"\n"); *)
   if  
     (not (check_exists_unlock_lock candidate customGraph ))&&
       (check_prefix candidate customGraph )
       &&(check_compatible_lockSets lastCandidateNode lastGraphNode) 
        then (
         print_string "mutexLock_join passed all checks\n";
      let merged_graph = LocalTraces.merge_graphs customGraph candidate
      in
      (* add unlock edge and add dependency edge from candidate end to graph end*)
      let depEdge : node * CustomEdge.t * node = (lastCandidateNode, DepMutex(mutex_vinfo), prevNode)
    in
      let result_graph = LocalTraces.extend_by_gEdge merged_graph depEdge
   in
   let myEdge = (prevNode,(EdgeImpl.convert_edge ctxEdge),lockingNode)
  in
   if LocalTraces.is_valid_merged_graph result_graph then [LocalTraces.extend_by_gEdge result_graph myEdge]  
   else (print_string ("result_graph is not valid\nresult_graph:"^(LocalTraces.show result_graph)^"\n"); [])
        )
      else ( 
       (* Debug-info *)
       if not (check_prefix candidate customGraph ) then print_string "mutexLock_join failed the prefix-check\n";
       if (check_exists_unlock_lock candidate customGraph ) then print_string "mutexLock_join failed the exist unlock/lock node check\n";
       if not (check_compatible_lockSets lastCandidateNode lastGraphNode) then print_string "mutexLock_join failed the lockSet-compatibility check\n";
        []
      ) 


let mutexLock_join candidates graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} ctxEdge lockingNode mutex_vinfo =
 print_string ("mutexLock_join was invoked with |candidates| = "^(string_of_int (D.cardinal candidates))^"\n");
 let rec loop candidateList graphList =
   match candidateList with candidate::xs -> 
   let result_graph = mutexLock_join_helper graph graph candidate mutex_vinfo ctxEdge lockingNode {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}
  in
let result_list = 
   result_graph@graphList
in
       loop xs result_list
   | [] -> graphList
 in
 loop (graphSet_to_list candidates) []

let rec eval_global var graph node  = 
  let result_node, result_edge = LocalTraces.find_globvar_assign_node var graph node in 
print_string ("find_globvar_assign_node wurde aufgerufen, es wurde folgender Knoten gefunden: "^(NodeImpl.show result_node)^"\nmit Label: "^(EdgeImpl.show result_edge)^"\n");
(match result_edge with 
(Assign(_, edgeExp)) -> (
  let custom_glob_vinfo = makeVarinfo false "__goblint__traces__custom_nonglobal" (TInt(IInt,[]))
in
  let tmp_sigma_global,otherValues, _ = eval result_node.sigma custom_glob_vinfo edgeExp graph result_node
in 
let tmp = SigmaMap.find custom_glob_vinfo tmp_sigma_global
in print_string ("eval_global evaluated to "^(show_varDomain tmp)^"\n");
(tmp ,true,SigmaMap.empty, otherValues))
  | _ -> Printf.printf "Assignment to global variable was not found\n"; exit 0 )

and 
(* Evaluates the effects of an assignment to sigma *)
eval sigOld vinfo (rval: exp) graph node = 
(* print_string ("Eval wurde aufgerufen mit rval="^(CilType.Exp.show rval)^",
\nsigOld=["^(NodeImpl.show_sigma sigOld)^"],
\nfor node="^(NodeImpl.show node)^"\n"); *)
  (* dummy value 
     This is used whenever an expression contains a variable that is not in sigma (e.g. a global)
      In this case vinfo is considered to have an unknown value *)
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
  else Int(Big_int_Z.zero_big_int,Big_int_Z.big_int_of_int 1, ik))

  | _,_ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0 )
| Div -> fun x1 x2 -> (match (x1,x2) with 
(Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if l2 <= Big_int_Z.zero_big_int && u2 >= Big_int_Z.zero_big_int then raise Division_by_zero_Int else (Printf.printf "This type of assignment is not supported - as I do not allow Division yet\n"; exit 0)
  | _,_ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0)
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

| AddrOf (Var(v), NoOffset) ->  if v.vglob 
  then (Address(v), true, currentSigEnhanced, [], AddrOf (Var(customVinfoStore#getLocalVarinfo (make_local_global_varinfo v) v.vtype), NoOffset)) 
else (Address(v), true, currentSigEnhanced, [], AddrOf (Var(v), NoOffset))

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
  if ((SigmaMap.mem var1 sigOld) || (var1.vglob = true))&&((SigmaMap.mem var2 sigOld)|| (var2.vglob = true))
    then (
      let vd1 = if var1.vglob = true 
        then (
          let localGlobalVinfo1 = customVinfoStore#getLocalVarinfo (make_local_global_varinfo var1) var1.vtype  
        in
          if SigmaMap.mem localGlobalVinfo1 sigOld 
            then(
              SigmaMap.find localGlobalVinfo1 sigOld
            ) 
          else
          match eval_global var1 graph node with (vd,_, _,_) -> vd) 
    else (SigmaMap.find var1 sigOld)
      in 
      let vd2 = if var2.vglob = true 
        then (
      let localGlobalVinfo2 = customVinfoStore#getLocalVarinfo (make_local_global_varinfo var2) var2.vtype
    in
    if SigmaMap.mem localGlobalVinfo2 sigOld 
      then(
        SigmaMap.find localGlobalVinfo2 sigOld
      )
      else
      match eval_global var2 graph node with (vd,_, _,_) -> vd) 
    else (SigmaMap.find var2 sigOld)
      in
      match vd1,vd2 with
      | (Int(l1,u1,k1)), (Int(l2,u2,k2)) -> 
        if not (CilType.Ikind.equal k1 k2) then (Printf.printf "This type of assignment is not supported in eval_helper in BinOp(var, var)\n"; exit 0);
      if (u1 < l2) || (u2 <= l1) then ((get_binop_int Lt biopIk) (Int(l1, u1, k1)) (Int(l2, u2, k2)), true , currentSigEnhanced,[], newExpr) 
      else
        (* overlap split *)
      (if (l1 < u1)&&(l2 < u2) then (let m = middle_intersect_intervals l1 u1 l2 u2
      in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(Big_int_Z.add_big_int m (Big_int_Z.big_int_of_int 1), u2, k2)) (SigmaMap.add var1 (Int(l1,m, k1)) currentSigEnhanced), [], newExpr)
      )
      else if (l1 = u1)&&(l2 < u2) 
        then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(Big_int_Z.add_big_int l1 (Big_int_Z.big_int_of_int 1), u2, k2)) (SigmaMap.add var1 (Int(l1,l1, k1)) currentSigEnhanced), [], newExpr)
      else if (l1 < u1) &&(l2 = u2) then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(l2, l2, k2)) (SigmaMap.add var1 (Int(l1,Big_int_Z.sub_big_int l2 (Big_int_Z.big_int_of_int 1), k1)) currentSigEnhanced), [], newExpr)
      else (print_string "in overlap split there are two points, this should never happen actually\n";Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, var)\n"; exit 0))
      | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, var)\n"; exit 0
    )
    else (print_string "nopVal created at binop Lt of two variables. One of them or both are unknown\n";nopVal (currentSigEnhanced) newExpr)
    )

| BinOp(Lt, binopExp1,Lval(Var(var), NoOffset),TInt(biopIk, attr)) ->(
  let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype else var
  in
match eval_helper binopExp1 currentSigEnhanced with 
| (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp1) -> (
  let newExpr = BinOp(Lt, newBinOpExp1,Lval(Var(newVar), NoOffset),TInt(biopIk, attr))
in
  if SigmaMap.mem var sigEnhanced then
    (match SigmaMap.find var sigEnhanced with Int(lVar, uVar, kVar) -> 
      if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
        if uVar <= l || u < lVar 
          then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
        else (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
  else if SigmaMap.mem var sigOld then
    (match SigmaMap.find var sigOld with Int(lVar, uVar, kVar) -> 
      if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
        if uVar <= l || u < lVar 
          then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
        else (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
      else if var.vglob = true then 
        let vd = if SigmaMap.mem (customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype) sigOld then SigmaMap.find (customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype) sigOld
        else (let tmp, _, _, _ = eval_global var graph node in tmp)
        in
        (match vd with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
        
        if uVar <= l || u < lVar 
          then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
        else (* TODO design meaningful logic here *) (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
  else ( 
    let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add var (Int(Big_int_Z.add_big_int u (Big_int_Z.big_int_of_int 1), Big_int_Z.big_int_of_int intMax, k)) (SigmaMap.empty))
in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
    )
)
| (_,false, sigEnhanced,_, newBinOpExp1) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newBinOpExp1
| _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0
)

| BinOp(Lt, Lval(Var(var), NoOffset), binopExp2,TInt(biopIk, attr)) -> (
  let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype else var
  in
  match eval_helper binopExp2 currentSigEnhanced with 
  | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp2) -> (
    let newExpr = BinOp(Lt, Lval(Var(newVar), NoOffset), newBinOpExp2,TInt(biopIk, attr))
    in
    if SigmaMap.mem var sigEnhanced then 
      (match SigmaMap.find var sigEnhanced with Int(lVar, uVar, kVar) -> 
        if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0);
        if uVar < l || u <= lVar 
          then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
        else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0)
    else if SigmaMap.mem var sigOld then
      (match SigmaMap.find var sigOld with Int(lVar, uVar, kVar) -> 
        if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0);
        if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
        else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0)
      else if var.vglob = true then 
        (
          let vd = if SigmaMap.mem (customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype) sigOld then SigmaMap.find (customVinfoStore#getLocalVarinfo (make_local_global_varinfo var) var.vtype) sigOld
          else (let tmp, _, _, _ = eval_global var graph node in tmp)
          in
          match vd with Int(lVar, uVar, kVar)-> 
          if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0);
        if uVar <= l || u < lVar 
          then ( (get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr) 
        else (* TODO design meaningful logic here *) (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0)
    else ( 
      let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add var (Int(Big_int_Z.big_int_of_int intMin, Big_int_Z.sub_big_int l (Big_int_Z.big_int_of_int 1), k)) (SigmaMap.empty))
  in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
      ))
  | (_,false, sigEnhanced, _, newExp) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newExp
  | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp)\n"; exit 0)

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

let eval_catch_exceptions sigOld vinfo rval graph node =
  try (eval sigOld vinfo rval graph node, true) with 
  Division_by_zero_Int -> Messages.warn "Contains a trace with division by zero"; ((SigmaMap.add vinfo Error sigOld, [], rval) ,false)
  | Overflow_addition_Int -> Messages.warn "Contains a trace with overflow of Integer addition";((SigmaMap.add vinfo Error sigOld, [], rval) ,false)
  | Underflow_subtraction_Int -> Messages.warn "Contains a trace with underflow of Integer subtraction"; ((SigmaMap.add vinfo Error sigOld, [], rval) ,false)

(* Here, I would like to manage other generated values and return a set of sigmas *)
let eval_wrapper sigOld vinfo rval graph node =
  (* print_string ("in eval_wrapper, sigOld=["^(NodeImpl.show_sigma sigOld)^"]\n"); *)
  let rec iter_otherValues doneValues workList sigmaList =
match workList with 
(var,vd)::xs -> if List.mem (var,vd) doneValues then iter_otherValues doneValues xs sigmaList
else (let sigTmp = SigmaMap.add var vd sigOld
in 
let (anotherSig, moreValues, _), success_other = eval_catch_exceptions sigTmp vinfo rval graph node
in 
let newWorkList = List.fold (fun acc value -> if List.mem value acc then acc else value::acc) workList moreValues
in iter_otherValues ((var,vd)::doneValues) newWorkList (if (List.exists (fun sigma_exists -> NodeImpl.equal_sigma sigma_exists anotherSig) sigmaList) then sigmaList else (anotherSig::sigmaList)))
  | [] -> sigmaList
  in
  let (sigNew,otherValues, newExpr), success = eval_catch_exceptions sigOld vinfo rval graph node 
in 
(* print_string ("in eval_wrapper we get otherValues={"
^(List.fold (fun s (vinfo_fold, varDom_fold) -> s^";("^(CilType.Varinfo.show vinfo_fold)^","^(show_varDomain varDom_fold)^")") "" otherValues)^"}\n
and sigNew=["^(NodeImpl.show_sigma sigNew)^"]\n"); *)
let allSigmas = iter_otherValues [] otherValues [sigNew]
in
(* print_string ("allSigmas:"^(List.fold (fun acc sigma_fold -> acc^"\n<"^(NodeImpl.show_sigma sigma_fold)^">") "" allSigmas)^"\n|allSigmas| = "^(string_of_int (List.length allSigmas))^"\n"); *)
allSigmas, true, newExpr

(* collects all varinfos of globals. LockSet is used here as acc *)
let rec get_all_globals (expr:exp) (acc:LockSet.t) =
  match expr with 
  | Const(CInt(c, ik, _)) -> acc
  | Lval(Var(var), NoOffset) -> if var.vglob then LockSet.add var acc else acc
  | AddrOf (Var(v), NoOffset) -> if v.vglob then LockSet.add v acc else acc
  | UnOp(Neg, unopExp, TInt(unopIk, _)) -> get_all_globals unopExp acc
  | BinOp(PlusA, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | BinOp(MinusA, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | BinOp(Lt, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | BinOp(Div, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | _ -> acc

  let rec remove_global_locals_sigma sigma globalList =
    match globalList with x::xs -> remove_global_locals_sigma (SigmaMap.remove (customVinfoStore#getGlobalVarinfo (make_local_global_varinfo x)) sigma) xs
      | [] -> sigma

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
  let lvalLockedLs = LockSet.add customMutex lastNode.lockSet
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
let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(customMutex),lastNode)
in
let fistLockedGraph = if (LocalTraces.exists_lock_mutex graph customMutex) || (NodeImpl.equal firstNode lastNode) 
  then graph else  LocalTraces.extend_by_gEdge graph firstLockEdge
in
let lockedGraph = LocalTraces.extend_by_gEdge fistLockedGraph lockingEdge
  in 
  let lockedGraphList = 
    lockedGraph::(mutexLock_join allUnlockingTraces graph lastNode lockingLabel lockedNode customMutex)
in
let graphList = List.fold (fun resultGraphList lockedGraph -> 
let sigmaList,success_inner, _ = eval_wrapper lockedNode.sigma localGlobalVar (Lval(Var(global),NoOffset)) lockedGraph lockedNode
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
let assign_on_node graph ctx lval rval {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} = print_string "assign_on_node wurde aufgerufen\n";
(match lval with (Var x, _) -> 
  let lockVarinfo = customVinfoStore#getGlobalVarinfo "pthread_mutex_lock" 
  in
  let unlockVarinfo = customVinfoStore#getGlobalVarinfo "pthread_mutex_unlock"
  in
    let rvalGlobals = get_all_globals rval LockSet.empty
    in print_string ("in assign, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
    let someTmp = create_local_assignments [graph] (LockSet.to_list rvalGlobals) ctx
  in
  (* TODO: Anzahl in someTmp ausgeben. In der create_local_... funktion vlt ein gewisses Vorher/Nacher ausgeben 
     ansonsten vlt ein kleineres Beispiel finden, zum Reproduzieren? *)
    print_string ("someTmp: "^(List.fold (fun s_acc graph_fold -> (LocalTraces.show graph_fold)^"; "^s_acc) "" someTmp)^"\n");
    (* if this is a global, we define a custom mutex and add lock/unlock edges 
       additionally, we have to join other traces that 'unlock' the global, similiarly to special *)
    let resultList = List.fold (
fun resultList_outter graph_outter ->
  (
    let lastNode = LocalTraces.get_last_node graph_outter
    in
    let sigmaList,success_inner, newExp = eval_wrapper lastNode.sigma x rval  graph_outter lastNode in 
    if not success_inner then (print_string "assignment did not succeed!\n"; 
    [LocalTraces.extend_by_gEdge graph_outter (lastNode,Assign(lval, newExp),{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=LockSet.empty})] )
    else
    (List.fold ( fun outerGraphList evaluated ->
      let newSigma = remove_global_locals_sigma evaluated (LockSet.to_list rvalGlobals)
in
    if x.vglob then (
      let customMutex = customVinfoStore#getGlobalVarinfo (make_mutex_varinfo x)
  in
  let lockingLabel:Edge.t = Proc(None, Lval(Var(lockVarinfo), NoOffset),[AddrOf(Var(customMutex), NoOffset)])
  in
  let lvalLockedLs = LockSet.add customMutex ls
  in
  let lockedNode = {programPoint=programPoint;sigma=lastNode.sigma; id=(idGenerator#getID lastNode (EdgeImpl.convert_edge lockingLabel) programPoint sigma tid lvalLockedLs);tid=tid;lockSet=lvalLockedLs}
  in
    let lockingEdge = (lastNode,EdgeImpl.convert_edge lockingLabel,lockedNode)
  in
    let myTmp:V.t = Mutex(customMutex)
  in
    let allUnlockingTraces = ctx.global myTmp
in 
  let firstNode = LocalTraces.get_first_node graph_outter
in
print_string ("In special, firstNode is "^(NodeImpl.show firstNode)^" for graph_outter "^(LocalTraces.show graph_outter)^"\n");
let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(customMutex),lastNode)
in
let fistLockedGraph = if (LocalTraces.exists_lock_mutex graph_outter customMutex) || (NodeImpl.equal firstNode lastNode) then graph_outter else  LocalTraces.extend_by_gEdge graph_outter firstLockEdge
in
  let lockedGraph = LocalTraces.extend_by_gEdge fistLockedGraph lockingEdge
  in 
  let lockedGraphList = 
    lockedGraph::(mutexLock_join allUnlockingTraces graph_outter lastNode lockingLabel lockedNode customMutex)
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
let assign_fold_graphSet graph (lval, rval, ctx,set_acc) = print_string "assign_fold_graphSet wurde aufgerufen\n";
  let lastNode = 
    if (LocTraceGraph.is_empty graph)&&(match lval with (Var x, _) -> x.vglob | _ -> false) then
      (
        tidRecord#addTID 1;
      {programPoint=ctx.prev_node;sigma=SigmaMap.empty;id=idGenerator#increment();tid=1;lockSet=LockSet.empty}
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
  print_string ("Edge effect assign wurde aufgerufen with lval "^(CilType.Lval.show lval)^" and rval "^(CilType.Exp.show rval)^" mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let _, _, _, result =
D.fold assign_fold_graphSet ctx.local (lval, rval, ctx, D.empty ())
in result

(* BRANCH helper functions *)
(* perform branch-effect on given node *)
let branch_on_node graph ctx exp tv {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
let rvalGlobals = get_all_globals exp LockSet.empty
in print_string ("in branch, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
let someTmp = create_local_assignments [graph] (LockSet.to_list rvalGlobals) ctx
in
print_string ("someTmp: "^(List.fold (fun s_acc graph_fold -> (LocalTraces.show graph_fold)^"; "^s_acc) "" someTmp)^"\n");
List.fold (fun resultList_outter graph_outter -> 
  (
    let lastNode = LocalTraces.get_last_node graph_outter
    in
    let branch_sigma = SigmaMap.add LocalTraces.branch_vinfo Error lastNode.sigma 
in
    let sigmaList,success, newExp = eval_wrapper branch_sigma LocalTraces.branch_vinfo exp graph_outter lastNode
in
List.map (fun sigma_map ->
  let result_as_int = match (SigmaMap.find_default Error LocalTraces.branch_vinfo sigma_map) with
Int(i1,i2,_) -> print_string ("in branch, the result is ["^(Big_int_Z.string_of_big_int i1)^";"^(Big_int_Z.string_of_big_int i2)^"]");
if (Big_int_Z.int_of_big_int i1 <= 0)&&(Big_int_Z.int_of_big_int i2 >= 0) then 0 
else 1
  |_ -> -1
in
let sigmaNew = remove_global_locals_sigma (SigmaMap.remove LocalTraces.branch_vinfo (NodeImpl.destruct_add_sigma lastNode.sigma sigma_map)) (LockSet.to_list rvalGlobals)
in
print_string ("result_as_int: "^(string_of_int result_as_int)^"\n");
let myEdge:node*CustomEdge.t*node = (lastNode, Test(newExp, tv),
{programPoint=ctx.node;sigma=sigmaNew;id=(idGenerator#getID lastNode (Test(newExp, tv)) ctx.node sigmaNew tid ls);tid=tid;lockSet=ls})
in
print_string ("success="^(string_of_bool success)^", tv="^(string_of_bool tv)^", result_as_int="^(string_of_int result_as_int)^"\nand possible edge="^(LocalTraces.show_edge myEdge)^"\n");
let result_graph = if success&&((tv=true && result_as_int = 1)||(tv=false&&result_as_int=0) || (result_as_int= -1)) 
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
  print_string ("Edge effect branch wurde aufgerufen mit exp="^(CilType.Exp.show exp)^" and tv="^(string_of_bool tv)^" mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
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
  D.add (LocalTraces.extend_by_gEdge graph ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id= first_ID;tid=first_ID;lockSet=LockSet.empty},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=SigmaMap.empty; id= second_ID;tid=first_ID;lockSet=LockSet.empty})) set_acc)
else set_acc) else
 let result_graph = 
  body_on_node graph ctx lastNode
  in
    D.add result_graph set_acc)
  in
  (ctx, new_set)

let body ctx (f:fundec) : D.t = 
  predominatorRegistration#update ctx.prev_node ctx.node;
  print_string ("Edge effect body wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
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
      let sigma_returnVinfo= SigmaMap.add LocalTraces.return_vinfo Error sigma in
      {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma_returnVinfo;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma_returnVinfo tid ls);tid=tid;lockSet=ls}) 
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
        let rvalGlobals = get_all_globals ret_exp LockSet.empty
    in print_string ("in return, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
    let someTmp = create_local_assignments [graph] (LockSet.to_list rvalGlobals) ctx
  in
    print_string ("someTmp: "^(List.fold (fun s_acc graph_fold -> (LocalTraces.show graph_fold)^"; "^s_acc) "" someTmp)^"\n");
    let outterResultList = List.fold ( fun resultList_outter graph_outter ->
      (
      let lastNode = LocalTraces.get_last_node graph_outter
    in  
      let resultList, success, newExp = eval_wrapper lastNode.sigma LocalTraces.return_vinfo ret_exp graph_outter lastNode
    in
    List.map (fun sigma_map -> 
      let newSigma = remove_global_locals_sigma sigma_map (LockSet.to_list rvalGlobals)
      in
      let myEdge: node*CustomEdge.t*node =
        if success = false then (print_string "Evaluation of return expression was unsuccessful\n"; exit 0)
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
  print_string ("Edge effect return wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
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
  let firstLockEdge:node*CustomEdge.t*node =(firstNode, DepMutex(mutex_vinfo),{programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls})
  in
  (* First lock is only added, if this is the first lock *)
  let firstLockedGraph = if LocalTraces.exists_lock_mutex graph mutex_vinfo then graph else  (LocalTraces.extend_by_gEdge graph firstLockEdge)
  in
  let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid (LockSet.add mutex_vinfo ls));tid=tid;lockSet=(LockSet.add mutex_vinfo ls)})
      in
  let result_graph = LocalTraces.extend_by_gEdge firstLockedGraph myEdge
    in
    let lockedNode = {programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid (LockSet.add mutex_vinfo ls));tid=tid;lockSet=(LockSet.add mutex_vinfo ls)}
  in
  (* we create both: merged trace and trace that assumes first lock *)
  result_graph::(mutexLock_join allUnlockingTraces graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} ctx.edge lockedNode mutex_vinfo)
          )

    | Unlock(unlockExp), _ ->
      (
        let mutex_vinfo =
          match unlockExp with AddrOf(Var(vinfo), _) -> vinfo
            | _ -> Printf.printf "Expression in pthread_mutex_lock is not supported\n"; exit 0
        in
        print_string ("in special, we found Unlock with mutex_vinfo="^(CilType.Varinfo.show mutex_vinfo)^"\n");
        let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls},(EdgeImpl.convert_edge ctx.edge),{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid (LockSet.remove mutex_vinfo ls));tid=tid;lockSet=(LockSet.remove mutex_vinfo ls)})
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
      let tidSigmaList, success, newExp = eval_wrapper sigma special_varinfo tidExp graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}
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
print_string ("ThreadJoin did not succeed due to already joined TID for graph: \n"^(LocalTraces.show graph)^"\nand tidJoin: "^(string_of_int tidJoin)^"\n");
let graph_error_edge = LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=LockSet.empty}) 
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
let graph_error_edge = LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=LockSet.empty}) 
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
(* TODO: is union the right way? *)
let newLockSet = LockSet.union ls (LocalTraces.get_last_node trace_fold).lockSet
in
let destination_node = {programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid newLockSet);tid=tid;lockSet=newLockSet}
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
    match arglist with [AddrOf(Var(argVinfo),_)] -> if LockSet.mem argVinfo ls 
      then ( Messages.warn "mutex_destroy on locked mutex";
        let graph_error_edge = LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=LockSet.empty})
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
      | _ -> print_string "In special, lval for srand is not suitable\n"; exit 0
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
  print_string ("Edge effect special wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^" und ctx.edge "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^" und f "^(CilType.Varinfo.show f)^"\n");
let _, _, _, _, result =   D.fold special_fold_graphSet ctx.local (lval, f, arglist, ctx, D.empty ())
in result
    
(* ENTER helper functions *)
let construct_sigma_combinations varinfo varDomList sigmaList =
  let rec loop varDomList_loop sigAcc =
    match varDomList_loop with x::xs -> loop xs ((List.map (fun sigma -> (SigmaMap.add varinfo x sigma)) sigmaList)@sigAcc)
    | [] -> sigAcc
  in
  loop varDomList []

(* perform enter-effect on given node *)
let enter_on_node graph ctx f args {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
  let rvalGlobals = List.fold (fun ls_fold arg_fold -> get_all_globals arg_fold ls_fold) LockSet.empty args
  in
print_string ("in enter, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
let someTmp = create_local_assignments [graph] (LockSet.to_list rvalGlobals) ctx
  in
    print_string ("someTmp: "^(List.fold (fun s_acc graph_fold -> (LocalTraces.show graph_fold)^"; "^s_acc) "" someTmp)^"\n");
    let outterResultList = List.fold ( fun resultList_outter graph_outter ->
      (
        let lastNode = LocalTraces.get_last_node graph_outter
    in
      let sigma_formalList, _, newExpList = List.fold ( 
      fun (sigAcc, formalExp, expListAcc) formal -> (match formalExp with 
        | x::xs -> (
          let resultList, success, newExp = eval_wrapper lastNode.sigma formal x graph_outter lastNode
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
    {programPoint=(FunctionEntry(f));sigma= (remove_global_locals_sigma sigma_map (LockSet.to_list rvalGlobals));id=(idGenerator#getID lastNode newEdgeLabel (FunctionEntry(f)) sigma_map tid ls);tid=tid;lockSet=ls})
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
    print_string ("Edge effect enter wurde aufgerufen with function "^(CilType.Fundec.show f)^" with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^", edge label "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^"
  with formals "^(List.fold (fun s sformal -> s^", "^(CilType.Varinfo.show sformal)) "" f.sformals)^" and arguments "^(List.fold (fun s exp -> s^", "^(CilType.Exp.show exp)) "" args)^"\n");
  if D.is_empty ctx.local then (
    let first_ID = idGenerator#increment()
in
let second_ID = idGenerator#increment()
in
    [ctx.local, D.add (LocalTraces.extend_by_gEdge (LocTraceGraph.empty) ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id=first_ID;tid=first_ID;lockSet=LockSet.empty}, (EdgeImpl.convert_edge ctx.edge), {programPoint=(FunctionEntry(f));sigma=SigmaMap.empty;id= second_ID;tid=first_ID;lockSet=LockSet.empty})) (D.empty ())] )
  else
  let _, _, _, result = print_string ("In enter, neuer state wird erstellt\n mit ctx.local: "^(D.show ctx.local)^" und |ctx.local| = "^(string_of_int (D.cardinal ctx.local))^"\n"); 
  D.fold enter_fold_graphSet ctx.local (f, args, ctx, D.empty ())
  in
    [ctx.local, result]  
  
(* COMBINE helper functions *)
(* perform combine-effect on node *)
(* TODO do we here accumulate the locked mutexes as well?*)
let combine_on_node args ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} {programPoint=progP_returning;id=id_returning;sigma=sigma_returning;tid=tid_returning;lockSet=ls_returning} callee_local lval graph =
  let rvalGlobals = List.fold (fun ls_fold arg_fold -> get_all_globals arg_fold ls_fold) LockSet.empty args
  in
print_string ("in combine, rvalGlobals =["^(NodeImpl.show_lockSet rvalGlobals)^"]\n");
let newSigma = remove_global_locals_sigma sigma (LockSet.to_list rvalGlobals)
in
  (* D.iter (fun g_iter -> 
    print_string ("in combine, I test LocalTrace.find_returning_node with node="^(NodeImpl.show {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls})^"\nI get "^(NodeImpl.show (LocalTraces.find_returning_node ({programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls}) (EdgeImpl.convert_edge ctx.edge) g_iter))^"\n")) callee_local; *)
  if tid != tid_returning then (Printf.printf "TIDs from current node and found returning node are different in combine\n"; exit 0);
    let (myEdge:(node * CustomEdge.t * node)) = 
    (match lval with None -> 
      {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Skip,
      {programPoint=ctx.node;sigma=newSigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning; lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid_returning (LockSet.union ls_returning ls));tid=tid_returning;lockSet=(LockSet.union ls_returning ls)}
     |Some (Var x, y) ->  if x.vglob 
      then 
        ({programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Assign((Var(x), y), 
     (Lval(Var(LocalTraces.return_vinfo),NoOffset))),{programPoint=ctx.node;sigma=newSigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node newSigma tid_returning (LockSet.union ls_returning ls));tid=tid_returning;lockSet=(LockSet.union ls_returning ls)}) 
    else
      (let return_value = SigmaMap.find LocalTraces.return_vinfo sigma_returning
      in if equal_varDomain return_value Error then (print_string "In combine, a returning Nullpointer is assigned to some lval, this is not supported\n";exit 0) else
      let result_sigma = SigmaMap.add x return_value newSigma
  in {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning;lockSet=ls_returning},Skip,{programPoint=ctx.node;sigma=result_sigma; id=(idGenerator#getID {programPoint=programPoint;sigma=newSigma;id=id;tid=tid_returning;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node result_sigma tid_returning (LockSet.union ls_returning ls));tid=tid_returning; lockSet=(LockSet.union ls_returning ls)})
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
    print_string ("Edge effect combine wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^", edge label "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^"
    und lval "^(match lval with None -> "None" | Some(l) -> CilType.Lval.show l)^" und fexp "^(CilType.Exp.show fexp)^"\n");
let _,_,_,_, result=
   D.fold combine_fold_graphSet (*ctx.local*) callee_local (args, lval, callee_local, ctx, D.empty ())
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
    let new_id = idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma_formals id LockSet.empty
    in
    let myEdge = {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=(FunctionEntry(fd));sigma=sigma_formals;id=new_id;tid=id;lockSet=LockSet.empty}
    (* einspeichern der create-Beziehung *)
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
      print_string ("Edge effect threadenter wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^", ctx.edge "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^" und ctx.node "^(Node.show ctx.node)^"
    , lval "^(match lval with None -> "None" |Some(l) -> CilType.Lval.show l)^"
    \nund args={"^(List.fold (fun acc_fold exp_fold -> acc_fold^(CilType.Exp.show exp_fold)^"; ") "" args)^"}\n");
      let _, _, _, result = D.fold threadenter_fold_graphSet (ctx.local) (args, f, ctx, D.empty())
    in
      [result] (* Zustand von neuem Thread *)

    (* THREADSPAWN helper functions *)
   (* perform threadspawn-effect on given node *)
  let threadspawn_on_node graph ctx lval {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} =
    (* convention: TID of new thread is always the ID of creating node *)
    tidRecord#addTID id;
    let myEdge =  (match lval with None ->
    {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node sigma tid ls);tid=tid;lockSet=ls}
  (* |Some((Var x, _)) -> (
    print_string ("in threadspawn, we have some Lval "^(CilType.Varinfo.show x)^"\n");
    let result_sigma = SigmaMap.add x (Int(Big_int_Z.big_int_of_int id, Big_int_Z.big_int_of_int id, IInt)) sigma
    in 
    {programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=result_sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node result_sigma tid);tid=tid}
  ) *)
  | Some(Mem(CastE(TPtr(TNamed(tInfo, tAttr), ptrAttr), AddrOf(Var(lvalVinfo),_))), offset) -> (
    if String.equal tInfo.tname "pthread_t" 
      then (print_string ("input in threadspawn is pthread_t\n");
    let result_sigma = SigmaMap.add lvalVinfo (ThreadID(id)) sigma
    in 
    {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls}, EdgeImpl.convert_edge ctx.edge,{programPoint=ctx.node;sigma=result_sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} (EdgeImpl.convert_edge ctx.edge) ctx.node result_sigma tid ls);tid=tid;lockSet=ls}  
    )
  else (Printf.printf "Unsupported argument in threadspawn\n"; exit 0))
    (* | Some(Mem(CastE (_, CastE(_,Const(CInt(cilint,_,_))))), _) -> 
      (if (cilint_to_int cilint) = 0 then ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma tid);tid=tid}) else (print_string ("Unsupported argument in threadspawn\n"); exit 0)) *)
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

    let threadspawn ctx lval f args fctx = (* Creator; lval speichert neue Thread-ID *)
    predominatorRegistration#update ctx.prev_node ctx.node;
      print_string ("Edge effect threadspawn wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^", ctx.edge "^(EdgeImpl.show (EdgeImpl.convert_edge ctx.edge))^" und ctx.node "^(Node.show ctx.node)^"\n");
      let _, _, result = D.fold threadspawn_fold_graphSet (ctx.local) (lval, ctx, D.empty())
    in
    result
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)