open Prelude.Ana
open Analyses
open LocTraceDS
  
(* Custom exceptions for eval-function *)
exception Division_by_zero_Int
exception Overflow_addition_Int
exception Overflow_multiplication_Int
exception Underflow_multiplication_Int
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

let context fundec l =
  ()

let name () = "localTraces"

(* start state is a set of one empty graph *)
let startstate v = let g = D.empty () in let tmp = Printf.printf "Leerer Graph wird erstellt\n";D.add LocTraceGraph.empty g
in if D.is_empty tmp then tmp else tmp


let exitstate = startstate

let rec eval_global var graph node  = 
  let result_node, result_edge = LocalTraces.find_globvar_assign_node var graph node in 
print_string ("find_globvar_assign_node wurde aufgerufen, es wurde folgender Knoten gefunden: "^(NodeImpl.show result_node)^"\nmit Label: "^(EdgeImpl.show result_edge)^"\n");
(match result_edge with 
(Assign(_, edgeExp)) -> (
  let custom_glob_vinfo = makeVarinfo false "__goblint__traces__custom_nonglobal" (TInt(IInt,[]))
in
  let tmp_sigma_global,otherValues = eval result_node.sigma custom_glob_vinfo edgeExp graph result_node
in 
let tmp = SigmaMap.find custom_glob_vinfo tmp_sigma_global
in print_string ("eval_global evaluated to "^(show_valuedomain tmp)^"\n");
(tmp ,true,SigmaMap.empty, otherValues))
  | _ -> Printf.printf "Assignment to global variable was not found\n"; exit 0 )

and 
(* Evaluates the effects of an assignment to sigma *)
eval sigOld vinfo (rval: exp) graph node = 
  (* dummy value 
     This is used whenever an expression contains a variable that is not in sigma (e.g. a global)
      In this case vinfo is considered to have an unknown value *)
  let nopVal sigEnhanced = (Int((Big_int_Z.big_int_of_int (-13)), (Big_int_Z.big_int_of_int (-13)),IInt), false, sigEnhanced, [])

  (* returns a function which calculates [l1, u1] OP [l2, u2]*)
in let get_binop_int op ik = 
  if not (CilType.Ikind.equal ik IInt) then (Printf.printf "This type of assignment is not supported\n"; exit 0) else 
(match op with 
| PlusA -> 
fun x1 x2 -> (match (x1,x2) with 
(Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported\n"; exit 0); 
(if (Big_int_Z.add_big_int u1 u2 > Big_int_Z.big_int_of_int intMax) then raise Overflow_addition_Int else Int(Big_int_Z.add_big_int l1 l2, Big_int_Z.add_big_int u1 u2, ik))
| _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

| MinusA -> 
fun x1 x2 -> (match (x1,x2) with (* Minus sollte negieren dann addieren sein, sonst inkorrekt!! *)
(Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
  (let neg_second_lower = Big_int_Z.minus_big_int u2
in let neg_second_upper = Big_int_Z.minus_big_int l2
in print_string("get_binop_int with MinusA: l1="^(Big_int_Z.string_of_big_int l1)^", u1="^(Big_int_Z.string_of_big_int u1)^", neg_second_lower="^(Big_int_Z.string_of_big_int neg_second_lower)^", neg_second_upper="^(Big_int_Z.string_of_big_int neg_second_upper)^"\n"); 
if (Big_int_Z.add_big_int l1 neg_second_lower < Big_int_Z.big_int_of_int intMin) then raise Underflow_subtraction_Int else Int(Big_int_Z.add_big_int l1 neg_second_lower, Big_int_Z.add_big_int u1 neg_second_upper, ik))
| _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

| Lt -> 
  fun x1 x2 -> (match (x1,x2) with 
  | (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
  (if Big_int_Z.lt_big_int u1 l2 then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, ik) )
  else if Big_int_Z.le_big_int u2 l1 then (Int(Big_int_Z.zero_big_int,Big_int_Z.zero_big_int, ik))
  else Int(Big_int_Z.zero_big_int,Big_int_Z.big_int_of_int 1, ik))

  | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0 )
| Div -> fun x1 x2 -> (match (x1,x2) with 
(Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if l2 <= Big_int_Z.zero_big_int && u2 >= Big_int_Z.zero_big_int then raise Division_by_zero_Int else (Printf.printf "This type of assignment is not supported\n"; exit 0)
  | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

in
  let rec eval_helper subexp =
  (match subexp with

| Const(CInt(c, ik, _)) -> (match ik with
| IInt -> if c < Big_int_Z.big_int_of_int intMin then (Int (Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int intMin, ik), true,SigmaMap.empty, []) 
else if c > Big_int_Z.big_int_of_int intMax then (Int (Big_int_Z.big_int_of_int intMax,Big_int_Z.big_int_of_int intMax, ik), true,SigmaMap.empty, []) else (Int (c,c, ik), true,SigmaMap.empty, [])
| _ ->  Printf.printf "This type of assignment is not supported\n"; exit 0 )

| Lval(Var(var), NoOffset) -> if var.vglob = true 
  then eval_global var graph node
else
  (if SigmaMap.mem var sigOld then (
    match SigmaMap.find var sigOld with
    | Int(l,u,k) -> if l = u then (Int(l,u,k), true,SigmaMap.empty, []) else 
      (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (SigmaMap.empty),[])
    | rest -> (rest, true,SigmaMap.empty,[]) 
    )
else (print_string ("var="^(CilType.Varinfo.show var)^" not found in sigOld="^(NodeImpl.show_sigma sigOld)^"\nThis means we choose a value for this trace\n");
let randomNr = randomIntGenerator#getRandomValue (LocalTraces.hash graph) var 
in
let randomVd = Int((Big_int_Z.big_int_of_int (randomNr)), (Big_int_Z.big_int_of_int (randomNr)),IInt)
in
(randomVd, true, SigmaMap.add var randomVd (SigmaMap.empty), [(var, (Int((Big_int_Z.big_int_of_int (randomNr+1)), (Big_int_Z.big_int_of_int (randomNr+1)),IInt)));
(var, (Int((Big_int_Z.big_int_of_int (randomNr+2)), (Big_int_Z.big_int_of_int (randomNr+2)),IInt)));
(var, (Int((Big_int_Z.big_int_of_int (randomNr+3)), (Big_int_Z.big_int_of_int (randomNr+3)),IInt)))
]
)))

| AddrOf (Var(v), NoOffset) -> (Address(v), true,SigmaMap.empty, [])

(* unop expressions *)
(* for type Integer *)
| UnOp(Neg, unopExp, TInt(unopIk, _)) -> 
  (match eval_helper unopExp with (Int(l,u,ik), true, sigEnhanced, otherValues) ->
    (match ik with 
    |IInt -> if CilType.Ikind.equal unopIk IInt then( let negLowerBound = (if Big_int_Z.minus_big_int u < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
    else if Big_int_Z.minus_big_int u > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int u )
  in
  let negUpperBound = (if Big_int_Z.minus_big_int l < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
  else if Big_int_Z.minus_big_int l > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int l )
in
       (Int (negLowerBound, negUpperBound, unopIk), true, sigEnhanced, otherValues)) else (Printf.printf "This type of assignment is not supported\n"; exit 0)
    | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
    |(_, false, _,_) -> print_string "nopVal created at unop Neg for Int\n";nopVal SigmaMap.empty
    |(_, _,_,_) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 

(* binop expressions *)
(* Lt could be a special case since it has enhancements on sigma *)
(* in var1 < var2 case, I have not yet managed boundary cases, so here are definitely some bugs *)
| BinOp(Lt, Lval(Var(var1), NoOffset),Lval(Var(var2), NoOffset),TInt(biopIk, _)) ->(
  if ((SigmaMap.mem var1 sigOld) || (var1.vglob = true))&&((SigmaMap.mem var2 sigOld)|| (var2.vglob = true))
    then (
      let vd1 = if var1.vglob = true then (match eval_global var1 graph node with (vd,_, _,_) -> vd) else (SigmaMap.find var1 sigOld)
      in 
      let vd2 = if var2.vglob = true then (match eval_global var2 graph node with (vd,_, _,_) -> vd) else (SigmaMap.find var2 sigOld)
      in
      match vd1,vd2 with
      | (Int(l1,u1,k1)), (Int(l2,u2,k2)) -> if not (CilType.Ikind.equal k1 k2) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
      if (u1 < l2) || (u2 <= l1) then ((get_binop_int Lt biopIk) (Int(l1, u1, k1)) (Int(l2, u2, k2)), true , SigmaMap.empty,[]) 
      else
        (* overlap split *)
      (if (l1 < u1)&&(l2 < u2) then (let m = middle_intersect_intervals l1 u1 l2 u2
      in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(Big_int_Z.add_big_int m (Big_int_Z.big_int_of_int 1), u2, k2)) (SigmaMap.add var1 (Int(l1,m, k1)) sigOld), []))
      else if (l1 = u1)&&(l2 < u2) then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(Big_int_Z.add_big_int l1 (Big_int_Z.big_int_of_int 1), u2, k2)) (SigmaMap.add var1 (Int(l1,l1, k1)) sigOld), [])
      else if (l1 < u1) &&(l2 = u2) then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(l2, l2, k2)) (SigmaMap.add var1 (Int(l1,Big_int_Z.sub_big_int l2 (Big_int_Z.big_int_of_int 1), k1)) sigOld), [])
      else (print_string "in overlap split there are two points, this should never happen actually\n";Printf.printf "This type of assignment is not supported\n"; exit 0))
      | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
    )
    else (print_string "nopVal created at binop Lt of two variables. One of them or both are unknown\n";nopVal SigmaMap.empty)
    )

| BinOp(Lt, binopExp1,Lval(Var(var), NoOffset),TInt(biopIk, _)) ->(
match eval_helper binopExp1 with 
| (Int(l, u, k), true, sigEnhanced, otherValues) -> (
  if SigmaMap.mem var sigEnhanced then
    (match SigmaMap.find var sigEnhanced with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar <= l || u < lVar then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigEnhanced, otherValues) 
        else (* TODO design meaningful logic here *) (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
  else if SigmaMap.mem var sigOld then
    (match SigmaMap.find var sigOld with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar <= l || u < lVar then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigEnhanced, otherValues) 
        else (* TODO design meaningful logic here *) (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
      else if var.vglob = true then 
        (match eval_global var graph node with (Int(lVar, uVar, kVar),_,_,_) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        
        if uVar <= l || u < lVar then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigEnhanced, otherValues) 
        else (* TODO design meaningful logic here *) (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
  else ( 
    let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add var (Int(Big_int_Z.add_big_int u (Big_int_Z.big_int_of_int 1), Big_int_Z.big_int_of_int intMax, k)) (SigmaMap.empty))
in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, sigTmp, otherValues)
    )
)
| (_,false, sigEnhanced,_) -> nopVal sigEnhanced
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
)

| BinOp(Lt, Lval(Var(var), NoOffset), binopExp2,TInt(biopIk, _)) -> (
  match eval_helper binopExp2 with 
  | (Int(l, u, k), true, sigEnhanced, otherValues) -> (
    if SigmaMap.mem var sigEnhanced then 
      (match SigmaMap.find var sigEnhanced with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigEnhanced, otherValues) 
        else (* TODO design meaningful logic here *) (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

    else if SigmaMap.mem var sigOld then
      (match SigmaMap.find var sigOld with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigEnhanced, otherValues) 
        else (* TODO design meaningful logic here *) (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
      else if var.vglob = true then 
        (match eval_global var graph node with (Int(lVar, uVar, kVar),_,_,_) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar <= l || u < lVar then (print_string "We calculate get_binop_int\n"; (get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigEnhanced, otherValues) 
        else (* TODO design meaningful logic here *) (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
    else ( 
      let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add var (Int(Big_int_Z.big_int_of_int intMin, Big_int_Z.sub_big_int l (Big_int_Z.big_int_of_int 1), k)) (SigmaMap.empty))
  in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, sigTmp, otherValues)
      ))
  | (_,false, sigEnhanced, _) -> nopVal sigEnhanced
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

(* for type Integer *)
| BinOp(op, binopExp1, binopExp2,TInt(biopIk, _)) ->
  (match (eval_helper binopExp1, eval_helper binopExp2) with 
  | ((Int(l1,u1, ik1), true,sigEnhanced1,otherValues1),(Int(l2,u2, ik2), true,sigEnhanced2,otherValues2)) -> if CilType.Ikind.equal ik1 ik2 then ((get_binop_int op biopIk) (Int(l1,u1, ik1)) (Int(l2,u2, ik2)), true, NodeImpl.intersect_sigma sigEnhanced1 sigEnhanced2, (*not sure if correct as is*) otherValues1@otherValues2) else (Printf.printf "This type of assignment is not supported\n"; exit 0)
  | ((_,_,sigEnhanced1,_), (_,false, sigEnhanced2,_)) -> print_string "nopVal created at binop for Integer 1\n";nopVal (NodeImpl.intersect_sigma sigEnhanced1 sigEnhanced2)
  | ((_,false, sigEnhanced1,_), (_,_,sigEnhanced2,_)) -> print_string "nopVal created at binop for Integer 2\n";nopVal (NodeImpl.intersect_sigma sigEnhanced1 sigEnhanced2)
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 

| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
(* sigNew will collect all enhancements and then we need to apply sigOld (+) sigEnhancedEffects *)
in let (result,success,sigEnhancedEffects, otherValues) = if vinfo.vglob = true then (print_string "There is a global on the left side, so no evaluation\n"; nopVal SigmaMap.empty) else eval_helper rval 
in
(* otherValues needs to be propagated still, e.g. global = x < 3 still poses new information on x *)
let sigNew = NodeImpl.destruct_add_sigma sigOld sigEnhancedEffects
in if vinfo.vglob = true then (sigOld, otherValues) else
if success then (SigmaMap.add vinfo result sigNew, otherValues)  else (print_string "Eval could not evaluate expression\n"; exit 0)

let eval_catch_exceptions sigOld vinfo rval graph node =
  try (eval sigOld vinfo rval graph node, true) with 
  Division_by_zero_Int -> Messages.warn "Contains a trace with division by zero"; ((SigmaMap.add vinfo Error sigOld, []) ,false)
  | Overflow_addition_Int -> Messages.warn "Contains a trace with overflow of Integer addition";((SigmaMap.add vinfo Error sigOld, []) ,false)
  | Underflow_subtraction_Int -> Messages.warn "Contains a trace with underflow of Integer subtraction"; ((SigmaMap.add vinfo Error sigOld, []) ,false)

(* Here, I would like to manage other generated values and return a set of sigmas *)
let eval_wrapper sigOld vinfo rval graph node =
  let rec iter_otherValues doneValues workList sigmaList =
match workList with 
(var,vd)::xs -> if List.mem (var,vd) doneValues then iter_otherValues doneValues xs sigmaList
else (let sigTmp = SigmaMap.add var vd sigOld
in 
let (anotherSig, moreValues), success_other = eval_catch_exceptions sigTmp vinfo rval graph node
in 
let newWorkList = List.fold (fun acc value -> if List.mem value acc then acc else value::acc) workList moreValues
in iter_otherValues ((var,vd)::doneValues) newWorkList (if (List.exists (fun sigma_exists -> NodeImpl.equal_sigma sigma_exists anotherSig) sigmaList) then sigmaList else (anotherSig::sigmaList)))
  | [] -> sigmaList
  in
  let (sigNew,otherValues), success = eval_catch_exceptions sigOld vinfo rval graph node 
in 
print_string ("in eval_wrapper we get otherValues={"
^(List.fold (fun s (vinfo_fold, varDom_fold) -> s^";("^(CilType.Varinfo.show vinfo_fold)^","^(show_valuedomain varDom_fold)^")") "" otherValues)^"}\n");
let allSigmas = iter_otherValues [] otherValues [sigNew]
in
print_string ("allSigmas:"^(List.fold (fun acc sigma_fold -> acc^"\n"^(NodeImpl.show_sigma sigma_fold)) "" allSigmas)^"\n");
sigNew, true

(* ASSIGN helper functions *)
(* perform assign-effect on given node *)
let assign_on_node graph ctx lval rval {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
  let (myEdge:(node * edge * node)) , success =  match lval with (Var x, _) ->
    let evaluated,success_inner = eval_wrapper sigma x rval  graph {programPoint=programPoint;sigma=sigma;id=id;tid=tid} in 
    print_string ("new sigma in assign: "^(NodeImpl.show_sigma evaluated )^"\n");

     (if Edge.equal ctx.edge Skip then ({programPoint=programPoint;sigma=sigma;id=id;tid=tid}, (Assign(lval,rval)),{programPoint=ctx.node;sigma=evaluated;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node evaluated);tid=tid})
    else ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=evaluated; id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node evaluated);tid=tid})), success_inner

    | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
  in
  let result_graph = 
  if success then (print_string ("assignment succeeded so we add the edge "^(LocalTraces.show_edge myEdge)^"\n");LocalTraces.extend_by_gEdge graph myEdge) 
  else (print_string "assignment did not succeed!\n"; LocalTraces.extend_by_gEdge graph ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty;id= -1;tid= -1}) )
in
    result_graph

(* iterate over the graphs in previous state *)
let assign_fold_graphSet graph (lval, rval, ctx,set_acc) =
  let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
   in
  let new_set = 
    (if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In assign, we have a trace that does not end in previous node:
    "^(LocalTraces.show graph)^"\n"); set_acc) else 
 let result_graph = 
  assign_on_node graph ctx lval rval lastNode
  in
  if (LocalTraces.equal graph result_graph)  then set_acc else D.add result_graph set_acc)
  in
  (lval, rval, ctx, new_set)

let assign ctx (lval:lval) (rval:exp) : D.t = 
  print_string ("assign wurde aufgerufen with lval "^(CilType.Lval.show lval)^" and rval "^(CilType.Exp.show rval)^" mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let _, _, _, result =
   D.fold assign_fold_graphSet ctx.local (lval, rval, ctx, D.empty ())
in result

(* BRANCH helper functions *)
(* perform branch-effect on given node *)
let branch_on_node graph ctx exp tv {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
  let branch_sigma = SigmaMap.add LocalTraces.branch_vinfo Error sigma 
in
print_string ("sigma = "^(NodeImpl.show_sigma sigma)^"; branch_sigma = "^(NodeImpl.show_sigma branch_sigma)^"\n");
let result_branch,success = eval_wrapper branch_sigma LocalTraces.branch_vinfo exp graph {programPoint=programPoint;sigma=sigma;id=id;tid=tid}
in
let result_as_int = match (SigmaMap.find_default Error LocalTraces.branch_vinfo result_branch) with
Int(i1,i2,_) -> print_string ("in branch, the result is ["^(Big_int_Z.string_of_big_int i1)^";"^(Big_int_Z.string_of_big_int i2)^"]");if (Big_int_Z.int_of_big_int i1 <= 0)&&(Big_int_Z.int_of_big_int i2 >= 0) then 0 
else 1
  |_ -> -1
in
let sigmaNew = SigmaMap.remove LocalTraces.branch_vinfo (NodeImpl.destruct_add_sigma sigma result_branch)
in
print_string ("result_as_int: "^(string_of_int result_as_int)^"\n");
let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigmaNew;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigmaNew);tid=tid})
in
print_string ("success="^(string_of_bool success)^", tv="^(string_of_bool tv)^", result_as_int="^(string_of_int result_as_int)^"\nand possible edge="^(LocalTraces.show_edge myEdge)^"\n");
let result_graph = if success&&((tv=true && result_as_int = 1)||(tv=false&&result_as_int=0) || (result_as_int= -1)) then LocalTraces.extend_by_gEdge graph myEdge else (print_string "no edge added for current sigma in branch\n";graph)
in
    result_graph

(* iterate over the graphs in previous state *)
let branch_fold_graphSet graph (exp, tv, ctx,set_acc) =
  let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
   in
  let new_set =
 (if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In branch, we have a trace that does not end in previous node:
  "^(LocalTraces.show graph)^"\n"); set_acc) else
 let result_graph = 
  branch_on_node graph ctx exp tv lastNode
  in
  if (LocalTraces.equal graph result_graph) then set_acc else D.add result_graph set_acc)
  in
  (exp, tv, ctx, new_set)

let branch ctx (exp:exp) (tv:bool) : D.t = print_string ("branch wurde aufgerufen mit exp="^(CilType.Exp.show exp)^" and tv="^(string_of_bool tv)^" mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let _, _, _, result =
   D.fold branch_fold_graphSet ctx.local (exp, tv, ctx, D.empty ())
in result

(* BODY helper functions *)
(* perform body-effect on given node *)
let body_on_node graph ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
  let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma);tid=tid})
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
  D.add (LocalTraces.extend_by_gEdge graph ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id= first_ID;tid=first_ID},ctx.edge,{programPoint=ctx.node;sigma=SigmaMap.empty; id= second_ID;tid=first_ID})) set_acc)
else set_acc) else
 let result_graph = 
  body_on_node graph ctx lastNode
  in
    D.add result_graph set_acc)
  in
  (ctx, new_set)

let body ctx (f:fundec) : D.t = 
  print_string ("body wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let _, result =
   D.fold body_fold_graphSet ctx.local (ctx, D.empty ())
in
 print_string ("Resulting state of body: "^(D.fold (fun foldGraph acc -> (LocalTraces.show foldGraph)^", "^acc) result "")^"\n");result
   
 (* RETURN helper functions *)
 (* perform return-effect on given node *)
 let return_on_node graph ctx exp {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
  let myEdge = (
match exp with 
| None ->  print_string "In return case None\n";
({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma);tid=tid})
| Some(ret_exp) -> ( print_string ("return expression: "^(CilType.Exp.show ret_exp)^"\n");
  match ret_exp with
  | CastE(TPtr(TVoid(_), attrList2),Const(CInt(cilint,IInt,_))) ->
    if Cilint.is_zero_cilint cilint then (
      let sigma_returnVinfo= SigmaMap.add LocalTraces.return_vinfo Error sigma in
      {programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma_returnVinfo;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma_returnVinfo);tid=tid}) else (print_string "In return, unsupported expression\n"; exit 0)
    | _ -> 
  let result, success = eval_wrapper sigma LocalTraces.return_vinfo ret_exp graph {programPoint=programPoint;sigma=sigma;id=id;tid=tid}
in if success = false then (print_string "Evaluation of return expression was unsuccessful\n"; exit 0)
else ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=result;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node result);tid=tid})
))
in
let result_graph = LocalTraces.extend_by_gEdge graph myEdge
in
    result_graph

 (* iterate over the graphs in previous state *)
 let return_fold_graphSet graph (exp, ctx,set_acc) =
  let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
   in
   let new_set = 
 if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In return, we have a trace that does not end in previous node:
  "^(LocalTraces.show graph)^"\n"); set_acc) else
 let result_graph = 
  return_on_node graph ctx exp lastNode
  in
  D.add result_graph set_acc
  in
  (exp, ctx, new_set)

let return ctx (exp:exp option) (f:fundec) : D.t = 
  print_string ("return wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
   let _, _, result = D.fold return_fold_graphSet ctx.local (exp, ctx, D.empty ())
in 
result

   (* SPECIAL helper functions *)
   (* perform special-effect on given node *)
  let special_on_node graph ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
    let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma);tid=tid})
  in
  let result_graph = LocalTraces.extend_by_gEdge graph myEdge
  in
      result_graph

   (* iterate over the graphs in previous state *)
   let special_fold_graphSet graph (ctx,set_acc) =
    let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
   in
   let new_set =
   if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In special, we have a trace that does not end in previous node:
    "^(LocalTraces.show graph)^"\n"); set_acc) else 
   let result_graph = 
    special_on_node graph ctx lastNode
    in
    D.add result_graph set_acc
    in
    (ctx, new_set)

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = 
  print_string ("special wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let _, result =   D.fold special_fold_graphSet ctx.local (ctx, D.empty ())
in result
    
(* ENTER helper functions *)
(* perform enter-effect on given node *)
let enter_on_node graph ctx f args {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
  let sigma_formals, _ = List.fold (
      fun (sigAcc, formalExp) formal -> (match formalExp with 
        | x::xs -> (let result, success = eval_wrapper sigAcc formal x graph {programPoint=programPoint;sigma=sigma;id=id;tid=tid}
  in if success = true then (result, xs) else (sigAcc, xs))
        | [] -> Printf.printf "Fatal error: missing expression for formals in enter\n"; exit 0)
      ) (sigma, args) f.sformals
    in print_string ("sigma_formals: "^(NodeImpl.show_sigma sigma_formals)^"\n");
      let myEdge = ({programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=(FunctionEntry(f));sigma=sigma_formals;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge (FunctionEntry(f)) sigma_formals);tid=tid})
  in
  let result_graph = LocalTraces.extend_by_gEdge graph myEdge
in
    result_graph

(* iterate over the graphs in previous state *)
let enter_fold_graphSet graph (f, args, ctx,set_acc) =
  let lastNode = LocalTraces.get_last_node_progPoint graph ctx.prev_node
   in
   let new_set = 
 if Node.equal lastNode.programPoint LocalTraces.error_node then (print_string ("In enter, we have a trace that does not end in previous node:
  "^(LocalTraces.show graph)^"\n"); set_acc) else
 let result_graph =
 enter_on_node graph ctx f args lastNode
  in
  D.add result_graph set_acc
  in
  (f, args,ctx, new_set)

   let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = 
    print_string ("enter wurde aufgerufen with function "^(CilType.Fundec.show f)^" with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^", edge label "^(EdgeImpl.show ctx.edge)^"
  with formals "^(List.fold (fun s sformal -> s^", "^(CilType.Varinfo.show sformal)) "" f.sformals)^" and arguments "^(List.fold (fun s exp -> s^", "^(CilType.Exp.show exp)) "" args)^"\n");
  if D.is_empty ctx.local then (
    let first_ID = idGenerator#increment()
in
let second_ID = idGenerator#increment()
in
    [ctx.local, D.add (LocalTraces.extend_by_gEdge (LocTraceGraph.empty) ({programPoint=ctx.prev_node;sigma=SigmaMap.empty;id=first_ID;tid=first_ID}, ctx.edge, {programPoint=(FunctionEntry(f));sigma=SigmaMap.empty;id= second_ID;tid=first_ID})) (D.empty ())] )
  else
  let _, _, _, result = print_string ("In enter, neuer state wird erstellt\n mit ctx.local: "^(D.show ctx.local)^" und |ctx.local| = "^(string_of_int (D.cardinal ctx.local))^"\n"); 
  D.fold enter_fold_graphSet ctx.local (f, args, ctx, D.empty ())
  in
    [ctx.local, result]  
  
(* COMBINE helper functions *)
(* perform combine-effect on node *)
let combine_on_node ctx {programPoint=programPoint;id=id;sigma=sigma;tid=tid} {programPoint=progP_returning;id=id_returning;sigma=sigma_returning;tid=tid_returning} callee_local lval graph =
  D.iter (fun g_iter -> print_string ("in combine, I test LocalTrace.find_returning_node with node="^(NodeImpl.show {programPoint=programPoint;id=id;sigma=sigma;tid=tid})^"\nI get "^(NodeImpl.show (LocalTraces.find_returning_node ({programPoint=programPoint;id=id;sigma=sigma;tid=tid}) ctx.edge g_iter))^"\n")) callee_local;
  if tid != tid_returning then (Printf.printf "TIDs from current node and found returning node are different in combine\n"; exit 0);
    let (myEdge:(node * edge * node)) = 
    (match lval with None -> {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning},Skip,{programPoint=ctx.node;sigma=sigma; id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid_returning} ctx.edge ctx.node sigma);tid=tid_returning}
     |Some (Var x, y) ->  if x.vglob 
      then ({programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning},Assign((Var(x), y), (Lval(Var(LocalTraces.return_vinfo),NoOffset))),{programPoint=ctx.node;sigma=sigma; id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid_returning} ctx.edge ctx.node sigma);tid=tid_returning}) 
    else
      (let return_value = SigmaMap.find LocalTraces.return_vinfo sigma_returning
      in if equal_varDomain return_value Error then (print_string "In combine, a returning Nullpointer is assigned to some lval, this is not supported\n";exit 0) else
      let result_sigma = SigmaMap.add x return_value sigma
  in {programPoint=progP_returning;sigma=sigma_returning;id=id_returning;tid=tid_returning},Skip,{programPoint=ctx.node;sigma=result_sigma; id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid_returning} ctx.edge ctx.node result_sigma);tid=tid_returning})
     | _ -> Printf.printf "Invalid Lval format in combine\n"; exit 0)
  in
  let result_graph = LocalTraces.extend_by_gEdge graph myEdge
in
result_graph


(* iterate over the graphs in previous state *)
let combine_fold_graphSet graph (lval, callee_local, ctx,set_acc) =
  (* wir müssen schauen, ob ein returnender Knoten für irgendein ctx.prev_node ein last node ist*)
  let lastGraphNode = LocalTraces.get_last_node graph 
in if Node.equal lastGraphNode.programPoint LocalTraces.error_node then (print_string "In combine, we have an empty graph\n"; ( lval, callee_local, ctx,set_acc)) 
else (
   let currentNode = LocalTraces.find_calling_node lastGraphNode graph ctx.prev_node
  in
   let new_set = 
    (if Node.equal currentNode.programPoint LocalTraces.error_node then (print_string ("In combine, we could not find the calling node:
      "^(LocalTraces.show graph)^"\n"); set_acc) else
 let result_graph= 
 combine_on_node ctx currentNode lastGraphNode callee_local lval graph
  in
  D.add result_graph set_acc)
  in
  ( lval, callee_local, ctx, new_set))

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t = 
    print_string ("combine wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^", edge label "^(EdgeImpl.show ctx.edge)^"
    und lval "^(match lval with None -> "None" | Some(l) -> CilType.Lval.show l)^" und fexp "^(CilType.Exp.show fexp)^"\n");
let _,_,_, result=
   D.fold combine_fold_graphSet (*ctx.local*) callee_local (lval, callee_local, ctx, D.empty ())
in result

   (* THREADENTER helper functions *)
   (* perform threadenter-effect on node *)
  let threadenter_on_node graph ctx f args {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
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
    let new_id = idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma_formals
    in
    let myEdge = {programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=(FunctionEntry(fd));sigma=sigma_formals;id=new_id;tid=new_id}
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
      print_string ("threadenter wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^", ctx.edge "^(EdgeImpl.show ctx.edge)^" und ctx.node "^(Node.show ctx.node)^"
    , lval "^(match lval with None -> "None" |Some(l) -> CilType.Lval.show l)^"
    \nund args={"^(List.fold (fun acc_fold exp_fold -> acc_fold^(CilType.Exp.show exp_fold)^"; ") "" args)^"}\n");
      let _, _, _, result = D.fold threadenter_fold_graphSet (ctx.local) (args, f, ctx, D.empty())
    in
      [result] (* Zustand von neuem Thread *)

    (* THREADSPAWN helper functions *)
   (* perform threadspawn-effect on given node *)
  let threadspawn_on_node graph ctx lval {programPoint=programPoint;id=id;sigma=sigma;tid=tid} =
    let myEdge =  (match lval with None ->
    {programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma);tid=tid}
  |Some((Var x, _)) -> (
    let result_sigma = SigmaMap.add x (Int(Big_int_Z.big_int_of_int id, Big_int_Z.big_int_of_int id, IInt)) sigma
    in 
    {programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=result_sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node result_sigma);tid=tid}
  )
  | Some(z) -> {programPoint=programPoint;sigma=sigma;id=id;tid=tid},ctx.edge,{programPoint=ctx.node;sigma=sigma;id=(idGenerator#getID {programPoint=programPoint;sigma=sigma;id=id;tid=tid} ctx.edge ctx.node sigma);tid=tid}
    (* print_string ("Invalid Lval format in threadspawn. Lval="^(CilType.Lval.show z)^"\n"); exit 0 *)
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
      print_string ("threadspawn wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^", ctx.edge "^(EdgeImpl.show ctx.edge)^" und ctx.node "^(Node.show ctx.node)^"\n");
      let _, _, result = D.fold threadspawn_fold_graphSet (ctx.local) (lval, ctx, D.empty())
    in
    result
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)