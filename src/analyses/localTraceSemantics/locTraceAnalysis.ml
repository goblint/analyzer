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

(* Evaluates the effects of an assignment to sigma *)
(* TODO eval needs to be checked on overflow and div by 0 --> custom exception managment could be useful *)
let eval sigOld vinfo (rval: exp) graph node = 
  (* dummy value 
     This is used whenever an expression contains a variable that is not in sigma (e.g. a global)
      In this case vinfo is considered to have an unknown value *)
  let nopVal sigSide = (Int((Big_int_Z.big_int_of_int (-13)), (Big_int_Z.big_int_of_int (-13)),IInt), false, sigSide)

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
  (if Big_int_Z.lt_big_int u1 l2 then Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, ik) 
  else if Big_int_Z.le_big_int u2 l1 then Int(Big_int_Z.zero_big_int,Big_int_Z.zero_big_int, ik)
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
| IInt -> if c < Big_int_Z.big_int_of_int intMin then (Int (Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int intMin, ik), true,SigmaMap.empty) 
else if c > Big_int_Z.big_int_of_int intMax then (Int (Big_int_Z.big_int_of_int intMax,Big_int_Z.big_int_of_int intMax, ik), true,SigmaMap.empty) else (Int (c,c, ik), true,SigmaMap.empty)
| _ ->  Printf.printf "This type of assignment is not supported\n"; exit 0 )
| Lval(Var(var), NoOffset) -> if var.vglob = true 
  then (let result = LocalTraces.find_globvar_assign_node var graph node in 
print_string ("find_globvar_assign_node wurde aufgerufen, es wurde folgender Knoten gefunden: "^(NodeImpl.show result)^"\n"); nopVal SigmaMap.empty)
else
  (if SigmaMap.mem var sigOld then ((SigmaMap.find var sigOld), true,SigmaMap.empty) 
else (print_string ("var="^(CilType.Varinfo.show var)^" not found in sigOld="^(NodeImpl.show_sigma sigOld)^"\nnopVal created at Lval\n");nopVal SigmaMap.empty))
| AddrOf (Var(v), NoOffset) -> (Address(v), true,SigmaMap.empty)

(* unop expressions *)
(* for type Integer *)
| UnOp(Neg, unopExp, TInt(unopIk, _)) -> 
  (match eval_helper unopExp with (Int(l,u,ik), true, sigSide) ->
    (match ik with 
    |IInt -> if CilType.Ikind.equal unopIk IInt then( let negLowerBound = (if Big_int_Z.minus_big_int u < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
    else if Big_int_Z.minus_big_int u > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int u )
  in
  let negUpperBound = (if Big_int_Z.minus_big_int l < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
  else if Big_int_Z.minus_big_int l > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int l )
in
       (Int (negLowerBound, negUpperBound, unopIk), true, sigSide)) else (Printf.printf "This type of assignment is not supported\n"; exit 0)
    | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
    |(_, false, sigSide) -> print_string "nopVal created at unop Neg for Int\n";nopVal SigmaMap.empty
    |(_, _,_) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 

(* binop expressions *)
(* Lt could be a special case since it has side-effects on sigma *)
(* in var1 < var2 case, I have not yet managed boundary cases, so here are definitely some bugs *)
| BinOp(Lt, Lval(Var(var1), NoOffset),Lval(Var(var2), NoOffset),TInt(biopIk, _)) ->(
  if (SigmaMap.mem var1 sigOld)&&(SigmaMap.mem var2 sigOld)
    then (
      match (SigmaMap.find var1 sigOld),(SigmaMap.find var2 sigOld) with
      | (Int(l1,u1,k1)), (Int(l2,u2,k2)) -> if not (CilType.Ikind.equal k1 k2) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
      if (u1 < l2) || (u2 <= l1) then ((get_binop_int Lt biopIk) (Int(l1, u1, k1)) (Int(l2, u2, k2)), true , SigmaMap.empty) 
      else
        (* overlap split *)
      (if (l1 < u1)&&(l2 < u2) then (let m = middle_intersect_intervals l1 u1 l2 u2
      in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(Big_int_Z.add_big_int m (Big_int_Z.big_int_of_int 1), u2, k2)) (SigmaMap.add var1 (Int(l1,m, k1)) sigOld)))
      else if (l1 = u1)&&(l2 < u2) then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(Big_int_Z.add_big_int l1 (Big_int_Z.big_int_of_int 1), u2, k2)) (SigmaMap.add var1 (Int(l1,l1, k1)) sigOld))
      else if (l1 < u1) &&(l2 = u2) then (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true,SigmaMap.add var2 (Int(l2, l2, k2)) (SigmaMap.add var1 (Int(l1,Big_int_Z.sub_big_int l2 (Big_int_Z.big_int_of_int 1), k1)) sigOld))
      else (print_string "in overlap split there are two points, this should never happen actually\n";Printf.printf "This type of assignment is not supported\n"; exit 0))
      | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
    )
    else (print_string "nopVal created at binop Lt of two variables. One of them or both are unknown\n";nopVal SigmaMap.empty)
    )

| BinOp(Lt, binopExp1,Lval(Var(var), NoOffset),TInt(biopIk, _)) ->(
match eval_helper binopExp1 with 
| (Int(l, u, k), true, sigSide) -> (
  if SigmaMap.mem var sigSide then
    (match SigmaMap.find var sigSide with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar <= l || u < lVar then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigSide) 
        else (* TODO design meaningful logic here *) ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigSide)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
  else if SigmaMap.mem var sigOld then
    (match SigmaMap.find var sigOld with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar <= l || u < lVar then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigSide) 
        else (* TODO design meaningful logic here *) ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , sigSide)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
  else ( 
    let sigTmp = NodeImpl.destruct_add_sigma sigSide (SigmaMap.add var (Int(Big_int_Z.add_big_int u (Big_int_Z.big_int_of_int 1), Big_int_Z.big_int_of_int intMax, k)) (SigmaMap.empty))
in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, sigTmp)
    )
)
| (_,false, sigSide) -> nopVal sigSide
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
)

| BinOp(Lt, Lval(Var(var), NoOffset), binopExp2,TInt(biopIk, _)) -> (
  match eval_helper binopExp2 with 
  | (Int(l, u, k), true, sigSide) -> (
    if SigmaMap.mem var sigSide then 
      (match SigmaMap.find var sigSide with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigSide) 
        else (* TODO design meaningful logic here *) ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigSide)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

    else if SigmaMap.mem var sigOld then
      (match SigmaMap.find var sigOld with Int(lVar, uVar, kVar) -> if not (CilType.Ikind.equal k kVar) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
        if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigSide) 
        else (* TODO design meaningful logic here *) ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , sigSide)
        | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

    else ( 
      let sigTmp = NodeImpl.destruct_add_sigma sigSide (SigmaMap.add var (Int(Big_int_Z.big_int_of_int intMin, Big_int_Z.sub_big_int l (Big_int_Z.big_int_of_int 1), k)) (SigmaMap.empty))
  in (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, sigTmp)
      ))
  | (_,false, sigSide) -> nopVal sigSide
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

(* for type Integer *)
| BinOp(op, binopExp1, binopExp2,TInt(biopIk, _)) ->
  (match (eval_helper binopExp1, eval_helper binopExp2) with 
  | ((Int(l1,u1, ik1), true,sigSide1),(Int(l2,u2, ik2), true,sigSide2)) -> if CilType.Ikind.equal ik1 ik2 then ((get_binop_int op biopIk) (Int(l1,u1, ik1)) (Int(l2,u2, ik2)), true, NodeImpl.intersect_sigma sigSide1 sigSide2) else (Printf.printf "This type of assignment is not supported\n"; exit 0)
  | ((_,_,sigSide1), (_,false, sigSide2)) -> print_string "nopVal created at binop for Integer 1\n";nopVal (NodeImpl.intersect_sigma sigSide1 sigSide2)
  | ((_,false, sigSide1), (_,_,sigSide2)) -> print_string "nopVal created at binop for Integer 2\n";nopVal (NodeImpl.intersect_sigma sigSide1 sigSide2)
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 

| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
(* sigNew will collect all side-effects and then we need to apply sigOld (+) sigSideEffects *)
in let tmp = eval_helper rval 
in let (result,success,sigSideEffects) = if vinfo.vglob = true then (print_string "There is a global on the left side, so no evaluation\n"; nopVal SigmaMap.empty) else tmp 
in
let sigNew = NodeImpl.destruct_add_sigma sigOld sigSideEffects
in 
if success then SigmaMap.add vinfo result sigNew  else (print_string "Sigma has not been updated. Vinfo is removed\n"; SigmaMap.remove vinfo sigNew)

(* TODO output corresponding nodes in addition s.t. the edge is unique *)
let eval_catch_exceptions sigOld vinfo rval stateEdge graph node =
try (eval sigOld vinfo rval graph node, true) with 
Division_by_zero_Int -> Messages.warn "Contains a trace with division by zero"; (SigmaMap.add vinfo Error sigOld ,false)
| Overflow_addition_Int -> Messages.warn "Contains a trace with overflow of Integer addition";(SigmaMap.add vinfo Error sigOld ,false)
| Underflow_subtraction_Int -> Messages.warn "Contains a trace with underflow of Integer subtraction"; (SigmaMap.add vinfo Error sigOld ,false)

let assign ctx (lval:lval) (rval:exp) : D.t = 
  print_string ("assign wurde aufgerufen with lval "^(CilType.Lval.show lval)^" and rval "^(CilType.Exp.show rval)^" mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let assign_helper graph sigma =
  (let myEdge, success =  match lval with (Var x, _) ->
    let evaluated,success_inner = eval_catch_exceptions sigma x rval ctx.edge graph {programPoint=ctx.prev_node;sigma=sigma} in 
    print_string ("new sigma in assign: "^(NodeImpl.show_sigma evaluated )^"\n");
     ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=evaluated}), success_inner
    | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
  in
  if success then (print_string ("assignment succeeded so we add the edge "^(LocalTraces.show_edge myEdge)^"\n");LocalTraces.extend_by_gEdge graph myEdge) 
  else (print_string "assignment did not succeed!\n"; LocalTraces.extend_by_gEdge graph ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=LocalTraces.error_node ;sigma=SigmaMap.empty}) )
  )
in
let tmp = List.fold assign_helper g oldSigma 
in
if LocalTraces.equal g tmp then set else
  D.add tmp set 
in
let tmp2 =
   D.fold fold_helper ctx.local (D.empty ())
in if D.is_empty tmp2 then (*D.add LocTraceGraph.empty*) D.empty () else tmp2
  
let branch ctx (exp:exp) (tv:bool) : D.t = print_string ("branch wurde aufgerufen mit exp="^(CilType.Exp.show exp)^" and tv="^(string_of_bool tv)^" mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let branch_vinfo = makeVarinfo false "__goblint__traces__branch" (TInt(IInt,[]))
in
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let branch_helper graph sigma =
(let branch_sigma = SigmaMap.add branch_vinfo Error sigma 
in
print_string ("oldSigma = "^(NodeImpl.show_sigma sigma)^"; branch_sigma = "^(NodeImpl.show_sigma branch_sigma)^"\n");
let result_branch,success = eval_catch_exceptions branch_sigma branch_vinfo exp ctx.edge graph {programPoint=ctx.prev_node;sigma=sigma}
in
let result_as_int = match (SigmaMap.find_default Error branch_vinfo result_branch) with
Int(i1,i2,_) -> if (Big_int_Z.int_of_big_int i1 <= 0)&&(Big_int_Z.int_of_big_int i2 >= 0) then 0 
else 1
  |_ -> -1
in
let sigmaNew = SigmaMap.remove branch_vinfo (NodeImpl.destruct_add_sigma sigma result_branch)
in
print_string ("result_as_int: "^(string_of_int result_as_int)^"\n");
let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigmaNew})
in
if success&&((tv=true && result_as_int = 1)||(tv=false&&result_as_int=0) || (result_as_int= -1)) then LocalTraces.extend_by_gEdge graph myEdge else (print_string "no edge added for current sigma in branch\n";graph))
in
let tmp = List.fold branch_helper g oldSigma 
in
if LocalTraces.equal g tmp then (print_string "No changes on graph in branch, thus this one is not added\n";set) else
  D.add tmp set 
in
let tmp2 =
   D.fold fold_helper ctx.local (D.empty ())
in if D.is_empty tmp2 then (*D.add LocTraceGraph.empty*) D.empty () else tmp2

let body ctx (f:fundec) : D.t = 
  print_string ("body wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let body_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold body_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())
      
let return ctx (exp:exp option) (f:fundec) : D.t = 
  print_string ("return wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
  let return_vinfo = makeVarinfo false "__goblint__traces__return" (TInt(IInt,[]))
in
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let return_helper graph sigma =(
  let myEdge = (
match exp with 
| None ->  
({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
| Some(ret_exp) -> (
  let result, success = eval_catch_exceptions sigma return_vinfo ret_exp ctx.edge graph {programPoint=ctx.prev_node;sigma=sigma}
in if success = false then ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
else ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=result})
))
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold return_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = 
  print_string ("special wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let special_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold special_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())
    

   let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = 
    print_string ("enter wurde aufgerufen with function "^(CilType.Fundec.show f)^" with ctx.prev_node "^(Node.show ctx.prev_node)^" and ctx.node "^(Node.show ctx.node)^" 
  with formals "^(List.fold (fun s sformal -> s^", "^(CilType.Varinfo.show sformal)) "" f.sformals)^" and arguments "^(List.fold (fun s exp -> s^", "^(CilType.Exp.show exp)) "" args)^"\n");
  let fold_helper g set = print_string "fold_helper wurde aufgerufen\n";
  let oldSigma = LocalTraces.get_sigma g ctx.prev_node (* Debuggen, gucken was rauskommt, und ob iteriert wird *)
  in
  if List.is_empty oldSigma then print_string("In enter, oldSigma-list is empty\n") else print_string("In enter, oldSigma-list is not empty\n");
  let enter_helper graph sigma = print_string "enter_helper wurde aufgerufen\n";
    (let sigma_formals, _ = List.fold (
      fun (sigAcc, formalExp) formal -> (match formalExp with 
        | x::xs -> (let result, success = eval_catch_exceptions sigAcc formal x ctx.edge graph {programPoint=ctx.prev_node;sigma=sigma}
  in if success = true then (result, xs) else (sigAcc, xs))
        | [] -> Printf.printf "Fatal error: missing expression for formals in enter\n"; exit 0)
      ) (sigma, args) f.sformals
    in print_string ("sigma_formals: "^(NodeImpl.show_sigma sigma_formals)^"\n");
      let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=(FunctionEntry(f));sigma=sigma_formals})
  in LocalTraces.extend_by_gEdge graph myEdge)
  in
  let tmp = (List.fold enter_helper g oldSigma) 
  in
  print_string ("new graph is "^(LocalTraces.show tmp)^"\n");
    D.add (tmp) set 
  in
  if D.is_empty ctx.local then [ctx.local, D.add (LocalTraces.extend_by_gEdge (LocTraceGraph.empty) ({programPoint=ctx.prev_node;sigma=SigmaMap.empty}, ctx.edge, {programPoint=(FunctionEntry(f));sigma=SigmaMap.empty})) (D.empty ())] else
  let state = print_string ("In enter, neuer state wird erstellt\n mit ctx.local: "^(D.show ctx.local)^" und |ctx.local| = "^(string_of_int (D.cardinal ctx.local))^"\n"); D.fold fold_helper ctx.local (D.empty ())
  in
    [ctx.local, state]  
  

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t = 
    print_string ("combine wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"
    und lval "^(match lval with None -> "None" | Some(l) -> CilType.Lval.show l)^" und fexp "^(CilType.Exp.show fexp)^"\n");
  let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let combine_helper graph sigma =(
let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold combine_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())

   
    let threadenter ctx lval f args = 
      print_string ("threadenter wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
      [D.top ()]
    let threadspawn ctx lval f args fctx = 
      print_string ("threadspawn wurde aufgerufen mit ctx.prev_node "^(Node.show ctx.prev_node)^" und ctx.node "^(Node.show ctx.node)^"\n");
    ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
