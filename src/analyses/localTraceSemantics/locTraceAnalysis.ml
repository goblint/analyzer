open Prelude.Ana
open Analyses
open LocTraceDS
  
(* Custom exceptions for eval-function *)
exception Division_by_zero_Int
exception Division_by_zero_Float
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
let eval sigOld vinfo (rval: exp) = 
  (* dummy value 
     This is used whenever an expression contains a variable that is not in sigma (e.g. a global)
      In this case vinfo is considered to have an unknown value *)
  let nopVal = (Int((Big_int_Z.big_int_of_int (-13)), (Big_int_Z.big_int_of_int (-13)),IInt), false)

  (* returns a function which calculates [l1, u1] OP [l2, u2]*)
in let get_binop_int op ik = if not (CilType.Ikind.equal ik IInt) then (Printf.printf "This type of assignment is not supported\n"; exit 0) else 
(match op with 
| PlusA -> 
fun x1 x2 -> (match (x1,x2) with 
(Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported\n"; exit 0); (if (Big_int_Z.add_big_int u1 u2 > Big_int_Z.big_int_of_int intMax) then raise Overflow_addition_Int else Int(Big_int_Z.add_big_int l1 l2, Big_int_Z.add_big_int u1 u2, ik))
| _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

| MinusA -> 
fun x1 x2 -> (match (x1,x2) with (* Minus sollte negieren dann addieren sein, sonst inkorrekt!! *)
(Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported\n"; exit 0);
  (let neg_second_lower = Big_int_Z.minus_big_int u2
in let neg_second_upper = Big_int_Z.minus_big_int l2
in if (Big_int_Z.add_big_int u1 neg_second_upper > Big_int_Z.big_int_of_int intMax) then raise Overflow_addition_Int else Int(Big_int_Z.add_big_int l1 neg_second_lower, Big_int_Z.add_big_int u1 neg_second_upper, ik))
| _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

| Lt -> 
  fun x1 x2 -> (match (x1,x2) with 
  | (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((CilType.Ikind.equal ik1 IInt) && (CilType.Ikind.equal ik2 IInt)) then (Printf.printf "This type of assignment is not supported\n"; exit 0);(if Big_int_Z.lt_big_int u1 l2 then Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, ik) 
  else if Big_int_Z.le_big_int u2 l1 then Int(Big_int_Z.zero_big_int,Big_int_Z.zero_big_int, ik) else Int(Big_int_Z.zero_big_int,Big_int_Z.big_int_of_int 1, ik))

  | (Float(fl1,fu1,_)),(Float(fl2,fu2,_)) -> if fu1 < fl2 then Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, ik)
  else if fu2 <= fl1 then Int(Big_int_Z.zero_big_int,Big_int_Z.zero_big_int, ik) else Int(Big_int_Z.zero_big_int,Big_int_Z.big_int_of_int 1, ik)
  | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0 )
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

in let get_binop_float op fk = if not (CilType.Fkind.equal fk FFloat) then (Printf.printf "This type of assignment is not supported\n"; exit 0) else
  (match op with 
  | PlusA -> 
    fun x1 x2 -> (match (x1,x2) with
    (Float(fLower1, fUpper1, _)),(Float(fLower2, fUpper2, _)) -> Float(fLower1 +. fLower2, fUpper1 +. fUpper2, fk)
    | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
  | MinusA -> 
    fun x1 x2 -> (match (x1,x2) with
    (Float(fLower1, fUpper1, _)),(Float(fLower2, fUpper2, _)) -> Float(fLower1 -. fUpper2, fUpper1 -. fLower2, fk)
    | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in
  let rec eval_helper subexp =
  (match subexp with
| Const(CInt(c, ik, _)) -> (match ik with
| IInt -> if c < Big_int_Z.big_int_of_int intMin then (Int (Big_int_Z.big_int_of_int intMin,Big_int_Z.big_int_of_int intMin, ik), true) 
else if c > Big_int_Z.big_int_of_int intMax then (Int (Big_int_Z.big_int_of_int intMax,Big_int_Z.big_int_of_int intMax, ik), true) else (Int (c,c, ik), true)
| _ ->  Printf.printf "This type of assignment is not supported\n"; exit 0 )
| Const(CReal(f, fk, _)) -> (Float (f, f, fk), true)
| Lval(Var(var), NoOffset) -> if SigmaMap.mem var sigOld then ((SigmaMap.find var sigOld), true) else (print_string ("var="^(CilType.Varinfo.show var)^" not found in sigOld="^(NodeImpl.show_sigma sigOld)^"\nnopVal created at Lval\n");nopVal)
| AddrOf (Var(v), NoOffset) -> (Address(v), true)

(* unop expressions *)
(* for type Integer *)
| UnOp(Neg, unopExp, TInt(unopIk, _)) -> 
  (match eval_helper unopExp with (Int(l,u,ik), true) ->
    (match ik with 
    |IInt -> if CilType.Ikind.equal unopIk IInt then let negLowerBound = (if Big_int_Z.minus_big_int u < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
    else if Big_int_Z.minus_big_int u > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int u )
  in
  let negUpperBound = (if Big_int_Z.minus_big_int l < Big_int_Z.big_int_of_int intMin then Big_int_Z.big_int_of_int intMin
  else if Big_int_Z.minus_big_int l > Big_int_Z.big_int_of_int intMax then Big_int_Z.big_int_of_int intMax else Big_int_Z.minus_big_int l )
in
       (Int (negLowerBound, negUpperBound, unopIk), true) else (Printf.printf "This type of assignment is not supported\n"; exit 0)
    | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
    |(_, false) -> print_string "nopVal created at unop Neg for Int\n";nopVal
    |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 
(* for type float *)
| UnOp(Neg, unopExp, TFloat(unopFk, _)) -> 
      (match eval_helper unopExp with (Float(fLower, fUpper, _), true) -> (Float(-. fUpper,-. fLower, unopFk), true)
        | (_, false) -> print_string "nopVal created at unop Neg for Float\n";nopVal
        |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0)     

(* binop expressions *)
(* for type Integer *)
| BinOp(op, binopExp1, binopExp2,TInt(biopIk, _)) ->
  (match (eval_helper binopExp1, eval_helper binopExp2) with 
  | ((Int(l1,u1, ik1), true),(Int(l2,u2, ik2), true)) -> if CilType.Ikind.equal ik1 ik2 then ((get_binop_int op biopIk) (Int(l1,u1, ik1)) (Int(l2,u2, ik2)), true) else (print_string "nopVal is created at binop for int with different ikinds\n";nopVal)
  | ((Float(fLower1, fUpper1, fk1), true),(Float(fLower2, fUpper2, fk2), true)) -> if CilType.Fkind.equal fk1 fk2 then ((get_binop_int op biopIk) (Float(fLower1,fUpper1, fk1)) (Float(fLower2,fUpper2, fk2)), true) else(print_string "nopVal is created at binop for int with different fkinds\n";nopVal)
  | (_, (_,false)) -> print_string "nopVal created at binop for Integer 1\n";nopVal
  | ((_,false), _) -> print_string "nopVal created at binop for Integer 2\n";nopVal
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 

  (* for type Float *)
  | BinOp(op, binopExp1, binopExp2,TFloat(biopFk, _)) ->
    (match (eval_helper binopExp1, eval_helper binopExp2) with 
    | ((Float(fLower1, fUpper1, fk1), true),(Float(fLower2, fUpper2, fk2), true)) -> if CilType.Fkind.equal fk1 fk2 then ((get_binop_float op biopFk) (Float(fLower1,fUpper1, fk1)) (Float(fLower2,fUpper2, fk2)), true) else(print_string "nopVal is created at binop for int with different fkinds\n";nopVal)
    | (_, (_,false)) -> print_string "nopVal created at binop for Float 1\n";nopVal
  | ((_,false), _) -> print_string "nopVal created at binop for Float 2\n";nopVal
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0)
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in let (result,success) = eval_helper rval 
in if success then SigmaMap.add vinfo result sigOld else (print_string "Sigma has not been updated. Vinfo is removed\n"; SigmaMap.remove vinfo sigOld)

(* TODO output corresponding nodes in addition s.t. the edge is unique *)
let eval_catch_exceptions sigOld vinfo rval stateEdge =
try (eval sigOld vinfo rval, true) with 
Division_by_zero_Int -> print_string ("The CFG edge ["^(EdgeImpl.show stateEdge)^"] definitely contains an Integer division by zero.\n"); (SigmaMap.add vinfo Error sigOld ,false)
| Division_by_zero_Float -> print_string ("The CFG edge ["^(EdgeImpl.show stateEdge)^"] definitely contains a Float division by zero.\n"); (SigmaMap.add vinfo Error sigOld ,false)
| Overflow_addition_Int -> print_string ("The CFG edge ["^(EdgeImpl.show stateEdge)^"] definitely contains an Integer addition that overflows.\n"); (SigmaMap.add vinfo Error sigOld ,false)
| Underflow_subtraction_Int -> print_string ("The CFG edge ["^(EdgeImpl.show stateEdge)^"] definitely contains an Integer subtraction that underflows.\n"); (SigmaMap.add vinfo Error sigOld ,false)
| Overflow_multiplication_Int -> print_string ("The CFG edge ["^(EdgeImpl.show stateEdge)^"] definitely contains an Integer multiplication that overflows.\n"); (SigmaMap.add vinfo Error sigOld ,false)
| Underflow_multiplication_Int -> print_string ("The CFG edge ["^(EdgeImpl.show stateEdge)^"] definitely contains an Integer multiplication that underflows.\n"); (SigmaMap.add vinfo Error sigOld ,false)

let assign ctx (lval:lval) (rval:exp) : D.t = Printf.printf "assign wurde aufgerufen\n";
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let assign_helper graph sigma =
  (let myEdge, success =  match lval with (Var x, _) ->
    let evaluated,success_inner = eval_catch_exceptions sigma x rval ctx.edge in 
    print_string ("new sigma in assign: "^(NodeImpl.show_sigma evaluated )^"\n");
     ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=evaluated}), success_inner
    | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
  in
  if success then (print_string ("assignment succeeded so we add the edge "^(LocalTraces.show_edge myEdge)^"\n");LocalTraces.extend_by_gEdge graph myEdge) else (print_string "assignment did not succeed!\n"; graph)
  )
in
let tmp = List.fold assign_helper g oldSigma 
in
if LocalTraces.equal g tmp then set else
  D.add tmp set 
in
let tmp2 =
   D.fold fold_helper ctx.local (D.empty ())
in if D.is_empty tmp2 then D.add LocTraceGraph.empty (D.empty ()) else tmp2
  
let branch ctx (exp:exp) (tv:bool) : D.t = print_string ("branch wurde aufgerufen mit exp="^(CilType.Exp.show exp)^" and tv="^(string_of_bool tv)^" \n");
let branch_vinfo = makeVarinfo false "__goblint__traces__branch" (TInt(IInt,[]))
in
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let branch_helper graph sigma =
(let branch_sigma = SigmaMap.add branch_vinfo (Int(Big_int_Z.zero_big_int,Big_int_Z.zero_big_int,IInt)) sigma 
in
print_string ("oldSigma = "^(NodeImpl.show_sigma sigma)^"; branch_sigma = "^(NodeImpl.show_sigma branch_sigma)^"\n");
let result_branch,success = eval_catch_exceptions branch_sigma branch_vinfo exp ctx.edge
in
let result_as_int = match (SigmaMap.find_default Error branch_vinfo result_branch) with
Int(i1,i2,_) -> if (Big_int_Z.int_of_big_int i1 <= 0)&&(Big_int_Z.int_of_big_int i2 >= 0) then 0 
else 1
  |_ -> -1
in
let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in
if success&&((tv=true && result_as_int = 1)||(tv=false&&result_as_int=0) || (result_as_int= -1)) then LocalTraces.extend_by_gEdge graph myEdge else graph)
in
let tmp = List.fold branch_helper g oldSigma 
in
if LocalTraces.equal g tmp then set else
  D.add tmp set 
in
let tmp2 =
   D.fold fold_helper ctx.local (D.empty ())
in if D.is_empty tmp2 then D.add LocTraceGraph.empty (D.empty ()) else tmp2

let body ctx (f:fundec) : D.t = Printf.printf "body wurde aufgerufen\n";
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let body_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold body_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())
      
let return ctx (exp:exp option) (f:fundec) : D.t = Printf.printf "return wurde aufgerufen\n";
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let return_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold return_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = Printf.printf "special wurde aufgerufen\n";
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let special_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold special_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())
    
let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = Printf.printf "enter wurde aufgerufen\n";
let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let enter_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold enter_helper g oldSigma) set 
in
let state =   D.fold fold_helper ctx.local (D.empty ())
in
  [ctx.local, state]  
  

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t = Printf.printf "combine wurde aufgerufen\n";
  let fold_helper g set = let oldSigma = LocalTraces.get_sigma g ctx.prev_node
in
let combine_helper graph sigma =(let myEdge = ({programPoint=ctx.prev_node;sigma=sigma},ctx.edge,{programPoint=ctx.node;sigma=sigma})
in LocalTraces.extend_by_gEdge graph myEdge)
in
  D.add (List.fold combine_helper g oldSigma) set 
in
   D.fold fold_helper ctx.local (D.empty ())

    let threadenter ctx lval f args = Printf.printf "threadenter wurde aufgerufen\n";[D.top ()]
    let threadspawn ctx lval f args fctx = Printf.printf "threadspawn wurde aufgerufen\n";ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
