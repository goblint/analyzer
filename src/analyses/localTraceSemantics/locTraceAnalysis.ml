open Prelude.Ana
open Analyses
open LocTraceDS
  
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
in if D.is_empty tmp then (Printf.printf "Obwohl leerer Graph hinzugefügt, ist Ergebnis immer noch leer\n"; tmp) else tmp


let exitstate = startstate

(* Evaluates the effects of an assignment to sigmar *)
(* TODO eval needs to be checked on overflow and div by 0 --> custom exception managment could be useful *)
let eval sigOld vinfo (rval: exp) = 
  (* dummy value 
     This is used whenever an expression contains a variable that is not in sigmar (e.g. a global)
      In this case vinfo is considered to have an unknown value *)
  let nopVal = (Int((Big_int_Z.big_int_of_int (-13)),IInt), false) 
in let get_binop_int op =
(match op with 
| PlusA -> Big_int_Z.add_big_int
| MinusA -> Big_int_Z.sub_big_int
| Mult -> Big_int_Z.mult_big_int
| Div -> Big_int_Z.div_big_int
| Mod -> Big_int_Z.mod_big_int
| Shiftlt -> fun x1 x2 -> Big_int_Z.shift_left_big_int x1 (Big_int_Z.int_of_big_int x2)
| Shiftrt -> fun x1 x2 -> Big_int_Z.shift_right_big_int x1 (Big_int_Z.int_of_big_int x2)
| Lt -> fun x1 x2 -> if x1 < x2 then Big_int_Z.big_int_of_int 1 else Big_int_Z.big_int_of_int 0
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in
let get_binop_float op =
  (match op with 
  | PlusA -> Float.add
  | MinusA -> Float.sub
  | Mult -> Float.mul
  | Div -> Float.div
  | Mod -> Float.modulo
  (*| Lt -> fun x1 x2 -> if x1 < x2 then 1. else 0. *)
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in
  let rec eval_helper subexp =
  (match subexp with
| Const(CInt(c, ik, _)) -> (Int (c, ik), true)
| Const(CReal(f, fk, _)) -> (Float (f, fk), true)
| Lval(Var(var), NoOffset) -> if SigmarMap.mem var sigOld then ((SigmarMap.find var sigOld), true) else nopVal
| AddrOf (Var(v), NoOffset) -> (Address(v), true)
| UnOp(Neg, unopExp, TInt(unopIk, _)) -> 
  (match eval_helper unopExp with (Int(i,_), true) ->(Int (Big_int_Z.minus_big_int i, unopIk), true)
    |(_, false) -> nopVal
    |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 
| UnOp(Neg, unopExp, TFloat(unopFk, _)) -> 
      (match eval_helper unopExp with (Float(f, _), true) -> (Float(-. f, unopFk), true)
        | (_, false) -> nopVal
        |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0)     
|UnOp(LNot, unopExp,TInt(unopIk, _)) -> 
  (match eval_helper unopExp with (Int(i,_), true) -> (Int(Big_int_Z.big_int_of_int (lnot (Big_int_Z.int_of_big_int i)), unopIk), true)
  | (_, false) -> nopVal
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0)
| BinOp(op, binopExp1, binopExp2,TInt(unopIk, _)) ->
  (match (eval_helper binopExp1, eval_helper binopExp2) with ((Int(i1, ik1), true),(Int(i2, ik2), true)) -> if CilType.Ikind.equal ik1 ik2 then (Int((get_binop_int op) i1 i2, unopIk), true) else nopVal
  | (_, (_,false)) -> nopVal
  | ((_,false), _) -> nopVal
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 
| BinOp(op, binopExp1, binopExp2,TFloat(unopFk, _)) ->
    (match (eval_helper binopExp1, eval_helper binopExp2) with ((Float(f1, fk1), true),(Float(f2, fk2),true)) -> if CilType.Fkind.equal fk1 fk2 then (Float((get_binop_float op) f1 f2, unopFk), true) else nopVal
    | (_, (_,false)) -> nopVal
  | ((_,false), _) -> nopVal
  |(_, _) -> Printf.printf "This type of assignment is not supported\n"; exit 0) 
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in let (result,success) = eval_helper rval 
in if success then SigmarMap.add vinfo result sigOld else (print_string "Sigmar has not been updated. Vinfo is removed."; SigmarMap.remove vinfo sigOld)


let assign ctx (lval:lval) (rval:exp) : D.t = Printf.printf "assign wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge =  match lval with (Var x, _) -> ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=eval oldSigmar x rval})
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
  
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())
  
let branch ctx (exp:exp) (tv:bool) : D.t = Printf.printf "branch wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())

let body ctx (f:fundec) : D.t = Printf.printf "body wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())
      
let return ctx (exp:exp option) (f:fundec) : D.t = Printf.printf "return wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = Printf.printf "special wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())
    
let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = Printf.printf "enter wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
let state =   D.fold fold_helper ctx.local (D.empty ())
in
  [ctx.local, state]  
  

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t = Printf.printf "combine wurde aufgerufen\n";
  let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocalTraces.extend_by_gEdge g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())

    let threadenter ctx lval f args = Printf.printf "threadenter wurde aufgerufen\n";[D.top ()]
    let threadspawn ctx lval f args fctx = Printf.printf "threadspawn wurde aufgerufen\n";ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
