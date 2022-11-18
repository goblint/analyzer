open Prelude.Ana
open Analyses
open Graph

type edge = MyCFG.edge
module EdgePrinter = Printable.SimplePretty(Edge)
module SigmarMap = Map.Make(CilType.Varinfo)

type varDomain = 
Int of Cilint.cilint * ikind 
| Float of float * fkind
| Address of varinfo   

type node = {
  programPoint : MyCFG.node;
  sigmar : varDomain SigmarMap.t;
}

module NodeImpl =
struct 
(* TODO: Implement functions *)
type t = node

let compare n1 n2 = match (n1, n2) with
({programPoint=p1;sigmar=s1},{programPoint=p2;sigmar=s2}) -> String.compare (Node.show_id p1) (Node.show_id p2) (* sigmar erstmal ignoriert *)

let hash n = match n with {programPoint=Statement(stmt);sigmar=s} -> stmt.sid
| {programPoint=Function(fd);sigmar=s} -> fd.svar.vid
| {programPoint=FunctionEntry(fd);sigmar=s} -> fd.svar.vid

let equal n1 n2 = (compare n1 n2) = 0
let show n = 
  let show_valuedomain vd =
    match vd with Int(cili, ik) -> "Integer of "^(Big_int_Z.string_of_big_int cili)^" with ikind: "^(CilType.Ikind.show ik)
    | Float(f, fk) -> "Float of "^(string_of_float f)^" with fkind: "^(CilType.Fkind.show fk)
    | Address(vinfo) -> "Address of "^(CilType.Varinfo.show vinfo)
in
  match n with {programPoint=p;sigmar=s} -> "node:{programPoint="^(Node.show p)^"; sigmar=["^(SigmarMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_valuedomain vd)^";") s "")^"]}"

end

module EdgeImpl =
struct
(* TODO: Implement functions *)  
type t = edge
let default = Edge.Skip
let compare e1 e2 =
Edge.compare e1 e2

let show e = EdgePrinter.show e

end
module LocTraceGraph = Persistent.Digraph.ConcreteBidirectionalLabeled (NodeImpl) (EdgeImpl)


(* Eigentliche Datenstruktur *)
module LocalTraces =
(* TODO implement functions for graph*)
struct
include Printable.Std
type t = LocTraceGraph.t (* Hier kommt der Graph rein *)
let name () = "traceDataStruc"

let show (g:t) =
  "Graph:{{number of edges: "^(string_of_int (LocTraceGraph.nb_edges g))^", number of nodes: "^(string_of_int (LocTraceGraph.nb_vertex g))^","^(LocTraceGraph.fold_edges_e (fun e s -> match e with (v1, ed, v2) -> s^"[node_prev:"^(NodeImpl.show v1)^",label:"^(EdgeImpl.show ed)^",node_dest:"^(NodeImpl.show v2)^"]-----\n") g "" )^"}}\n\n"
  include Printable.SimpleShow (struct
  type nonrec t = t
  let show = show
end)

let equal g1 g2 = 
  let tmp =
LocTraceGraph.fold_edges_e (fun e b -> (LocTraceGraph.mem_edge_e g2 e) && b ) g1 false 
  in tmp

let hash g1 = 42

let compare g1 g2 = if equal g1 g2 then 0 else 43

let to_yojson g1 :Yojson.Safe.t = `Variant("bam", None)

let get_sigmar g (progPoint:MyCFG.node) =  (* TODO implement get_node*)
LocTraceGraph.fold_vertex (fun {programPoint=p1;sigmar=s1} sigmap -> if NodeImpl.equal {programPoint=p1;sigmar=s1} {programPoint=progPoint;sigmar=SigmarMap.empty} then s1 else sigmap) g SigmarMap.empty

end

(* Graph-printing modules for exporting *)
module GPrinter = struct
  include LocTraceGraph
  let vertex_name v = NodeImpl.show v

  let get_subgraph v = None
  
  let graph_attributes g = []

  let default_vertex_attributes g = []

  let vertex_attributes v = []
  let default_edge_attributes g = []

  let edge_attributes e = [] (*[`Comment(EdgeImpl.show e)]*)
end
module DotExport = Graph.Graphviz.Dot(GPrinter)
(* let export_graph g filename =
  let file = open_out filename in
  DotExport.output_graph file g;
  close_out file
*)

module GraphSet = SetDomain.Make(LocalTraces)

(* Creates a copy of a set of graph - is this necessary? *)
let copy_graph_set set = GraphSet.fold (fun g acc -> GraphSet.add g acc) set (GraphSet.empty ())
  

module Spec : Analyses.MCPSpec =
struct
include Analyses.DefaultSpec
module D = GraphSet (* Graphstruktur muss vom Typ Lattice.S sein ..*)

module C = Lattice.Unit (* und was ist Modul C? *)

let context fundec l =
  ()

let name () = "localTraces"

let startstate v = let g = D.empty () in let tmp = Printf.printf "Leerer Graph wird erstellt\n";D.add LocTraceGraph.empty g
in if D.is_empty tmp then (Printf.printf "Obwohl leerer Graph hinzugefügt, ist Ergebnis immer noch leer\n"; tmp) else tmp (* ein leerer Graph sollte initial mit drin sein. *)

let exitstate = startstate

let eval sigOld graph vinfo (rval: exp) = 
  let nopVal = (Int((Big_int_Z.big_int_of_int (-13)),IInt), false) 
in let get_binop_int op =
(match op with 
| PlusA -> Big_int_Z.add_big_int
| MinusA -> Big_int_Z.sub_big_int
| Mult -> Big_int_Z.mult_big_int
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in
let get_binop_float op =
  (match op with 
  | PlusA -> Float.add
  | MinusA -> Float.sub
  | Mult -> Float.mul
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in
  let rec eval_helper subexp =
  (match subexp with
| Const(CInt(c, ik, _)) -> (Int (c, ik), true)
| Const(CReal(f, fk, _)) -> (Float (f, fk), true)
| Lval(Var(var), NoOffset) -> if SigmarMap.mem var sigOld then ((SigmarMap.find var sigOld), true) else nopVal
| AddrOf (Var(v), NoOffset) -> (Address(v), true)
| UnOp(Neg, unopExp, _) -> 
  (match eval_helper unopExp with (Int(i,ik), true) ->(Int (Big_int_Z.minus_big_int i, ik), true)
    |(Float(f, fk), true) -> (Float(-. f, fk), true)
    |(_, _) -> nopVal) 
|UnOp(LNot, unopExp,_) -> 
  (match eval_helper unopExp with (Int(i,ik), true) -> (Int(Big_int_Z.big_int_of_int (lnot (Big_int_Z.int_of_big_int i)), ik), true)
  |_,_ -> nopVal)
| BinOp(op, binopExp1, binopExp2,_) ->
  (match (eval_helper binopExp1, eval_helper binopExp2) with ((Int(i1, ik1), true),(Int(i2, ik2), true)) -> if CilType.Ikind.equal ik1 ik2 then (Int((get_binop_int op) i1 i2, ik1), true) else nopVal
  | ((Float(f1, fk1), true),(Float(f2, fk2),true)) -> if CilType.Fkind.equal fk1 fk2 then (Float((get_binop_float op) f1 f2, fk1), true) else nopVal
  | _,_ -> nopVal) 
| _ -> Printf.printf "This type of assignment is not supported\n"; exit 0)
in let (result,success) = eval_helper rval 
in if success then SigmarMap.add vinfo result sigOld else (print_string "Sigmar has not been updated."; sigOld)


let assign ctx (lval:lval) (rval:exp) : D.t = Printf.printf "assign wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge =  match lval with (Var x, _) -> ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=eval oldSigmar g x rval})
  | _ -> Printf.printf "This type of assignment is not supported\n"; exit 0
  
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())
  
let branch ctx (exp:exp) (tv:bool) : D.t = Printf.printf "branch wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())

let body ctx (f:fundec) : D.t = Printf.printf "body wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())
      
let return ctx (exp:exp option) (f:fundec) : D.t = Printf.printf "return wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t = Printf.printf "special wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())
    
let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = Printf.printf "enter wurde aufgerufen\n";
let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
let state =   D.fold fold_helper ctx.local (D.empty ())
in
  [ctx.local, state]  (*[D.bot (), D.bot ()] *)
  

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t = Printf.printf "combine wurde aufgerufen\n";
  let fold_helper g set = let oldSigmar = LocalTraces.get_sigmar g ctx.prev_node
in
let myEdge = ({programPoint=ctx.prev_node;sigmar=oldSigmar},ctx.edge,{programPoint=ctx.node;sigmar=oldSigmar})
in
  D.add (LocTraceGraph.add_edge_e g myEdge) set 
in
   D.fold fold_helper ctx.local (D.empty ())

    let threadenter ctx lval f args = Printf.printf "threadenter wurde aufgerufen\n";[D.top ()]
    let threadspawn ctx lval f args fctx = Printf.printf "threadspawn wurde aufgerufen\n";ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
