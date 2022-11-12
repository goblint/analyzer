open Prelude.Ana
open Analyses
open Graph

type edge = MyCFG.edge
module EdgePrinter = Printable.SimplePretty(Edge)
module SigmarMap = Map.Make(CilType.Varinfo)

type varDomain = Int of int
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
let show n = match n with {programPoint=p;sigmar=s} -> "node:{programPoint="^(Node.show p)^"; sigmar=["^(SigmarMap.fold (fun vinfo vd s -> "vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain is missing yet") s "")^"]}"
(* TODO implement printing of valuedomain*)
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

end

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

let assign ctx (lval:lval) (rval:exp) : D.t =
  let state = if D.is_empty ctx.local then (Printf.printf "Kein Graph im Set bei assign\n"; ctx.local) else ctx.local in
  let myEdge = Printf.printf "assign wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) state (D.empty ())
  
let branch ctx (exp:exp) (tv:bool) : D.t =
  let myEdge = Printf.printf "branch wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
  D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) ctx.local (D.empty ())

let body ctx (f:fundec) : D.t =
  let myEdge = Printf.printf "body wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
  D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) ctx.local (D.empty ())
      
let return ctx (exp:exp option) (f:fundec) : D.t =
  let myEdge = Printf.printf "return wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
  D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) ctx.local (D.empty ())

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
  let myEdge = Printf.printf "special wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
  D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) ctx.local (D.empty ())
    
let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = Printf.printf "enter wurde aufgerufen\n";
  let myEdge = Printf.printf "enter wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
  let state = D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) ctx.local (D.empty ()) in
  [ctx.local, state]  (*[D.bot (), D.bot ()] *)
  

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t =
    let myEdge = Printf.printf "combine wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
    D.fold (fun g set  -> let tmp = D.add (LocTraceGraph.add_edge_e g myEdge) set in D.remove g tmp) ctx.local (D.empty ())

    let threadenter ctx lval f args = Printf.printf "threadenter wurde aufgerufen\n";[D.top ()]
    let threadspawn ctx lval f args fctx = Printf.printf "threadspawn wurde aufgerufen\n";ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)


(* module G = Imperative.Graph.Abstract(struct type t = int * int end) *)

(* module locTraceGraph = ConcreteBidirectionalLabeled (node_old) (edge_old) *)

(* and now: https://ocamlgraph.lri.fr/index.en.html
   using https://ocamlgraph.lri.fr/doc/Imperative.Digraph.ConcreteBidirectionalLabeled.html *)