open Prelude.Ana
open Graph

(* Pure edge type *)
type edge = MyCFG.edge

module SigmarMap = Map.Make(CilType.Varinfo)

(* Value domain for variables contained in sigmar mapping.
   The supported types are: Integer, Float and Address of a variable *)
type varDomain = 
Int of Cilint.cilint * ikind 
| Float of float * fkind
| Address of varinfo   

(* Pure node type *)
type node = {
  programPoint : MyCFG.node;
  sigmar : varDomain SigmarMap.t;
}

(* Module wrap for node implementing necessary functions for ocamlgraph *)
module NodeImpl =
struct 
type t = node

let compare n1 n2 = match (n1, n2) with
({programPoint=p1;sigmar=s1},{programPoint=p2;sigmar=s2}) -> String.compare (Node.show_id p1) (Node.show_id p2)

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

module EdgePrinter = Printable.SimplePretty(Edge)

(* Module wrap for edge implementing necessary functions for ocamlgraph *)
module EdgeImpl =
struct
type t = edge
let default = Edge.Skip
let compare e1 e2 =
Edge.compare e1 e2

let show e = EdgePrinter.show e

end

(* ocamlgraph datastructure *)
module LocTraceGraph = Persistent.Digraph.ConcreteBidirectionalLabeled (NodeImpl) (EdgeImpl)


(* Module wrap for graph datastructure implementing necessary functions for analysis framework *)
module LocalTraces =
struct
include Printable.Std
type t = LocTraceGraph.t
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

(* Dummy hash function *)
let hash g1 = 42

let compare g1 g2 = if equal g1 g2 then 0 else 43

(* Dummy to_yojson function *)
let to_yojson g1 :Yojson.Safe.t = `Variant("bam", None)

(* Retrieves the sigmar mapping in a graph of a given node *)
let get_sigmar g (progPoint:MyCFG.node) =  
LocTraceGraph.fold_vertex (fun {programPoint=p1;sigmar=s1} sigmap -> if NodeImpl.equal {programPoint=p1;sigmar=s1} {programPoint=progPoint;sigmar=SigmarMap.empty} then s1 else sigmap) g SigmarMap.empty

(* Applies an gEdge to graph 
   Explicit function for future improvements *)
let extend_by_gEdge gr gEdge =
  LocTraceGraph.add_edge_e gr gEdge 
end

(* Set domain for analysis framework *)
module GraphSet = SetDomain.Make(LocalTraces)


(* TODO Graph-printing modules for exporting *)
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

