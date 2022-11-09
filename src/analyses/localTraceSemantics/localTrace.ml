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

let compare n1 n2 = Printf.printf "NodeImpl-compare has been invoked\n";match (n1, n2) with
({programPoint=p1;sigmar=s1},{programPoint=p2;sigmar=s2}) -> String.compare (Node.show_id p1) (Node.show_id p2) (* sigmar erstmal ignoriert *)

let hash n = Printf.printf "NodeImpl-hash has been invoked\n";match n with {programPoint=Statement(stmt);sigmar=s} -> stmt.sid
| {programPoint=Function(fd);sigmar=s} -> fd.svar.vid
| {programPoint=FunctionEntry(fd);sigmar=s} -> fd.svar.vid

let equal n1 n2 = Printf.printf "NodeImpl-equal has been invoked\n";(compare n1 n2) = 0
let show n = match n with {programPoint=p;sigmar=s} -> "node:{programPoint="^(Node.show p)^"; sigmar=["^(SigmarMap.fold (fun vinfo vd s -> "vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain is missing yet") s "")^"]}"
(* TODO implement printing of valuedomain*)
end

module EdgeImpl =
struct
(* TODO: Implement functions *)  
type t = edge
let default = Printf.printf "EdgeImpl-default has been invoked\n";Edge.Skip
let compare e1 e2 = Printf.printf "EdgeImpl-compare has been invoked\n";-3 (* Na, wie vergleicht man denn zwei Edges?*)

let show e = EdgePrinter.show e

end
module LocTraceGraph = Imperative.Digraph.ConcreteBidirectionalLabeled (NodeImpl) (EdgeImpl)

(* type edge_old = Assignment of (string * int)
type node_old = {
  programPoint : int;
  sigmar : (string * exp ) list
} *)
(* module L for Printer *)
module L =
struct
  let vertex_properties = []
  let edge_properties = []
  let map_vertex (n) = match n with
  {programPoint=p;sigmar=s} -> [Node.show p,SigmarMap.fold (fun vinfo (Int(i)) s -> String.concat s ["- varinfo: ";(CilType.Varinfo.show vinfo);", value of var: "; (string_of_int i)]) s ""]

  let map_edge (e:LocTraceGraph.edge) = 
   match e with (v1,ed,v2) -> ["edge", (NodeImpl.show v1)^","^(EdgeImpl.show ed)^","^(NodeImpl.show v2)]  (*TODO: implement print functions for nodeImpl and edgeImpl*)
    (* ["edge", EdgePrinter.show e]*)

  let vertex_uid n = 3
  let edge_uid e = 5
end


(* Eigentliche Datenstruktur *)
module LocalTraces =
(* TODO implement functions for graph*)
struct
include Printable.Std
type t = LocTraceGraph.t (* Hier kommt der Graph rein *)
let name () = "traceDataStruc"

let pretty () v =
  text (LocTraceGraph.fold_edges_e (fun e s -> match e with (v1, ed, v2) -> String.concat s ["[node_prev:"^(NodeImpl.show v1)^",label:"^(EdgeImpl.show ed)^",node_dest:"^(NodeImpl.show v2)^"]\n"]) v "" )

let show v = 
  LocTraceGraph.fold_edges_e (fun e s -> match e with (v1, ed, v2) -> String.concat s ["[node_prev:"^(NodeImpl.show v1)^",label:"^(EdgeImpl.show ed)^",node_dest:"^(NodeImpl.show v2)^"]\n"]) v "" 

  include Printable.SimpleShow (struct
  type nonrec t = t
  let show = show
end)

(* Ja und wie vergleiche ich jetzt Graphen? *)
let equal e1 e2 = false

let hash e1 = 42

let compare e1 e2 = if equal e1 e2 then 0 else 43

let to_yojson e1 :Yojson.Safe.t = `Variant("bam", None)

end

module GraphSet = SetDomain.Make(LocalTraces)

(* Creates a copy of a set of graph - is this necessary? *)
let copy_graph_set set = GraphSet.fold (fun g acc -> GraphSet.add (LocTraceGraph.copy g) acc) set (GraphSet.empty ())
  

module Spec : Analyses.MCPSpec =
struct
include Analyses.DefaultSpec
module D = GraphSet (* Graphstruktur muss vom Typ Lattice.S sein ..*)

module C = D (* und was ist Modul C? *)

let name () = "localTraces"

let startstate v = let g = D.empty () in let tmp = Printf.printf "Leerer Graph wird erstellt\n";D.add (LocTraceGraph.create ()) g
in if D.is_empty tmp then (Printf.printf "Obwohl leerer Graph hinzugefügt, ist Ergebnis immer noch leer\n"; tmp) else tmp (* ein leerer Graph sollte initial mit drin sein. *)

let exitstate = startstate

let assign ctx (lval:lval) (rval:exp) : D.t =
  let state = if D.is_empty ctx.local then (Printf.printf "Kein Graph im Set\n"; ctx.local) else ctx.local in
  let myEdge = Printf.printf "assign wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen.\n";LocTraceGraph.add_edge_e g myEdge) state ; state
  
  (* match lval, rval with
  | (Var x, _), Const(CInt(c, _, _)) -> ()
  | _ -> ctx.local*)

let branch ctx (exp:exp) (tv:bool) : D.t =
  let myEdge = Printf.printf "branch wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
  D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen in branch.\n";LocTraceGraph.add_edge_e g myEdge) ctx.local ;
 ctx.local

let body ctx (f:fundec) : D.t =
  let myEdge = Printf.printf "body wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen in body.\n";LocTraceGraph.add_edge_e g myEdge) ctx.local ;
  ctx.local
      
let return ctx (exp:exp option) (f:fundec) : D.t =
  let myEdge = Printf.printf "return wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen in return.\n";LocTraceGraph.add_edge_e g myEdge) ctx.local ;
  ctx.local

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
  let myEdge = Printf.printf "special wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen in special.\n";LocTraceGraph.add_edge_e g myEdge) ctx.local ;
  ctx.local
    
let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = Printf.printf "enter wurde aufgerufen\n";
let state =  copy_graph_set ctx.local in
  let myEdge = Printf.printf "enter wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen in enter.\n";LocTraceGraph.add_edge_e g myEdge) state ; [ctx.local, state] (* [D.bot (), D.bot ()]*)
  

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t =
    let myEdge = Printf.printf "combine wurde aufgerufen\n";({programPoint=ctx.prev_node;sigmar=SigmarMap.empty},ctx.edge,{programPoint=ctx.node;sigmar=SigmarMap.empty}) in
   D.iter (fun g -> Printf.printf "Eine Iteration wurde durchlaufen in combine.\n";LocTraceGraph.add_edge_e g myEdge) ctx.local ;
    ctx.local

    let threadenter ctx lval f args = Printf.printf "threadenter wurde aufgerufen\n";[D.top ()]
    let threadspawn ctx lval f args fctx = Printf.printf "threadspawn wurde aufgerufen\n";ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)


(* module G = Imperative.Graph.Abstract(struct type t = int * int end) *)

(* module locTraceGraph = ConcreteBidirectionalLabeled (node_old) (edge_old) *)

(* and now: https://ocamlgraph.lri.fr/index.en.html
   using https://ocamlgraph.lri.fr/doc/Imperative.Digraph.ConcreteBidirectionalLabeled.html *)