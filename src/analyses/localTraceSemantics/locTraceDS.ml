open Prelude.Ana
open Graph

(* Pure edge type *)
type edge = MyCFG.edge

module SigmarMap = Map.Make(CilType.Varinfo)

(* Value domain for variables contained in sigmar mapping.
   The supported types are: Integer, Float and Address of a variable *)
type varDomain = 
Int of Cilint.cilint * Cilint.cilint * ikind 
| Float of float * float * fkind
| Address of varinfo   
| Error

let equal_varDomain vd1 vd2 =
  match vd1, vd2 with
  Int(iLower1, iUpper1, ik1), Int(iLower2, iUpper2, ik2) -> (Big_int_Z.eq_big_int iLower1 iLower2) && (Big_int_Z.eq_big_int iUpper1 iUpper2)
  && (CilType.Ikind.equal ik1 ik2)
  | Float(fLower1, fUpper1, fk1), Float(fLower2, fUpper2, fk2) -> (fLower1 = fLower2) && (fUpper1 = fUpper2) && (CilType.Fkind.equal fk1 fk2)
  | Address(vinfo1), Address(vinfo2) -> CilType.Varinfo.equal vinfo1 vinfo2
  | _ -> false

  let show_valuedomain vd =
    match vd with Int(iLower, iUpper, ik) -> "Integer of ["^(Big_int_Z.string_of_big_int iLower)^";"^(Big_int_Z.string_of_big_int iUpper)^"] with ikind: "^(CilType.Ikind.show ik)
    | Float(fLower,fUpper, fk) -> "Float of ["^(string_of_float fLower)^";"^(string_of_float fUpper)^"] with fkind: "^(CilType.Fkind.show fk)
    | Address(vinfo) -> "Address of "^(CilType.Varinfo.show vinfo)
    | Error -> "ERROR"

(* Pure node type *)
type node = {
  programPoint : MyCFG.node;
  sigmar : varDomain SigmarMap.t;
}

(* Module wrap for node implementing necessary functions for ocamlgraph *)
module NodeImpl =
struct 
type t = node

let equal_sigmar s1 s2 = if (SigmarMap.is_empty s1) && (SigmarMap.is_empty s2) then true else
   let fold_helper vinfo varDom b = 
    SigmarMap.exists (fun exist_vinfo exist_varDom -> if (CilType.Varinfo.equal vinfo exist_vinfo)&&(equal_varDomain varDom exist_varDom) then true else b) s2
  in
  SigmarMap.fold fold_helper s1 false 

  (* MyCFG.compare hier möglich*)
let compare n1 n2 = match (n1, n2) with 
({programPoint=p1;sigmar=s1},{programPoint=p2;sigmar=s2}) -> if (equal_sigmar s1 s2) then String.compare (Node.show_id p1) (Node.show_id p2) else -13

let hash n = match n with {programPoint=Statement(stmt);sigmar=s} -> stmt.sid
| {programPoint=Function(fd);sigmar=s} -> fd.svar.vid
| {programPoint=FunctionEntry(fd);sigmar=s} -> fd.svar.vid



let show_sigmar s = (SigmarMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_valuedomain vd)^";") s "")
let show n = 
  match n with {programPoint=p;sigmar=s} -> "node:{programPoint="^(Node.show p)^"; |sigmar|="^(string_of_int (SigmarMap.cardinal s))^", sigmar=["^(SigmarMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_valuedomain vd)^";") s "")^"]}"

  let equal n1 n2 = (compare n1 n2) = 0
end

module EdgePrinter = Printable.SimplePretty(Edge)

(* Module wrap for edge implementing necessary functions for ocamlgraph *)
module EdgeImpl =
struct
type t = edge
let default = Edge.Skip
let show e = EdgePrinter.show e
let compare e1 e2 = Edge.compare e1 e2

  let equal e1 e2 = (compare e1 e2) = 0


end

(* ocamlgraph datastructure *)
module LocTraceGraph = Persistent.Digraph.ConcreteBidirectionalLabeled (NodeImpl) (EdgeImpl)


(* Module wrap for graph datastructure implementing necessary functions for analysis framework *)
module LocalTraces =
struct
include Printable.Std
type t = LocTraceGraph.t

let name () = "traceDataStruc"

let get_all_edges g =
  LocTraceGraph.fold_edges_e (fun x l -> x::l) g []

let show (g:t) =
  "Graph:{{number of edges: "^(string_of_int (LocTraceGraph.nb_edges g))^", number of nodes: "^(string_of_int (LocTraceGraph.nb_vertex g))^","^(LocTraceGraph.fold_edges_e (fun e s -> match e with (v1, ed, v2) -> s^"[node_prev:"^(NodeImpl.show v1)^",label:"^(EdgeImpl.show ed)^",node_dest:"^(NodeImpl.show v2)^"]-----\n") g "" )^"}}\n\n"
  include Printable.SimpleShow (struct
  type nonrec t = t
  let show = show
end)

let show_edge e =
  match e with (v1, ed, v2) -> "[node_prev:"^(NodeImpl.show v1)^",label:"^(EdgeImpl.show ed)^",node_dest:"^(NodeImpl.show v2)^"]"

  (* Im gonna implement this manually.  *)
  let rec equal_helper2 (node_prev1, edge1, node_dest1) edgeList =
    match edgeList with (node_prev2, edge2, node_dest2)::xs -> 
      ((NodeImpl.equal node_prev1 node_prev2)&&(EdgeImpl.equal edge1 edge2)&&(NodeImpl.equal node_dest1 node_dest2)) || (equal_helper2 (node_prev1, edge1, node_dest1) xs )
      | [] -> false

let rec equal_helper1 edgeList1 edgeList2 =
  match edgeList1 with x::xs -> (equal_helper2 x edgeList2)&&(equal_helper1 xs edgeList2)
  | [] -> true
let equal g1 g2 = print_string "\nGraph.equal BEGIN\n";
if (LocTraceGraph.nb_edges g1 != LocTraceGraph.nb_edges g2) || (LocTraceGraph.nb_vertex g1 != LocTraceGraph.nb_vertex g2) then (print_string "g1 and g2 are NOT the same, they have different length\nGraph.equal END\n";false) else (
print_string ("g1-edges="^(LocTraceGraph.fold_edges_e (fun ed s -> (show_edge ed)^",\n"^s) g1 "")^"\n");
print_string ("g2-edges="^(LocTraceGraph.fold_edges_e (fun ed s -> (show_edge ed)^",\n"^s) g2 "")^"\n");
  let tmp = equal_helper1 (get_all_edges g1) (get_all_edges g2)
  in if tmp = false then print_string "g1 and g2 are NOT the same with even length\n" else print_string "g1 and g2 are the same with even length\n"; print_string "\nGraph.equal END\n";tmp)

    (* eventuell liegt hier der Fehler?*)
let hash g1 = LocTraceGraph.nb_edges g1

let compare g1 g2 = print_string "\nGraph.compare BEGIN\n";if equal g1 g2 then ( print_string ("The two graphs are equal in compare: g1="^(show g1)^"\n and g2="^(show g2)^" \n\nGraph.compare END\n");0) else (print_string ("Graphs are not equal in compare with g1="^(show g1)^"\n and g2="^(show g2)^"\n\nGraph.compare END\n"); 43)

(* Dummy to_yojson function *)
let to_yojson g1 :Yojson.Safe.t = `Variant("bam", None)

(* Retrieves the sigmar mapping in a graph of a given node *)
let get_sigmar g (progPoint:MyCFG.node) =  
LocTraceGraph.fold_vertex (fun {programPoint=p1;sigmar=s1} sigmap -> if Node.equal p1 progPoint then s1 else sigmap) g SigmarMap.empty

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