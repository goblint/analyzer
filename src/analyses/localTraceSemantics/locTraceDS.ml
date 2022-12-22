open Prelude.Ana
open Graph

(* Pure edge type *)
type edge = MyCFG.edge

module VarinfoImpl =
struct
include CilType.Varinfo

(* In order for inner and outter variables to be equal *)
let compare vinfo1 vinfo2 =
  if String.equal vinfo1.vname vinfo2.vname then 0 
  else CilType.Varinfo.compare vinfo1 vinfo2
  
end

module SigmaMap = Map.Make(VarinfoImpl)

(* Value domain for variables contained in sigma mapping.
   The supported types are: Integer and Address of a variable *)
type varDomain = 
  Int of Cilint.cilint * Cilint.cilint * ikind
| Address of varinfo   
| Error

let middle_intersect_intervals l1 u1 l2 u2 = if u1 = l2 then u1 else if l1 = u2 then l1 else
(  let l_intersect, u_intersect = if u1 > l2 then l2, u1 else l1, u2
in 
let delta = Big_int_Z.abs_big_int (Big_int_Z.sub_big_int u_intersect l_intersect) 
in Big_int_Z.add_big_int l_intersect (Big_int_Z.div_big_int delta (Big_int_Z.big_int_of_int 2))  ) 


let equal_varDomain vd1 vd2 =
  match vd1, vd2 with
  Int(iLower1, iUpper1, ik1), Int(iLower2, iUpper2, ik2) -> (Big_int_Z.eq_big_int iLower1 iLower2) && (Big_int_Z.eq_big_int iUpper1 iUpper2)
  && (CilType.Ikind.equal ik1 ik2)
  | Address(vinfo1), Address(vinfo2) -> CilType.Varinfo.equal vinfo1 vinfo2
  | _ -> false

  let show_valuedomain vd =
    match vd with Int(iLower, iUpper, ik) -> "Integer of ["^(Big_int_Z.string_of_big_int iLower)^";"^(Big_int_Z.string_of_big_int iUpper)^"] with ikind: "^(CilType.Ikind.show ik)
    | Address(vinfo) -> "Address of "^(CilType.Varinfo.show vinfo)
    | Error -> "ERROR"

let hash_valuedomain vd =
  match vd with Int(iLower, iUpper, ik) -> Big_int_Z.int_of_big_int (Big_int_Z.sub_big_int iLower iUpper)
  | Address(vinfo) -> CilType.Varinfo.hash vinfo
  | Error -> 13

(* Pure node type *)
type node = {
  programPoint : MyCFG.node;
  sigma : varDomain SigmaMap.t;
}

(* Module wrap for node implementing necessary functions for ocamlgraph *)
module NodeImpl =
struct 
type t = node

let equal_sigma s1 s2 = if (SigmaMap.is_empty s1) && (SigmaMap.is_empty s2) then true else
   let fold_helper vinfo varDom b = 
    SigmaMap.exists (fun exist_vinfo exist_varDom -> if (CilType.Varinfo.equal vinfo exist_vinfo)&&(equal_varDomain varDom exist_varDom) then true else b) s2
  in
  SigmaMap.fold fold_helper s1 false 

  (* MyCFG.compare hier möglich*)
let compare n1 n2 = match (n1, n2) with 
({programPoint=p1;sigma=s1},{programPoint=p2;sigma=s2}) -> if (equal_sigma s1 s2) then Node.compare p1 p2 else -13 

let show_sigma s = (SigmaMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_valuedomain vd)^";") s "")

let hash_sigma s = (SigmaMap.fold (fun vinfo vd i -> (CilType.Varinfo.hash vinfo) + (hash_valuedomain vd) + i) s 0)

let intersect_sigma sigma1 sigma2 =
  SigmaMap.fold (fun vinfo varDom sigAcc -> if SigmaMap.mem vinfo sigma2 = false then sigAcc else if equal_varDomain varDom (SigmaMap.find vinfo sigma2) then SigmaMap.add vinfo varDom sigAcc else sigAcc) sigma1 SigmaMap.empty

let destruct_add_sigma sigma1 sigma2 = SigmaMap.fold (fun vinfo varDom sigAcc -> SigmaMap.add vinfo varDom sigAcc) sigma2 sigma1

let hash {programPoint=n;sigma=s} = Node.hash n + hash_sigma s


let show n = 
  match n with {programPoint=p;sigma=s} -> "node:{programPoint="^(Node.show p)^"; |sigma|="^(string_of_int (SigmaMap.cardinal s))^", sigma=["^(SigmaMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_valuedomain vd)^";") s "")^"]}"

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

let get_all_nodes g =
  LocTraceGraph.fold_vertex  (fun x l -> x::l) g []

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
let hash g1 =
  let tmp = (List.fold (fun i node -> (NodeImpl.hash node)+i) 0 (get_all_nodes g1))
in
  (LocTraceGraph.nb_edges g1) + (LocTraceGraph.nb_vertex g1) + tmp

let compare g1 g2 = print_string "\nGraph.compare BEGIN\n";if equal g1 g2 then ( print_string ("The two graphs are equal in compare: g1="^(show g1)^" with hash="^(string_of_int (hash g1))^"\n and g2="^(show g2)^" with hash="^(string_of_int (hash g2))^" \n\nGraph.compare END\n");0) else (print_string ("Graphs are not equal in compare with g1="^(show g1)^" with hash="^(string_of_int (hash g1))^"\n and g2="^(show g2)^" with hash="^(string_of_int (hash g2))^"\n\nGraph.compare END\n"); 43)

(* Dummy to_yojson function *)
let to_yojson g1 :Yojson.Safe.t = `Variant("bam", None)

(* Retrieves a list of sigma mappings in a graph of a given node *)
let get_sigma g (progPoint:MyCFG.node) = 
  if LocTraceGraph.is_empty g then
  (match progPoint with 
  | FunctionEntry({svar={vname=s;_}; _})-> if String.equal s "@dummy" then (print_string "Hey, I found a dummy node on a yet empty graph\n";[SigmaMap.empty]) else [] 
    | _ -> [])
  else  
  (let tmp =
LocTraceGraph.fold_vertex (fun {programPoint=p1;sigma=s1} l -> if Node.equal p1 progPoint then s1::l else l) g []
  in if List.is_empty tmp then [] else tmp)

(* Applies an gEdge to graph 
   Explicit function for future improvements *)
let extend_by_gEdge gr gEdge =
  LocTraceGraph.add_edge_e gr gEdge 

  let error_node : MyCFG.node = 
    FunctionEntry({svar=makeVarinfo false "__goblint__traces__error" (TInt(IInt,[])); 
                   sformals=[];
                   slocals=[];
                   smaxid=0;
                   sbody={battrs=[];bstmts=[]};
                   smaxstmtid=None;
                   sallstmts=[]})

    let get_predecessors_edges graph node =
      List.fold 
      (fun list edge -> match edge with (prev_node, edgeLabel, dest_node) -> if NodeImpl.equal node dest_node then edge::list else list)
       [] (get_all_edges graph)

    let get_predecessors_nodes graph node =
        List.fold 
        (fun list edge -> match edge with (prev_node, edgeLabel, dest_node) -> if NodeImpl.equal node dest_node then prev_node::list else list)
         [] (get_all_edges graph)

  let find_globvar_assign_node global graph node = print_string ("find_globvar_assign_node global wurde aufgerufen\n");
    let workQueue = Queue.create ()
  in Queue.add node workQueue;
  let rec loop visited = (print_string ("loop wurde aufgerufen mit |workQueue| = "^(string_of_int (Queue.length workQueue))^" und peek: "^(NodeImpl.show (Queue.peek workQueue))^"\n");
    let q = Queue.pop workQueue
in let predecessors = print_string ("\nin loop we get the predessecors in graph:"^(show graph)^"\n");get_predecessors_edges graph q
in let tmp_result = print_string ("the predecessors are: "^(List.fold (fun s ed -> s^", "^(show_edge ed)) "" predecessors)^"\n");
List.fold (fun optionAcc (prev_node, (edge:MyCFG.edge), _) -> 
match edge with 
    | (Assign((Var(edgevinfo),_), edgeExp)) -> if CilType.Varinfo.equal global edgevinfo then (print_string ("Assignment mit global wurde gefunden! global="^(CilType.Varinfo.show global)^", edgevinfo="^(CilType.Varinfo.show edgevinfo)^"\n");Some(prev_node,edge)) else optionAcc
    | _ -> optionAcc
  ) None predecessors
in
let skip_edge:edge = Skip (* This is needed otherwise it errors with unbound constructor *)
in
match tmp_result with
| None -> List.iter (fun pred -> if List.mem pred visited then () else Queue.add pred workQueue) (get_predecessors_nodes graph q);
  if Queue.is_empty workQueue then ({programPoint=error_node;sigma=SigmaMap.empty}, skip_edge) else loop (q::visited)
| Some(nodeGlobalAssignment) -> nodeGlobalAssignment
  )
in loop []


end

(* Set domain for analysis framework *)
module GraphSet = struct
include SetDomain.Make(LocalTraces)

let mem graph graphSet = fold (fun x b -> if LocalTraces.equal x graph then b || true else b) graphSet false

let subset graphSet1 graphSet2 = 
  fold (fun graph b ->let tmp = mem graph graphSet2 
in if tmp = false then print_string ("graph: "^(LocalTraces.show graph)^" \nist angeblich nicht enthalten in graphSet2: "^(show graphSet2)^"\n");
    
    b && (tmp)) graphSet1 true

let leq graphSet1 graphSet2 = subset graphSet1 graphSet2

end


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