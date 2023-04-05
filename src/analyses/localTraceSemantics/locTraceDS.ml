open Prelude.Ana
open Graph

(* Custom version of the Edge module in order to introduce one additional edge: DepMutex *)
module CustomEdge =
struct 
(* copy paste from Edge module *)
type asm_out = (string option * string * CilType.Lval.t) list [@@deriving eq, ord, hash, to_yojson]
type asm_in  = (string option * string * CilType.Exp.t ) list [@@deriving eq, ord, hash, to_yojson]

type t =
  | Assign of CilType.Lval.t * CilType.Exp.t
  | Proc of CilType.Lval.t option * CilType.Exp.t * CilType.Exp.t list
  | Entry of CilType.Fundec.t
  | Ret of CilType.Exp.t option * CilType.Fundec.t
  | Test of CilType.Exp.t * bool
  | ASM of string list * asm_out * asm_in
  | VDecl of CilType.Varinfo.t
  | Skip
  (* DepMutex implements dependency edges as described in the paper *)
  | DepMutex of CilType.Varinfo.t
[@@deriving eq, ord, hash, to_yojson]


let pretty () = function
  | Test (exp, b) -> if b then Pretty.dprintf "Pos(%a)" dn_exp exp else Pretty.dprintf "Neg(%a)" dn_exp exp
  | Assign (lv,rv) -> Pretty.dprintf "%a = %a" dn_lval lv dn_exp rv
  | Proc (Some ret,f,args) -> Pretty.dprintf "%a = %a(%a)" dn_lval ret dn_exp f (d_list ", " dn_exp) args
  | Proc (None,f,args) -> Pretty.dprintf "%a(%a)" dn_exp f (d_list ", " dn_exp) args
  | Entry (f) -> Pretty.text "(body)"
  | Ret (Some e,f) -> Pretty.dprintf "return %a" dn_exp e
  | Ret (None,f) -> Pretty.dprintf "return"
  | ASM (_,_,_) -> Pretty.text "ASM ..."
  | Skip -> Pretty.text "skip"
  | VDecl v -> Cil.defaultCilPrinter#pVDecl () v
  | DepMutex (mv) -> Pretty.text ("depMutex ("^(CilType.Varinfo.show mv)^")")

let pretty_plain () = function
  | Assign (lv,rv) -> dprintf "Assign '%a = %a' " d_lval lv d_exp rv
  | Proc (None  ,f,ars) -> dprintf "Proc '%a(%a)'" d_exp f (d_list ", " d_exp) ars
  | Proc (Some r,f,ars) -> dprintf "Proc '%a = %a(%a)'" d_lval r d_exp f (d_list ", " d_exp) ars
  | Entry f -> dprintf "Entry %s" f.svar.vname
  | Ret (None,fd) -> dprintf "Ret (None, %s)" fd.svar.vname
  | Ret (Some r,fd) -> dprintf "Ret (Some %a, %s)" d_exp r fd.svar.vname
  | Test (p,b) -> dprintf "Test (%a,%b)" d_exp p b
  | ASM _ -> text "ASM ..."
  | Skip -> text "Skip"
  | VDecl v -> dprintf "VDecl '%a %s;'" d_type v.vtype v.vname
  | DepMutex (mv) -> dprintf "DepMutex (%a)" d_varinfo mv

end 

(* Explicit varinfo definition for custom compare-implementation *)
module VarinfoImpl =
struct
include CilType.Varinfo

(* In order for inner and outter variables to be equal *)
let compare vinfo1 vinfo2 =
  if String.equal vinfo1.vname vinfo2.vname then 0 
  else CilType.Varinfo.compare vinfo1 vinfo2
  
end

module SigmaMap = Map.Make(VarinfoImpl)
module VarinfoSet = Set.Make(VarinfoImpl)

(* Value domain for variables contained in sigma mapping.
   The supported types are: Integer and Address of a variable *)
type varDomain = 
  Int of Cilint.cilint * Cilint.cilint * ikind
| Address of varinfo  
| ThreadID of int 
| Error 

(* Calculates the middle of an interval s.t. it can be split into two intervals *)
let middle_intersect_intervals l1 u1 l2 u2 = if u1 = l2 then u1 else if l1 = u2 then l1 else
(  let l_intersect, u_intersect = if u1 > l2 then l2, u1 else l1, u2
in 
let delta = Big_int_Z.abs_big_int (Big_int_Z.sub_big_int u_intersect l_intersect) 
in Big_int_Z.add_big_int l_intersect (Big_int_Z.div_big_int delta (Big_int_Z.big_int_of_int 2))  ) 

(* Helper functions for varDomain *)
let equal_varDomain vd1 vd2 =
  match vd1, vd2 with
  Int(iLower1, iUpper1, ik1), Int(iLower2, iUpper2, ik2) -> (Big_int_Z.eq_big_int iLower1 iLower2) && (Big_int_Z.eq_big_int iUpper1 iUpper2)
  && (CilType.Ikind.equal ik1 ik2)
  | Address(vinfo1), Address(vinfo2) -> CilType.Varinfo.equal vinfo1 vinfo2
  | Error, Error -> true
  | ThreadID(tid1), ThreadID(tid2) -> tid1 = tid2
  | _ -> false

  let show_varDomain vd =
    match vd with Int(iLower, iUpper, ik) -> "Integer of ["^(Big_int_Z.string_of_big_int iLower)^";"^(Big_int_Z.string_of_big_int iUpper)^"] with ikind: "^(CilType.Ikind.show ik)
    | Address(vinfo) -> "Address of "^(CilType.Varinfo.show vinfo)
    | ThreadID(tid) -> "ThreadID of "^(string_of_int tid)
    | Error -> "ERROR"

    (* I need a better hash function for intervals!! *)
let hash_varDomain vd =
  match vd with Int(iLower, iUpper, ik) -> Big_int_Z.int_of_big_int (Big_int_Z.add_big_int iLower iUpper)
  | Address(vinfo) -> CilType.Varinfo.hash vinfo
  | ThreadID(tid) -> tid
  | Error -> 13

(* Node type *)
type node = {
  id: int;
  programPoint : MyCFG.node;
  sigma : varDomain SigmaMap.t;
  tid:int;
  lockSet: VarinfoSet.t;
}

(* Helper functions for constructing varinfo names *)
let make_mutex_varinfo x = "__goblint__traces__mutex__"^x.vname

let make_local_global_varinfo x = "__goblint__traces__"^x.vname

(* Module wrap for node implementing necessary functions for ocamlgraph *)
module NodeImpl =
struct 
type t = node

let show_sigma s = (SigmaMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_varDomain vd)^";") s "")

let show_lockSet ls = VarinfoSet.fold (fun mutex s_fold -> (CilType.Varinfo.show mutex)^"; "^s_fold) ls ""

let equal_sigma s1 s2 = 
  if (SigmaMap.is_empty s1) && (SigmaMap.is_empty s2) then true 
  else if (SigmaMap.cardinal s1) != (SigmaMap.cardinal s2) then false
  else
    SigmaMap.fold (fun vinfo varDom b -> b && (if SigmaMap.mem vinfo s2 then equal_varDomain (SigmaMap.find vinfo s2) varDom else false)) s1 true

  let show n = 
    match n with {programPoint=p;sigma=s;id=id;tid=tid;lockSet=lockSet} -> "node:{programPoint="^(Node.show p)^"; id="^(string_of_int id)^"; |sigma|="^(string_of_int (SigmaMap.cardinal s))^", sigma=["^(SigmaMap.fold (fun vinfo vd s -> s^"vinfo="^(CilType.Varinfo.show vinfo)^", ValueDomain="^(show_varDomain vd)^";") s "")^"]; 
    tid="^(string_of_int tid)^"; lockSet=["^(show_lockSet lockSet)^"]}"

let compare n1 n2 = match (n1, n2) with 
({programPoint=p1;sigma=s1;id=id1;tid=tid1;lockSet=ls1},{programPoint=p2;sigma=s2;id=id2;tid=tid2;lockSet=ls2}) -> if (equal_sigma s1 s2)&&(id1 = id2)&&(tid1 = tid2)&&(VarinfoSet.equal ls1 ls2) then Node.compare p1 p2 else -13 

let hash_sigma s = (SigmaMap.fold (fun vinfo vd i -> (CilType.Varinfo.hash vinfo) + (hash_varDomain vd) + i) s 0)

let hash_lockSet ls = VarinfoSet.fold (fun mutex acc -> acc+(CilType.Varinfo.hash mutex)) ls 0

(* Unions two sigma where all updates of sigma2 overwrite sigma1 *)
let destruct_add_sigma sigma1 sigma2 = if SigmaMap.is_empty sigma1 then sigma2 else
  SigmaMap.fold (fun vinfo varDom sigAcc -> SigmaMap.add vinfo varDom sigAcc) sigma2 sigma1

let hash {programPoint=n;sigma=s;id=id;tid=tid;lockSet=ls} = Node.hash n + hash_sigma s + id + tid + hash_lockSet ls

  let equal n1 n2 = (compare n1 n2) = 0
end

module EdgePrinter = Printable.SimplePretty(CustomEdge)

(* Module wrap for edge implementing necessary functions for ocamlgraph *)
module EdgeImpl =
struct
type t = CustomEdge.t
let default:t = Skip
let show e = EdgePrinter.show e

let compare e1 e2 = CustomEdge.compare e1 e2

let equal e1 e2 = (compare e1 e2) = 0

(* Helper function to convert Edge.t to CustomEdge.t *)
let convert_edge (edge:Edge.t) :t =
  match edge with
  | Assign(l,e) -> Assign(l,e)
  | Proc(l, e, el) -> Proc(l, e, el)
  | Entry(f) -> Entry(f)
  | Ret(e, f) -> Ret(e, f)
  | Test(e, b) -> Test(e, b)
  | ASM(s, a, o) -> ASM(s, a, o)
  | VDecl(v) -> VDecl(v)
  | Skip -> Skip

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

  (* Helper function for equal *)
  let rec equal_helper2 (node_prev1, edge1, node_dest1) edgeList =
    match edgeList with (node_prev2, edge2, node_dest2)::xs -> 
      ((NodeImpl.equal node_prev1 node_prev2)&&(EdgeImpl.equal edge1 edge2)&&(NodeImpl.equal node_dest1 node_dest2)) || (equal_helper2 (node_prev1, edge1, node_dest1) xs )
      | [] -> false

let rec equal_helper1 edgeList1 edgeList2 =
  match edgeList1 with x::xs -> (equal_helper2 x edgeList2)&&(equal_helper1 xs edgeList2)
  | [] -> true

let equal g1 g2 = 
if (LocTraceGraph.nb_edges g1 != LocTraceGraph.nb_edges g2) || (LocTraceGraph.nb_vertex g1 != LocTraceGraph.nb_vertex g2) then (false) else (
let edgeList1 = get_all_edges g1
in let edgeList2 = get_all_edges g2
in
let tmp = equal_helper1 (edgeList1) (edgeList2)
  in tmp)

let equal_edge (prev_node1, edge1, dest_node1) (prev_node2, edge2, dest_node2) = 
  (NodeImpl.equal prev_node1 prev_node2)&&(EdgeImpl.equal edge1 edge2)&&(NodeImpl.equal dest_node1 dest_node2)

let hash g1 =
  let tmp = (List.fold (fun i node -> (NodeImpl.hash node)+i) 0 (get_all_nodes g1))
in
  (LocTraceGraph.nb_edges g1) + (LocTraceGraph.nb_vertex g1) + tmp

  let compare g1 g2 =  
  if equal g1 g2 then 0 else compare g1 g2

(* Dummy to_yojson function *)
let to_yojson g1 :Yojson.Safe.t = `Variant("bam", None)

(* Adds an edge to a graph
   Explicit function for future improvements *)
let extend_by_gEdge gr gEdge = 
  (* print_string "LocalTraces.extend_by_gEdge was invoked\n"; *)
  if (List.fold (fun acc edge_fold -> (equal_edge edge_fold gEdge)||acc) false (get_all_edges gr)) 
    then gr 
else let tmp = LocTraceGraph.add_edge_e gr gEdge in tmp 

(* An error node. A trace ending here is a faulty one *)
  let error_node : MyCFG.node = 
    FunctionEntry({svar=makeVarinfo false "__goblint__traces__error" (TInt(IInt,[])); 
                   sformals=[];
                   slocals=[];
                   smaxid=0;
                   sbody={battrs=[];bstmts=[]};
                   smaxstmtid=None;
                   sallstmts=[]})

  (* Varinfos used in the analysis *)
  let return_vinfo = makeVarinfo false "__goblint__traces__return" (TInt(IInt,[]))

  let branch_vinfo = makeVarinfo false "__goblint__traces__branch" (TInt(IInt,[]))

    let get_predecessors_edges graph node = 
      List.fold 
      (fun list edge -> match edge with (prev_node, edgeLabel, dest_node) -> if NodeImpl.equal node dest_node then edge::list else list)
       [] (get_all_edges graph)

    let get_predecessors_nodes graph node =
        List.fold 
        (fun list edge -> match edge with (prev_node, edgeLabel, dest_node) -> if NodeImpl.equal node dest_node then prev_node::list else list)
         [] (get_all_edges graph)

    let get_successors_edges graph node =
      List.fold 
      (fun list edge -> match edge with (prev_node, edgeLabel, dest_node) -> if NodeImpl.equal node prev_node then edge::list else list)
       [] (get_all_edges graph)

       (* Crucial function implementing the global search *)
    let find_globvar_assign_node global graph node = 
      (* As a worklist we use a queue. Thus, search is performed in a BFS-style:
         For a node, we check all preceeding edges for an assignment defining global.
         If we find nothing, then we add all predecessors to the queue and continue. *)
    let workQueue = Queue.create ()
  in Queue.add node workQueue;
  let rec loop visited = (let q = Queue.pop workQueue
in let predecessors = 
  get_predecessors_edges graph q
in let tmp_result = 
List.fold (fun optionAcc (prev_node, (edge:CustomEdge.t), _) -> 
match edge with 
    | (Assign((Var(edgevinfo),_), edgeExp)) -> 
      if CilType.Varinfo.equal global edgevinfo 
      then Some(prev_node,edge) else optionAcc
    | _ -> optionAcc
  ) None predecessors
in
let skip_edge:(CustomEdge.t) = Skip (* This is needed otherwise compilation errors with unbound constructor *)
in
match tmp_result with
| None -> List.iter (fun pred -> if List.mem pred visited then () else Queue.add pred workQueue) (get_predecessors_nodes graph q);
  if Queue.is_empty workQueue then ({programPoint=error_node;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty}, skip_edge) 
  else (
    loop (q::visited))
| Some(nodeGlobalAssignment) -> nodeGlobalAssignment
  )
in loop []

  (* Returns next node succeeding previous node with given label *)
    let get_succeeding_node prev_node edge_label graph =
  print_string ("LocalTraces.get_succeeding_node was invoked with prev_node="^(NodeImpl.show prev_node)^", edge_label="^(EdgeImpl.show edge_label)^" and\n"^(show graph)^"\n");
  let edgeList = get_all_edges graph
in let tmp =
List.fold (fun acc_node ((prev_fold:node), edge_fold, (dest_fold:node)) -> 
  if (NodeImpl.equal prev_node prev_fold)&&(CustomEdge.equal edge_label edge_fold) 
    then dest_fold
    else acc_node
      ) {programPoint=error_node;sigma=SigmaMap.empty;id=(-1);tid= -1;lockSet=VarinfoSet.empty} edgeList
  in print_string ("in LocalTraces.get_succeeding_node we found the node "^(NodeImpl.show tmp)^"\n"); tmp

(* Finds the return endpoint of a calling node. 
   Since function calls can be needed, we use a saldo which indicates in what calling depth the computation is starting at prev_node. *)
    let find_returning_node prev_node edge_label graph =
  let node_start = get_succeeding_node prev_node edge_label graph
in
print_string ("find_returning_node was invoked with node_start="^(NodeImpl.show node_start)^"\n");
let rec find_returning_node_helper current_node current_saldo =(let succeeding_edges = get_successors_edges graph current_node
in
List.fold (fun acc_node ((prev_fold:node), (edge_fold:CustomEdge.t), (dest_fold:node)) -> 
  if prev_fold.tid != dest_fold.tid then acc_node else
  match edge_fold with 
    | Proc(_, Lval(Var(fvinfo), NoOffset), _) -> 
      (* check whether function is one of my supported special functions*)
      if (String.equal fvinfo.vname "pthread_mutex_lock") 
        || (String.equal fvinfo.vname "pthread_mutex_unlock")
      || (String.equal fvinfo.vname "pthread_create")
      || (String.equal fvinfo.vname "pthread_join")
      || (String.equal fvinfo.vname "pthread_mutex_destroy")
      || (String.equal fvinfo.vname "pthread_exit")
      || (String.equal fvinfo.vname "time")
      || (String.equal fvinfo.vname "srand")
      || (String.equal fvinfo.vname "pthread_mutex_init") then find_returning_node_helper dest_fold current_saldo
      else find_returning_node_helper dest_fold (current_saldo +1)
    | Proc(_) -> find_returning_node_helper dest_fold (current_saldo +1)
    | Ret(_) -> if current_saldo = 1 then dest_fold else find_returning_node_helper dest_fold (current_saldo - 1)
    | _ -> find_returning_node_helper dest_fold current_saldo
  ) {programPoint=error_node;sigma=SigmaMap.empty;id=(-1);tid= -1;lockSet=VarinfoSet.empty} succeeding_edges
  )
in find_returning_node_helper node_start 1

(* Interface for possible improvement: 
Instead of conducting each time a search for the last node, store it as a component at the local trace datastructure *)

(* Searches for last node in a trace. It has to contain the programPoint, otherwise an error-node is returned *)
    let get_last_node_progPoint graph programPoint =
      let allNodes = get_all_nodes graph
    in 
    let rec loop nodeList =
      match nodeList with 
        | x::xs -> if (List.is_empty (get_successors_edges graph x))&&(Node.equal x.programPoint programPoint) then x else loop xs
        | [] -> {programPoint=error_node;sigma=SigmaMap.empty; tid= -1; id= -1; lockSet=VarinfoSet.empty}
    in loop allNodes

    (* Searches for last node in a trace independently of the program point *)
    let get_last_node graph =
      let allNodes = get_all_nodes graph
    in 
    let rec loop nodeList =
      match nodeList with 
        | x::xs -> if (List.is_empty (get_successors_edges graph x)) then x else loop xs
        | [] -> {programPoint=error_node;sigma=SigmaMap.empty; tid= -1; id= -1; lockSet=VarinfoSet.empty}
    in loop allNodes

  (* Searches for the first node in a trace. 
     As the last node, this too could be stored as a component. *)
    let get_first_node graph =
      let allNodes = get_all_nodes graph
    in
    let rec loop nodeList =
      match nodeList with
      | x::xs -> if (List.is_empty (get_predecessors_edges graph x)) then x else loop xs
      | [] -> {programPoint=error_node;sigma=SigmaMap.empty; tid= -1; id= -1; lockSet=VarinfoSet.empty}
    in loop allNodes

    (* Gets all nodes with corresponding program point. *)
    let get_all_nodes_progPoint graph programPoint=
    let allNodes = get_all_nodes graph
  in 
  let rec loop nodeList acc =
    match nodeList with 
      | x::xs -> if (Node.equal x.programPoint programPoint) then loop xs (x::acc) else loop xs acc
      | [] -> acc
  in loop allNodes []
  
(* Finds corresponding calling node to a given returning node. *)
(* This function also has potential for efficiency improvements.
   For example, each function call could be registered in an external datastructure where invocation and end point can be stored. *)
let find_calling_node return_node graph progPoint =
  let allNodes = get_all_nodes_progPoint graph progPoint
in
let rec inner_loop node_candidate edgeList =
  match edgeList with (_,e,_)::es -> 
    let tmp = find_returning_node node_candidate e graph
  in
  if NodeImpl.equal return_node tmp then node_candidate
  else inner_loop node_candidate es
  | [] -> 
    {programPoint=error_node;sigma= SigmaMap.empty;id= -1; tid= -1;lockSet=VarinfoSet.empty}
in
let rec loop nodeList =
  match nodeList with
  | x::xs -> (
    let succeedingEdgesList = get_successors_edges graph x
in
    let tmp_result = inner_loop x succeedingEdgesList
in 
if Node.equal tmp_result.programPoint error_node then loop xs else tmp_result
  )
    | [] -> print_string ("find_calling_node did not succeed, ended in loop\n");
      {programPoint=error_node;sigma= SigmaMap.empty;id= -1; tid= -1;lockSet=VarinfoSet.empty}
    in loop allNodes

    (* Finds the creating node of a thread endpoint. *)
let find_creating_node last_node graph  =
  print_string ("find_creating_node was invoked with last_node="^(NodeImpl.show last_node)^",\ngraph="^(show graph)^"\n");
  let lastNodeTid = last_node.tid
in
let workQueue = Queue.create ()
  in Queue.add last_node workQueue;
  
  let rec loop () =
    if Queue.is_empty workQueue then ({programPoint=error_node;sigma=SigmaMap.empty;id= -1;tid= -1;lockSet=VarinfoSet.empty})
    else(
    let current_node = Queue.pop workQueue
  in
  let rec inner_loop edges =
    match edges with
    (precedingNode, (edgeLabel:CustomEdge.t),_)::xs ->
      (match edgeLabel with (Proc(_, Lval(Var(v),_), _)) -> (
        if (precedingNode.tid != lastNodeTid)&&(current_node.tid = lastNodeTid)&&(String.equal v.vname "pthread_create") then Some(precedingNode)
      else inner_loop xs)
        | _ ->  inner_loop xs )
      | [] -> None
  in
    let precedingEdges = get_predecessors_edges graph current_node
  in
  match inner_loop precedingEdges with
  None -> List.iter (fun node -> Queue.add node workQueue ) (get_predecessors_nodes graph current_node);
  loop ()
  | Some(found_node) -> found_node)
in loop ()

(* Checks whether a node exists in a trace. *)
let exists_node graph node =
let all_nodes = get_all_nodes graph
in
List.exists (fun node_exists -> NodeImpl.equal node node_exists) all_nodes

(* Checks whether two edge lists are equal. *)
  let equal_edge_lists edgeList1 edgeList2 =
    if List.length edgeList1 != List.length edgeList2 then false else
      (
        let rec loop list =
          match list with x::xs ->  if List.exists (fun edge -> equal_edge edge x) edgeList2 then loop xs else false
            | [] -> true
        in
        loop edgeList1
      )

      (* Merges two graphs into one. *)
  let merge_graphs graph1 graph2 =
    List.fold (fun graph_fold edge_fold -> extend_by_gEdge graph_fold edge_fold) graph2 (get_all_edges graph1)  

    (* Checks whether a thread-ID was already joined into a trace. *)
  let is_already_joined tid graph =
    let allEdges = get_all_edges graph
  in
  let rec loop (edgeList: ((node * CustomEdge.t * node) list)) =
    match edgeList with (prev_node, Proc(_, Lval(Var(v),_), _), dest_node)::xs -> 
      if (prev_node.tid = tid) && (dest_node.tid != tid)&&(String.equal v.vname "pthread_join") then true else loop xs
      | (prev_node, edge, dest_node)::xs -> loop xs
      | [] -> false
    in loop allEdges  

(* Checks whether node1 precedes node2 *)
    let precedes_other_node node1 node2 graph =
      let workQueue = Queue.create ()
    in
      let rec loop visited =
        if Queue.is_empty workQueue then false else
        let currentNode = Queue.pop workQueue
      in
        let precedingNodes = get_predecessors_nodes graph currentNode
      in
      if List.exists (fun node_exists -> NodeImpl.equal node_exists node1) precedingNodes then true
      else
        (List.iter (fun node_iter -> if (List.exists (fun node_exists -> NodeImpl.equal node_exists node_iter) visited)
          then () else Queue.add node_iter workQueue
          ) precedingNodes;
        loop (currentNode::visited))
      in 
      Queue.add node2 workQueue;
      loop []

      (* Gets last node before graph1 and graph2 diverge. This is also the prefix node *)
      let get_recent_divergent_node graph1 graph2 =
          let lastNode1 = get_last_node graph1
        in
        let rec loop currentNode =
          if exists_node graph2 currentNode then currentNode
          else (
            let predecessors = get_predecessors_nodes graph1 currentNode
        in
        let nextNode = List.fold (
          fun acc_fold node_fold ->
            if Node.equal acc_fold.programPoint error_node then node_fold 
            else if currentNode.tid = node_fold.tid then node_fold 
            else acc_fold
        ) {programPoint=error_node;tid= -1;id= -1;sigma=SigmaMap.empty;lockSet=VarinfoSet.empty} predecessors
          in
          if Node.equal nextNode.programPoint error_node then {programPoint=error_node;tid= -1;id= -1;sigma=SigmaMap.empty;lockSet=VarinfoSet.empty} else
          loop nextNode
          )
        in loop lastNode1

        (* Checks whether mutex was locked at least once in graph *)
    let exists_lock_mutex graph mutexVinfo =
      let allEdges = get_all_edges graph
    in
    let rec loop (edgeList:LocTraceGraph.edge list) =
      match edgeList with
      | (_, Proc(None, Lval(Var(lvalVinfo), _),[AddrOf(Var(argVinfo), _)]), _) ::xs -> 
        if (String.equal lvalVinfo.vname "pthread_mutex_lock") && (CilType.Varinfo.equal mutexVinfo argVinfo) then true else loop xs
      | x::xs -> loop xs
      | [] -> false
      in loop allEdges

      (* Helper functions for validity check of merged graphs *)
      module DepMutexCount = Map.Make(CilType.Varinfo)
      (* Checks whether from each node at most on DepMutex-edge exists *)
      (* This needs to be reimplemented to the condition given in the paper: 
        for a mutex a every lock is preceded by
        exactly one unlock (or it is the first lock, then by start) of a, and each unlock is directly
        followed by at most one lock
        Additionally, at a point a cycle check is needed  *)
    let maintains_depMutex_condition graph =
      let allNodes = get_all_nodes graph
    in let rec loop nodeList =
      match nodeList with node::xs ->
      let edgeList:(node* CustomEdge.t* node) list = get_successors_edges graph node
      in 
      print_string ("edgeList=["^(List.fold (fun s_fold edge_fold -> (show_edge edge_fold)^", "^s_fold) "" edgeList)^"]\n");
      let depCount = List.fold (fun count_fold (edge_fold:LocTraceGraph.edge) ->
        match edge_fold with (prev_node,DepMutex(vinfo),dest_node) -> 
          if DepMutexCount.mem vinfo count_fold 
            then (
              print_string ("vinfo already contained in count_fold\n");
              DepMutexCount.add vinfo ((DepMutexCount.find vinfo count_fold)+1) count_fold 
              )
          else (
            print_string ("vinfo not yet contained in count_fold\n");
          DepMutexCount.add vinfo 1 count_fold
          )
          | _ -> count_fold
        ) DepMutexCount.empty edgeList
    in 
    print_string ("depCount=["^(DepMutexCount.fold (fun vinfo_fold count_fold s_fold -> "("^(CilType.Varinfo.show vinfo_fold)^", "^(string_of_int count_fold)^"), "^s_fold) depCount "")^"]\n");
    if DepMutexCount.exists (fun vinfo_exists count_exists -> if count_exists > 1 then true else false) depCount then false else loop xs
      | [] -> true
      in loop allNodes

      module NodeImplSet = Set.Make(NodeImpl)
      (* Checks whether the graph contains a cycle *)
      let is_acyclic graph = 
        let firstNode = get_first_node graph
      in
      let rec loop_nodes visited currentNode =
        let currentVisited = NodeImplSet.add currentNode visited
      in
      let successorEdges = get_successors_edges graph currentNode
    in if loop_edges successorEdges currentVisited then
    List.fold (fun b_fold (_,_,dest_node_fold) -> if b_fold then loop_nodes currentVisited dest_node_fold else false) true successorEdges
    else false
    and loop_edges edgeList visited =
      match edgeList with (_,_,dest_node)::xs -> if NodeImplSet.mem dest_node visited then false else loop_edges xs visited
      | [] -> true
      in loop_nodes NodeImplSet.empty firstNode

      (* Checks that every node has exactly one edge except some exceptions
         this is not the optimal way, it would be better to ensure not to produce such kind of merged traces 
         this needs to be improved in some point in the future *)
         (* Here I assume that if one egde in the list is a pthread_create edge, then others are aswell*)
      let are_pthread_create_edges (edgeList : (node * CustomEdge.t * node) list) =
        match edgeList with (_, Proc(_, Lval(Var(fvinfo), NoOffset), _), _)::xs ->
          String.equal fvinfo.vname "pthread_create"
          | _ -> false

          (* Checks whether the edgeList with cardinality > 1 contains exactly one actual edge beside DepMutexes *)
        let are_mostly_depMutexes allEdges =
          let rec loop (edgeList : (node * CustomEdge.t * node) list) counter =
            match edgeList with (_, DepMutex(_), _)::xs -> loop xs counter
              | _::xs -> loop xs (counter + 1)
              | [] -> counter = 1 (* counter counts edges that are not depMutexes *)
          in
          loop allEdges 0

          (* Checks whether nodes have a reasonable amount of successor edges 
             Either a node has at most one successor edges or the additional successor edges are create or dependency edges *)
      let has_valid_edge_branching graph = 
        let allNodes = get_all_nodes graph 
      in
      let rec loop nodeList =
        match nodeList with 
        | node::xs -> let edgeList = get_successors_edges graph node
      in
      if List.length edgeList < 2 then loop xs
      else
        (
          print_string ("node="^(NodeImpl.show node)^" has more than one successor:
          \n{"^(List.fold (fun acc edge -> (show_edge edge)^"; "^acc) "" edgeList)^"}\n");
        if (are_pthread_create_edges edgeList) || (are_mostly_depMutexes edgeList) then loop xs else false
        )
        | [] -> true
        in
        loop allNodes

        (* Crucial function. Performs post-checks on merged graph *)
    let is_valid_merged_graph graph =
      (maintains_depMutex_condition graph) 
      && (is_acyclic graph)
       && (has_valid_edge_branching graph)

  end

(* Set domain for analysis framework *)
module GraphSet = struct
include SetDomain.Make(LocalTraces)
end

(* Converts a GraphSet.t to a list *)
let graphSet_to_list graphSet =
  GraphSet.fold (fun g acc -> g::acc) graphSet []

(* ID Generator 
   maintains info what node has which ID such that the ID's are consistent among local traces *)
class id_generator = 
object(self)
 val mutable currentID = 0
 val mutable edges:((node * CustomEdge.t * node) list) = []
 method increment () =
  currentID <- currentID + 1; 
  currentID

  (* Maybe prev_node and edge are enough to determine the ID of a dest_node? 
     This needs to be tried out at some point in the future *)
  method getID (prev_node:node) (edge:CustomEdge.t) (dest_programPoint:MyCFG.node) (dest_sigma:varDomain SigmaMap.t) (dest_tid:int) (dest_ls:VarinfoSet.t) =
    print_string "getID wurde aufgerufen\n";
    let id = List.fold (fun acc (prev_node_find, edge_find, {programPoint=p_find;sigma=s_find;id=id_find;tid=tid_find;lockSet=ls_find}) -> 
     if (NodeImpl.equal prev_node prev_node_find)&&(CustomEdge.equal edge edge_find)&&(Node.equal dest_programPoint p_find)&&(NodeImpl.equal_sigma dest_sigma s_find)&&(tid_find = dest_tid)&&(VarinfoSet.equal ls_find dest_ls) then id_find else acc) (-1) edges 
  in 
  if id = (-1) then ( 
  edges <- (prev_node, edge, {programPoint=dest_programPoint;sigma=dest_sigma;id=currentID+1;tid=dest_tid;lockSet=dest_ls})::edges; 
  self#increment ()) else (print_string ("id was found: "^(string_of_int id)^"\n"); id)
end

let idGenerator = new id_generator

(* Random int Generator
  Generates values in the interval of [-seed; seed] *)
class random_int_generator =
object(self)
  val mutable traceVarList:((int * varinfo * int) list) = []
  val mutable seed:int = 5

  method getRandomValue (hash:int) (var:varinfo) = 
    if List.is_empty traceVarList then Random.init 100;
    print_string ("random_int_generator#getRandomValue was invoked with hash="^(string_of_int hash)^", var="^(CilType.Varinfo.show var)^";
    \nwith current traceVarList="^(List.fold (fun acc (int_fold, vinfo_fold, value_fold) -> acc^"; ("^(string_of_int int_fold)^","^(CilType.Varinfo.show vinfo_fold)^","^(string_of_int value_fold)^")") "" traceVarList)^"\n");
    if List.exists ( fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list) ) traceVarList 
      then (print_string "random value exists already\n";
        let _,_,randomValue =List.find (fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list)) traceVarList
in randomValue) 
  else
    ( print_string "new random value is generated\n";
      let randomValue = if(Random.int 2) = 0 then (*negative*) (Random.int seed) * (-1)
      else (*positive*) (Random.int seed) 
  in
  traceVarList <- (hash, var, randomValue)::traceVarList;
  randomValue)

  method getRandomValueFullCInt (hash:int) (var:varinfo) =
    if List.is_empty traceVarList then Random.self_init ();
    if List.exists ( fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list) ) traceVarList 
      then (
        let _,_,randomValue =List.find (fun (int_list, vinfo_list,_) -> (int_list = hash)&&(CilType.Varinfo.equal var vinfo_list)) traceVarList
in randomValue) 
  else
    ( print_string "new random value is generated\n";
    (* I want randomValue to have the range [intMin; intMax]*)
      let randomValue = if (Random.int 2) = 0 then (*negative*) ((Random.int 1073741823) + (Random.int 1073741823)) * -1
      else (*positive*) (Random.int 1073741823) + (Random.int 1073741823)
  in
  traceVarList <- (hash, var, randomValue)::traceVarList;
  randomValue)

end

let randomIntGenerator = new random_int_generator

module VinfoMap = Map.Make(String)

(* Stores mapping of variable name to varinfo 
   This is needed because several created varinfos with same vname are not equal *)
class custom_vinfo_store =
object(self)
val mutable vinfoMap: varinfo VinfoMap.t = VinfoMap.empty

  method getGlobalVarinfo (name:string) =
    if VinfoMap.mem name vinfoMap 
      then (VinfoMap.find name vinfoMap)
      else (
        let newVinfo = makeGlobalVar name (TVoid([]))
    in
    vinfoMap <- VinfoMap.add name newVinfo vinfoMap;
    newVinfo
      )

      method getLocalVarinfo (name:string) (typ:typ) =
        if VinfoMap.mem name vinfoMap 
          then (VinfoMap.find name vinfoMap)
          else (
            let newVinfo = makeVarinfo false name typ
        in
        vinfoMap <- VinfoMap.add name newVinfo vinfoMap;
        newVinfo
          )
end

let customVinfoStore = new custom_vinfo_store

(* Module for thread ID *)
module ThreadIDLocTrace = struct
  type t = int
  let is_write_only _ = false

  let equal (tid1:t) (tid2:t) = tid1 = tid2

  let hash tid = tid

  let compare tid1 tid2 = tid1 - tid2

  let show tid = string_of_int tid

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)

    let name () = "threadID"

    let tag tid = failwith ("tag")

    let arbitrary tid = failwith ("no arbitrary")

    let relift tid = failwith ("no relift")
end

module TIDSet = Set.Make(ThreadIDLocTrace)

(* TID record 
   Stores all existing thread IDs *)
class tid_record =
object(self)
val mutable tidSet : TIDSet.t = TIDSet.empty

method addTID (tid:int) =
  tidSet <- TIDSet.add tid tidSet

method existsTID (tid:int) =
  TIDSet.mem tid tidSet
end

let tidRecord = new tid_record

(* Module that is used as a domain for side effect. *)
module SideEffectDomain = struct
  type t = ThreadID of int | Mutex of varinfo
  let is_write_only _ = false

  let equal (t1:t) (t2:t) = 
    match t1, t2 with ThreadID(tid1), ThreadID(tid2) -> tid1 = tid2
    | Mutex(m1), Mutex(m2) -> CilType.Varinfo.equal m1 m2
    | _ -> false

  let hash t = match t with ThreadID(tid) -> tid
  | Mutex(m) -> CilType.Varinfo.hash m

  (* Mutex has a higher value then thread ID *)
  let compare t1 t2 = match t1, t2 with ThreadID(tid1), ThreadID(tid2) -> tid1 - tid2
  | Mutex(m1), Mutex(m2) -> CilType.Varinfo.compare m1 m2
  | ThreadID(tid), Mutex(m) -> let tmp = (hash (ThreadID tid)) - (hash (Mutex m)) in if tmp < 0 then tmp else -1000
  | Mutex(m), ThreadID(tid) -> let tmp = (hash (Mutex m)) - (hash (ThreadID tid)) in if tmp > 0 then tmp else 1000

  let show t = match t with ThreadID(tid) -> "ThreadID of"^(string_of_int tid)
  | Mutex(m) -> "Mutex of "^(CilType.Varinfo.show m) 

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)

    let name () = "threadID"

    let tag tid = failwith ("tag")

    let arbitrary tid = failwith ("no arbitrary")

    let relift tid = failwith ("no relift")
end