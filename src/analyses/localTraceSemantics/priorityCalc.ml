open Prelude.Ana

module NodeSet = Set.Make(Node)

let print_nodeSet ns = NodeSet.fold (fun node acc -> (Node.show node)^"; "^acc) ns ""

module PredominatorMap = Map.Make(Node)

let print_predomMap pm = PredominatorMap.fold (fun node nodeSet acc ->"[<"^(Node.show node)^">, {"^(print_nodeSet nodeSet)^"}];\n"^acc) pm ""

class predominator_registration =
object(self)
val mutable predominatorMap : NodeSet.t PredominatorMap.t = PredominatorMap.empty
val mutable loopHeads : NodeSet.t = NodeSet.empty

method update (prev_node:Node.t) (dest_node:Node.t) =
  if (self#isBackedge prev_node dest_node) then loopHeads <- NodeSet.add dest_node loopHeads;
  if self#isLoopHead dest_node then print_string ("We found a loop head: "^(Node.show dest_node)^"\n");
  let prevNodePreDoms = 
    if PredominatorMap.mem prev_node predominatorMap then PredominatorMap.find prev_node predominatorMap 
    else NodeSet.empty
  in 
  let destNodePredDoms =
    if PredominatorMap.mem dest_node predominatorMap then PredominatorMap.find dest_node predominatorMap 
    else NodeSet.empty
  in
  let newNodeSet = if PredominatorMap.mem dest_node predominatorMap then (NodeSet.inter (NodeSet.add dest_node prevNodePreDoms) destNodePredDoms)
  else (NodeSet.add dest_node prevNodePreDoms)
in
  predominatorMap <- PredominatorMap.add dest_node newNodeSet predominatorMap

method isBackedge (prev_node:Node.t) (dest_node:Node.t) =
  if not (PredominatorMap.mem prev_node predominatorMap) then false else
  NodeSet.mem dest_node (PredominatorMap.find prev_node predominatorMap)
  
method isLoopHead (node:Node.t) = NodeSet.mem node loopHeads

method printOut () =
  print_string ("predominatorMap = ["^(print_predomMap predominatorMap)^"]\n")

method getBackEdgeNode (loopHead:Node.t) (depNodes: Node.t list) =
  let rec loop nodeList = match nodeList with x::xs -> if self#isBackedge x loopHead then Some(x) else loop xs
    | [] -> None
in
loop depNodes

(* Returns a partition of the dependency nodes: (non-priority nodes, priority nodes) *)
method getPriorityNodePartition (loopHead:Node.t) (depNodes: Node.t list) : Node.t list * Node.t list =
let backEdgeNodeOp = self#getBackEdgeNode loopHead depNodes
in
match backEdgeNodeOp with None -> depNodes, []
| Some backEdgeNode ->(
let rec loop nodeList nonPrio prio =
  match nodeList with x::xs -> 
    if (NodeSet.mem x (PredominatorMap.find backEdgeNode predominatorMap) )
      then loop xs (x::nonPrio) prio
      else loop xs nonPrio (x::prio)
    | [] -> nonPrio, prio
in
if (PredominatorMap.mem backEdgeNode predominatorMap)
  then loop depNodes [] [] else depNodes, [])

end


let predominatorRegistration = new predominator_registration
