open Prelude.Ana

module NodeSet = Set.Make(Node)

let print_nodeSet ns = NodeSet.fold (fun node acc -> (Node.show node)^"; "^acc) ns ""

module PredominatorMap = Map.Make(Node)

let print_predomMap pm = PredominatorMap.fold (fun node nodeSet acc ->"[<"^(Node.show node)^">, {"^(print_nodeSet nodeSet)^"}];\n"^acc) pm ""

class predominator_registration =
  object(self)
    val mutable predominatorMap : NodeSet.t PredominatorMap.t = PredominatorMap.empty
    val mutable loopHeads : NodeSet.t = NodeSet.empty
    val mutable loopHead2BackEdge: Node.t PredominatorMap.t = PredominatorMap.empty

    method update (prev_node:Node.t) (dest_node:Node.t) =
      if (PredominatorMap.mem prev_node predominatorMap)&&(NodeSet.mem dest_node (PredominatorMap.find prev_node predominatorMap)) 
        then loopHeads <- NodeSet.add dest_node loopHeads; 
      loopHead2BackEdge <- PredominatorMap.add dest_node prev_node loopHead2BackEdge;
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

    method isBackedgeOf (prev_node:Node.t) (dest_node:Node.t) =
      (PredominatorMap.mem dest_node loopHead2BackEdge) && (Node.equal prev_node (PredominatorMap.find dest_node loopHead2BackEdge))

    method isLoopHead (node:Node.t) = NodeSet.mem node loopHeads

    method printOut () =
      print_string ("predominatorMap = ["^(print_predomMap predominatorMap)^"]\n")

    method getBackEdgeNode (loopHead:Node.t)  =
      if PredominatorMap.mem loopHead loopHead2BackEdge then Some (PredominatorMap.find loopHead loopHead2BackEdge) else None

    (* Returns a partition of the dependency nodes: (non-priority nodes, priority nodes) *)
    method getPriorityNodePartition (loopHead:Node.t) (depNodes: Node.t list) : Node.t list * Node.t list =
      let backEdgeNodeOp = self#getBackEdgeNode loopHead
      in
      match backEdgeNodeOp with None -> depNodes, []
                              | Some backEdgeNode ->(
                                  let rec loop nodeList nonPrio prio =
                                    match nodeList with x::xs -> 
                                      if (NodeSet.mem x (PredominatorMap.find backEdgeNode predominatorMap) )
                                      then (
                                        print_string ("x="^(Node.show x)^" is contained in predominator-set="^(print_nodeSet (PredominatorMap.find backEdgeNode predominatorMap))^"
        \nof backedgeNode="^(Node.show backEdgeNode)^", so we add x to nonPrio\n");
                                        loop xs (x::nonPrio) prio)
                                      else (
                                        loop xs nonPrio (x::prio)
                                      )
                                                      | [] -> nonPrio, prio
                                  in
                                  if (PredominatorMap.mem backEdgeNode predominatorMap)
                                  then loop depNodes [] [] else depNodes, [])

  end


let predominatorRegistration = new predominator_registration
