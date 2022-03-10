(** UpdateCil functions to avoid dependency cycles.*)

module NodeMap = Hashtbl.Make(Node0)

let location_map = ref (NodeMap.create 103: Cil.location NodeMap.t)

let init () =
  NodeMap.clear !location_map

let getLoc (node: Node0.t) =
  (* In case this belongs to a changed function, we will find the true location in the map*)
  try
    NodeMap.find !location_map node
  with Not_found ->
    Node0.location node

let store_node_location (n: Node0.t) (l: Cil.location): unit =
  NodeMap.add !location_map n l
