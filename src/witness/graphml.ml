(** Streaming GraphML output. *)

module type GraphMlWriter =
sig
  type t
  type node

  val start: out_channel -> t
  val write_key: t -> string -> string -> string -> string option -> unit
  val start_graph: t -> unit
  val write_metadata: t -> string -> string -> unit
  val write_node: t -> node -> (string * string) list -> unit
  val write_edge: t -> node -> node -> (string * string) list -> unit
  val stop: t -> unit
end

module type StringGraphMlWriter = GraphMlWriter with type node = string

module XmlGraphMlWriter: StringGraphMlWriter =
struct
  type t = unit BatIO.output
  type node = string

  let escape = XmlUtil.escape

  let start out =
    let f = BatIO.output_channel out in
    BatPrintf.fprintf f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    (* BatPrintf.fprintf f "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n"; *)
    BatPrintf.fprintf f "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n";
    f

  let write_key f fr key typ default_value =
    match default_value with
    | Some value ->
      BatPrintf.fprintf f "  <key id=\"%s\" for=\"%s\" attr.name=\"%s\" attr.type=\"%s\">\n    <default>%s</default>\n  </key>\n" (escape key) (escape fr) (escape key) (escape typ) (escape value)
    | None ->
      BatPrintf.fprintf f "  <key id=\"%s\" for=\"%s\" attr.name=\"%s\" attr.type=\"%s\"/>\n" (escape key) (escape fr) (escape key) (escape typ)

  let start_graph f =
    BatPrintf.fprintf f "  <graph edgedefault=\"directed\">\n"

  let write_metadata f key value =
    BatPrintf.fprintf f "    <data key=\"%s\">%s</data>\n" (escape key) (escape value)

  let write_data f (key, value) =
    BatPrintf.fprintf f "      <data key=\"%s\">%s</data>\n" (escape key) (escape value)
  let write_datas = BatList.print ~first:"" ~sep:"" ~last:"" write_data

  let write_node f id datas =
    match datas with
    | [] ->
      BatPrintf.fprintf f "    <node id=\"%s\"/>\n" (escape id)
    | _ ->
      BatPrintf.fprintf f "    <node id=\"%s\">\n%a    </node>\n" (escape id) write_datas datas

  let write_edge f source target datas =
    match datas with
    | [] ->
      BatPrintf.fprintf f "    <edge source=\"%s\" target=\"%s\"/>\n" (escape source) (escape target)
    | _ ->
      BatPrintf.fprintf f "    <edge source=\"%s\" target=\"%s\">\n%a    </edge>\n" (escape source) (escape target) write_datas datas

  let stop f =
    BatPrintf.fprintf f "  </graph>\n</graphml>\n";
    BatIO.close_out f
end

(* TODO: generalize N argument to just to_string *)
module ArgNodeGraphMlWriter (N: MyARG.Node) (M: StringGraphMlWriter):
  GraphMlWriter with type node = N.t =
struct
  type t = M.t
  type node = N.t

  let string_of_node = N.to_string

  let start = M.start
  let write_key = M.write_key
  let start_graph = M.start_graph
  let write_metadata = M.write_metadata
  let write_node g node datas = M.write_node g (string_of_node node) datas
  let write_edge g source target datas = M.write_edge g (string_of_node source) (string_of_node target) datas
  let stop = M.stop
end

module EnumerateNodeGraphMlWriter (N: Hashtbl.HashedType) (M: StringGraphMlWriter):
  GraphMlWriter with type node = N.t =
struct
  module H = Hashtbl.Make (N)

  type t =
    {
      delegate: M.t;
      node_numbers: int H.t;
      mutable next_number: int
    }
  type node = N.t

  let string_of_node ({node_numbers; next_number; _} as g) node =
    let number = match H.find_opt node_numbers node with
      | Some number -> number
      | None ->
        let number = next_number in
        H.replace node_numbers node number;
        g.next_number <- number + 1;
        number
    in
    "N" ^ string_of_int number

  let start out = { delegate = M.start out; node_numbers = H.create 100; next_number = 0 }
  let write_key {delegate; _} = M.write_key delegate
  let start_graph {delegate; _} = M.start_graph delegate
  let write_metadata {delegate; _} = M.write_metadata delegate
  let write_node ({delegate; _} as g) node datas = M.write_node delegate (string_of_node g node) datas
  let write_edge ({delegate; _} as g) source target datas = M.write_edge delegate (string_of_node g source) (string_of_node g target) datas
  let stop {delegate; _} = M.stop delegate
end

module DeDupGraphMlWriter (Node: Hashtbl.HashedType) (M: GraphMlWriter with type node = Node.t):
  GraphMlWriter with type node = Node.t =
struct
  module H = Hashtbl.Make(Node)

  type t =
    {
      delegate: M.t;
      added_nodes: unit H.t
    }
  type node = M.node

  let start out = { delegate = M.start out; added_nodes = H.create 100 }
  let write_key {delegate; _} = M.write_key delegate
  let start_graph {delegate; _} = M.start_graph delegate
  let write_metadata {delegate; _} = M.write_metadata delegate
  let write_node {delegate; added_nodes} node datas =
    if not (H.mem added_nodes node) then begin
      H.add added_nodes node ();
      M.write_node delegate node datas
    end

  let write_edge {delegate; _} = M.write_edge delegate
  let stop {delegate; _} = M.stop delegate

end
