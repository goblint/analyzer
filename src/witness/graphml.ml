module type GraphMlWriter =
sig
  type t
  type node

  val start: out_channel -> t
  val write_key: t -> string -> string -> string -> string option -> unit
  val write_metadata: t -> string -> string -> unit
  val write_node: t -> node -> (string * string) list -> unit
  val write_edge: t -> node -> node -> (string * string) list -> unit
  val stop: t -> unit
end

module type StringGraphMlWriter = GraphMlWriter with type node = string

module XmlGraphMlWriter: StringGraphMlWriter =
struct
  type t =
    {
      out: out_channel;
      mutable children: Xml.xml list;
      mutable keys: Xml.xml list
    }
  type node = string

  let start out = { out; children = []; keys = [] }

  let write_key g fr key typ default_value =
    let xml = Xml.Element ("key", [("id", key); ("for", fr); ("attr.name", key); ("attr.type", typ)], match default_value with
      | Some value -> [Xml.Element ("default", [], [Xml.PCData value])]
      | None -> []
      )
    in
    g.keys <- xml :: g.keys

  let write_child g xml = g.children <- xml :: g.children

  let xml_data key value = Xml.Element ("data", [("key", key)], [Xml.PCData value])
  let xml_datas = List.map (fun (key, value) -> xml_data key value)
  let write_metadata g key value = write_child g (xml_data key value)

  let xml_node id datas =
    Xml.Element ("node", [("id", id)], xml_datas datas)
  let write_node g id datas = write_child g (xml_node id datas)

  let xml_edge source target datas =
    Xml.Element ("edge", [("source", source); ("target", target)], xml_datas datas)
  let write_edge g source target datas = write_child g (xml_edge source target datas)

  let stop g =
    let xml =
      Xml.Element ("graphml", [
          ("xmlns", "http://graphml.graphdrawing.org/xmlns");
          ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
          ("xsi:schemaLocation", "http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd")
        ], List.rev g.keys @ [
          Xml.Element ("graph", [("edgedefault", "directed")], List.rev g.children)
        ])
    in
    output_string g.out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output_string g.out (Xml.to_string_fmt xml);
    flush g.out
end

module ArgNodeGraphMlWriter (N: MyARG.Node) (M: StringGraphMlWriter):
  GraphMlWriter with type node = N.t =
struct
  type t = M.t
  type node = N.t

  let string_of_node = N.to_string

  let start = M.start
  let write_key = M.write_key
  let write_metadata = M.write_metadata
  let write_node g node datas = M.write_node g (string_of_node node) datas
  let write_edge g source target datas = M.write_edge g (string_of_node source) (string_of_node target) datas
  let stop = M.stop
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
  let write_key {delegate} = M.write_key delegate
  let write_metadata {delegate} = M.write_metadata delegate
  let write_node {delegate; added_nodes} node datas =
    if not (H.mem added_nodes node) then begin
      H.add added_nodes node ();
      M.write_node delegate node datas
    end
  let write_edge {delegate} = M.write_edge delegate
  let stop {delegate} = M.stop delegate
end
