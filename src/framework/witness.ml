open MyCFG

let write_file (module Cfg:CfgBidir) entrystates (invariant:node -> Invariant.t): unit =
  let module NH = Hashtbl.Make (Node) in

  let node_name = function
    | Statement stmt  -> Printf.sprintf "%d" stmt.sid
    | Function f      -> Printf.sprintf "ret%d%s" f.vid f.vname
    | FunctionEntry f -> Printf.sprintf "fun%d%s" f.vid f.vname
  in

  let xml_data key value = Xml.Element ("data", [("key", key)], [Xml.PCData value]) in
  let xml_node node =
    Xml.Element ("node", [("id", node_name node)], List.concat [
        begin match node with
          | FunctionEntry f when f.vname = "main" ->
            [xml_data "entry" "true"]
          | _ -> []
        end;
        begin match invariant node with
          | Some i -> [xml_data "invariant" i]
          | _ -> []
        end;
        begin match node with
          | Statement s ->
            [xml_data "sourcecode" (Pretty.sprint 80 (Basetype.CilStmt.pretty () s))] (* TODO: sourcecode not official? especially on node? *)
          | _ -> []
        end
      ])
  in
  let xml_edge from_node ((loc, edge):Cil.location * edge) to_node =
    Xml.Element ("edge", [("source", node_name from_node); ("target", node_name to_node)], List.concat [
        if loc.line <> -1 then
          [xml_data "startline" (string_of_int loc.line); xml_data "endline" (string_of_int loc.line)]
        else
          []
      ])
  in

  let graph_children = ref [] in
  let add_graph_child xml =
    graph_children := xml :: !graph_children
  in

  let added_nodes = NH.create 100 in
  let add_node node =
    if not (NH.mem added_nodes node) then begin
      NH.add added_nodes node ();
      add_graph_child (xml_node node)
    end
  in
  let add_edge from_node locedge to_node =
    add_graph_child (xml_edge from_node locedge to_node)
  in
  let add_edges from_node locedges to_node =
    List.iter (fun locedge -> add_edge from_node locedge to_node) locedges
  in

  let itered_nodes = NH.create 100 in
  let rec iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      add_node node;
      List.iter (fun (locedges, to_node) ->
          add_node to_node;
          add_edges node locedges to_node
        ) (Cfg.next node);
      List.iter (fun (locedges, to_node) ->
          iter_node to_node
        ) (Cfg.next node)
    end
  in

  List.iter (fun ((n, _), _) -> iter_node n) entrystates;

  let xml =
    Xml.Element ("graphml", [], [
        Xml.Element ("graph", [], List.rev !graph_children)
      ])
  in
  let out = open_out "witness.graphml" in
  Printf.fprintf out "%s" (Xml.to_string_fmt xml);
  flush out;
  close_out_noerr out