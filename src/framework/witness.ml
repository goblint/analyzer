open MyCFG

let write_file (module Cfg:CfgBidir) entrystates (invariant:node -> Invariant.t): unit =
  let module NH = Hashtbl.Make (Node) in

  let node_name = function
    | Statement stmt  -> Printf.sprintf "s%d" stmt.sid
    | Function f      -> Printf.sprintf "ret%d%s" f.vid f.vname
    | FunctionEntry f -> Printf.sprintf "fun%d%s" f.vid f.vname
  in

  let xml_data key value = Xml.Element ("data", [("key", key)], [Xml.PCData value]) in
  let xml_node ~entry node =
    Xml.Element ("node", [("id", node_name node)], List.concat [
        begin if entry then
            [xml_data "entry" "true"]
          else
            []
        end;
        begin match node, invariant node with
          | Statement _, Some i ->
            [xml_data "invariant" i; xml_data "invariant.scope" (getFun node).svar.vname]
          | _ ->
            (* ignore entry and return invariants, variables of wrong scopes *)
            (* TODO: don't? fix scopes? *)
            []
        end;
        begin match node with
          | Statement s ->
            [xml_data "sourcecode" (Pretty.sprint 80 (Basetype.CilStmt.pretty () s))] (* TODO: sourcecode not official? especially on node? *)
          | _ -> []
        end
      ])
  in
  let xml_edge ?(to_loop_head=false) from_node ((loc, edge):Cil.location * edge) to_node =
    Xml.Element ("edge", [("source", node_name from_node); ("target", node_name to_node)], List.concat [
        begin if loc.line <> -1 then
            [xml_data "startline" (string_of_int loc.line); xml_data "endline" (string_of_int loc.line)]
          else
            []
        end;
        begin if to_loop_head then
            [xml_data "enterLoopHead" "true"]
          else
            []
        end;
        begin match from_node, to_node with
          | _, FunctionEntry f ->
            [xml_data "enterFunction" f.vname]
          | Function f, _ ->
            [xml_data "returnFromFunction" f.vname]
          | _, _ -> []
        end
      ])
  in

  let graph_children = ref [] in
  let add_graph_child xml =
    graph_children := xml :: !graph_children
  in

  let added_nodes = NH.create 100 in
  let add_node ?(entry=false) node =
    if not (NH.mem added_nodes node) then begin
      NH.add added_nodes node ();
      add_graph_child (xml_node ~entry node)
    end
  in

  let itered_nodes = NH.create 100 in
  let rec add_edge ~to_loop_head from_node (loc, edge) to_node = match edge with
    | Proc (_, Lval (Var f, _), _) -> (* TODO: doesn't cover all cases? *)
      (* splice in function body *)
      let entry_node = FunctionEntry f in
      let return_node = Function f in
      iter_node entry_node;
      add_graph_child (xml_edge from_node (loc, edge) entry_node);
      if NH.mem added_nodes return_node then
        add_graph_child (xml_edge return_node (loc, edge) to_node)
      else
        () (* return node missing, function never returns *)
    | _ ->
      add_graph_child (xml_edge ~to_loop_head from_node (loc, edge) to_node)
  and add_edges ~to_loop_head from_node locedges to_node =
    List.iter (fun locedge -> add_edge ~to_loop_head from_node locedge to_node) locedges
  and iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      add_node node;
      List.iter (fun (locedges, to_node) ->
          let to_loop_head = NH.mem added_nodes to_node in (* TODO: not exactly correct *)
          add_node to_node;
          add_edges ~to_loop_head node locedges to_node
        ) (Cfg.next node);
      List.iter (fun (locedges, to_node) ->
          iter_node to_node
        ) (Cfg.next node)
    end
  in

  let (main_entry_nodes, other_entry_nodes) =
    entrystates
    |> List.map (fun ((n, _), _) -> n)
    |> List.partition (function
        | FunctionEntry f -> f.vname = "main"
        | _ -> false
      )
  in
  let main_entry = match main_entry_nodes, other_entry_nodes with
    | [], _ -> failwith "no main_entry_nodes"
    | _ :: _ :: _, _ -> failwith "multiple main_entry_nodes"
    | _, _ :: _ -> failwith "some other_entry_nodes"
    | [main_entry], [] -> main_entry
  in

  add_node ~entry:true main_entry;
  iter_node main_entry;

  let xml =
    Xml.Element ("graphml", [], [
        Xml.Element ("graph", [("edgedefault", "directed")], List.append [
            xml_data "witness-type" "correctness_witness";
            xml_data "sourcecodelang" "C";
            xml_data "producer" "Goblint";
            xml_data "specification" "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )";
            xml_data "programfile" (getLoc main_entry).file
          ] (List.rev !graph_children))
      ])
  in
  let out = open_out "witness.graphml" in
  Printf.fprintf out "%s" (Xml.to_string_fmt xml);
  flush out;
  close_out_noerr out