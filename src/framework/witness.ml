open MyCFG

module NH = Hashtbl.Make (Node)
module NS = Set.Make (Node)

let find_main_entry entrystates =
  let (main_entry_nodes, other_entry_nodes) =
    entrystates
    |> List.map (fun ((n, _), _) -> n)
    |> List.partition (function
        | FunctionEntry f -> f.vname = "main"
        | _ -> false
      )
  in
  match main_entry_nodes, other_entry_nodes with
  | [], _ -> failwith "no main_entry_nodes"
  | _ :: _ :: _, _ -> failwith "multiple main_entry_nodes"
  | _, _ :: _ -> failwith "some other_entry_nodes"
  | [main_entry], [] -> main_entry

let find_loop_heads (module Cfg:CfgBidir) (file:Cil.file): unit NH.t =
  let loop_heads = NH.create 100 in
  let global_visited_nodes = NH.create 100 in

  (* DFS *)
  let rec iter_node path_visited_nodes node =
    if NS.mem node path_visited_nodes then
      NH.add loop_heads node ()
    else if not (NH.mem global_visited_nodes node) then begin
      NH.add global_visited_nodes node ();
      let new_path_visited_nodes = NS.add node path_visited_nodes in
      List.iter (fun (_, to_node) ->
          iter_node new_path_visited_nodes to_node
        ) (Cfg.next node)
    end
  in

  Cil.iterGlobals file (function
      | GFun (fd, _) ->
        let entry_node = FunctionEntry fd.svar in
        iter_node NS.empty entry_node
      | _ -> ()
    );

  loop_heads


module type GraphMlWriter =
sig
  type t
  type node

  val start: out_channel -> t
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
      mutable children: Xml.xml list
    }
  type node = string

  let start out = { out; children = [] }

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
      Xml.Element ("graphml", [], [
          Xml.Element ("graph", [("edgedefault", "directed")], List.rev g.children)
        ])
    in
    output_string g.out (Xml.to_string_fmt xml);
    flush g.out
end

module NodeGraphMlWriter (M: StringGraphMlWriter):
  (GraphMlWriter with type node = MyCFG.node) =
struct
  type t = M.t
  type node = MyCFG.node

  let string_of_node = function
    | Statement stmt  -> Printf.sprintf "s%d" stmt.sid
    | Function f      -> Printf.sprintf "ret%d%s" f.vid f.vname
    | FunctionEntry f -> Printf.sprintf "fun%d%s" f.vid f.vname

  let start = M.start
  let write_metadata = M.write_metadata
  let write_node g node datas = M.write_node g (string_of_node node) datas
  let write_edge g source target datas = M.write_edge g (string_of_node source) (string_of_node target) datas
  let stop = M.stop
end

module DeDupGraphMlWriter (Node: Hashtbl.HashedType) (M: GraphMlWriter with type node = Node.t): (GraphMlWriter with type node = Node.t) =
struct
  module H = Hashtbl.Make(Node)

  type t =
    {
      delegate: M.t;
      added_nodes: unit H.t
    }
  type node = M.node

  let start out = { delegate = M.start out; added_nodes = H.create 100 }
  let write_metadata {delegate} = M.write_metadata delegate
  let write_node {delegate; added_nodes} node datas =
    if not (H.mem added_nodes node) then begin
      H.add added_nodes node ();
      M.write_node delegate node datas
    end
  let write_edge {delegate} = M.write_edge delegate
  let stop {delegate} = M.stop delegate
end

let write_file filename (module Cfg:CfgBidir) (file:Cil.file) entrystates (invariant:node -> Invariant.t) (is_live:node -> bool): unit =
  let main_entry = find_main_entry entrystates in
  let loop_heads = find_loop_heads (module Cfg) file in


  let module GML = DeDupGraphMlWriter (Node) (NodeGraphMlWriter (XmlGraphMlWriter)) in
  let out = open_out filename in
  let g = GML.start out in

  GML.write_metadata g "witness-type" "correctness_witness";
  GML.write_metadata g "sourcecodelang" "C";
  GML.write_metadata g "producer" "Goblint";
  GML.write_metadata g "specification" "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )";
  GML.write_metadata g "programfile" (getLoc main_entry).file;

  let write_node ~entry node =
    GML.write_node g node (List.concat [
        begin if entry then
            [("entry", "true")]
          else
            []
        end;
        begin match node, invariant node with
          | Statement _, Some i ->
            [("invariant", i);
             ("invariant.scope", (getFun node).svar.vname)]
          | _ ->
            (* ignore entry and return invariants, variables of wrong scopes *)
            (* TODO: don't? fix scopes? *)
            []
        end;
        begin match node with
          | Statement s ->
            [("sourcecode", Pretty.sprint 80 (Basetype.CilStmt.pretty () s))] (* TODO: sourcecode not official? especially on node? *)

          (* violation actually only allowed in violation witness *)
          (* maybe should appear on from_node of entry edge instead *)
          | FunctionEntry f when f.vname = "__VERIFIER_error" ->
            [("violation", "true")]
          | _ -> []
        end
      ])
  in
  let write_edge from_node ((loc, edge):Cil.location * edge) to_node =
    GML.write_edge g from_node to_node (List.concat [
        begin if loc.line <> -1 then
            [("startline", string_of_int loc.line);
             ("endline", string_of_int loc.line)]
          else
            []
        end;
        begin if NH.mem loop_heads to_node then
            [("enterLoopHead", "true")]
          else
            []
        end;
        begin match from_node, to_node with
          | _, FunctionEntry f ->
            [("enterFunction", f.vname)]
          | Function f, _ ->
            [("returnFromFunction", f.vname)]
          | _, _ -> []
        end;
        begin match edge with
          (* control actually only allowed in violation witness *)
          | Test (_, b) ->
            [("control", "condition-" ^ string_of_bool b)]
          (* enter and return on other side of nodes,
             more correct loc (startline) but had some scope problem? *)
          | Entry f ->
            [("enterFunction2", f.svar.vname)]
          | Ret (_, f) ->
            [("returnFromFunction2", f.svar.vname)]
          | _ -> []
        end
      ])
  in

  let add_node ?(entry=false) node =
    write_node ~entry node
  in

  (* BFS, just for nicer ordering of witness graph children *)
  let itered_nodes = NH.create 100 in
  let rec add_edge from_node (loc, edge) to_node = match edge with
    | Proc (_, Lval (Var f, _), _) -> (* TODO: doesn't cover all cases? *)
      (* splice in function body *)
      let entry_node = FunctionEntry f in
      let return_node = Function f in
      iter_node entry_node;
      write_edge from_node (loc, edge) entry_node;
      if NH.mem itered_nodes return_node then
        write_edge return_node (loc, edge) to_node
      else
        () (* return node missing, function never returns *)
    | _ ->
      write_edge from_node (loc, edge) to_node
  and add_edges from_node locedges to_node =
    List.iter (fun locedge -> add_edge from_node locedge to_node) locedges
  and iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      add_node node;
      let locedges_to_nodes =
        Cfg.next node
        |> List.filter (fun (_, to_node) -> is_live to_node)
      in
      List.iter (fun (locedges, to_node) ->
          add_node to_node;
          add_edges node locedges to_node
        ) locedges_to_nodes;
      List.iter (fun (locedges, to_node) ->
          iter_node to_node
        ) locedges_to_nodes
    end
  in

  add_node ~entry:true main_entry;
  iter_node main_entry;

  GML.stop g;
  close_out_noerr out