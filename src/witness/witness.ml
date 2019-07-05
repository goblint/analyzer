open MyCFG
open WitnessUtil
open Graphml

module GML = DeDupGraphMlWriter (Node) (NodeGraphMlWriter (XmlGraphMlWriter))
module NH = Hashtbl.Make (Node)

module type Task =
sig
  val file: Cil.file
  val specification: Svcomp.specification

  val main_entry: node
  module Cfg: CfgBidir (* TODO: only needs CfgForward? *)
end

module type TaskResult =
sig
  val result: bool
  val is_live: node -> bool

  (* correctness witness *)
  val invariant: node -> Invariant.t

  (* violation witness *)
  val is_violation: node -> bool
  val is_sink: node -> bool
end


let write_file filename (module Task:Task) (module TaskResult:TaskResult): unit =
  let main_entry = Task.main_entry in
  let module Cfg = Task.Cfg in
  let loop_heads = find_loop_heads (module Cfg) Task.file in

  let out = open_out filename in
  let g = GML.start out in

  GML.write_key g "graph" "witness-type" "string" None;
  GML.write_key g "graph" "sourcecodelang" "string" None;
  GML.write_key g "graph" "producer" "string" None;
  GML.write_key g "graph" "specification" "string" None;
  GML.write_key g "graph" "programfile" "string" None;
  GML.write_key g "graph" "programhash" "string" None;
  GML.write_key g "graph" "architecture" "string" None;
  GML.write_key g "graph" "creationtime" "string" None;
  GML.write_key g "node" "entry" "boolean" (Some "false");
  GML.write_key g "node" "sink" "boolean" (Some "false");
  GML.write_key g "node" "violation" "boolean" (Some "false");
  GML.write_key g "node" "invariant" "string" None;
  GML.write_key g "node" "invariant.scope" "string" None;
  GML.write_key g "edge" "assumption" "string" None;
  GML.write_key g "edge" "assumption.scope" "string" None;
  GML.write_key g "edge" "assumption.resultfunction" "string" None;
  GML.write_key g "edge" "control" "string" None;
  GML.write_key g "edge" "startline" "int" None;
  GML.write_key g "edge" "endline" "int" None;
  GML.write_key g "edge" "startoffset" "int" None;
  GML.write_key g "edge" "endoffset" "int" None;
  GML.write_key g "edge" "enterLoopHead" "boolean" (Some "false");
  GML.write_key g "edge" "enterFunction" "string" None;
  GML.write_key g "edge" "returnFromFunction" "string" None;
  GML.write_key g "edge" "threadId" "string" None;
  GML.write_key g "edge" "createThread" "string" None;

  GML.write_metadata g "witness-type" (if TaskResult.result then "correctness_witness" else "violation_witness");
  GML.write_metadata g "sourcecodelang" "C";
  GML.write_metadata g "producer" (Printf.sprintf "Goblint (%s)" Version.goblint);
  GML.write_metadata g "specification" Task.specification;
  GML.write_metadata g "programfile" (getLoc main_entry).file;
  GML.write_metadata g "creationtime" (TimeUtil.iso8601_now ());

  let write_node ~entry node =
    GML.write_node g node (List.concat [
        begin if entry then
            [("entry", "true")]
          else
            []
        end;
        begin match node, TaskResult.invariant node with
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
          | _ -> []
        end;
        (* violation actually only allowed in violation witness *)
        (* maybe should appear on from_node of entry edge instead *)
        begin if TaskResult.is_violation node then
            [("violation", "true")]
          else
            []
        end;
        begin if TaskResult.is_sink node then
            [("sink", "true")]
          else
            []
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
      (* TODO: what should happen when same function enters and returns in different places? *)
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
      let is_sink = TaskResult.is_violation node || TaskResult.is_sink node in
      if not is_sink then begin
        let locedges_to_nodes =
          Cfg.next node
          |> List.filter (fun (_, to_node) -> TaskResult.is_live to_node)
          (* TODO: keep control (Test) edges to dead (sink) nodes for violation witness? *)
        in
        List.iter (fun (locedges, to_node) ->
            add_node to_node;
            add_edges node locedges to_node
          ) locedges_to_nodes;
        List.iter (fun (locedges, to_node) ->
            iter_node to_node
          ) locedges_to_nodes
      end
    end
  in

  add_node ~entry:true main_entry;
  iter_node main_entry;

  GML.stop g;
  close_out_noerr out