open MyCFG
open WitnessUtil
open Graphml
open Svcomp
open GobConfig

let write_file filename (module Task:Task) (module TaskResult:TaskResult): unit =
  let module Cfg = Task.Cfg in
  let loop_heads = find_loop_heads (module Cfg) Task.file in

  let module TaskResult = StackTaskResult (Cfg) (TaskResult) in
  let module N = TaskResult.Arg.Node in
  let module IsInteresting =
  struct
    type t = N.t
    let minwitness = get_bool "exp.minwitness"
    let is_interesting_real from_node edge to_node =
      (* TODO: don't duplicate this logic with write_node, write_edge *)
      (* startlines aren't currently interesting because broken, see below *)
      let from_cfgnode = N.cfgnode from_node in
      let to_cfgnode = N.cfgnode to_node in
      if TaskResult.is_violation to_node || TaskResult.is_sink to_node then
        true
      else if WitnessUtil.NH.mem loop_heads to_cfgnode then
        true
      else begin match edge with
        | Test _ -> true
        | _ -> false
      end || begin match to_cfgnode, TaskResult.invariant to_node with
          | Statement _, Some _ -> true
          | _, _ -> false
        end || begin match from_cfgnode, to_cfgnode with
          | _, FunctionEntry f -> true
          | Function f, _ -> false
          | _, _ -> false
        end
    let is_interesting from_node edge to_node =
      not minwitness || is_interesting_real from_node edge to_node
  end
  in
  let module Arg = TaskResult.Arg in
  let module Arg = MyARG.InterestingArg (Arg) (IsInteresting) in

  let module N = Arg.Node in
  let module GML = DeDupGraphMlWriter (N) (ArgNodeGraphMlWriter (N) (XmlGraphMlWriter)) in
  let module NH = Hashtbl.Make (N) in

  let main_entry = Arg.main_entry in

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

  GML.write_key g "node" "goblintNode" "string" None;
  GML.write_key g "edge" "goblintEdge" "string" None;
  GML.write_key g "edge" "goblintLine" "string" None;

  GML.write_metadata g "witness-type" (if TaskResult.result then "correctness_witness" else "violation_witness");
  GML.write_metadata g "sourcecodelang" "C";
  GML.write_metadata g "producer" (Printf.sprintf "Goblint (%s)" Version.goblint);
  GML.write_metadata g "specification" Task.specification;
  let programfile = (getLoc (N.cfgnode main_entry)).file in
  GML.write_metadata g "programfile" programfile;
  let programhash =
    (* TODO: calculate SHA-256 hash without external process *)
    let in_channel = Unix.open_process_in ("sha256sum " ^ programfile) in
    let line = really_input_string in_channel 64 in
    close_in in_channel;
    line
  in
  GML.write_metadata g "programhash" programhash;
  GML.write_metadata g "architecture" (get_string "exp.architecture");
  GML.write_metadata g "creationtime" (TimeUtil.iso8601_now ());

  let write_node ?(entry=false) node =
    let cfgnode = N.cfgnode node in
    GML.write_node g node (List.concat [
        begin if entry then
            [("entry", "true")]
          else
            []
        end;
        begin match cfgnode, TaskResult.invariant node with
          | Statement _, Some i ->
            [("invariant", i);
             ("invariant.scope", (getFun cfgnode).svar.vname)]
          | _ ->
            (* ignore entry and return invariants, variables of wrong scopes *)
            (* TODO: don't? fix scopes? *)
            []
        end;
        begin match cfgnode with
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
        end;
        [("goblintNode", match cfgnode with
           | Statement stmt  -> Printf.sprintf "s%d" stmt.sid
           | Function f      -> Printf.sprintf "ret%d%s" f.vid f.vname
           | FunctionEntry f -> Printf.sprintf "fun%d%s" f.vid f.vname
          )]
      ])
  in
  let write_edge from_node edge to_node =
    let from_cfgnode = N.cfgnode from_node in
    let to_cfgnode = N.cfgnode to_node in
    GML.write_edge g from_node to_node (List.concat [
        (* TODO: add back loc as argument with edge? *)
        (* begin if loc.line <> -1 then
               [("startline", string_of_int loc.line);
                ("endline", string_of_int loc.line)]
             else
               []
           end; *)
        begin let loc = getLoc from_cfgnode in
          (* exclude line numbers from sv-comp.c and unknown line numbers *)
          if loc.file = programfile && loc.line <> -1 then
            (* TODO: startline disabled because Ultimate doesn't like our line numbers for some reason *)
            (* [("startline", string_of_int loc.line)] *)
            [("goblintLine", string_of_int loc.line)]
          else
            []
        end;
        begin if WitnessUtil.NH.mem loop_heads to_cfgnode then
            [("enterLoopHead", "true")]
          else
            []
        end;
        begin match from_cfgnode, to_cfgnode with
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
        end;
        [("goblintEdge", Pretty.sprint 80 (pretty_edge () edge))]
      ])
  in

  (* DFS with BFS-like child ordering, just for nicer ordering of witness graph children *)
  let itered_nodes = NH.create 100 in
  let rec iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      write_node node;
      let is_sink = TaskResult.is_violation node || TaskResult.is_sink node in
      if not is_sink then begin
        let edge_to_nodes =
          Arg.next node
          (* TODO: keep control (Test) edges to dead (sink) nodes for violation witness? *)
        in
        List.iter (fun (edge, to_node) ->
            write_node to_node;
            write_edge node edge to_node
          ) edge_to_nodes;
        List.iter (fun (edge, to_node) ->
            iter_node to_node
          ) edge_to_nodes
      end
    end
  in

  write_node ~entry:true main_entry;
  iter_node main_entry;

  GML.stop g;
  close_out_noerr out