open MyCFG
open WitnessUtil
open Graphml

module HashedPair (M1: Hashtbl.HashedType) (M2: Hashtbl.HashedType):
  Hashtbl.HashedType with type t = M1.t * M2.t =
struct
  type t = M1.t * M2.t
  (* copied from Printable.Prod *)
  let equal (x1,x2) (y1,y2) = M1.equal x1 y1 && M2.equal x2 y2
  let hash (x,y) = M1.hash x + M2.hash y * 17
end

module HashedList (M: Hashtbl.HashedType):
  Hashtbl.HashedType with type t = M.t list =
struct
  type t = M.t list
  (* copied from Printable.Liszt *)
  let equal x y = try List.for_all2 M.equal x y with Invalid_argument _ -> false
  let hash = List.fold_left (fun xs x -> xs + M.hash x) 996699
end

module type ArgNode =
sig
  include Hashtbl.HashedType

  val cfgnode: t -> MyCFG.node
  val to_string: t -> string
end

(* Abstract Reachability Graph *)
module type Arg =
sig
  module Node: ArgNode

  val main_entry: Node.t
  val next: Node.t -> (MyCFG.edge * Node.t) list
end

module StackArgNode (ArgNode: ArgNode):
  ArgNode with type t = ArgNode.t list =
struct
  include HashedList (ArgNode)

  let cfgnode nl = ArgNode.cfgnode (List.hd nl)
  let to_string nl =
    nl
    |> List.map ArgNode.to_string
    |> String.concat "@"
end

module StackArg (Cfg:CfgForward) (Arg: Arg):
  Arg with module Node = StackArgNode (Arg.Node) =
struct
  module Node = StackArgNode (Arg.Node)

  let main_entry = [Arg.main_entry]

  let next = function
    | [] -> failwith "StackArg.next: empty"
    | n :: stack ->
      let cfgnode = Arg.Node.cfgnode n in
      match cfgnode with
      | Function _ -> (* TODO: can this be done without Cfg? *)
        begin match stack with
          (* | [] -> failwith "StackArg.next: return stack empty" *)
          | [] -> [] (* main return *)
          | call_n :: call_stack ->
            let call_cfgnode = Arg.Node.cfgnode call_n in
            let call_next =
              Cfg.next call_cfgnode
              (* filter because infinite loops starting with function call
                 will have another Neg(1) edge from the head *)
              |> List.filter (fun (locedges, to_node) ->
                  List.exists (function
                      | (_, Proc _) -> true
                      | (_, _) -> false
                    ) locedges
                )
            in
            begin match call_next with
              | [] -> failwith "StackArg.next: call next empty"
              | [(_, return_node)] ->
                Arg.next n
                |> List.filter (fun (edge, to_n) ->
                    let to_cfgnode = Arg.Node.cfgnode to_n in
                    MyCFG.Node.equal to_cfgnode return_node
                  )
                |> List.map (fun (edge, to_n) ->
                    let to_n' = to_n :: call_stack in
                    (edge, to_n')
                  )
              | _ :: _ :: _ -> failwith "StackArg.next: call next ambiguous"
            end
        end
      | _ ->
        Arg.next n
        |> List.map (fun (edge, to_n) ->
            let to_cfgnode = Arg.Node.cfgnode to_n in
            let to_n' = match to_cfgnode with
              | FunctionEntry _ -> to_n :: n :: stack
              | _ -> to_n :: stack
            in
            (edge, to_n')
          )
end


module type Task =
sig
  val file: Cil.file
  val specification: Svcomp.specification

  module Cfg: CfgBidir (* TODO: only needs CfgForward? *)
end

module type TaskResult =
sig
  module Arg: Arg

  val result: bool

  (* correctness witness *)
  val invariant: Arg.Node.t -> Invariant.t

  (* violation witness *)
  val is_violation: Arg.Node.t -> bool
  val is_sink: Arg.Node.t -> bool
end

module StackTaskResult (Cfg:CfgForward) (TaskResult: TaskResult) =
struct
  module Arg = StackArg (Cfg) (TaskResult.Arg)

  let result = TaskResult.result

  let invariant nl = TaskResult.invariant (List.hd nl)

  let is_violation nl = TaskResult.is_violation (List.hd nl)
  let is_sink nl = TaskResult.is_sink (List.hd nl)
end


(* copied from NodeGraphMlWriter *)
(* TODO: move to somewhere else but don't create cycle *)
module ArgNodeGraphMlWriter (N: ArgNode) (M: StringGraphMlWriter):
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

let write_file filename (module Task:Task) (module TaskResult:TaskResult): unit =
  let module Cfg = Task.Cfg in
  let module TaskResult = StackTaskResult (Cfg) (TaskResult) in
  let module Arg = TaskResult.Arg in
  let module N = Arg.Node in
  let module GML = DeDupGraphMlWriter (N) (ArgNodeGraphMlWriter (N) (XmlGraphMlWriter)) in
  let module NH = Hashtbl.Make (N) in

  let main_entry = Arg.main_entry in
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
  let programfile = (getLoc (N.cfgnode main_entry)).file in
  GML.write_metadata g "programfile" programfile;
  (* TODO: programhash *)
  (* TODO: architecture *)
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
        end
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
            [("startline", string_of_int loc.line)]
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
        end
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