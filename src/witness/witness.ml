open MyCFG
open WitnessUtil
open Graphml

module HashedPair (M1: Hashtbl.HashedType) (M2: Hashtbl.HashedType): (Hashtbl.HashedType with type t = M1.t * M2.t) =
struct
  type t = M1.t * M2.t
  (* copied from Printable.Prod *)
  let equal (x1,x2) (y1,y2) = M1.equal x1 y1 && M2.equal x2 y2
  let hash (x,y) = M1.hash x + M2.hash y * 17
end

module HashedList (M: Hashtbl.HashedType): (Hashtbl.HashedType with type t = M.t list) =
struct
  type t = M.t list
  (* copied from Printable.Liszt *)
  let equal x y = try List.for_all2 M.equal x y with Invalid_argument _ -> false
  let hash = List.fold_left (fun xs x -> xs + M.hash x) 996699
end

module type ArgNode =
sig
  include Hashtbl.HashedType

  val node: t -> MyCFG.node
  val to_string: t -> string
end

(* Abstract Reachability Graph *)
module type Arg =
sig
  module Node: ArgNode

  val main_entry: Node.t
  val next: Node.t -> (MyCFG.edge * Node.t) list
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


(* copied from NodeCtxStackGraphMlWriter *)
(* TODO: move to somewhere else but don't create cycle *)
module ArgNodeStackGraphMlWriter (N: ArgNode) (M: StringGraphMlWriter):
  (GraphMlWriter with type node = N.t list) =
struct
  type t = M.t
  type node = N.t list

  let string_of_nodectxstack nc =
    nc
    |> List.map N.to_string
    |> String.concat "@"

  let start = M.start
  let write_key = M.write_key
  let write_metadata = M.write_metadata
  let write_node g node datas = M.write_node g (string_of_nodectxstack node) datas
  let write_edge g source target datas = M.write_edge g (string_of_nodectxstack source) (string_of_nodectxstack target) datas
  let stop = M.stop
end

let write_file filename (module Task:Task) (module TaskResult:TaskResult): unit =
  let module Arg = TaskResult.Arg in
  let module N = Arg.Node in
  let module NL = HashedList (N) in
  let module GML = DeDupGraphMlWriter (NL) (ArgNodeStackGraphMlWriter (N) (XmlGraphMlWriter)) in
  let module NH = Hashtbl.Make (Node) in
  let module NLH = Hashtbl.Make (NL) in

  let main_entry = Arg.main_entry in
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
  GML.write_metadata g "programfile" (getLoc (N.node main_entry)).file;
  (* TODO: programhash *)
  (* TODO: architecture *)
  GML.write_metadata g "creationtime" (TimeUtil.iso8601_now ());

  let write_node ~entry nodectxstack =
    let nodectx = List.hd nodectxstack in
    let node = N.node nodectx in
    GML.write_node g nodectxstack (List.concat [
        begin if entry then
            [("entry", "true")]
          else
            []
        end;
        begin match node, TaskResult.invariant nodectx with
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
        begin if TaskResult.is_violation nodectx then
            [("violation", "true")]
          else
            []
        end;
        begin if TaskResult.is_sink nodectx then
            [("sink", "true")]
          else
            []
        end
      ])
  in
  let write_edge from_nodectxstack edge to_nodectxstack =
    let from_node = N.node (List.hd from_nodectxstack) in
    let to_node = N.node (List.hd to_nodectxstack) in
    GML.write_edge g from_nodectxstack to_nodectxstack (List.concat [
        (* TODO: add back loc as argument with edge? *)
        (* begin if loc.line <> -1 then
               [("startline", string_of_int loc.line);
                ("endline", string_of_int loc.line)]
             else
               []
           end; *)
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

  let add_node ?(entry=false) nodectxstack =
    write_node ~entry nodectxstack
  in

  (* DFS with BFS-like child ordering, just for nicer ordering of witness graph children *)
  let itered_nodestacks = NLH.create 100 in
  let rec add_edge from_nodectxstack edge to_nodectxstack =
    write_edge from_nodectxstack edge to_nodectxstack
  and iter_nodestack = function
    | [] -> failwith "empty nodectxstack"
    | (nodectx :: nodectxtl) as nodectxstack ->
      if not (NLH.mem itered_nodestacks nodectxstack) then begin
        NLH.add itered_nodestacks nodectxstack ();
        add_node nodectxstack;
        let is_sink = TaskResult.is_violation nodectx || TaskResult.is_sink nodectx in
        if not is_sink then begin
          let locedges_to_nodectxstacks =
            Arg.next nodectx
            (* TODO: keep control (Test) edges to dead (sink) nodes for violation witness? *)
            |> List.map (fun (edge, to_nodectx) -> (edge, to_nodectx :: nodectxtl))
          in
          List.iter (fun (edge, to_nodectxstack) ->
              add_node to_nodectxstack;
              add_edge nodectxstack edge to_nodectxstack
            ) locedges_to_nodectxstacks;
          List.iter (fun (edge, to_nodectxstack) ->
              iter_nodestack to_nodectxstack
            ) locedges_to_nodectxstacks
        end
      end
  in

  let main_entrystack = [main_entry] in
  add_node ~entry:true main_entrystack;
  iter_nodestack main_entrystack;

  GML.stop g;
  close_out_noerr out