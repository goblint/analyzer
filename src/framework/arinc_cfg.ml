(** ARINC Control-flow graph implementation. *)

module GU = Goblintutil
module CF = Cilfacade
open Cil
open Deriving.Cil
include Arinc_node

type edgeAct =
  | StartComputation of int
  (** Computation that takes a certain WCET *)
  | FinishComputation
  | ResumeTask of int
  (** Resume another task *)
  | SuspendTask of int
  (** Suspend another task *)
  | TimedWait of int
  | SetEvent of int
  | WaitEvent of int
  | ResetEvent of int
  | WaitSemaphore of int
  | SignalSemaphore of int
  | PeriodicWait
  | WaitingForPeriod
  | WaitingForEndWait of int
  | NOP
  [@@deriving yojson]

type edge = int * edgeAct

type arinc_cfg = arinc_node -> ((location * edge) list * arinc_node) list

type task_node = PC of int [@@deriving yojson]
type task_edge = edgeAct [@@deriving yojson]
(* with the assumption that every node appears in the list of task_node tuples exactly once *)
(* id * priority * cfg *)
type arinc_task_cfg = int * int * (task_node * (task_edge list * task_node) list) list [@@deriving yojson]

type arinc_tasks = arinc_task_cfg list * int list [@@deriving yojson]


module type CfgBackward =
sig
  val prev : arinc_node -> ((location * edge) list * arinc_node) list
end

module type CfgForward =
sig
  val next : arinc_node -> ((location * edge) list * arinc_node) list
end

module type CfgBidir =
sig
  include CfgBackward
  include CfgForward
  val startnode: int list
  val taskinfo: int list
end

module H = BatHashtbl.Make(Arinc_Node)

(* Dumps a statement to the standard output *)
let pstmt stmt = dumpStmt defaultCilPrinter stdout 0 stmt; print_newline ()

let current_node : arinc_node option ref = ref None


(* Utility function to add stmt edges to the cfg *)
let mkEdge cfgF cfgB fromNode edge toNode =
  let addCfg t (e,f)=
    let addCfg' t xs f =
      H.add cfgB t (xs,f);
      H.add cfgF f (xs,t);
      Messages.trace "cfg" "done\n\n"
    in
    addCfg' t [Cil.locUnknown ,e] f
  in
  addCfg  toNode (edge, fromNode); ()

let create_from (a:arinc_task_cfg) (b:arinc_task_cfg) =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in
  let mkEdge = mkEdge cfgF cfgB in
  let (id0, priority0, edges0) = a in
  let (id1, priority1, edges1) = b in
  let nodeCount0 = List.length edges0 in
  let nodeCount1 = List.length edges1 in
  for i = 0 to nodeCount1 -1 do
    List.iter (function (PC from_node, edges_to_node) ->
      List.iter (function (edges, PC to_node) ->
        List.iter (function edge ->
          mkEdge (PCCombined [from_node; i]) (0,edge) (PCCombined [to_node; i])
        ) edges
      ) edges_to_node
    ) edges0
  done;
  for i = 0 to nodeCount0 -0 do
    List.iter (function (PC from_node, edges_to_node) ->
      List.iter (function (edges, PC to_node) ->
        List.iter (function edge ->
          mkEdge (PCCombined [i; from_node]) (1,edge) (PCCombined [i; to_node])
        ) edges
      ) edges_to_node
    ) edges1
  done;
  H.find_all cfgF, H.find_all cfgB

let print_to_json () =
  let tasks = [], [] in
  let yo = arinc_tasks_to_yojson tasks in
  Yojson.Safe.to_file "extracted.json"  yo;
  failwith "over and out"

let get_cfg () =
  let file = Yojson.Safe.from_file (List.nth !Goblintutil.jsonFiles 0) in
  let tasks, start = match arinc_tasks_of_yojson file  with
    | Error e -> failwith "invalid input"
    | Ok b -> b
  in
  let taskinfo = List.map (fun (a,b,c) -> b) tasks in
  let one, two = create_from (List.nth tasks 0) (List.nth tasks 1) in
  one, two, start, taskinfo
