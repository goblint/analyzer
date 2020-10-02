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
type arinc_task_cfg = int * (task_node * (task_edge list * task_node) list) list [@@deriving yojson]

type arinc_tasks = arinc_task_cfg list [@@deriving yojson]


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

let example_extracted () =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in
  let mkEdge = mkEdge cfgF cfgB in

  for i = 0 to 7 do
    mkEdge (PCCombined ([0; i])) (0, StartComputation 10) (PCCombined [13; i]);
    mkEdge (PCCombined ([13; i])) (0, FinishComputation) (PCCombined [1; i]);

    mkEdge (PCCombined ([1; i])) (0, PeriodicWait) (PCCombined [2; i]);
    mkEdge (PCCombined ([2; i])) (0, WaitingForPeriod) (PCCombined [3; i]);
    mkEdge (PCCombined ([3; i])) (0, SetEvent 0) (PCCombined [4; i]);

    mkEdge (PCCombined ([4; i])) (0, StartComputation 20) (PCCombined [14; i]);
    mkEdge (PCCombined ([14; i])) (0, FinishComputation) (PCCombined [5; i]);

    mkEdge (PCCombined ([5; i])) (0, ResumeTask 1) (PCCombined [6; i]);
    mkEdge (PCCombined ([6; i])) (0, TimedWait 20) (PCCombined[12;i]);
    mkEdge (PCCombined ([12; i])) (0, WaitingForEndWait 20) (PCCombined[16;i]);
    mkEdge (PCCombined ([6; i])) (0, WaitEvent 1) (PCCombined [7; i]);
    mkEdge (PCCombined ([7; i])) (0, ResetEvent 1) (PCCombined [8; i]);

    mkEdge (PCCombined ([16; i])) (0, NOP) (PCCombined[9;i]);

    mkEdge (PCCombined ([8; i])) (0, StartComputation 42) (PCCombined [15; i]);
    mkEdge (PCCombined ([15; i])) (0, FinishComputation) (PCCombined [9; i]);

    mkEdge (PCCombined ([9; i])) (0, SuspendTask 1) (PCCombined [10; i]);
    mkEdge (PCCombined ([10; i])) (0, PeriodicWait) (PCCombined [11; i]);
    mkEdge (PCCombined ([11; i])) (0, WaitingForPeriod) (PCCombined [4; i]);

  done;
  for i = 0 to 17 do
    mkEdge (PCCombined ([i; 0])) (1, SuspendTask 1) (PCCombined [i; 1]);
    mkEdge (PCCombined ([i; 1])) (1, WaitSemaphore 0) (PCCombined [i; 2]);

    mkEdge (PCCombined ([i; 2])) (1, StartComputation 40) (PCCombined [i; 6]);
    mkEdge (PCCombined ([i; 6])) (1, FinishComputation) (PCCombined [i; 3]);

    mkEdge (PCCombined ([i; 3])) (1, SignalSemaphore 0) (PCCombined [i; 4]);
    mkEdge (PCCombined ([i; 4])) (1, SetEvent 1) (PCCombined [i; 5]);
    mkEdge (PCCombined ([i; 5])) (1, NOP) (PCCombined [i; 1]);
  done;
  (* Printf.printf "!!!!!!!Edge count %i\n" (List.length (H.find_all cfgB (PCCombined ([9; 4])))); *)
  (* H.iter (fun n (e,t) -> match n,t with PCCombined [a;b], PCCombined[c;d] -> Printf.printf "[%i,%i] -> [%i,%i]\n" a b c d;) cfgF; *)
  H.find_all cfgF, H.find_all cfgB

let minimal_problematic () =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in
  let mkEdge = mkEdge cfgF cfgB in

  for i = 0 to 2 do
    mkEdge (PCCombined ([0; i])) (0, TimedWait 20) (PCCombined[1;i]);
    mkEdge (PCCombined ([1; i])) (0, WaitingForEndWait 20) (PCCombined[2;i]);
    mkEdge (PCCombined ([2; i])) (0, PeriodicWait) (PCCombined [3; i]);
    mkEdge (PCCombined ([3; i])) (0, WaitingForPeriod) (PCCombined [0; i]);

  done;
  for i = 0 to 3 do
    mkEdge (PCCombined ([i; 0])) (1, StartComputation 40) (PCCombined [i; 1]);
    mkEdge (PCCombined ([i; 1])) (1, FinishComputation) (PCCombined [i; 2]);
    mkEdge (PCCombined ([i; 2])) (1, NOP) (PCCombined [i; 0]);
  done;
  (* Printf.printf "!!!!!!!Edge count %i\n" (List.length (H.find_all cfgB (PCCombined([9; 4])))); *)
  (* H.iter (fun n (e,t) -> match n,t with PCCombined[a;b], PC[c;d] -> Printf.printf "[%i,%i] -> [%i,%i]\n" a b c d;) cfgF; *)
  H.find_all cfgF, H.find_all cfgB

let create_from (a:arinc_task_cfg) (b:arinc_task_cfg) =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in
  let mkEdge = mkEdge cfgF cfgB in
  let (id0, edges0) = a in
  let (id1, edges1) = b in
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

let get_cfg i =
  match i with
  | 0 -> example_extracted ()
  | 1 ->
    let file = Yojson.Safe.from_file (List.nth !Goblintutil.jsonFiles 0) in
    let tasks = match arinc_tasks_of_yojson file  with
      | Error e -> failwith "invalid input"
      | Ok b -> b
    in
    create_from (List.nth tasks 0) (List.nth tasks 1)
  | _ -> failwith ("Selected unknown CFG " ^ string_of_int(i))
