(** ARINC Control-flow graph implementation. *)

module GU = Goblintutil
module CF = Cilfacade
open Cil
open Deriving.Cil
open Pretty
open GobConfig
include Arinc_node

type edge =
  | Computation of int
  (** Computation that takes a certain WCET *)
  | ResumeTask of int
  (** Resume another task *)
  | SuspendTask of int
  (** Suspend another task *)
  | SetEvent of int
  | WaitEvent of int
  | ResetEvent of int
  | WaitSemaphore of int
  | SignalSemaphore of int
  | PeriodicWait
  | NOP
  [@@deriving to_yojson]

type arinc_cfg = arinc_node -> ((location * edge) list * arinc_node) list

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


let our_arinc_cfg:arinc_cfg*arinc_cfg =
  let cfgF = H.create 113 in
  let cfgB = H.create 113 in

  (* Utility function to add stmt edges to the cfg *)
  let addCfg' t xs f =
    H.add cfgB t (xs,f);
    H.add cfgF f (xs,t);
    Messages.trace "cfg" "done\n\n" in
  let addCfg t (e,f) = addCfg' t [Cil.locUnknown ,e] f in
  let mkEdge fromNode edge toNode = addCfg toNode (edge, fromNode) in
  for i = 0 to 6 do
    mkEdge (PC ([0; i])) (Computation 10) (PC [1; i]);
    mkEdge (PC ([1; i])) (PeriodicWait) (PC [2; i]);
    mkEdge (PC ([2; i])) (SetEvent 0) (PC [3; i]);
    mkEdge (PC ([3; i])) (Computation 20) (PC [4; i]);
    mkEdge (PC ([4; i])) (ResumeTask 1) (PC [5; i]);
    mkEdge (PC ([5; i])) (WaitEvent 1) (PC [6; i]);
    mkEdge (PC ([6; i])) (ResetEvent 1) (PC [7; i]);
    mkEdge (PC ([7; i])) (Computation 42) (PC [8; i]);
    mkEdge (PC ([8; i])) (SuspendTask 1) (PC [9; i]);
    mkEdge (PC ([9; i])) (PeriodicWait) (PC [10; i]);
    mkEdge (PC ([10; i])) (NOP) (PC [3; i]);
  done;
  for i = 0 to 11 do
    mkEdge (PC ([i; 0])) (SuspendTask 1) (PC [i; 1]);
    mkEdge (PC ([i; 1])) (WaitSemaphore 0) (PC [i; 2]);
    mkEdge (PC ([i; 2])) (Computation 40) (PC [i; 3]);
    mkEdge (PC ([i; 3])) (SignalSemaphore 0) (PC [i; 4]);
    mkEdge (PC ([i; 4])) (SetEvent 1) (PC [i; 5]);
    mkEdge (PC ([i; 5])) (NOP) (PC [i; 1]);
  done;
  H.find_all cfgF, H.find_all cfgB
