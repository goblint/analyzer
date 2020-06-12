open Cil
open Batteries

type specification = string
(* TODO: don't hardcode specification *)
let unreach_call_specification = "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )"

let is_error_function f = match f.vname with
  | "__VERIFIER_error" (* old error function *)
  | "reach_error" -> true (* new error function (https://github.com/sosy-lab/sv-benchmarks/pull/1077) *)
  | _ -> false

let is_special_function f =
  let loc = f.vdecl in
  let is_svcomp = String.ends_with loc.file "sv-comp.c" in (* only includes/sv-comp.c functions, not __VERIFIER_assert in benchmark *)
  let is_verifier = match f.vname with
    | fname when String.starts_with fname "__VERIFIER" -> true
    | "reach_error" -> true
    | _ -> false
  in
  is_svcomp && is_verifier


module type Task =
sig
  val file: Cil.file
  val specification: specification

  module Cfg: MyCFG.CfgForward
end

module type TaskResult =
sig
  module Arg: MyARG.S

  val result: bool

  (* correctness witness *)
  val invariant: Arg.Node.t -> Invariant.t

  (* violation witness *)
  val is_violation: Arg.Node.t -> bool
  val is_sink: Arg.Node.t -> bool
end

module StackTaskResult (Cfg:MyCFG.CfgForward) (TaskResult: TaskResult) =
struct
  module Arg = MyARG.Stack (Cfg) (TaskResult.Arg)

  let result = TaskResult.result

  let invariant nl = TaskResult.invariant (List.hd nl)

  let is_violation nl = TaskResult.is_violation (List.hd nl)
  let is_sink nl = TaskResult.is_sink (List.hd nl)
end
