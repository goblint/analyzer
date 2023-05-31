(** SV-COMP tasks and results. *)

open GoblintCil
open Batteries

module Specification = SvcompSpec

module type Task =
sig
  val file: Cil.file
  val specification: Specification.t

  module Cfg: MyCFG.CfgBidir
end

let task: (module Task) option ref = ref None


let is_error_function f =
  let module Task = (val (Option.get !task)) in
  match Task.specification with
  | UnreachCall f_spec -> f.vname = f_spec
  | _ -> false

(* TODO: unused, but should be used? *)
let is_special_function f =
  let loc = f.vdecl in
  let is_svcomp = String.ends_with loc.file "sv-comp.c" in (* only includes/sv-comp.c functions, not __VERIFIER_assert in benchmark *)
  let is_verifier = match f.vname with
    | fname when String.starts_with fname "__VERIFIER" -> true
    | fname ->
      let module Task = (val (Option.get !task)) in
      match Task.specification with
      | UnreachCall f_spec -> fname = f_spec
      | _ -> false
  in
  is_svcomp && is_verifier


module Result =
struct
  type t =
    | True
    | False of Specification.t option
    | Unknown

  let to_string = function
    | True -> "true"
    | False None -> "false"
    | False (Some spec) ->
      let result_spec = match spec with
        | UnreachCall _ -> "unreach-call"
        | NoOverflow -> "no-overflow"
        | NoDataRace -> "no-data-race" (* not yet in SV-COMP/Benchexec *)
      in
      "false(" ^ result_spec ^ ")"
    | Unknown -> "unknown"
end

module type TaskResult =
sig
  module Arg: MyARG.S

  val result: Result.t

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
