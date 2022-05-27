open Cil
open Batteries

module Specification =
struct
  type t =
    | UnreachCall of string
    | NoDataRace
    | NoOverflow

  let of_string s =
    let s = String.strip s in
    let regexp = Str.regexp "CHECK( init(main()), LTL(G ! \\(.*\\)) )" in
    if Str.string_match regexp s 0 then
      let global_not = Str.matched_group 1 s in
      if global_not = "data-race" then
        NoDataRace
      else if global_not = "overflow" then
        NoOverflow
      else
        let call_regex = Str.regexp "call(\\(.*\\)())" in
        if Str.string_match call_regex global_not 0 then
          let f = Str.matched_group 1 global_not in
          UnreachCall f
        else
          failwith "Svcomp.Specification.of_string: unknown global not expression"
    else
      failwith "Svcomp.Specification.of_string: unknown expression"

  let of_file path =
    let s = BatFile.with_file_in path BatIO.read_all in
    of_string s

  let of_option () =
    let s = GobConfig.get_string "ana.specification" in
    if Sys.file_exists s then
      of_file s
    else
      of_string s

  let to_string spec =
    let global_not = match spec with
      | UnreachCall f -> "call(" ^ f ^ "())"
      | NoDataRace -> "data-race"
      | NoOverflow -> "overflow"
    in
    "CHECK( init(main()), LTL(G ! " ^ global_not ^ ") )"
end

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

  let to_bool = function
    | True -> true
    | False _ -> false
    | Unknown -> failwith "Svcomp.Result.to_bool: Unknown"

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
