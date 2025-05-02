(** SV-COMP tasks and results. *)

open GoblintCil

module Specification = SvcompSpec

module type Task =
sig
  include MyCFG.FileCfg
  val specification: Specification.multi
end

let task: (module Task) option ref = ref None


let is_error_function' f spec =
  List.exists (function
      | Specification.UnreachCall f_spec -> f.vname = f_spec
      | _ -> false
    ) spec

let is_error_function f =
  let module Task = (val (Option.get !task)) in
  is_error_function' f Task.specification

(* TODO: unused, but should be used? *)
let is_special_function f =
  let loc = f.vdecl in
  let is_svcomp = String.ends_with loc.file ~suffix:"sv-comp.c" in (* only includes/sv-comp.c functions, not __VERIFIER_assert in benchmark *)
  let is_verifier = match f.vname with
    | fname when String.starts_with fname ~prefix:"__VERIFIER" -> true
    | fname -> is_error_function f
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
        | Termination -> "termination"
        | ValidFree -> "valid-free"
        | ValidDeref -> "valid-deref"
        | ValidMemtrack -> "valid-memtrack"
        | ValidMemcleanup -> "valid-memcleanup"
      in
      "false(" ^ result_spec ^ ")"
    | Unknown -> "unknown"
end

exception Error of string

let errorwith s = raise (Error s)
