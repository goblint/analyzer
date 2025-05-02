(** Output of ARG as GraphML. *)

open MyCFG
open Svcomp
open GobConfig

module M = Messages

(* Unused *)
let write_file filename (module Task:Task) (module Arg: MyARG.S with type Edge.t = MyARG.InlineEdge.t): unit =
  let module Invariant = WitnessUtil.Invariant (Task) in

  let module N = Arg.Node in
  let module NH = Hashtbl.Make (N) in

  let main_entry = Arg.main_entry in

  (* DFS with BFS-like child ordering, just for nicer ordering of witness graph children *)
  let itered_nodes = NH.create 100 in
  let rec iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      (* write_node node; *)
      if M.tracing then M.tracei "witness" "iter_node %s" (N.to_string node);
      let edge_to_nodes =
        Arg.next node
        (* TODO: keep control (Test) edges to dead (sink) nodes for violation witness? *)
        |> List.filter_map (fun ((edge, to_node) as edge_to_node) ->
            match edge with
            | MyARG.CFGEdge _ ->
              Some edge_to_node
            | InlineEntry (_, f, args) ->
              Some (InlineEntry (None, f, args), to_node) (* remove lval to avoid duplicate edges in witness *)
            | InlineReturn (lval, f, _) ->
              Some (InlineReturn (lval, f, []), to_node) (* remove args to avoid duplicate edges in witness *)
            | InlinedEdge _
            | ThreadEntry _ ->
              None
          )
        (* deduplicate after removed lvals/args *)
        |> BatList.unique_cmp ~cmp:[%ord: MyARG.inline_edge * N.t]
      in
      List.iter (fun (edge, to_node) ->
          if M.tracing then M.tracec "witness" "edge %a to_node %s" MyARG.pretty_inline_edge edge (N.to_string to_node);
          (* write_node to_node;
          write_edge node edge to_node *)
        ) edge_to_nodes;
      if M.tracing then M.traceu "witness" "iter_node %s" (N.to_string node);
      List.iter (fun (edge, to_node) ->
          iter_node to_node
        ) edge_to_nodes
    end
  in

  iter_node main_entry


let print_svcomp_result (s: string): unit =
  Logs.result "SV-COMP result: %s" s

let print_task_result result: unit =
  print_svcomp_result (Result.to_string result)

let init (module FileCfg: MyCFG.FileCfg) =
  (* TODO: toggle analyses based on specification *)
  let module Task = struct
    include FileCfg
    let specification = Svcomp.Specification.of_option ()
  end
  in
  Logs.info "SV-COMP specification: %s" (Svcomp.Specification.to_string Task.specification);
  Svcomp.task := Some (module Task)

module Result (R: ResultQuery.SpecSysSol2) =
struct
  open R
  open SpecSys
  open Svcomp

  module Query = ResultQuery.Query (SpecSys)
  module ArgTool = ArgTools.Make (R)
  module NHT = ArgTool.NHT

  module type BiArgInvariant =
  sig
    include ArgTools.BiArg
    val find_invariant: Node.t -> Invariant.t
  end

  let determine_result entrystates (module Task:Task) (spec: Svcomp.Specification.t): Svcomp.Result.t =
    let module Arg: BiArgInvariant =
    struct
      module Node = ArgTool.Node
      module Edge = MyARG.InlineEdge
      let next _ = []
      let prev _ = []
      let find_invariant _ = Invariant.none
      let main_entry =
        let lvar = WitnessUtil.find_main_entry entrystates in
        (fst lvar, snd lvar, -1)
      let iter_nodes f = f main_entry
      let query _ q = Queries.Result.top q
    end
    in

    match spec with
    | UnreachCall _ ->
      (* error function name is globally known through Svcomp.task *)
      let is_unreach_call =
        LHT.for_all (fun (n, c) v ->
            match n with
            (* FunctionEntry isn't used for extern __VERIFIER_error... *)
            | FunctionEntry f when Svcomp.is_error_function f.svar ->
              let is_dead = Spec.D.is_bot v in
              is_dead
            | _ -> true
          ) lh
      in

      if is_unreach_call then
        Result.True
      else (
        let is_violation = function
          | FunctionEntry f when Svcomp.is_error_function f.svar -> true
          | _ -> false
        in
        (* redefine is_violation to shift violations back by one, so enterFunction __VERIFIER_error is never used *)
        let is_violation n =
          Arg.next n
          |> List.exists (fun (_, to_n) -> is_violation (Arg.Node.cfgnode to_n))
        in
        let violations =
          (* TODO: fold_nodes?s *)
          let acc = ref [] in
          Arg.iter_nodes (fun lvar ->
              if is_violation lvar then
                acc := lvar :: !acc
            );
          !acc
        in
        let module ViolationArg =
        struct
          include Arg

          let violations = violations
        end
        in
        let result_unknown () =
          (* TODO: exclude sinks before find_path? *)
          (* let is_sink = Violation.find_sinks (module ViolationArg) in *)
          Result.Unknown
        in
        if get_bool "ana.wp" then (
          match Violation.find_path (module ViolationArg) (module ViolationZ3.WP (ViolationArg.Node)) with
          | Feasible (module PathArg) ->
            (* TODO: add assumptions *)
            Result.False (Some spec)
          | Infeasible subpath ->
            (* TODO: match edges in observer? *)
            let observer_path = List.map (fun (n1, e, n2) ->
                (Arg.Node.cfgnode n1, Arg.Node.cfgnode n2)
              ) subpath
            in
            let module Spec = ObserverAnalysis.MakePathSpec (
              struct
                let path = observer_path
              end
            )
            in
            MCP.register_analysis (module Spec);
            (* TODO: don't modify JSON but have ref vars for these instead *)
            (* GobConfig.set_list "ana.activated" (Json.Build.string (Spec.name ()) :: GobConfig.get_list "ana.activated");
            GobConfig.set_list "ana.path_sens" (Json.Build.string (Spec.name ()) :: GobConfig.get_list "ana.path_sens"); *)
            (* TODO: don't append to end; currently done to get observer order to be nice *)
            GobConfig.set_list "ana.activated" (GobConfig.get_list "ana.activated" @ [`String (Spec.name ())]);
            GobConfig.set_list "ana.path_sens" (GobConfig.get_list "ana.path_sens" @ [`String (Spec.name ())]);

            raise Refinement.RestartAnalysis
          | Unknown ->
            result_unknown ()
        )
        else
          result_unknown ()
      )
    | NoDataRace ->
      if !Access.is_all_safe then
        Result.True
      else
        Result.Unknown
    | Termination ->
      if not !AnalysisState.svcomp_may_not_terminate then
        Result.True
      else
        Result.Unknown
    | NoOverflow ->
      if not !AnalysisState.svcomp_may_overflow then
        Result.True
      else
        Result.Unknown
    | ValidFree ->
      if not !AnalysisState.svcomp_may_invalid_free then
        Result.True
      else
        Result.Unknown
    | ValidDeref ->
      if not !AnalysisState.svcomp_may_invalid_deref then
        Result.True
      else
        Result.Unknown
    | ValidMemtrack ->
      if not !AnalysisState.svcomp_may_invalid_memtrack then
        Result.True
      else
        Result.Unknown
    | ValidMemcleanup ->
      if not !AnalysisState.svcomp_may_invalid_memcleanup then
        Result.True
      else
        Result.Unknown

  let determine_result entrystates (module Task:Task): Svcomp.Result.t =
    Task.specification
    |> List.fold_left (fun acc spec ->
        let result = determine_result entrystates (module Task) spec in
        match acc with
        | None -> Some result
        | Some acc ->
          match acc, result with
          (* keep old violation/unknown *)
          | False _, True
          | False _, Unknown
          | Unknown, True -> Some acc
          (* use new violation/unknown *)
          | True, False _
          | Unknown, False _
          | True, Unknown -> Some result
          (* both same, arbitrarily keep old *)
          | True, True -> Some acc
          | Unknown, Unknown -> Some acc
          | False _, False _ -> failwith "multiple violations"
      ) None
    |> Option.get

  let write entrystates =
    let module Task = (val (BatOption.get !task)) in
    let result = Timing.wrap "sv-comp result" (determine_result entrystates) (module Task) in
    print_task_result result

  let write yaml_validate_result entrystates =
    match !AnalysisState.verified, !AnalysisState.unsound_both_branches_dead with
    | _, Some true -> print_svcomp_result "ERROR (both branches dead)"
    | Some false, _ -> print_svcomp_result "ERROR (verify)"
    | _, _ ->
      match yaml_validate_result with
      | Some (Stdlib.Error msg) ->
        print_svcomp_result ("ERROR (" ^ msg ^ ")")
      | Some (Ok (Svcomp.Result.False _ | Unknown as result)) ->
        print_svcomp_result (Result.to_string result)
      | Some (Ok True)
      | None ->
        write entrystates
end
