(** Output of ARG as GraphML. *)

open MyCFG
open Graphml
open Svcomp
open GobConfig

module M = Messages

module type WitnessTaskResult = TaskResult with module Arg.Edge = MyARG.InlineEdge

let write_file filename (module Task:Task) (module TaskResult:WitnessTaskResult): unit =
  let module Invariant = WitnessUtil.Invariant (Task) in

  let module TaskResult =
    (val if get_bool "witness.graphml.stack" then
        (module StackTaskResult (TaskResult) : WitnessTaskResult)
      else
        (module TaskResult)
    )
  in
  let module N = TaskResult.Arg.Node in
  let module IsInteresting =
  struct
    (* type node = N.t
    type edge = TaskResult.Arg.Edge.t *)
    let minwitness = get_bool "witness.graphml.minimize"
    let is_interesting_real from_node edge to_node =
      (* TODO: don't duplicate this logic with write_node, write_edge *)
      (* startlines aren't currently interesting because broken, see below *)
      let from_cfgnode = N.cfgnode from_node in
      let to_cfgnode = N.cfgnode to_node in
      if TaskResult.is_violation to_node || TaskResult.is_sink to_node then
        true
      else if WitnessUtil.NH.mem Invariant.loop_heads to_cfgnode then
        true
      else begin match edge with
        | MyARG.CFGEdge (Test _) -> true
        | _ -> false
      end || begin if Invariant.is_invariant_node to_cfgnode then
               match to_cfgnode, TaskResult.invariant to_node with
               | Statement _, `Lifted _ -> true
               | _, _ -> false
             else
               false
           end || begin match from_cfgnode, to_cfgnode with
          | _, FunctionEntry f -> true
          | Function f, _ -> true
          | _, _ -> false
        end
    let is_interesting from_node edge to_node =
      not minwitness || is_interesting_real from_node edge to_node
  end
  in
  let module Arg = TaskResult.Arg in
  let module Arg = MyARG.InterestingArg (Arg) (IsInteresting) in

  let module N = Arg.Node in
  let module GML = XmlGraphMlWriter in
  let module GML =
    (val match get_string "witness.graphml.id" with
      | "node" ->
        (module ArgNodeGraphMlWriter (N) (GML) : GraphMlWriter with type node = N.t)
      | "enumerate" ->
        (module EnumerateNodeGraphMlWriter (N) (GML))
      | _ -> failwith "witness.graphml.id: illegal value"
    )
  in
  let module GML = DeDupGraphMlWriter (N) (GML) in
  let module NH = Hashtbl.Make (N) in

  let main_entry = Arg.main_entry in

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

  (* GML.write_key g "node" "goblintNode" "string" None; *)
  (* GML.write_key g "node" "sourcecode" "string" None; *)
  GML.write_key g "edge" "goblintEdge" "string" None;
  GML.write_key g "edge" "goblintLine" "string" None;
  (* TODO: remove *)
  (* GML.write_key g "edge" "enterFunction2" "string" None;
  GML.write_key g "edge" "returnFromFunction2" "string" None; *)

  GML.start_graph g;

  GML.write_metadata g "witness-type" (
      match TaskResult.result with
      | Result.True -> "correctness_witness"
      | Result.False _ -> "violation_witness"
      | Result.Unknown -> "unknown_witness"
    );
  GML.write_metadata g "sourcecodelang" "C";
  GML.write_metadata g "producer" (Printf.sprintf "Goblint (%s)" Goblint_build_info.version);
  GML.write_metadata g "specification" (Svcomp.Specification.to_string Task.specification);
  let programfile = (Node.location (N.cfgnode main_entry)).file in
  GML.write_metadata g "programfile" programfile;
  let programhash = Sha256.(to_hex (file programfile)) in
  GML.write_metadata g "programhash" programhash;
  GML.write_metadata g "architecture" (get_string "exp.architecture");
  GML.write_metadata g "creationtime" (TimeUtil.iso8601_now ());

  let write_node ?(entry=false) node =
    let cfgnode = N.cfgnode node in
    GML.write_node g node (List.concat [
        begin if entry then
            [("entry", "true")]
          else
            []
        end;
        begin
          if Invariant.is_invariant_node cfgnode then
            match cfgnode, TaskResult.invariant node with
            | Statement _, `Lifted i ->
              let i = InvariantCil.exp_replace_original_name i in
              [("invariant", CilType.Exp.show i);
              ("invariant.scope", (Node.find_fundec cfgnode).svar.vname)]
            | _ ->
              (* ignore entry and return invariants, variables of wrong scopes *)
              (* TODO: don't? fix scopes? *)
              []
          else
            []
        end;
        (* begin match cfgnode with
          | Statement s ->
            [("sourcecode", GobPretty.sprint Basetype.CilStmt.pretty s)] (* TODO: sourcecode not official? especially on node? *)
          | _ -> []
        end; *)
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
        end;
        (* [("goblintNode", match cfgnode with
           | Statement stmt  -> Printf.sprintf "s%d" stmt.sid
           | Function f      -> Printf.sprintf "ret%d%s" f.vid f.vname
           | FunctionEntry f -> Printf.sprintf "fun%d%s" f.vid f.vname
          )] *)
        (* [("goblintNode", N.to_string node)] *)
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
        begin let loc = Node.location from_cfgnode in
          (* exclude line numbers from sv-comp.c and unknown line numbers *)
          if loc.file = programfile && loc.line <> -1 then
            (* TODO: startline disabled because Ultimate doesn't like our line numbers for some reason *)
            (* [("startline", string_of_int loc.line)] *)
            [("goblintLine", string_of_int loc.line)]
          else
            []
        end;
        begin if WitnessUtil.NH.mem Invariant.loop_heads to_cfgnode then
            [("enterLoopHead", "true")]
          else
            []
        end;
        begin match from_cfgnode, to_cfgnode with
          | _, FunctionEntry f ->
            [("enterFunction", f.svar.vname)]
          | Function f, _ ->
            [("returnFromFunction", f.svar.vname)]
          | _, _ -> []
        end;
        begin match edge with
          (* control actually only allowed in violation witness *)
          | MyARG.CFGEdge (Test (_, b)) ->
            [("control", "condition-" ^ string_of_bool b)]
          (* enter and return on other side of nodes,
             more correct loc (startline) but had some scope problem? *)
          (* | MyARG.CFGEdge (Entry f) ->
            [("enterFunction2", f.svar.vname)]
          | MyARG.CFGEdge (Ret (_, f)) ->
            [("returnFromFunction2", f.svar.vname)] *)
          | _ -> []
        end;
        [("goblintEdge", Arg.Edge.to_string edge)]
      ])
  in

  (* DFS with BFS-like child ordering, just for nicer ordering of witness graph children *)
  let itered_nodes = NH.create 100 in
  let rec iter_node node =
    if not (NH.mem itered_nodes node) then begin
      NH.add itered_nodes node ();
      write_node node;
      let is_sink = TaskResult.is_violation node || TaskResult.is_sink node in
      if M.tracing then M.tracei "witness" "iter_node %s" (N.to_string node);
      if not is_sink then begin
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
            write_node to_node;
            write_edge node edge to_node
          ) edge_to_nodes;
        if M.tracing then M.traceu "witness" "iter_node %s" (N.to_string node);
        List.iter (fun (edge, to_node) ->
            iter_node to_node
          ) edge_to_nodes
      end
      else
      if M.tracing then M.traceu "witness" "iter_node %s" (N.to_string node);
    end
  in

  write_node ~entry:true main_entry;
  iter_node main_entry;

  GML.stop g;
  close_out_noerr out


let print_svcomp_result (s: string): unit =
  Logs.result "SV-COMP result: %s" s

let print_task_result (module TaskResult:TaskResult): unit =
  print_svcomp_result (Result.to_string TaskResult.result)

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

  let determine_result entrystates (module Task:Task) (spec: Svcomp.Specification.t): (module WitnessTaskResult) =
    let module Arg: BiArgInvariant =
      (val if GobConfig.get_bool "witness.graphml.enabled" then (
           let module Arg = (val ArgTool.create entrystates) in
           let module Arg =
           struct
             include Arg

             let find_invariant (n, c, i) =
               let context = {Invariant.default_context with path = Some i} in
               ask_local (n, c) (Invariant context)
           end
           in
           (module Arg: BiArgInvariant)
         )
         else (
           let module Arg =
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
           (module Arg: BiArgInvariant)
         )
      )
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

      if is_unreach_call then (
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant = Arg.find_invariant
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
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
          let is_sink = Violation.find_sinks (module ViolationArg) in
          let module TaskResult =
          struct
            module Arg = Arg
            let result = Result.Unknown
            let invariant = Arg.find_invariant
            let is_violation = is_violation
            let is_sink = is_sink
          end
          in
          (module TaskResult:WitnessTaskResult)
        in
        if get_bool "ana.wp" then (
          match Violation.find_path (module ViolationArg) (module ViolationZ3.WP (ViolationArg.Node)) with
          | Feasible (module PathArg) ->
            (* TODO: add assumptions *)
            let module TaskResult =
            struct
              module Arg = PathArg
              let result = Result.False (Some spec)
              let invariant _ = Invariant.none
              let is_violation = is_violation
              let is_sink _ = false
            end
            in
            (module TaskResult:WitnessTaskResult)
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
      (* TODO: something better than trivial ARG *)
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if !Access.is_all_safe then (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.True
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )
    | Termination ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !AnalysisState.svcomp_may_not_terminate then
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.True
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )
    | NoOverflow ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !AnalysisState.svcomp_may_overflow then
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant = Arg.find_invariant
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )
    | ValidFree ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !AnalysisState.svcomp_may_invalid_free then (
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )
    | ValidDeref ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !AnalysisState.svcomp_may_invalid_deref then (
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )
    | ValidMemtrack ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !AnalysisState.svcomp_may_invalid_memtrack then (
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )
    | ValidMemcleanup ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !AnalysisState.svcomp_may_invalid_memcleanup then (
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
        let module TaskResult =
        struct
          module Arg = TrivialArg
          let result = Result.Unknown
          let invariant _ = Invariant.none
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      )

  let determine_result entrystates (module Task:Task): (module WitnessTaskResult) =
    Task.specification
    |> List.fold_left (fun acc spec ->
        let module TaskResult = (val determine_result entrystates (module Task) spec) in
        match acc with
        | None -> Some (module TaskResult: WitnessTaskResult)
        | Some (module Acc: WitnessTaskResult) ->
          match Acc.result, TaskResult.result with
          (* keep old violation/unknown *)
          | False _, True
          | False _, Unknown
          | Unknown, True -> Some (module Acc: WitnessTaskResult)
          (* use new violation/unknown *)
          | True, False _
          | Unknown, False _
          | True, Unknown -> Some (module TaskResult: WitnessTaskResult)
          (* both same, arbitrarily keep old *)
          | True, True -> Some (module Acc: WitnessTaskResult)
          | Unknown, Unknown -> Some (module Acc: WitnessTaskResult)
          | False _, False _ -> failwith "multiple violations"
      ) None
    |> Option.get

  let write entrystates =
    let module Task = (val (BatOption.get !task)) in
    let module TaskResult = (val (Timing.wrap "sv-comp result" (determine_result entrystates) (module Task))) in

    print_task_result (module TaskResult);

    if get_bool "witness.graphml.enabled" && (TaskResult.result <> Result.Unknown || get_bool "witness.graphml.unknown") then (
      let witness_path = get_string "witness.graphml.path" in
      Timing.wrap "graphml witness" (write_file witness_path (module Task)) (module TaskResult)
    )

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
