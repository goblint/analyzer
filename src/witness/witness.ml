open MyCFG
open Graphml
open Svcomp
open GobConfig

module type WitnessTaskResult = TaskResult with module Arg.Edge = MyARG.InlineEdge

let write_file filename (module Task:Task) (module TaskResult:WitnessTaskResult): unit =
  let module Cfg = Task.Cfg in
  let module Invariant = WitnessUtil.Invariant (struct let file = Task.file end) (Cfg) in

  let module TaskResult =
    (val if get_bool "witness.stack" then
        (module StackTaskResult (Cfg) (TaskResult) : WitnessTaskResult)
      else
        (module TaskResult)
    )
  in
  let module N = TaskResult.Arg.Node in
  let module IsInteresting =
  struct
    (* type node = N.t
    type edge = TaskResult.Arg.Edge.t *)
    let minwitness = get_bool "witness.minimize"
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
            | Statement _, Some _ -> true
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
    (val match get_string "witness.id" with
      | "node" ->
        (module ArgNodeGraphMlWriter (N) (GML) : GraphMlWriter with type node = N.t)
      | "enumerate" ->
        (module EnumerateNodeGraphMlWriter (N) (GML))
      | _ -> failwith "witness.id: illegal value"
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
  GML.write_metadata g "producer" (Printf.sprintf "Goblint (%s)" Version.goblint);
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
            | Statement _, Some i ->
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
            [("sourcecode", Pretty.sprint 80 (Basetype.CilStmt.pretty () s))] (* TODO: sourcecode not official? especially on node? *)
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
      if not is_sink then begin
        let edge_to_nodes =
          Arg.next node
          (* TODO: keep control (Test) edges to dead (sink) nodes for violation witness? *)
        in
        List.iter (fun (edge, to_node) ->
            write_node to_node;
            write_edge node edge to_node
          ) edge_to_nodes;
        List.iter (fun (edge, to_node) ->
            iter_node to_node
          ) edge_to_nodes
      end
    end
  in

  write_node ~entry:true main_entry;
  iter_node main_entry;

  GML.stop g;
  close_out_noerr out


let print_svcomp_result (s: string): unit =
  Printf.printf "SV-COMP result: %s\n" s

let print_task_result (module TaskResult:TaskResult): unit =
  print_svcomp_result (Result.to_string TaskResult.result)


open Analyses
module Result (Cfg : CfgBidir)
              (Spec : Spec)
              (EQSys : GlobConstrSys with module LVar = VarF (Spec.C)
                                  and module GVar = GVarF (Spec.V)
                                  and module D = Spec.D
                                  and module G = Spec.G)
              (LHT : BatHashtbl.S with type key = EQSys.LVar.t)
              (GHT : BatHashtbl.S with type key = EQSys.GVar.t) =
struct
  open Svcomp
  let init file =
    (* TODO: toggle analyses based on specification *)
    let module Task = struct
      let file = file
      let specification = Svcomp.Specification.of_option ()

      module Cfg = Cfg
    end
    in
    Printf.printf "SV-COMP specification: %s\n" (Svcomp.Specification.to_string Task.specification);
    Svcomp.task := Some (module Task)

  let determine_result lh gh entrystates (module Task:Task): (module WitnessTaskResult) =
    let get: node * Spec.C.t -> Spec.D.t =
      fun nc -> LHT.find_default lh nc (Spec.D.bot ())
    in
    let ask_local (lvar:EQSys.LVar.t) local =
      (* build a ctx for using the query system *)
      let rec ctx =
        { ask    = (fun (type a) (q: a Queries.t) -> Spec.query ctx q)
        ; emit   = (fun _ -> failwith "Cannot \"emit\" in witness context.")
        ; node   = fst lvar
        ; prev_node = MyCFG.dummy_node
        ; control_context = Obj.repr (fun () -> snd lvar)
        ; context = (fun () -> snd lvar)
        ; edge    = MyCFG.Skip
        ; local  = local
        ; global = GHT.find gh
        ; presub = (fun _ -> raise Not_found)
        ; postsub= (fun _ -> raise Not_found)
        ; spawn  = (fun v d    -> failwith "Cannot \"spawn\" in witness context.")
        ; split  = (fun d es   -> failwith "Cannot \"split\" in witness context.")
        ; sideg  = (fun v g    -> failwith "Cannot \"sideg\" in witness context.")
        }
      in
      Spec.query ctx
    in
    let ask_indices lvar =
      let local = get lvar in
      let indices = ref [] in
      ignore ((ask_local lvar local) (Queries.IterVars (fun i ->
          indices := i :: !indices
        )));
      !indices
    in

    let module CfgNode = Node in

    let module Node =
    struct
      type t = MyCFG.node * Spec.C.t * int

      let equal (n1, c1, i1) (n2, c2, i2) =
        EQSys.LVar.equal (n1, c1) (n2, c2) && i1 = i2

      let hash (n, c, i) = 31 * EQSys.LVar.hash (n, c) + i

      let cfgnode (n, c, i) = n

      let to_string (n, c, i) =
        (* copied from NodeCtxStackGraphMlWriter *)
        let c_tag = Spec.C.tag c in
        let i_str = string_of_int i in
        match n with
        | Statement stmt  -> Printf.sprintf "s%d(%d)[%s]" stmt.sid c_tag i_str
        | Function f      -> Printf.sprintf "ret%d%s(%d)[%s]" f.svar.vid f.svar.vname c_tag i_str
        | FunctionEntry f -> Printf.sprintf "fun%d%s(%d)[%s]" f.svar.vid f.svar.vname c_tag i_str

      (* TODO: less hacky way (without ask_indices) to move node *)
      let is_live (n, c, i) = not (Spec.D.is_bot (get (n, c)))
      let move_opt (n, c, i) to_n =
        match ask_indices (to_n, c) with
        | [] -> None
        | [to_i] ->
          let to_node = (to_n, c, to_i) in
          BatOption.filter is_live (Some to_node)
        | _ :: _ :: _ ->
          failwith "Node.move_opt: ambiguous moved index"
      let equal_node_context (n1, c1, i1) (n2, c2, i2) =
        EQSys.LVar.equal (n1, c1) (n2, c2)
    end
    in

    let module NHT = BatHashtbl.Make (Node) in

    let (witness_prev_map, witness_prev, witness_next) =
      let prev = NHT.create 100 in
      let next = NHT.create 100 in
      LHT.iter (fun lvar local ->
          ignore ((ask_local lvar local) (Queries.IterPrevVars (fun i (prev_node, prev_c_obj, j) edge ->
              let lvar' = (fst lvar, snd lvar, i) in
              let prev_lvar: NHT.key = (prev_node, Obj.obj prev_c_obj, j) in
              NHT.modify_def [] lvar' (fun prevs -> (edge, prev_lvar) :: prevs) prev;
              NHT.modify_def [] prev_lvar (fun nexts -> (edge, lvar') :: nexts) next
            )))
        ) lh;

      (prev,
        (fun n ->
          NHT.find_default prev n []), (* main entry is not in prev at all *)
        (fun n ->
          NHT.find_default next n [])) (* main return is not in next at all *)
    in
    let witness_main =
      let lvar = WitnessUtil.find_main_entry entrystates in
      let main_indices = ask_indices lvar in
      (* TODO: get rid of this hack for getting index of entry state *)
      assert (List.compare_length_with main_indices 1 = 0);
      let main_index = List.hd main_indices in
      (fst lvar, snd lvar, main_index)
    in

    let module Arg =
    struct
      module Node = Node
      module Edge = MyARG.InlineEdge
      let main_entry = witness_main
      let next = witness_next
    end
    in
    let module Arg =
    struct
      open MyARG
      module ArgIntra = UnCilTernaryIntra (UnCilLogicIntra (CfgIntra (Cfg)))
      include Intra (ArgIntra) (Arg)
    end
    in

    let find_invariant (n, c, i) =
      let context: Invariant.context = {
          scope=CfgNode.find_fundec n;
          i;
          lval=None;
          offset=Cil.NoOffset;
          deref_invariant=(fun _ _ _ -> Invariant.none) (* TODO: should throw instead? *)
        }
      in
      Spec.D.invariant context (get (n, c))
    in

    match Task.specification with
    | UnreachCall _ ->
      (* error function name is globally known through Svcomp.task *)
      let is_unreach_call =
        LHT.fold (fun (n, c) v acc ->
            match n with
            (* FunctionEntry isn't used for extern __VERIFIER_error... *)
            | FunctionEntry f when Svcomp.is_error_function f.svar ->
              let is_dead = Spec.D.is_bot v in
              acc && is_dead
            | _ -> acc
          ) lh true
      in

      if is_unreach_call then (
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant = find_invariant
          let is_violation _ = false
          let is_sink _ = false
        end
        in
        (module TaskResult:WitnessTaskResult)
      ) else (
        let is_violation = function
          | FunctionEntry f, _, _ when Svcomp.is_error_function f.svar -> true
          | _, _, _ -> false
        in
        (* redefine is_violation to shift violations back by one, so enterFunction __VERIFIER_error is never used *)
        let is_violation n =
          Arg.next n
          |> List.exists (fun (_, to_n) -> is_violation to_n)
        in
        let violations =
          NHT.fold (fun lvar _ acc ->
              if is_violation lvar then
                lvar :: acc
              else
                acc
            ) witness_prev_map []
        in
        let module ViolationArg =
        struct
          include Arg

          let prev = witness_prev
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
            let invariant = find_invariant
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
              let result = Result.False (Some Task.specification)
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
    | NoOverflow ->
      let module TrivialArg =
      struct
        include Arg
        let next _ = []
      end
      in
      if not !Goblintutil.svcomp_may_overflow then
        let module TaskResult =
        struct
          module Arg = Arg
          let result = Result.True
          let invariant = find_invariant
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


  let write lh gh entrystates =
    let module Task = (val (BatOption.get !task)) in
    let module TaskResult = (val (Stats.time "determine" (determine_result lh gh entrystates) (module Task))) in

    print_task_result (module TaskResult);

    (* TODO: use witness.enabled elsewhere as well *)
    if get_bool "witness.enabled" && (TaskResult.result <> Result.Unknown || get_bool "witness.unknown") then (
      let witness_path = get_string "witness.path" in
      Stats.time "write" (write_file witness_path (module Task)) (module TaskResult)
    )

  let write lh gh entrystates =
    match !Goblintutil.verified with
    | Some false -> print_svcomp_result "ERROR (verify)"
    | _ -> write lh gh entrystates

  let write lh gh entrystates =
    Stats.time "witness" (write lh gh) entrystates
end
