(** Violation checking in an ARG. *)
open MyARG

module type ViolationArg =
sig
  include MyARG.S with module Edge = MyARG.InlineEdge

  val prev: Node.t -> (Edge.t * Node.t) list
  val violations: Node.t list
end

let find_sinks (type node) (module Arg:ViolationArg with type Node.t = node) =
  let module NHT = BatHashtbl.Make (Arg.Node) in

  let non_sinks = NHT.create 100 in

  (* DFS *)
  let rec iter_node node =
    if not (NHT.mem non_sinks node) then begin
      NHT.replace non_sinks node ();
      List.iter (fun (_, prev_node) ->
          iter_node prev_node
        ) (Arg.prev node)
    end
  in

  List.iter iter_node Arg.violations;

  fun n ->
    not (NHT.mem non_sinks n)



module type Feasibility =
sig
  module Node: MyARG.Node

  (* TODO: avoid copying this to every Feasibility? *)
  type result =
    | Feasible
    | Infeasible of (Node.t * MyARG.inline_edge * Node.t) list
    | Unknown

  val check_path: (Node.t * MyARG.inline_edge * Node.t) list -> result
end

module CfgNode = Node

module UnknownFeasibility (Arg: ArgTools.BiArg): Feasibility with module Node = Arg.Node =
struct
  module Node = Arg.Node
  module SegMap = Map.Make (Node)
  module SegNrToPathMap = Map.Make (Int)

  type result =
    | Feasible
    | Infeasible of (Node.t * MyARG.inline_edge * Node.t) list
    | Unknown

  let write (path : (Node.t * MyARG.inline_edge * Node.t) list) =
    let module FileCfg =
    struct
      let file = !Cilfacade.current_file
      module Cfg = (val !MyCFG.current_cfg)
    end in
    let module WitnessInvariant = WitnessUtil.YamlInvariant (FileCfg) in
    let module UnCilArg = Intra (UnCilTernaryIntra (UnCilLogicIntra (CfgIntra (FileCfg.Cfg)))) (Arg) in

    let open YamlWitness.Entry in
    (* TODO: duplicate code copied from YamlWitness.write *)
    let input_files = GobConfig.get_string_list "files" in
    let data_model = match GobConfig.get_string "exp.architecture" with
      | "64bit" -> "LP64"
      | "32bit" -> "ILP32"
      | _ -> failwith "invalid architecture"
    in
    let specification = Option.map (fun (module Task: Svcomp.Task) ->
        Svcomp.Specification.to_string Task.specification
      ) !Svcomp.task
    in
    let task = task ~input_files ~data_model ~specification in

    let entries = [] in

    let entries, segToPathMap, segments =
      if YamlWitness.entry_type_enabled YamlWitnessType.ViolationSequence.entry_type then (
        let open GobOption.Syntax in

        let loc prev =
          let cfgNode = Node.cfgnode prev in
          let+ location = WitnessInvariant.location_location cfgNode in
          let fundec = CfgNode.find_fundec cfgNode in
          let location_function = fundec.svar.vname in
          YamlWitness.Entry.location ~location ~location_function
        in

        let segment_for_edge prev edge =
          match edge with
          (* TODO: Correct locations for function entry and return are currently unavailable.
             As specified by the Witness 2.0 format, these locations must point to
             the closing parenthesis after the function's parameter list.
          *)
          (*
          | MyARG.InlineEntry _ ->
            let function_enter = function_enter ~location ~action:"follow" in
            let waypoints = [waypoint ~waypoint_type:(FunctionEnter function_enter)] in
            segment ~waypoints
          | MyARG.InlineReturn _ ->
            let constraint_ = constraint_ ~value:(String "1") in
            let function_return = function_return ~location ~action:"follow" ~constraint_ in
            let waypoints = [waypoint ~waypoint_type:(FunctionReturn function_return)] in
            segment ~waypoints
          *)
          | MyARG.CFGEdge Ret (None, _) -> None
          | MyARG.CFGEdge Test (_, b) ->
            let+ location = loc prev in
            let constraint_ = constraint_ ~value:(String (Bool.to_string b)) in
            let branching = branching ~location ~action:"follow" ~constraint_ in
            let waypoints = [waypoint ~waypoint_type:(Branching branching)] in
            segment ~waypoints
          | _ ->
            let+ location = loc prev in
            let constraint_ = constraint_ ~value:(String "1") in
            let assumption = assumption ~location ~constraint_ in
            let waypoints = [waypoint ~waypoint_type:(Assumption assumption)] in
            segment ~waypoints
        in

        let find_next_segment prev edge node segmap =
          let nexts = UnCilArg.next prev in
          let potential_nodes = List.filter_map (fun (new_edge, new_node) ->
              match new_edge with
              | MyARG.InlinedEdge _ -> None
              | _ ->
                let+ res : YamlWitnessType.ViolationSequence.Segment.t list = SegMap.find_opt new_node segmap in
                (new_edge, res)
            ) nexts in
          assert (List.length potential_nodes = 1); (* TODO: there might be more than one node *)
          List.hd potential_nodes
        in

        let rec build_segments path segToPathMap segNr =
          match path with
          | [] -> SegMap.empty, segToPathMap, 0
          | [(prev, edge, node)] as sub_path ->
            let target = segment ~waypoints:[waypoint ~waypoint_type:(Target (violation_target ~location:(Option.get (loc node))))] in
            let this_seg, segToPathMap, segNr = match segment_for_edge prev edge with
              | Some seg ->
                let segToPathMap1 = SegNrToPathMap.add 0 sub_path segToPathMap in
                let segToPathMap2 = SegNrToPathMap.add 1 sub_path segToPathMap1 in
                seg :: [target], segToPathMap2, 2
              | None ->
                [target], SegNrToPathMap.add 0 sub_path segToPathMap, 1
            in
            let segmap = SegMap.singleton node [target] in
            SegMap.add prev this_seg segmap, segToPathMap, segNr
          | (prev, edge, node) :: rest as sub_path ->
            let segmap, segToPathMap, segNr = build_segments rest segToPathMap segNr in
            let new_edge, uncilled = find_next_segment prev edge node segmap in
            let this_seg, segToPathMap, segNr = match segment_for_edge prev new_edge with
              | Some seg -> seg :: uncilled, SegNrToPathMap.add segNr sub_path segToPathMap, segNr + 1
              | None -> uncilled, segToPathMap, segNr
            in
            SegMap.add prev this_seg segmap, segToPathMap, segNr
        in

        let segmap, segToPathMap, _ = build_segments path SegNrToPathMap.empty 0 in
        let segments =
          match path with
          | [] -> []
          | (prev, _, _) :: _ -> SegMap.find prev segmap
        in

        let entry = YamlWitness.Entry.violation_sequence ~task ~violation:segments in
        [entry], segToPathMap, segments
      )
      else
        entries, SegNrToPathMap.empty, []
    in

    let yaml_entries = List.rev_map YamlWitnessType.Entry.to_yaml entries in
    (* TODO: "witness generation summary" message *)
    YamlWitness.yaml_entries_to_file yaml_entries (Fpath.v (GobConfig.get_string "witness.yaml.path"));
    segToPathMap, segments

  let read_command_output command =
    let (ic, _) as process = Unix.open_process command in
    let rec read_lines acc =
      try
        let line = input_line ic in
        Logs.info "Witch: %s" line;
        read_lines (line :: acc)
      with End_of_file ->
        ignore (Unix.close_process process);
        acc
    in
    read_lines []

  let extract_result_line lines =
    let re = Str.regexp "^RESULT: \\(.*\\)$" in
    List.find_map (fun line -> if Str.string_match re line 0 then Some (Str.matched_group 1 line) else None) lines

  (* TODO: find both (result and seg nr) with one traversal *)
  let extract_unreach_seg_nr lines =
    let re = Str.regexp "segment \\([0-9]+\\) cannot be passed" in
    List.find_map (fun line ->
        try ignore (Str.search_forward re line 0); Some (int_of_string (Str.matched_group 1 line))
        with Not_found -> None
      ) lines

  let run_witch witch =
    let files = String.concat " " (GobConfig.get_string_list "files") in
    let data_model = match GobConfig.get_string "exp.architecture" with
      | "64bit" -> "--64"
      | "32bit" -> "--32"
      | _ -> failwith "invalid architecture"
    in
    let witness_file_path = GobConfig.get_string "witness.yaml.path" in
    (*  ../witch/scripts/symbiotic --witness-check ../analyzer/witness.yml --32 ../analyzer/violation-witness.c *)
    let command = Printf.sprintf "%s --witness-check %s %s --guide-only %s" witch witness_file_path data_model files in
    read_command_output command

  let get_unreachable_path lines (path: (Node.t * inline_edge * Node.t) list) (segToPathMap, segments) =
    let module NHT = BatHashtbl.Make (Arg.Node) in
    let node_to_stack = NHT.create 100 in

    let collect_callstack node edge next_node =
      let prev_stack = NHT.find_option node_to_stack node |> Option.value ~default:[] in
      let new_stack = match edge with
        | InlineEntry _ -> node :: prev_stack
        | InlineReturn _ -> BatList.drop 1 prev_stack
        | _ -> prev_stack
      in
      NHT.replace node_to_stack next_node new_stack;
    in

    List.iter (fun (node, edge, next_node) -> collect_callstack node edge next_node) path;

    let has_no_branching_before_unreachable segments seg_nr =
      let rec check i: YamlWitnessType.ViolationSequence.Segment.t list -> bool = function
        | _ when i = seg_nr -> true
        | [] -> true
        | {segment = {waypoint_type = Branching _} :: _} :: _ -> false
        | _ :: rest -> check (i + 1) rest
      in
      check 0 segments
    in

    let is_within_loop (node, _, _) =
      let callstack = NHT.find_option node_to_stack node |> Option.value ~default:[] in
      let scc_components = CfgTools.node_scc_global in

      let is_in_loop node =
        match CfgTools.NH.find_option scc_components (Node.cfgnode node) with
        | Some scc when CfgTools.NH.length scc.nodes > 1 -> true
        | _ -> false
      in

      List.exists is_in_loop callstack
    in

    let has_setjump_calls (path: (Node.t * inline_edge * Node.t) list): bool =
      let rec check: (Node.t * inline_edge * Node.t) list -> bool = function
        | [] -> false
        | (_, MyARG.CFGEdge Proc (_, Lval((Var v, _)), _) , _) :: _ when v.vname = "_setjmp" -> true
        | _ :: rest -> check rest
      in
      check path
    in

    match extract_unreach_seg_nr lines with
    | Some seg_nr when has_no_branching_before_unreachable segments seg_nr ->
      (* TODO: consider seg_nr = 0 separately *)
      let path_suffix = SegNrToPathMap.find (List.length segments - seg_nr -1) segToPathMap in
      if not (is_within_loop (List.hd path_suffix)) && not (has_setjump_calls path) then path_suffix
      else path
    | _ -> path

  let check_feasability_with_witch lines path =
    match extract_result_line lines with
    | Some result when String.starts_with ~prefix:"true" result -> Printf.printf "Verification result: %s\n" result; Infeasible path
    | Some result when String.starts_with ~prefix:"false" result -> Printf.printf "Verification result: %s\n" result; Feasible
    | Some _ -> Unknown
    | None -> Unknown

  let check_path path =
    let seg = write path in
    let witch = GobConfig.get_string "exp.witch" in
    match witch with
    | "" -> Unknown
    | _ ->
      let lines = run_witch witch in
      let path = get_unreachable_path lines path seg in
      check_feasability_with_witch lines path
end


module type PathArg = MyARG.S with module Edge = MyARG.InlineEdge

type 'node result =
  | Feasible of (module PathArg with type Node.t = 'node)
  | Infeasible of ('node * MyARG.inline_edge * 'node) list
  | Unknown

let find_path (type node) (module Arg:ViolationArg with type Node.t = node) (module Feasibility:Feasibility with type Node.t = node): node result =
  let module NHT = BatHashtbl.Make (Arg.Node) in

  let rec trace_path next_nodes node2 =
    if NHT.mem next_nodes node2 then begin
      (* Logs.debug "PATH: %s" (Arg.Node.to_string node2); *)
      let (edge, next_node) = NHT.find next_nodes node2 in
      (* Logs.debug "  %a" MyCFG.pretty_edge edge; *)
      (node2, edge, next_node) :: trace_path next_nodes next_node
    end
    else
      []
  in

  let print_path path =
    List.iter (fun (n1, e, n2) ->
        Logs.info "  %s =[%s]=> %s" (Arg.Node.to_string n1) (Arg.Edge.to_string e) (Arg.Node.to_string n2)
      ) path
  in

  let exception Found of Arg.Node.t in

  let find_path nodes =
    let next_nodes = NHT.create 100 in
    let itered_nodes = NHT.create 100 in

    let rec bfs curs nexts = match curs with
      | node :: curs' ->
        if BatList.mem_cmp Arg.Node.compare node Arg.violations then
          raise (Found node)
        else if not (NHT.mem itered_nodes node) then begin
          NHT.replace itered_nodes node ();
          let next_nodes = List.filter_map (fun (edge, next_node) ->
              match edge with
              | MyARG.CFGEdge _
              | InlineEntry _
              | InlineReturn _ ->
                if not (NHT.mem itered_nodes next_node) then
                  NHT.replace next_nodes next_node (edge, node);
                Some next_node
              | InlinedEdge _
              | ThreadEntry _ -> None
            ) (Arg.next node)
          in
          bfs curs' (next_nodes @ nexts)
        end
        else
          bfs curs' nexts
      | [] ->
        match nexts with
        | [] -> ()
        | _ -> bfs nexts []
    in

    try bfs nodes []; None with
    | Found violation ->
      Some (List.rev_map (fun (n1, e, n2) -> (n2, e, n1)) (trace_path next_nodes violation)) (* TODO: inefficient rev *)
    in

    begin match find_path [Arg.main_entry] with
      | Some path ->
        print_path path;
        begin match Feasibility.check_path path with
        | Feasibility.Feasible ->
          Logs.debug "feasible";

          let module PathArg =
          struct
            module Node = Arg.Node
            module Edge = Arg.Edge

            let main_entry = BatTuple.Tuple3.first (List.hd path)

            let next =
              let module NHT = BatHashtbl.Make (Node) in
              let next = NHT.create (List.length path) in
              List.iter (fun (n1, e, n2) ->
                  NHT.modify_def [] n1 (fun nexts -> (e, n2) :: nexts) next
                ) path;

              (fun n -> NHT.find_default next n [])
          end
          in
          Feasible (module PathArg)
        | Feasibility.Infeasible subpath ->
          Logs.debug "infeasible";
          print_path subpath;

          Infeasible subpath
        | Feasibility.Unknown ->
          Logs.debug "unknown";
          Unknown
      end
    | None ->
      Unknown
  end
