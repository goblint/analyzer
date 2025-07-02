(** Violation checking in an ARG. *)

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

module UnknownFeasibility (Node: MyARG.Node): Feasibility with module Node = Node =
struct
  module Node = Node

  type result =
    | Feasible
    | Infeasible of (Node.t * MyARG.inline_edge * Node.t) list
    | Unknown

  let write (path : (Node.t * MyARG.inline_edge * Node.t) list) =
    let module FileCfg =
    struct
      let file = !Cilfacade.current_file
      module Cfg = (val !MyCFG.current_cfg)
    end  in
    let module WitnessInvariant = WitnessUtil.YamlInvariant (FileCfg) in

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

    let entries =
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
          let+ location = loc prev in
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
          | MyARG.CFGEdge Test (_, b) ->
            let constraint_ = constraint_ ~value:(String (Bool.to_string b)) in
            let branching = branching ~location ~action:"follow" ~constraint_ in
            let waypoints = [waypoint ~waypoint_type:(Branching branching)] in
            segment ~waypoints
          | _ ->
            let constraint_ = constraint_ ~value:(String "1") in
            let assumption = assumption ~location ~constraint_ in
            let waypoints = [waypoint ~waypoint_type:(Assumption assumption)] in
            segment ~waypoints
        in

        let rec build_segments = function
          | [] -> []
          | [(prev, edge, node)] ->
            let target = segment ~waypoints:[waypoint ~waypoint_type:(Target (violation_target ~location:(Option.get (loc node))))] in
            begin match segment_for_edge prev edge with
              | Some seg -> [seg; target]
              | None -> [target]
            end
          | (prev, edge, _) :: rest ->
            match segment_for_edge prev edge with
            | Some seg -> seg :: build_segments rest
            | None -> build_segments rest
        in

        let segments = build_segments path in

        let entry = YamlWitness.Entry.violation_sequence ~task ~violation:segments in
        [entry]
      )
      else
        entries
    in  

    let yaml_entries = List.rev_map YamlWitnessType.Entry.to_yaml entries in
    (* TODO: "witness generation summary" message *)
    YamlWitness.yaml_entries_to_file yaml_entries (Fpath.v (GobConfig.get_string "witness.yaml.path"))

  let read_command_output command =
    let (ic, _) as process = Unix.open_process command in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        ignore (Unix.close_process process);
        acc
    in
    read_lines []

  let extract_result_line lines =
    let re = Str.regexp "^RESULT: \\(.*\\)$" in
    List.find_map (fun line -> if Str.string_match re line 0 then Some (Str.matched_group 1 line) else None) lines

  let check_feasability_with_witch witch path =
    let files = String.concat " " (GobConfig.get_string_list "files") in
    let data_model = match GobConfig.get_string "exp.architecture" with
      | "64bit" -> "--64"
      | "32bit" -> "--32"
      | _ -> failwith "invalid architecture"
    in
    let witness_file_path = GobConfig.get_string "witness.yaml.path" in
    (*  ../witch/scripts/symbiotic --witness-check ../analyzer/witness.yml --32 ../analyzer/violation-witness.c *)
    let command = Printf.sprintf "%s --witness-check %s %s %s" witch witness_file_path data_model files in
    let lines = read_command_output command in
    match extract_result_line lines with
    | Some "true" -> Printf.printf "Verification result: false\n"; Infeasible path
    | Some "false" -> Printf.printf "Verification result: true\n"; Feasible
    | Some _ -> Unknown
    | None -> Unknown

  let check_path path =
    write path;
    let witch = GobConfig.get_string "exp.witch" in
    match witch with
    | "" -> Unknown
    | _ -> check_feasability_with_witch witch path
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
          List.iter (fun (edge, next_node) ->
              match edge with
              | MyARG.CFGEdge _
              | InlineEntry _
              | InlineReturn _
              | InlinedEdge _ ->
                if not (NHT.mem itered_nodes next_node) then
                  NHT.replace next_nodes next_node (edge, node)
              | ThreadEntry _ -> ()
            ) (Arg.next node);
          bfs curs' (List.map snd (Arg.next node) @ nexts)
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
