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

module UnknownFeasibility (Node: MyARG.Node): Feasibility with module Node = Node =
struct
  module Node = Node

  type result =
    | Feasible
    | Infeasible of (Node.t * MyARG.inline_edge * Node.t) list
    | Unknown

  let check_path _ = Unknown
end


exception Found

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

  let find_path nodes =
    let next_nodes = NHT.create 100 in

    let itered_nodes = NHT.create 100 in
    let rec bfs curs nexts = match curs with
      | node :: curs' ->
        if Arg.Node.equal node Arg.main_entry then
          raise Found
        else if not (NHT.mem itered_nodes node) then begin
          NHT.replace itered_nodes node ();
          List.iter (fun (edge, prev_node) ->
              match edge with
              | MyARG.CFGEdge _
              | InlineEntry _
              | InlineReturn _ ->
                if not (NHT.mem itered_nodes prev_node) then
                  NHT.replace next_nodes prev_node (edge, node)
              | InlinedEdge _
              | ThreadEntry _ -> ()
            ) (Arg.prev node);
          bfs curs' (List.map snd (Arg.prev node) @ nexts)
        end
        else
          bfs curs' nexts
      | [] ->
        match nexts with
        | [] -> ()
        | _ -> bfs nexts []
    in

    try bfs nodes []; None with
    | Found ->
      Some (trace_path next_nodes Arg.main_entry)
  in

  begin match find_path Arg.violations with
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
