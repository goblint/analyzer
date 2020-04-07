module type ViolationArg =
sig
  include MyARG.S

  val prev: Node.t -> (MyCFG.edge * Node.t) list
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


exception Found

let find_path (module Arg:ViolationArg) =
  let module NHT = BatHashtbl.Make (Arg.Node) in

  let rec trace_path next_nodes node2 =
    if NHT.mem next_nodes node2 then begin
      (* ignore (Pretty.printf "PATH: %s\n" (Arg.Node.to_string node2)); *)
      let (edge, next_node) = NHT.find next_nodes node2 in
      (* ignore (Pretty.printf "  %a\n" MyCFG.pretty_edge edge); *)
      (node2, edge, next_node) :: trace_path next_nodes next_node
    end
    else
      []
  in

  let print_path path =
    List.iter (fun (n1, e, n2) ->
        ignore (Pretty.printf "  %s =[%a]=> %s\n" (Arg.Node.to_string n1) MyCFG.pretty_edge e (Arg.Node.to_string n2))
      ) path
  in

  let find_path node =
    (* TODO: delete crap find_path *)
    let next_nodes = NHT.create 100 in

    let itered_nodes = NHT.create 100 in
    (* DFS *)
    (* TODO: replace with BFS for short paths *)
    let rec iter_node node =
      if Arg.Node.equal node Arg.main_entry then
        raise Found
      else if not (NHT.mem itered_nodes node) then begin
        NHT.replace itered_nodes node ();
        List.iter (fun (edge, prev_node) ->
            NHT.replace next_nodes prev_node (edge, node);
            iter_node prev_node
          ) (Arg.prev node)
      end
    in

    try iter_node node with
    | Found ->
      let path = trace_path next_nodes Arg.main_entry in
      print_path path
  in

  let find_path2 nodes =
    let next_nodes = NHT.create 100 in

    let itered_nodes = NHT.create 100 in
    let rec bfs curs nexts = match curs with
      | node :: curs' ->
        if Arg.Node.equal node Arg.main_entry then
          raise Found
        else if not (NHT.mem itered_nodes node) then begin
          NHT.replace itered_nodes node ();
          List.iter (fun (edge, prev_node) ->
              NHT.replace next_nodes prev_node (edge, node)
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

    try bfs nodes [] with
    | Found ->
      let path = trace_path next_nodes Arg.main_entry in
      print_path path
  in

  (* find_path (List.hd Arg.violations);
  print_endline (String.make 80 '='); *)
  find_path2 Arg.violations;
  ()
