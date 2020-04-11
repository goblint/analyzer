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

    try bfs nodes []; None with
    | Found ->
      Some (trace_path next_nodes Arg.main_entry)
  in

  let wp_path path =
    let open Z3 in
    let cfg = [
      ("model", "true");
      ("unsat_core", "true");
      (* TODO: do these do anything? are these necessary? *)
      (* ("smt.core.minimize", "true");
      ("sat.core.minimize", "true"); *)
    ]
    in
    let ctx = mk_context cfg in

    let rec exp_to_expr = function
      | Cil.Const (Cil.CInt64 (i, _, _)) ->
        Arithmetic.Integer.mk_numeral_s ctx (Int64.to_string i)
      | Cil.Lval (Cil.Var v, Cil.NoOffset) ->
        Arithmetic.Integer.mk_const_s ctx v.vname
      | Cil.BinOp (Cil.PlusA, e1, e2, Cil.TInt _) ->
        Arithmetic.mk_add ctx [exp_to_expr e1; exp_to_expr e2]
      | Cil.BinOp (Cil.Eq, e1, e2, Cil.TInt _) ->
        Boolean.mk_ite ctx (Boolean.mk_eq ctx (exp_to_expr e1) (exp_to_expr e2)) (Arithmetic.Integer.mk_numeral_i ctx 1) (Arithmetic.Integer.mk_numeral_i ctx 0)
      | Cil.BinOp (Cil.Ne, e1, e2, Cil.TInt _) ->
        Boolean.mk_ite ctx (Boolean.mk_distinct ctx [exp_to_expr e1; exp_to_expr e2]) (Arithmetic.Integer.mk_numeral_i ctx 1) (Arithmetic.Integer.mk_numeral_i ctx 0)
      | _ ->
        failwith "exp_to_expr"
    in

    let wp_step (_, edge, _) = match edge with
      | MyCFG.Assign ((Cil.Var v, Cil.NoOffset), e) ->
        Boolean.mk_eq ctx (Arithmetic.Integer.mk_const_s ctx v.vname) (exp_to_expr e)
      | MyCFG.Test (e, true) ->
        Boolean.mk_distinct ctx [exp_to_expr e; Arithmetic.Integer.mk_numeral_i ctx 0]
      | MyCFG.Test (e, false) ->
        Boolean.mk_eq ctx (exp_to_expr e) (Arithmetic.Integer.mk_numeral_i ctx 0)
      | _ ->
        Boolean.mk_true ctx
    in

    path
    |> List.map wp_step
    (* |> List.map Expr.to_string
    |> List.iter print_endline *)
    |> (fun asserts ->
      let solver = Solver.mk_simple_solver ctx in
      print_endline (Solver.string_of_status (Solver.check solver asserts))
    )
  in

  (* find_path (List.hd Arg.violations);
  print_endline (String.make 80 '='); *)
  begin match find_path2 Arg.violations with
    | Some path ->
      print_path path;
      wp_path path
    | None ->
      ()
  end