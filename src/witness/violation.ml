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



module WP =
struct
  open Z3
  open Cil

  let cfg = [
    ("model", "true");
    ("unsat_core", "true");
    (* TODO: do these do anything? are these necessary? *)
    (* ("smt.core.minimize", "true");
    ("sat.core.minimize", "true"); *)
  ]
  let ctx = mk_context cfg

  type var = string

  module type Env =
  sig
      type t
      val empty: t
      val get_const: t -> var -> Expr.expr
      val freshen: t -> var -> t
  end

  module Env: Env =
  struct
      module StringMap = Map.Make (String)

      type t = Expr.expr StringMap.t
      let empty = StringMap.empty
      let get_const m x =
          match StringMap.find_opt x m with
          | Some x -> x
          | None -> Arithmetic.Integer.mk_const_s ctx x
      let sort = Arithmetic.Integer.mk_sort ctx
      let freshen env x =
          StringMap.add x (Expr.mk_fresh_const ctx x sort) env
  end

  let bool_to_int expr =
    Boolean.mk_ite ctx expr (Arithmetic.Integer.mk_numeral_i ctx 1) (Arithmetic.Integer.mk_numeral_i ctx 0)

  let rec exp_to_expr env = function
    | Const (CInt64 (i, _, _)) ->
      Arithmetic.Integer.mk_numeral_s ctx (Int64.to_string i)
    | Lval (Var v, NoOffset) ->
      Env.get_const env v.vname
    | BinOp (PlusA, e1, e2, TInt _) ->
      Arithmetic.mk_add ctx [exp_to_expr env e1; exp_to_expr env e2]
    | BinOp (Eq, e1, e2, TInt _) ->
      bool_to_int (Boolean.mk_eq ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | BinOp (Ne, e1, e2, TInt _) ->
      bool_to_int (Boolean.mk_distinct ctx [exp_to_expr env e1; exp_to_expr env e2])
    | BinOp (Gt, e1, e2, TInt _) ->
      bool_to_int (Arithmetic.mk_gt ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | e ->
      failwith @@ Pretty.sprint ~width:80 @@ Pretty.dprintf "exp_to_expr: %a" Cil.d_exp e

  let wp_assert env (_, edge, _) = match edge with
    | MyCFG.Assign ((Var v, NoOffset), e) ->
      let env' = Env.freshen env v.vname in
      (env', Boolean.mk_eq ctx (Env.get_const env v.vname) (exp_to_expr env' e))
    | MyCFG.Test (e, true) ->
      (env, Boolean.mk_distinct ctx [exp_to_expr env e; Arithmetic.Integer.mk_numeral_i ctx 0])
    | MyCFG.Test (e, false) ->
      (env, Boolean.mk_eq ctx (exp_to_expr env e) (Arithmetic.Integer.mk_numeral_i ctx 0))
    | _ ->
      (* (env, Boolean.mk_true ctx) *)
      failwith @@ Pretty.sprint ~width:80 @@ Pretty.dprintf "wp_assert: %a" MyCFG.pretty_edge edge

  let const_get_symbol (expr: Expr.expr): Symbol.symbol =
    assert (Expr.is_const expr);
    let func_decl = Expr.get_func_decl expr in
    FuncDecl.get_name func_decl


  type 'a result =
    | Feasible
    | Infeasible of ('a * MyCFG.edge * 'a) list
    | Unknown

  let wp_path path =
    let solver = Solver.mk_simple_solver ctx in
    let rec iter_wp revpath i env = match revpath with
      | [] -> Feasible
      | step :: revpath' ->
        let (env', expr) = wp_assert env step in
        Printf.printf "%d: %s\n" i (Expr.to_string expr);

        let track_const = Boolean.mk_const ctx (Symbol.mk_int ctx i) in
        Solver.assert_and_track solver expr track_const;

        let status = Solver.check solver [] in
        Printf.printf "%d: %s\n" i (Solver.string_of_status status);
        match Solver.check solver [] with
        | Solver.SATISFIABLE ->
          Printf.printf "%d: %s\n" i (Model.to_string (BatOption.get @@ Solver.get_model solver));
          iter_wp revpath' (i - 1) env'

        | Solver.UNSATISFIABLE ->
          (* TODO: this doesn't exist in Z3 API? *)
          let extract_track expr =
            assert (Expr.is_const expr);
            let symbol = const_get_symbol expr in
            assert (Symbol.is_int_symbol symbol);
            Symbol.get_int symbol
          in
          let unsat_core = Solver.get_unsat_core solver in
          let unsat_core_is =
            unsat_core
            |> List.map extract_track
            |> List.sort compare
          in
          unsat_core_is
          |> List.map string_of_int
          |> String.concat " "
          |> print_endline;

          let (mini, maxi) = BatList.min_max unsat_core_is in
          let unsat_path = BatList.filteri (fun i _ -> mini <= i && i <= maxi) path in (* TODO: optimize subpath *)
          Infeasible unsat_path

        | Solver.UNKNOWN ->
          Unknown
    in
    iter_wp (List.rev path) (List.length path - 1) Env.empty
end


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
              if not (NHT.mem itered_nodes prev_node) then
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

  begin match find_path Arg.violations with
    | Some path ->
      print_path path;
      begin match WP.wp_path path with
      | WP.Feasible ->
        print_endline "feasible"
      | WP.Infeasible subpath ->
        print_endline "infeasible";
        print_path subpath;

        let get_sid = function
          | MyCFG.Statement s -> s.sid
          | _ -> -1
        in
        let observer_path = List.map (fun (n1, e, n2) ->
            (get_sid (Arg.Node.cfgnode n1), get_sid (Arg.Node.cfgnode n2))
          ) subpath
        in
        let module Spec = ObserverAnalysis.MakeSpec (
          struct
            let path = observer_path
          end
        )
        in
        MCP.register_analysis (module Spec)

      | WP.Unknown ->
        print_endline "unknown"
      end
    | None ->
      ()
  end
