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



module WP (Node: MyARG.Node) =
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

  type var = varinfo

  module Var =
  struct
    type t = var
    let equal x y = x.vid = y.vid
    let compare x y = compare x.vid y.vid
  end

  module type Env =
  sig
    type t
    val empty: t
    val get_const: t -> var -> Expr.expr
    val freshen: t -> var -> t
  end

  module Env: Env =
  struct
    module VarMap = Map.Make (Var)

    type t = Expr.expr VarMap.t
    let empty = VarMap.empty
    let get_name x = x.vname
    let get_const m x =
      match VarMap.find_opt x m with
      | Some x -> x
      | None -> Arithmetic.Integer.mk_const_s ctx (get_name x)
    let sort = Arithmetic.Integer.mk_sort ctx
    let freshen env x =
      VarMap.add x (Expr.mk_fresh_const ctx (get_name x) sort) env
  end

  let bool_to_int expr =
    Boolean.mk_ite ctx expr (Arithmetic.Integer.mk_numeral_i ctx 1) (Arithmetic.Integer.mk_numeral_i ctx 0)

  let int_to_bool expr =
    Boolean.mk_distinct ctx [expr; Arithmetic.Integer.mk_numeral_i ctx 0]

  let rec exp_to_expr env = function
    | Const (CInt64 (i, _, _)) ->
      Arithmetic.Integer.mk_numeral_s ctx (Int64.to_string i)
    | Lval (Var v, NoOffset) ->
      Env.get_const env v
    | BinOp (PlusA, e1, e2, TInt _) ->
      Arithmetic.mk_add ctx [exp_to_expr env e1; exp_to_expr env e2]
    | BinOp (MinusA, e1, e2, TInt _) ->
      Arithmetic.mk_sub ctx [exp_to_expr env e1; exp_to_expr env e2]
    | BinOp (Mult, e1, e2, TInt _) ->
      Arithmetic.mk_mul ctx [exp_to_expr env e1; exp_to_expr env e2]
    | BinOp (Eq, e1, e2, TInt _) ->
      bool_to_int (Boolean.mk_eq ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | BinOp (Ne, e1, e2, TInt _) ->
      bool_to_int (Boolean.mk_distinct ctx [exp_to_expr env e1; exp_to_expr env e2])
    | BinOp (Gt, e1, e2, TInt _) ->
      bool_to_int (Arithmetic.mk_gt ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | BinOp (Lt, e1, e2, TInt _) ->
      bool_to_int (Arithmetic.mk_lt ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | BinOp (Ge, e1, e2, TInt _) ->
      bool_to_int (Arithmetic.mk_ge ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | BinOp (Le, e1, e2, TInt _) ->
      bool_to_int (Arithmetic.mk_le ctx (exp_to_expr env e1) (exp_to_expr env e2))
    | UnOp (LNot, e, TInt _) ->
      bool_to_int (Boolean.mk_not ctx (int_to_bool (exp_to_expr env e)))
    | e ->
      failwith @@ Pretty.sprint ~width:80 @@ Pretty.dprintf "exp_to_expr: %a" Cil.d_exp e

  let get_arg_vname i = Goblintutil.create_var (Cil.makeVarinfo false ("_arg" ^ string_of_int i) Cil.intType) (* TODO: correct type in general *)
  let return_vname = Goblintutil.create_var (Cil.makeVarinfo false "_return" Cil.intType) (* TODO: correct type in general *)

  let wp_assert env (from_node, (edge: MyARG.inline_edge), _) = match edge with
    | MyARG.CFGEdge (MyCFG.Assign ((Var v, NoOffset), e)) ->
      let env' = Env.freshen env v in
      (env', [Boolean.mk_eq ctx (Env.get_const env v) (exp_to_expr env' e)])
    | MyARG.CFGEdge (MyCFG.Test (e, true)) ->
      (env, [Boolean.mk_distinct ctx [exp_to_expr env e; Arithmetic.Integer.mk_numeral_i ctx 0]])
    | MyARG.CFGEdge (MyCFG.Test (e, false)) ->
      (env, [Boolean.mk_eq ctx (exp_to_expr env e) (Arithmetic.Integer.mk_numeral_i ctx 0)])
    | MyARG.CFGEdge (MyCFG.Entry fd) ->
      let env' = List.fold_left (fun acc formal ->
          Env.freshen acc formal
        ) env fd.sformals
      in
      let eqs = List.mapi (fun i formal ->
          let arg_vname = get_arg_vname i in
          Boolean.mk_eq ctx (Env.get_const env formal) (Env.get_const env' arg_vname)
        ) fd.sformals
      in
      (env', eqs)
    | MyARG.InlineEntry args ->
      let env' = BatList.fold_lefti (fun acc i arg ->
          let arg_vname = get_arg_vname i in
          Env.freshen acc arg_vname
        ) env args
      in
      let eqs = List.mapi (fun i arg ->
          let arg_vname = get_arg_vname i in
          Boolean.mk_eq ctx (Env.get_const env arg_vname) (exp_to_expr env' arg)
        ) args
      in
      (env', eqs)
    | MyARG.CFGEdge (MyCFG.Ret (None, fd)) ->
      (env, [])
    | MyARG.CFGEdge (MyCFG.Ret (Some e, fd)) ->
      let env' = Env.freshen env return_vname in
      (env', [Boolean.mk_eq ctx (Env.get_const env return_vname) (exp_to_expr env' e)])
    | MyARG.InlineReturn None ->
      (env, [])
    | MyARG.InlineReturn (Some (Var v, NoOffset)) ->
      let env' = Env.freshen env v in
      (env', [Boolean.mk_eq ctx (Env.get_const env v) (Env.get_const env' return_vname)])
    | _ ->
      (* (env, Boolean.mk_true ctx) *)
      failwith @@ Pretty.sprint ~width:80 @@ Pretty.dprintf "wp_assert: %a" MyARG.pretty_inline_edge edge

  let const_get_symbol (expr: Expr.expr): Symbol.symbol =
    assert (Expr.is_const expr);
    let func_decl = Expr.get_func_decl expr in
    FuncDecl.get_name func_decl


  type result =
    | Feasible
    | Infeasible of (Node.t * MyARG.inline_edge * Node.t) list
    | Unknown

  let wp_path path =
    let solver = Solver.mk_simple_solver ctx in
    let rec iter_wp revpath i env = match revpath with
      | [] -> Feasible
      | step :: revpath' ->
        let (env', asserts) = wp_assert env step in
        begin match asserts with
          | [] -> iter_wp revpath' (i - 1) env'
          | [expr] -> do_assert revpath' i env' expr
          | exprs ->
            let expr = Boolean.mk_and ctx exprs in
            do_assert revpath' i env' expr
        end

    and do_assert revpath' i env' expr =
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

module type PathArg = MyARG.S with module Edge = MyARG.InlineEdge

type 'node result =
  | Feasible of (module PathArg with type Node.t = 'node)
  | Infeasible
  | Unknown

let find_path (type node) (module Arg:ViolationArg with type Node.t = node): node result =
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
        ignore (Pretty.printf "  %s =[%s]=> %s\n" (Arg.Node.to_string n1) (Arg.Edge.to_string e) (Arg.Node.to_string n2))
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

  let module WP = WP (Arg.Node) in
  begin match find_path Arg.violations with
    | Some path ->
      print_path path;
      begin match WP.wp_path path with
      | WP.Feasible ->
        print_endline "feasible";

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
      | WP.Infeasible subpath ->
        print_endline "infeasible";
        print_path subpath;

        let observer_path = List.map (fun (n1, e, n2) ->
            (Arg.Node.cfgnode n1, Arg.Node.cfgnode n2)
          ) subpath
        in
        let module Spec = ObserverAnalysis.MakeSpec (
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
        GobConfig.set_list "ana.activated" (GobConfig.get_list "ana.activated" @ [Json.Build.string (Spec.name ())]);
        GobConfig.set_list "ana.path_sens" (GobConfig.get_list "ana.path_sens" @ [Json.Build.string (Spec.name ())]);
        Infeasible
      | WP.Unknown ->
        print_endline "unknown";
        Unknown
      end
    | None ->
      Unknown
  end
