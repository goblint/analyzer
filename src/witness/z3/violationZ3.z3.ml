open Violation

module WP (Node: MyARG.Node): Feasibility with module Node = Node =
struct
  module Node = Node

  open Z3
  open GoblintCil

  let cfg = [
    ("model", "true");
    ("unsat_core", "true");
    (* TODO: do these do anything? are these necessary? *)
    (* ("smt.core.minimize", "true"); *)
    (* ("sat.core.minimize", "true"); *)
  ]
  let ctx = mk_context cfg

  type var = varinfo

  module Var = CilType.Varinfo

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
    | Const (CInt (i, _, _)) ->
      Arithmetic.Integer.mk_numeral_s ctx (Z.to_string i)
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
      failwith @@ GobPretty.sprintf "exp_to_expr: %a" Cil.d_exp e

  let get_arg_vname i = Cilfacade.create_var (Cil.makeVarinfo false ("_arg" ^ string_of_int i) Cil.intType) (* TODO: correct type in general *)
  let return_vname = Cilfacade.create_var (Cil.makeVarinfo false "_return" Cil.intType) (* TODO: correct type in general *)

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
    | MyARG.InlineEntry (_, _, args) ->
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
    | MyARG.InlineReturn (None, _, _) ->
      (env, [])
    | MyARG.InlineReturn (Some (Var v, NoOffset), _, _) ->
      let env' = Env.freshen env v in
      (env', [Boolean.mk_eq ctx (Env.get_const env v) (Env.get_const env' return_vname)])
    | _ ->
      (* (env, Boolean.mk_true ctx) *)
      failwith @@ GobPretty.sprintf "wp_assert: %a" MyARG.pretty_inline_edge edge

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

  let check_path = wp_path
end
