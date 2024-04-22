(** A Weakly-Relational Pointer Analysis..([wrpointer])*)

(** TODO description *)

open Analyses
open GoblintCil
open WeaklyRelationalPointerDomain
module CC = CongruenceClosure
open CC.CongruenceClosure(Var)
open Batteries

module Spec =
struct
  include DefaultSpec
  include Analyses.IdentitySpec
  module D = D
  module C = D

  let name () = "wrpointer"
  let startstate v = D.empty()
  let exitstate v = D.empty()

  (* Returns Some true if we know for sure that it is true,
     and Some false if we know for sure that it is false,
     and None if we don't know anyhing. *)
  let eval_guard t e =
    match t with
      None -> Some false
    | Some t ->
      let prop_list = T.prop_of_cil e true in
      let res = match split prop_list with
        | [], [] -> None
        | x::xs, _ -> if fst (eq_query t x) then Some true else if neq_query t x then Some false else None
        | _, y::ys ->  if neq_query t y then Some true else if fst (eq_query t y) then Some false else None
      in if M.tracing then M.trace "wrpointer" "EVAL_GUARD:\n Actual guard: %a; prop_list: %s; res = %s\n"
          d_exp e (show_conj prop_list) (Option.map_default string_of_bool "None" res); res

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | EvalInt e -> begin match eval_guard ctx.local e with
        | None -> Result.top q
        | Some res ->
          let ik = Cilfacade.get_ikind_exp e in
          ID.of_bool ik res
      end
    (* TODO what is type a
       | Queries.Invariant context -> get_normal_form context*)
    | _ -> Result.top q

  let assign_lval t ask lval expr =
    match T.of_lval lval, T.of_cil expr with
    (* Indefinite assignment *)
    | (Some lterm, Some loffset), (None, _) -> D.remove_may_equal_terms ask lterm t
    (* Definite assignment *)
    | (Some lterm, Some loffset), (Some term, Some offset) when Z.equal loffset Z.zero ->
      if M.tracing then M.trace "wrpointer-assign" "assigning: var: %s + %s; expr: %s + %s\n" (T.show lterm) (Z.to_string loffset) (T.show term) (Z.to_string offset);
      t |> meet_conjs_opt [Equal (Disequalities.dummy_var, term, offset)] |>
      D.remove_may_equal_terms ask lterm |>
      meet_conjs_opt [Equal (lterm, Disequalities.dummy_var, Z.zero)] |>
      D.remove_terms_containing_variable Disequalities.dummy_var
    (* invertibe assignment *)
    | _ -> t (* TODO what if lhs is None? Just ignore? -> Not a good idea *)

  let assign ctx lval expr =
    let res = assign_lval ctx.local (ask_of_ctx ctx) lval expr in
    if M.tracing then M.trace "wrpointer-assign" "ASSIGN: var: %a; expr: %a; result: %s. UF: %s\n" d_lval lval d_exp expr (D.show res) (Option.map_default (fun r -> TUF.show_uf r.uf) "" res); res

  let branch ctx e pos =
    let props = T.prop_of_cil e pos in
    let res = meet_conjs_opt props ctx.local in
    if D.is_bot res then raise Deadcode;
    if M.tracing then M.trace "wrpointer" "BRANCH:\n Actual equality: %a; pos: %b; prop_list: %s\n"
        d_exp e pos (show_conj props);
    res

  let body ctx f = ctx.local (*DONE*)

  let assign_return t return_var expr =
    (* the return value is not stored on the heap, therefore we don't need to remove any terms *)
    match T.of_cil expr with
    | (Some term, Some offset) -> meet_conjs_opt [Equal (return_var, term, offset)] (insert_set_opt t (SSet.TSet.of_list [return_var; term]))
    | _ -> t

  let return ctx exp_opt f =
    let res = match exp_opt with
      | Some e ->
        assign_return ctx.local Disequalities.dummy_var e
      | None -> ctx.local
    in if M.tracing then M.trace "wrpointer-function" "RETURN: exp_opt: %a; state: %s; result: %s\n" d_exp (BatOption.default (Disequalities.dummy_lval) exp_opt) (D.show ctx.local) (D.show res);res

  let special ctx var_opt v exprs  =
    let desc = LibraryFunctions.find v in
    match desc.special exprs, v.vname with
    | Assert { exp; refine; _ }, _ -> if not refine then
        ctx.local
      else
        branch ctx exp true
    | _, _ -> ctx.local

  let enter ctx var_opt f args =
    (* assign function parameters *)
    let arg_assigns = GobList.combine_short f.sformals args in
    let new_state = List.fold_left (fun st (var, exp) -> assign_lval st (ask_of_ctx ctx) (Var var, NoOffset) exp) ctx.local arg_assigns in
    (* remove callee vars *)
    let arg_vars = List.map fst arg_assigns in
    let reachable_variables = arg_vars (**@ all globals bzw not_locals*)
    in
    let new_state = D.remove_terms_not_containing_variables reachable_variables new_state in
    if M.tracing then M.trace "wrpointer-function" "ENTER: var_opt: %a; state: %s; result: %s\n" d_lval (BatOption.default (Var Disequalities.dummy_varinfo, NoOffset) var_opt) (D.show ctx.local) (D.show new_state);
    [ctx.local, new_state]

  let combine_env ctx var_opt expr f exprs t_context_opt t ask =
    let local_vars = f.sformals @ f.slocals in
    let t = D.meet ctx.local t in
    let res =
      D.remove_terms_containing_variables local_vars t
    in if M.tracing then M.trace "wrpointer-function" "COMBINE_ENV: var_opt: %a; local_state: %s; t_state: %s; result: %s\n" d_lval (BatOption.default (Var Disequalities.dummy_varinfo, NoOffset) var_opt) (D.show ctx.local) (D.show t) (D.show res); res

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask =
    let ask = (ask_of_ctx ctx) in
    let t' = combine_env ctx var_opt expr f exprs t_context_opt t ask in
    let t' = match var_opt with
      | None -> t'
      | Some var -> assign_lval t' ask var Disequalities.dummy_lval
    in
    let res = D.remove_terms_containing_variable Disequalities.dummy_var t'
    in if M.tracing then M.trace "wrpointer-function" "COMBINE_ASSIGN: var_opt: %a; local_state: %s; t_state: %s; result: %s\n" d_lval (BatOption.default (Var Disequalities.dummy_varinfo, NoOffset) var_opt) (D.show ctx.local) (D.show t) (D.show res); res

  let threadenter ctx ~multiple var_opt v exprs = [ctx.local]
  let threadspawn ctx ~multiple var_opt v exprs ctx2 = ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
