(** A Weakly-Relational Pointer Analysis. The analysis can infer equalities and disequalities between terms which are built from pointer variables, with the addition of constants and dereferencing. ([wrpointer])*)

open Analyses
open GoblintCil
open WeaklyRelationalPointerDomain
module CC = CongruenceClosure
open CC.CongruenceClosure
open Batteries

module Spec =
struct
  include DefaultSpec
  include Analyses.IdentitySpec
  module D = D
  module C = D

  let name () = "wrpointer"
  let startcontext () = D.empty ()

  (* find reachable variables in a function *)
  let reachable_from_args ctx args =
    let res =
      List.fold (fun vs e -> vs @ (ctx.ask (ReachableFrom e) |> Queries.AD.to_var_may)) [] args in
    if M.tracing then M.tracel "wrpointer-reachable" "reachable vars: %s\n" (List.fold_left (fun s v -> s ^v.vname ^"; ") "" res); res

  (* Returns Some true if we know for sure that it is true,
     and Some false if we know for sure that it is false,
     and None if we don't know anyhing. *)
  let eval_guard ask t e =
    let prop_list = T.prop_of_cil ask e true in
    let res = match split prop_list with
      | [], [] -> None
      | x::xs, _ -> if fst (eq_query t x) then Some true else if neq_query t x then Some false else None
      | _, y::ys ->  if neq_query t y then Some true else if fst (eq_query t y) then Some false else None
    in if M.tracing then M.trace "wrpointer" "EVAL_GUARD:\n Actual guard: %a; prop_list: %s; res = %s\n"
        d_exp e (show_conj prop_list) (Option.map_default string_of_bool "None" res); res

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | EvalInt e -> begin match eval_guard (ask_of_ctx ctx) ctx.local e with
        | None -> Result.top q
        | Some res ->
          let ik = Cilfacade.get_ikind_exp e in
          ID.of_bool ik res
      end
    (* TODO Invariant.
       | Queries.Invariant context -> get_normal_form context*)
    | _ -> Result.top q

  let assign_lval t ask lval expr =
    match T.get_element_size_in_bits (typeOfLval lval), T.of_lval ask lval, T.of_cil ask expr with
    (* Indefinite assignment *)
    | s, lterm, (None, _) -> D.remove_may_equal_terms ask s lterm t
    (* Definite assignment *)
    | s, lterm, (Some term, Some offset) ->
      let dummy_var = MayBeEqual.dummy_var (typeOfLval lval) in
      if M.tracing then M.trace "wrpointer-assign" "assigning: var: %s; expr: %s + %s. \nTo_cil: lval: %a; expr: %a\n" (T.show lterm) (T.show term) (Z.to_string offset) d_exp (T.to_cil lterm) d_exp (T.to_cil term);
      t |> meet_conjs_opt [Equal (dummy_var, term, offset)] |>
      D.remove_may_equal_terms ask s lterm |>
      meet_conjs_opt [Equal (lterm, dummy_var, Z.zero)] |>
      D.remove_terms_containing_variable @@ MayBeEqual.dummy_varinfo (typeOfLval lval)
    (* invertibe assignment *)
    | exception (T.UnsupportedCilExpression _) -> D.top ()
    (* the assigned variables couldn't be parsed, so we don't know which addresses were written to.
       We have to forget all the information we had.
       This should almost never happen.
       Except if the left hand side is an abstract type, then we don't know the size of the lvalue. *)
    | _ -> D.top ()

  let assign ctx lval expr =
    let res = assign_lval ctx.local (ask_of_ctx ctx) lval expr in
    if M.tracing then M.trace "wrpointer-assign" "ASSIGN: var: %a; expr: %a; result: %s. UF: %s\n" d_lval lval d_plainexp expr (D.show res) (Option.map_default (fun r -> TUF.show_uf r.uf) "" res); res

  let branch ctx e pos =
    let props = T.prop_of_cil (ask_of_ctx ctx) e pos in
    let res = meet_conjs_opt props ctx.local in
    if D.is_bot res then raise Deadcode;
    if M.tracing then M.trace "wrpointer" "BRANCH:\n Actual equality: %a; pos: %b; prop_list: %s\n"
        d_exp e pos (show_conj props);
    res

  let body ctx f = ctx.local (*DONE*)

  let assign_return ask t return_var expr =
    (* the return value is not stored on the heap, therefore we don't need to remove any terms *)
    match T.of_cil ask expr with
    | (Some term, Some offset) -> meet_conjs_opt [Equal (return_var, term, offset)] t
    | _ -> t

  let return ctx exp_opt f =
    let res = match exp_opt with
      | Some e ->
        assign_return (ask_of_ctx ctx) ctx.local (MayBeEqual.return_var (typeOf e)) e
      | None -> ctx.local
    in if M.tracing then M.trace "wrpointer-function" "RETURN: exp_opt: %a; state: %s; result: %s\n" d_exp (BatOption.default (MayBeEqual.dummy_lval (TVoid [])) exp_opt) (D.show ctx.local) (D.show res);res

  let special ctx var_opt v exprs  =
    let desc = LibraryFunctions.find v in
    match desc.special exprs, v.vname with
    | Assert { exp; refine; _ }, _ -> if not refine then
        ctx.local
      else
        branch ctx exp true
    | _, _ -> ctx.local

  let duplicated_variable var = { var with vid = - var.vid - 4; vname = "wrpointer__" ^ var.vname ^ "'" }
  let original_variable var = { var with vid = - (var.vid + 4); vname = String.lchop ~n:11 @@ String.rchop var.vname }

  (*First all local variables of the function are duplicated (by negating their ID),
    then we remember the value of each local variable at the beginning of the function
    by using the analysis startState. This way we can infer the relations between the
    local variables of the caller and the pointers that were modified by the function. *)
  let enter ctx var_opt f args =
    (* add duplicated variables, and set them equal to the original variables *)
    let added_equalities = (List.map (fun v -> CC.Equal (T.term_of_varinfo (duplicated_variable v), T.term_of_varinfo v, Z.zero)) f.sformals) in
    let state_with_duplicated_vars = meet_conjs_opt added_equalities ctx.local in
    if M.tracing then M.trace "wrpointer-function" "ENTER1: var_opt: %a; state: %s; state_with_duplicated_vars: %s\n" d_lval (BatOption.default (Var (MayBeEqual.dummy_varinfo (TVoid [])), NoOffset) var_opt) (D.show ctx.local) (D.show state_with_duplicated_vars);
    (* remove callee vars that are not reachable and not global *)
    let reachable_variables =
      f.sformals @ f.slocals @ List.map duplicated_variable f.sformals @ reachable_from_args ctx args
    in
    let new_state = D.remove_terms_not_containing_variables reachable_variables state_with_duplicated_vars in
    if M.tracing then M.trace "wrpointer-function" "ENTER2: result: %s\n" (D.show new_state);
    [ctx.local, new_state]

  (*ctx caller, t callee, ask callee, t_context_opt context vom callee -> C.t
     expr funktionsaufruf*)
  let combine_env ctx var_opt expr f exprs t_context_opt t (ask: Queries.ask) =
    ctx.local

  (*ctx.local is after combine_env, t callee*)
  let combine_assign ctx var_opt expr f args t_context_opt t (ask: Queries.ask) =
    let og_t = t in
    (* assign function parameters to duplicated values *)
    let arg_assigns = GobList.combine_short f.sformals args in
    let state_with_assignments = List.fold_left (fun st (var, exp) -> assign_lval st (ask_of_ctx ctx) (Var (duplicated_variable var), NoOffset) exp) ctx.local arg_assigns in
    if M.tracing then M.trace "wrpointer-function" "COMBINE_ASSIGN0: state_with_assignments: %s\n" (D.show state_with_assignments);
    (*remove all variables that were tainted by the function*)
    let tainted = (* find out the tainted variables from startState *)
      ask.f (MayPointTo (MayBeEqual.return_lval (dummyFunDec.svar.vtype)))
    in
    if M.tracing then M.trace "wrpointer-tainted" "combine_env: %a\n" MayBeEqual.AD.pretty tainted;
    let local = D.remove_tainted_terms ask tainted state_with_assignments in
    let t = D.meet local t in
    if M.tracing then M.trace "wrpointer-function" "COMBINE_ASSIGN1: var_opt: %a; local_state: %s; t_state: %s; meeting everything: %s\n" d_lval (BatOption.default (Var (MayBeEqual.dummy_varinfo (TVoid[])), NoOffset) var_opt) (D.show ctx.local) (D.show og_t) (D.show t);
    let t = match var_opt with
      | None -> t
      | Some var -> assign_lval t ask var (MayBeEqual.return_lval (typeOfLval var))
    in
    if M.tracing then M.trace "wrpointer-function" "COMBINE_ASSIGN2: assigning return value: %s\n" (D.show_all t);
    let local_vars = f.sformals @ f.slocals in
    let duplicated_vars = List.map duplicated_variable f.sformals in
    let t =
      D.remove_terms_containing_variables (MayBeEqual.return_varinfo (TVoid [])::local_vars @ duplicated_vars) t
    in if M.tracing then M.trace "wrpointer-function" "COMBINE_ASSIGN3: result: %s\n" (D.show t); t

  let startstate v = D.top ()
  let threadenter ctx ~multiple lval f args = [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx = D.top()
  let exitstate  v = D.top ()

end

let _ =
  MCP.register_analysis ~dep:["startState"; "taintPartialContexts"] (module Spec : MCPSpec)
