(** A Weakly-Relational Pointer Analysis..([wrpointer])*)

(** TODO description *)

open Analyses
open GoblintCil
open WeaklyRelationalPointerDomain
module CC = CongruenceClosure
open CC.CongruenceClosure(Var)

module Operations =
struct
  let assign_lval (t:D.domain) ask lval expr =
    match t with
    | None -> None
    | Some t ->
      match T.of_lval lval, T.of_cil expr with
      (* Indefinite assignment *)
      | (Some lterm, Some loffset), (None, _) -> Some (D.remove_may_equal_terms t ask lterm)
      (* Definite assignment *)
      | (Some lterm, Some loffset), (Some term, Some offset) when Z.compare loffset Z.zero = 0 ->
        meet_conjs_opt (insert_set (D.remove_may_equal_terms t ask lterm) (SSet.TSet.of_list [lterm; term])) [Equal (lterm, term, offset)]
      (* invertibe assignment *)
      | _ -> Some t (* TODO what if lhs is None? Just ignore? -> Not a good idea *)

  let branch_fn ctx e neg =
    match ctx.local with
    | None -> None
    | Some st ->
      let props = T.prop_of_cil e neg in
      let res = meet_conjs_opt st props in
      if D.is_bot res then raise Deadcode;
      if M.tracing then M.trace "wrpointer" "BRANCH:\n Actual equality: %a; neg: %b; prop_list: %s\n"
          d_exp e neg (show_conj props);
      res

  let assert_fn ctx e refine =
    if not refine then
      ctx.local
    else
      branch_fn ctx e false

  (* Returns Some true if we know for sure that it is true,
     and Some false if we know for sure that it is false,
     and None if we don't know anyhing. *)
  let eval_guard t e =
    match t with
      None -> Some false
    | Some t ->
      let prop_list = T.prop_of_cil e false in
      let res = match split prop_list with
        | [], [] -> None
        | x::xs, _ -> if fst (eq_query t x) then Some true else if neq_query t x then Some false else None
        | _, y::ys ->  if neq_query t y then Some true else if fst (eq_query t y) then Some false else None
      in if M.tracing then M.trace "wrpointer" "EVAL_GUARD:\n Actual guard: %a; prop_list: %s; res = %s\n"
          d_exp e (show_conj prop_list) (Option.fold ~none:"None" ~some:string_of_bool res); res

end

module Spec : MCPSpec =
struct
  include DefaultSpec
  include Analyses.IdentitySpec
  include Operations
  module D = D
  module C = D

  let name () = "wrpointer"
  let startstate v = D.empty()
  let exitstate v = D.empty()

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

  let assign ctx var expr =
    let res = assign_lval ctx.local (ask_of_ctx ctx) var expr in
    if M.tracing then M.trace "wrpointer-assign" "ASSIGN: var: %a; expr: %a; result: %s. UF: %s\n" d_lval var d_exp expr (D.show res) (Option.fold ~none:"" ~some:(fun r -> TUF.show_uf r.part) res); res

  let branch ctx expr b = branch_fn ctx expr (not b)

  let body ctx f = ctx.local (*DONE*)

  let return ctx exp_opt f = ctx.local

  let special ctx var_opt v exprs  =
    let desc = LibraryFunctions.find v in
    match desc.special exprs, v.vname with
    | Assert { exp; refine; _ }, _ -> assert_fn ctx exp refine
    | _, _ -> ctx.local

  let enter ctx var_opt f args =
    let state = ctx.local in
    let arg_assigns =
      GobList.combine_short f.sformals args
    in
    let new_state = List.fold_left (fun st (var, exp) -> assign_lval st (ask_of_ctx ctx) (Var var, NoOffset) exp) state arg_assigns in
    if M.tracing then M.trace "wrpointer" "ENTER: result: %s\n" (D.show new_state);
    [ctx.local, new_state] (*TODO remove callee vars?*)
  let combine_env ctx var_opt expr f exprs t_context_opt t ask = t

  let combine_assign ctx var_opt expr f exprs t_context_opt t ask = ctx.local

  let threadenter ctx ~multiple var_opt v exprs = [ctx.local]
  let threadspawn ctx ~multiple var_opt v exprs ctx2 = ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
