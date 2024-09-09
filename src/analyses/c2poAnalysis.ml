(** C-2PO: A Weakly-Relational Pointer Analysis for C based on 2 Pointer Logic. The analysis can infer equalities and disequalities between terms which are built from pointer variables, with the addition of constants and dereferencing. ([c2po])*)

open Analyses
open GoblintCil
open C2poDomain
open CongruenceClosure
open Batteries
open SingleThreadedLifter
open DuplicateVars

module Spec =
struct
  include DefaultSpec
  include Analyses.IdentitySpec
  module D = D
  module C = D

  let name () = "c2po"
  let startcontext () = D.empty ()

  (** Find reachable variables in a function *)
  let reachable_from_args ctx args =
    let res =
      List.fold (fun vs e -> vs @ (ctx.ask (ReachableFrom e) |> Queries.AD.to_var_may)) [] args in
    if M.tracing then M.tracel "c2po-reachable" "reachable vars: %s\n" (List.fold_left (fun s v -> s ^v.vname ^"; ") "" res); res

  (* Returns Some true if we know for sure that it is true,
     and Some false if we know for sure that it is false,
     and None if we don't know anyhing. *)
  let eval_guard ask t e =
    let prop_list = T.prop_of_cil ask e true in
    let res = match split prop_list with
      | [], [], [] -> None
      | x::xs, _, [] -> if fst (eq_query t x) then Some true else if neq_query t x then Some false else None
      | _, y::ys, [] ->  if neq_query t y then Some true else if fst (eq_query t y) then Some false else None
      | _ -> None (*there should never be block disequalities here...*)
    in if M.tracing then M.trace "c2po" "EVAL_GUARD:\n Actual guard: %a; prop_list: %s; res = %s\n"
        d_exp e (show_conj prop_list) (Option.map_default string_of_bool "None" res); res

  (**Convert a conjunction to an invariant.*)
  let conj_to_invariant ask conjs t =
    List.fold (fun a prop -> match T.prop_to_cil prop with
        | exception (T.UnsupportedCilExpression _) ->  a
        | exp ->
          if M.tracing then M.trace "c2po-invariant" "Adding invariant: %a" d_exp exp;
          Invariant.(a && of_exp exp))
      (Invariant.top()) conjs

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | EvalInt e -> begin match eval_guard (ask_of_ctx ctx) ctx.local e with
        | None -> Result.top q
        | Some res ->
          let ik = Cilfacade.get_ikind_exp e in
          ID.of_bool ik res
      end
    | Queries.Invariant context ->
      let scope = Node.find_fundec ctx.node in
      begin match D.remove_vars_not_in_scope scope ctx.local with
        | None -> Invariant.top()
        | Some t ->
          (conj_to_invariant (ask_of_ctx ctx) (get_conjunction t) (Some t))
      end
    | _ -> Result.top q

  (** Assign the term `lterm` to the right hand side rhs, that is already
      converted to a C-2PO term. *)
  let assign_term t ask lterm rhs lval_t =
    (* ignore assignments to values that are not 64 bits *)
    match T.get_element_size_in_bits lval_t, rhs with
    (* Indefinite assignment *)
    | s, (None, _) -> D.remove_may_equal_terms ask s lterm t
    (* Definite assignment *)
    | s, (Some term, Some offset) ->
      let dummy_var = MayBeEqual.dummy_var lval_t in
      if M.tracing then M.trace "c2po-assign" "assigning: var: %s; expr: %s + %s. \nTo_cil: lval: %a; expr: %a\n" (T.show lterm) (T.show term) (Z.to_string offset) d_exp (T.to_cil lterm) d_exp (T.to_cil term);
      t |> meet_conjs_opt [Equal (dummy_var, term, offset)] |>
      D.remove_may_equal_terms ask s lterm |>
      meet_conjs_opt [Equal (lterm, dummy_var, Z.zero)] |>
      D.remove_terms_containing_variable @@ AssignAux lval_t
    | _ -> (* this is impossible *) D.top ()

  (** Assign Cil Lval to a right hand side that is already converted to
      C-2PO terms.*)
  let assign_lval t ask lval expr =
    let lval_t = typeOfLval lval in
    match T.of_lval ask lval with
    | lterm -> assign_term t ask lterm expr lval_t
    | exception (T.UnsupportedCilExpression _) ->
      (* the assigned variables couldn't be parsed, so we don't know which addresses were written to.
         We have to forget all the information we had.
         This should almost never happen.
         Except if the left hand side is a complicated expression like myStruct.field1[i]->field2[z+k], and Goblint can't infer the offset.*)
      if M.tracing then M.trace
          "c2po-invalidate" "INVALIDATE lval: %a" d_lval lval;
      D.top ()

  let assign ctx lval expr =
    let ask = (ask_of_ctx ctx) in
    let res = reset_normal_form @@ assign_lval ctx.local ask lval (T.of_cil ask expr) in
    if M.tracing then M.trace "c2po-assign" "ASSIGN: var: %a; expr: %a; result: %s. UF: %s\n" d_lval lval d_plainexp expr (D.show res) (Option.map_default (fun r -> TUF.show_uf r.uf) "None" res); res

  let branch ctx e pos =
    let props = T.prop_of_cil (ask_of_ctx ctx) e pos in
    let valid_props = T.filter_valid_pointers props in
    let res =
      if List.is_empty valid_props then ctx.local else
        reset_normal_form (meet_conjs_opt valid_props ctx.local)
    in
    if M.tracing then M.trace "c2po" "BRANCH:\n Actual equality: %a; pos: %b; valid_prop_list: %s; is_bot: %b\n"
        d_exp e pos (show_conj valid_props) (D.is_bot res);
    if D.is_bot res then raise Deadcode;
    res

  let body ctx f = ctx.local

  let assign_return ask t return_var expr =
    (* the return value is not stored on the heap, therefore we don't need to remove any terms *)
    match T.of_cil ask expr with
    | (Some term, Some offset) -> reset_normal_form (meet_conjs_opt [Equal (return_var, term, offset)] t)
    | _ -> t

  let return ctx exp_opt f =
    let res = match exp_opt with
      | Some e ->
        assign_return (ask_of_ctx ctx) ctx.local (MayBeEqual.return_var (typeOf e)) e
      | None -> ctx.local
    in if M.tracing then M.trace "c2po-function" "RETURN: exp_opt: %a; state: %s; result: %s\n" d_exp (BatOption.default (MayBeEqual.dummy_lval_print (TVoid [])) exp_opt) (D.show ctx.local) (D.show res);res

  (** var_opt is the variable we assign to. It has type lval. v=malloc.*)
  let special ctx var_opt v exprs =
    let desc = LibraryFunctions.find v in
    let ask = ask_of_ctx ctx in
    let t = begin match var_opt with
      | None ->
        ctx.local
      | Some varin ->
        (* forget information about var,
           but ignore assignments to values that are not 64 bits *)
        try
          (let s, lterm = T.get_element_size_in_bits (typeOfLval varin), T.of_lval ask varin in
           let t = D.remove_may_equal_terms ask s lterm ctx.local in
           begin match desc.special exprs with
             | Malloc _ | Calloc _ | Alloca _ ->
               add_block_diseqs t lterm
             | _ -> t
           end)
        with (T.UnsupportedCilExpression _) -> D.top ()
    end
    in
    match desc.special exprs with
    | Assert { exp; refine; _ } -> if not refine then
        ctx.local
      else
        branch ctx exp true
    | _ -> reset_normal_form t

  (**First all local variables of the function are duplicated (by negating their ID),
     then we remember the value of each local variable at the beginning of the function
     by using the analysis startState. This way we can infer the relations between the
     local variables of the caller and the pointers that were modified by the function. *)
  let enter ctx var_opt f args =
    (* add duplicated variables, and set them equal to the original variables *)
    let added_equalities = T.filter_valid_pointers (List.map (fun v -> Equal (T.term_of_varinfo (DuplicVar v), T.term_of_varinfo (NormalVar v), Z.zero)) f.sformals) in
    let state_with_duplicated_vars = meet_conjs_opt added_equalities ctx.local in
    if M.tracing then M.trace "c2po-function" "ENTER1: var_opt: %a; state: %s; state_with_duplicated_vars: %s\n" d_lval (BatOption.default (Var (Var.dummy_varinfo (TVoid [])), NoOffset) var_opt) (D.show ctx.local) (D.show state_with_duplicated_vars);
    (* remove callee vars that are not reachable and not global *)
    let reachable_variables =
      Var.from_varinfo (f.sformals @ f.slocals @ reachable_from_args ctx args) f.sformals
    in
    let new_state = D.remove_terms_not_containing_variables reachable_variables state_with_duplicated_vars in
    if M.tracing then M.trace "c2po-function" "ENTER2: result: %s\n" (D.show new_state);
    [ctx.local, reset_normal_form new_state]

  let remove_out_of_scope_vars t f =
    let local_vars = f.sformals @ f.slocals in
    let duplicated_vars = f.sformals in
    D.remove_terms_containing_variables (ReturnAux (TVoid [])::Var.from_varinfo local_vars duplicated_vars) t

  let combine_env ctx var_opt expr f args t_context_opt t (ask: Queries.ask) =
    let og_t = t in
    (* assign function parameters to duplicated values *)
    let arg_assigns = GobList.combine_short f.sformals args in
    let state_with_assignments = List.fold_left (fun st (var, exp) -> assign_term st (ask_of_ctx ctx) (T.term_of_varinfo (DuplicVar var)) (T.of_cil ask exp) var.vtype) ctx.local arg_assigns in
    if M.tracing then M.trace "c2po-function" "COMBINE_ASSIGN0: state_with_assignments: %s\n" (D.show state_with_assignments);
    (*remove all variables that were tainted by the function*)
    let tainted = ask.f (MayBeTainted)
    in
    if M.tracing then M.trace "c2po-tainted" "combine_env: %a\n" MayBeEqual.AD.pretty tainted;
    let local = D.remove_tainted_terms (ask_of_ctx ctx) tainted state_with_assignments in
    let t = D.meet local t in
    let t = reset_normal_form @@ remove_out_of_scope_vars t f in
    if M.tracing then M.trace "c2po-function" "COMBINE_ASSIGN1: var_opt: %a; local_state: %s; t_state: %s; meeting everything: %s\n" d_lval (BatOption.default (Var (Var.dummy_varinfo (TVoid[])), NoOffset) var_opt) (D.show ctx.local) (D.show og_t) (D.show t);t

  let combine_assign ctx var_opt expr f args t_context_opt t (ask: Queries.ask) =
    (* assign function parameters to duplicated values *)
    let arg_assigns = GobList.combine_short f.sformals args in
    let state_with_assignments = List.fold_left (fun st (var, exp) -> assign_term st (ask_of_ctx ctx) (T.term_of_varinfo (DuplicVar var)) (T.of_cil ask exp) var.vtype) ctx.local arg_assigns in
    let t = D.meet state_with_assignments t in
    let t = match var_opt with
      | None -> t
      | Some var -> assign_lval t ask var (Some (MayBeEqual.return_var (typeOfLval var)), Some Z.zero)
    in
    if M.tracing then M.trace "c2po-function" "COMBINE_ASSIGN2: assigning return value: %s\n" (D.show_all t);
    let t = reset_normal_form @@ remove_out_of_scope_vars t f
    in if M.tracing then M.trace "c2po-function" "COMBINE_ASSIGN3: result: %s\n" (D.show t); t

  let startstate v = D.top ()
  let threadenter ctx ~multiple lval f args = [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx = D.top()
  let exitstate  v = D.top ()

end

let _ =
  MCP.register_analysis ~dep:["startState"; "taintPartialContexts"] (module SingleThreadedLifter(Spec) : MCPSpec)