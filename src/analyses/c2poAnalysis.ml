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
  let startcontext () = D.top ()

  (** Find reachable variables in a function *)
  let reachable_from_args ctx args =
    let collect_reachable_from_exp acc e =
      let reachable_from_exp = ctx.ask (ReachableFrom e) in
      let reachable_from_exp = Queries.AD.to_var_may reachable_from_exp in
      reachable_from_exp @ acc
    in
    let res = List.fold collect_reachable_from_exp [] args in
    if M.tracing then M.tracel "c2po-reachable" "reachable vars: %s\n" (List.fold_left (fun s v -> s ^ v.vname ^"; ") "" res);
    res

  (* Returns Some true if we know for sure that it is true,
     and Some false if we know for sure that it is false,
     and None if we don't know anyhing. *)
  let eval_guard ask d e ik =
    let cc = d.data in
    let open Queries in
    let prop_list = T.prop_of_cil ask e true in
    match split prop_list with
    | [], [], [] ->
      ID.top()
    | x::xs, _, [] ->
      if fst (eq_query cc x) then
        ID.of_bool ik true
      else if neq_query cc x then
        ID.of_bool ik false
      else
        ID.top()
    | _, y::ys, [] ->
      if neq_query cc y then
        ID.of_bool ik true
      else if fst (eq_query cc y) then
        ID.of_bool ik false
      else
        ID.top()
    | _ ->
      ID.top() (*there should never be block disequalities here...*)

  (** Convert a conjunction to an invariant.*)
  let conj_to_invariant ask conjs =
    let f a prop =
      try
        let exp = T.prop_to_cil prop in (* May raise UnsupportedCilExpression *)
        if M.tracing then M.trace "c2po-invariant" "Adding invariant: %a" d_exp exp;
        Invariant.(a && of_exp exp)
      with T.UnsupportedCilExpression _ ->
        a
    in
    List.fold f (Invariant.top()) conjs

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match ctx.local with
    | `Bot -> Result.top q
    | `Lifted d ->
      match q with
      | EvalInt e ->
        let ik = Cilfacade.get_ikind_exp e in
        eval_guard (ask_of_man ctx) d e ik
      | Queries.Invariant context ->
        let scope = Node.find_fundec ctx.node in
        let cc = D.remove_vars_not_in_scope scope d.data in
        let conj = get_conjunction_from_data cc in
        let ask = ask_of_man ctx in
        conj_to_invariant ask conj
      | _ ->
        Result.top q

  (** Assign the right hand side rhs (that is already
      converted to a C-2PO term) to the term `lterm`. *)
  let assign_term d ask lterm rhs lval_t =
    let cc = d.data in
    (* ignore assignments to values that are not 64 bits *)
    match T.get_element_size_in_bits lval_t, rhs with
    (* Indefinite assignment *)
    | lval_size, (None, _) ->
      let cc = D.remove_may_equal_terms ask lval_size lterm cc in
      data_to_t cc
    (* Definite assignment *)
    | lval_size, (Some rterm, Some roffset) ->
      let dummy_var = MayBeEqual.dummy_var lval_t in

      if M.tracing then M.trace "c2po-assign" "assigning: var: %s; expr: %s + %s. \nTo_cil: lval: %a; expr: %a\n" (T.show lterm) (T.show rterm) (Z.to_string roffset) d_exp (T.to_cil lterm) d_exp (T.to_cil rterm);

      let equal_dummy_rterm = [Equal (dummy_var, rterm, roffset)] in
      let equal_dummy_lterm = [Equal (lterm, dummy_var, Z.zero)] in

      cc |>
      meet_conjs_opt equal_dummy_rterm |>
      D.remove_may_equal_terms ask lval_size lterm |>
      meet_conjs_opt equal_dummy_lterm |>
      D.remove_terms_containing_aux_variable |>
      data_to_t
    | _ -> (* this is impossible *)
      C2PODomain.top ()

  (** Assign Cil Lval to a right hand side that is already converted to
      C-2PO terms.*)
  let assign_lval cc ask lval expr =
    let lval_t = typeOfLval lval in
    try
      let lterm = T.of_lval ask lval in
      assign_term cc ask lterm expr lval_t
    with T.UnsupportedCilExpression _ ->
      (* the assigned variables couldn't be parsed, so we don't know which addresses were written to.
         We have to forget all the information we had.
         This should almost never happen.
         Except if the left hand side is a complicated expression like myStruct.field1[i]->field2[z+k], and Goblint can't infer the offset.*)
      if M.tracing then M.trace "c2po-invalidate" "Invalidate lval: %a" d_lval lval;
      C2PODomain.top ()

  let assign ctx lval expr =
    let ask = (ask_of_man ctx) in
    match ctx.local with
    | `Bot ->
      `Bot
    | `Lifted d ->
      let cc = assign_lval d ask lval (T.of_cil ask expr) in
      let cc = reset_normal_form cc in
      let res = `Lifted cc in
      if M.tracing then M.trace "c2po-assign" "assign: var: %a; expr: %a; result: %s.\n" d_lval lval d_plainexp expr (D.show res);
      res

  let branch ctx e pos =
    let props = T.prop_of_cil (ask_of_man ctx) e pos in
    let valid_props = T.filter_valid_pointers props in
    let res =
      match ctx.local with
      | `Bot -> `Bot
      | `Lifted d ->
        if List.is_empty valid_props then
          `Lifted d
        else
          try
            let meet = meet_conjs_opt valid_props d.data in
            let t = data_to_t meet in
            `Lifted t
          with Unsat ->
            `Bot
    in
    if M.tracing then M.trace "c2po" "branch:\n Actual equality: %a; pos: %b; valid_prop_list: %s; is_bot: %b\n" d_exp e pos (show_conj valid_props) (D.is_bot res);
    if D.is_bot res then raise Deadcode;
    res

  let body ctx f =
    ctx.local

  let assign_return ask d return_var expr =
    (* the return value is not stored on the heap, therefore we don't need to remove any terms *)
    match T.of_cil ask expr with
    | (Some term, Some offset) ->
      let ret_var_eq_term = [Equal (return_var, term, offset)] in
      let assign_by_meet = meet_conjs_opt ret_var_eq_term d.data in
      data_to_t assign_by_meet
    | _ -> d

  let return ctx exp_opt f =
    let res =
      match exp_opt with
      | Some e ->
        begin match ctx.local with
          | `Bot ->
            `Bot
          | `Lifted d ->
            let return_var = MayBeEqual.return_var (typeOf e) in
            let d = assign_return (ask_of_man ctx) d return_var e in
            `Lifted d
        end
      | None -> ctx.local
    in
    if M.tracing then M.trace "c2po-function" "return: exp_opt: %a; state: %s; result: %s\n" d_exp (BatOption.default (MayBeEqual.dummy_lval_print (TVoid [])) exp_opt) (D.show ctx.local) (D.show res);
    res

  (** var_opt is the variable we assign to. It has type lval. v=malloc.*)
  let special ctx lval_opt v exprs =
    let desc = LibraryFunctions.find v in
    let ask = ask_of_man ctx in
    match ctx.local with
    | `Bot -> `Bot
    | `Lifted d ->
      let t =
        begin match lval_opt with
          | None ->
            d
          | Some lval ->
            (* forget information about var,
               but ignore assignments to values that are not 64 bits *)
            try
              let size = T.get_element_size_in_bits (typeOfLval lval) in
              let lterm = T.of_lval ask lval in
              let cc = D.remove_may_equal_terms ask size lterm d.data in
              let cc = begin match desc.special exprs with
                | Malloc _
                | Calloc _
                | Alloca _ ->
                  add_block_diseqs cc lterm
                | _ -> cc
              end
              in
              data_to_t cc
            with T.UnsupportedCilExpression _ ->
              C2PODomain.top ()
        end
      in
      match desc.special exprs with
      | Assert { exp; refine; _ } ->
        if not refine then
          ctx.local
        else
          branch ctx exp true
      | _ ->
        `Lifted t

  (** First all local variables of the function are duplicated,
      then we remember the value of each local variable at the beginning of the function by using the analysis startState.
      This way we can infer the relations between the local variables of the caller and the pointers that were modified by the function. *)
  let enter ctx var_opt f args =
    (* add duplicated variables, and set them equal to the original variables *)
    match ctx.local with
    | `Bot -> [`Bot, `Bot]
    | `Lifted d ->
      let ghost_equality v =
        Equal (T.term_of_varinfo (DuplicVar v), T.term_of_varinfo (NormalVar v), Z.zero)
      in
      let ghost_equalities_for_params = List.map ghost_equality f.sformals in
      let equalities_to_add = T.filter_valid_pointers ghost_equalities_for_params in
      let state_with_ghosts = meet_conjs_opt equalities_to_add d.data in
      let state_with_ghosts = data_to_t state_with_ghosts in
      if M.tracing then begin
        let dummy_lval = Cil.var (Var.dummy_varinfo (TVoid [])) in
        let lval = BatOption.default dummy_lval var_opt in
        M.trace "c2po-function" "enter1: var_opt: %a; state: %s; state_with_ghosts: %s\n" d_lval lval (D.show ctx.local) (C2PODomain.show state_with_ghosts);
      end;
      (* remove callee vars that are not reachable and not global *)
      let reachable_variables =
        let reachable = f.sformals @ f.slocals @ reachable_from_args ctx args in
        Var.from_varinfo reachable f.sformals
      in
      let new_state = D.remove_terms_not_containing_variables reachable_variables state_with_ghosts.data in
      let new_state = data_to_t new_state in
      if M.tracing then M.trace "c2po-function" "enter2: result: %s\n" (C2PODomain.show new_state);
      let new_state = reset_normal_form new_state in
      [ctx.local, `Lifted new_state]

  let remove_out_of_scope_vars cc f =
    let local_vars = f.sformals @ f.slocals in
    let duplicated_vars = f.sformals in
    D.remove_terms_containing_variables (Var.from_varinfo local_vars duplicated_vars) cc

  let combine_env ctx lval_opt expr f args t_context_opt f_d (f_ask: Queries.ask) =
    match ctx.local with
    | `Bot -> `Bot
    | `Lifted d ->
      let caller_ask = ask_of_man ctx in
      (* assign function parameters to duplicated values *)
      let arg_assigns = GobList.combine_short f.sformals args in
      let assign_term st (var, arg) =
        let ghost_var = T.term_of_varinfo (DuplicVar var) in
        let arg = T.of_cil f_ask arg in
        assign_term st caller_ask ghost_var arg var.vtype
      in
      let state_with_assignments = List.fold_left assign_term d arg_assigns in

      if M.tracing then M.trace "c2po-function" "combine_env0: state_with_assignments: %s\n" (C2PODomain.show state_with_assignments);

      (*remove all variables that were tainted by the function*)
      let tainted = f_ask.f (MayBeTainted) in
      if M.tracing then M.trace "c2po-tainted" "combine_env1: %a\n" MayBeEqual.AD.pretty tainted;

      let local = D.remove_tainted_terms caller_ask tainted state_with_assignments.data in
      let local = data_to_t local in
      match D.meet (`Lifted local) f_d with
      | `Bot -> `Bot
      | `Lifted d ->
        let cc = remove_out_of_scope_vars d.data f in
        let d = data_to_t cc in
        if M.tracing then begin
          let dummy_lval = Cil.var (Var.dummy_varinfo (TVoid[])) in
          let lval = BatOption.default dummy_lval lval_opt in
          M.trace "c2po-function" "combine_env2: var_opt: %a; local_state: %s; f_state: %s; meeting everything: %s\n" d_lval lval (D.show ctx.local) (D.show f_d) (C2PODomain.show d)
        end;
        `Lifted d

  let combine_assign ctx var_opt expr f args t_context_opt f_d (f_ask: Queries.ask) =
    match ctx.local with
    | `Bot -> `Bot
    | `Lifted d ->
      let d =
        match var_opt with
        | None ->
          d
        | Some lval ->
          let return_type = typeOfLval lval in
          let return_var = MayBeEqual.return_var return_type  in
          let return_var = (Some return_var, Some Z.zero) in
          assign_lval d f_ask lval return_var
      in
      if M.tracing then M.trace "c2po-function" "combine_assign1: assigning return value: %s\n" (C2PODomain.show d);
      let d = D.remove_terms_containing_return_variable d.data in
      let d = data_to_t d in
      if M.tracing then M.trace "c2po-function" "combine_assign2: result: %s\n" (C2PODomain.show d);
      `Lifted d

  let startstate v =
    D.top ()
  let threadenter ctx ~multiple lval f args =
    [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx =
    D.top()
  let exitstate  v =
    D.top ()

end

let _ =
  MCP.register_analysis ~dep:["startState"; "taintPartialContexts"] (module SingleThreadedLifter(Spec) : MCPSpec)
