(** Domain for weakly relational pointer analysis C-2PO. *)

open Batteries
open GoblintCil
open CongruenceClosure
module M = Messages
open DuplicateVars

module C2PODomain = struct
  include Printable.StdLeaf

  type t = CongruenceClosure.t[@@deriving ord, hash]

  let show x = show_conj (get_conjunction x)
  let name () = "c2po"

  type domain = t
  include Printable.SimpleShow (struct type t = domain let show = show end)

  let equal_standard cc1 cc2 =
    let res =
      if exactly_equal cc1 cc2 then
        true
      else
        (* add all terms to both elements *)
        let terms1 = SSet.union cc1.set (BlDis.term_set cc1.bldis) in
        let terms2 = SSet.union cc2.set (BlDis.term_set cc2.bldis) in
        let terms = SSet.union terms1 terms2 in

        let cc1 = insert_set cc1 terms in
        let cc2 = insert_set cc2 terms in

        equal_eq_classes cc1 cc2
        && equal_diseqs cc1 cc2
        && equal_bldis cc1 cc2
    in
    if M.tracing then M.trace "c2po-equal" "equal eq classes. %b\nx=\n%s\ny=\n%s" res (show_all cc1) (show_all cc2);
    res

  let equal_normal_form x y =
    let res =
      if exactly_equal x y then
        true
      else
        let nf1 = get_normal_form x in
        let nf2 = get_normal_form y in
        if M.tracing then M.trace "c2po-min-repr" "Normal form of x = %s; Normal form of y = %s" (show_conj nf1) (show_conj nf2);
        T.props_equal nf1 nf2
    in
    if M.tracing then M.trace "c2po-equal" "equal min repr. %b\nx=\n%s\ny=\n%s" res (show_all x) (show_all y);
    res

  let equal a b =
    if M.tracing then M.trace "c2po-normal-form" "COMPUTING EQUAL";
    match GobConfig.get_string "ana.c2po.equal" with
    | "normal_form" ->
      equal_normal_form a b
    | _ ->
      equal_standard a b

  let equal a b = Timing.wrap "c2po-equal" (equal a) b

  let bot() = failwith "not supported"
  let is_bot x = false
  let empty () = init_cc ()
  let init () = empty ()
  let top () = empty ()
  let is_top cc =
    TUF.is_empty cc.uf &&
    Disequalities.is_empty cc.diseq &&
    BlDis.is_empty cc.bldis

  let join_f a b join_cc_function =
    let res =
      if exactly_equal a b then
        a
      else begin
        if M.tracing then M.tracel "c2po-join" "JOIN AUTOMATON. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\n"
            (show_all a) (show_all b);
        let cc, _ = join_cc_function a b in
        let cmap1, _ = Disequalities.comp_map a.uf  in
        let cmap2, _ = Disequalities.comp_map b.uf in
        let cc = join_bldis a.bldis b.bldis a b cc cmap1 cmap2 in
        let cc = join_neq a.diseq b.diseq a b cc cmap1 cmap2 in
        reset_normal_form cc
      end
    in
    if M.tracing then M.tracel "c2po-join" "JOIN. JOIN: %s\n"
        (show_all res);
    res

  let join a b =
    match GobConfig.get_string "ana.c2po.join_algorithm" with
    | "precise" ->
      if M.tracing then M.trace "c2po-join" "Join Automaton";
      join_f a b join_eq
    | _ ->
      if M.tracing then M.trace "c2po-join" "Join Eq classes";
      join_f a b join_eq_no_automata

  let join a b =
    Timing.wrap "join" (join a) b

  let widen_automata a b =
    (* we calculate the join and then restrict to the term set of a' *)
    let join_result = join a b in
    let not_in_a_set t =
      not @@ SSet.mem t a.set
    in
    let filtered_join = remove_terms not_in_a_set join_result in
    reset_normal_form filtered_join

  let widen_eq_classes a b =
    join_f a b widen_eq_no_automata

  let widen a b =
    if M.tracing then M.trace "c2po-widen" "WIDEN\n";
    match GobConfig.get_string "ana.c2po.join_algorithm" with
    | "precise" ->
      widen_automata a b
    | _ ->
      widen_eq_classes a b


  let meet a b =
    if M.tracing then M.trace "c2po-meet" "Meet x= %s; y=%s" (show a) (show b);
    let res =
      if exactly_equal a b then
        a
      else
        match get_conjunction a with
        | [] ->
          b
        | a_conj ->
          reset_normal_form (meet_conjs_opt a_conj b)
    in
    if M.tracing then M.trace "c2po-meet" "Meet result = %s" (show res);
    res

  let narrow a b =
    let res =
      if exactly_equal a b then
        a
      else
        let terms_contained_in_a = function
          | Equal (t1,t2,_)
          | Nequal (t1,t2,_)
          | BlNequal (t1,t2) ->
            SSet.mem t1 a.set && SSet.mem t2 a.set
        in
        let b_conj = get_conjunction b in
        let b_conj = List.filter terms_contained_in_a b_conj in
        let meet = meet_conjs_opt b_conj a in
        reset_normal_form meet
    in
    if M.tracing then M.trace "c2po-meet" "NARROW RESULT = %s" (show res);
    res

  let leq x y =
    try
      equal (meet x y) x
    with
      Unsat -> false

  let pretty_diff () (x,y) =
    let x_conj = get_conjunction x in
    let y_conj = get_conjunction y in
    let not_in conj a =
      not (List.mem_cmp compare_prop a conj)
    in
    let x_diff = List.filter (not_in y_conj) x_conj in
    let y_diff = List.filter (not_in x_conj) y_conj in
    Pretty.dprintf ("Additional propositions of first element:\n%s\nAdditional propositions of second element:\n%s\n") (show_conj x_diff) (show_conj y_diff)

end


module D = struct
  include Lattice.LiftBot (C2PODomain)

  let show_all = function
    | `Bot ->
      show `Bot
    | `Lifted x ->
      show_all x

  let meet a b =
    try
      meet a b
    with Unsat ->
      `Bot

  let narrow a b =
    try
      narrow a b
    with Unsat ->
      `Bot

  let printXml f x = match x with
    | `Lifted x ->
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\nmin. repr\n</key>\n<value>\n%s</value>\n<key>\ndiseq\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (`Lifted x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.uf)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
        (XmlUtil.escape (Format.asprintf "%s" (show_normal_form x.normal_form)))
        (XmlUtil.escape (Format.asprintf "%s" (Disequalities.show_neq x.diseq)))
    | `Bot ->
      BatPrintf.fprintf f "<value>\nbottom\n</value>\n"

  (** Remove terms from the data structure.
      It removes all terms that contain an AssignAux variable,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_aux_variable cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_aux_variable\n";
    let is_assign_aux_term t =
      let var = T.get_var t in
      Var.is_assign_aux var
    in
    remove_terms is_assign_aux_term cc

  (** Remove terms from the data structure.
      It removes all terms that contain an ReturnAux variable,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_return_variable cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_aux_variable\n";
    let is_return_aux_term t =
      let var = T.get_var t in
      Var.is_return_aux var
    in
    remove_terms is_return_aux_term  cc

  (** Remove terms from the data structure.
      It removes all terms which contain one of the "vars",
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^Var.show v) "" vars);
    remove_terms (T.contains_variable vars) cc

  (** Remove terms from the data structure.
      It removes all terms which do not contain one of the "vars",
      except the global vars are also kept (when vglob = true),
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_not_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_not_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^Var.show v) "" vars);
    let not_global_and_not_contains_variable t =
      let var = T.get_var t in
      not (DuplicateVars.VarType.vglob var) && not (T.contains_variable vars t)
    in
    remove_terms not_global_and_not_contains_variable cc

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms ask size term cc =
    if M.tracing then M.trace "c2po" "remove_may_equal_terms: %s\n" (T.show term);
    let _, cc = insert cc term in
    let may_equal_term =
      MayBeEqual.may_be_equal ask cc size term
    in
    remove_terms may_equal_term cc

  (** Remove terms from the data structure.
      It removes all terms that may point to one of the tainted addresses.*)
  let remove_tainted_terms ask address cc =
    if M.tracing then M.tracel "c2po-tainted" "remove_tainted_terms: %a\n" MayBeEqual.AD.pretty address;
    let may_be_tainted =
      MayBeEqual.may_point_to_one_of_these_addresses ask address cc
    in
    remove_terms may_be_tainted cc

  (** Remove terms from the data structure.
      It removes all terms that are not in the scope, and also those that are tmp variables.*)
  let remove_vars_not_in_scope scope cc =
    let not_in_scope t =
      let var = T.get_var t in
      let var = Var.to_varinfo var in
      InvariantCil.var_is_tmp var || not (InvariantCil.var_is_in_scope scope  var)
    in
    remove_terms not_in_scope cc
end
