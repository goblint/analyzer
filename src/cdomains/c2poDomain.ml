(** Domain for weakly relational pointer analysis C-2PO. *)

open Batteries
open GoblintCil
open CongruenceClosure
module M = Messages
open DuplicateVars

module D = struct

  include Printable.StdLeaf

  type domain = t option [@@deriving ord, hash]
  type t = domain [@@deriving ord, hash]

  (** Convert to string *)
  let show x = match x with
    | None -> "⊥\n"
    | Some x -> show_conj (get_conjunction x)

  let show_all = function
    | None -> "⊥\n"
    | Some x -> show_all x

  include Printable.SimpleShow(struct type t = domain let show = show end)

  let name () = "c2po"

  let equal_standard x y =
    let res =
      match x,y with
      | None, None -> true
      | Some cc1, Some cc2 ->
        if exactly_equal cc1 cc2 then
          true
        else
          (* add all terms to both elements *)
          let terms = SSet.union (SSet.union cc1.set (BlDis.term_set cc1.bldis))
              (SSet.union cc2.set (BlDis.term_set cc2.bldis)) in
          let cc1, cc2 = Option.get (insert_set (Some cc1) terms), Option.get (insert_set (Some cc2) terms) in
          equal_eq_classes cc1 cc2
          && equal_diseqs cc1 cc2
          && equal_bldis cc1 cc2
      | _ -> false
    in if M.tracing then M.trace "c2po-equal" "equal eq classes. %b\nx=\n%s\ny=\n%s" res (show_all x) (show_all y);res

  let equal_normal_form x y =
    let res = match x, y with
      | Some x, Some y ->
        if exactly_equal x y then
          true
        else
          let nf1, nf2 = get_normal_form x, get_normal_form y in
          if M.tracing then M.trace "c2po-min-repr" "Normal form of x = %s; Normal form of y = %s" (show_conj nf1) (show_conj nf2);
          T.props_equal nf1 nf2
      | None, None -> true
      | _ -> false
    in if M.tracing then M.trace "c2po-equal" "equal min repr. %b\nx=\n%s\ny=\n%s" res (show_all x) (show_all y);res

  let equal a b =
    if M.tracing then M.trace "c2po-normal-form" "COMPUTING EQUAL";
    if GobConfig.get_bool "ana.c2po.normal_form" then equal_normal_form a b else equal_standard a b

  let equal a b = Timing.wrap "c2po-equal" (equal a) b

  let empty () = Some init_cc

  let init () = empty ()

  let bot () = None
  let is_bot x = Option.is_none x
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc ->
                        TUF.is_empty cc.uf && Disequalities.is_empty cc.diseq && BlDis.is_empty cc.bldis

  let join a' b' join_cc_function =
    let res =
      match a',b' with
      | None, b -> b
      | a, None -> a
      | Some a, Some b ->
        if exactly_equal a b then
          a'
        else
          (if M.tracing then M.tracel "c2po-join" "JOIN AUTOMATON. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\n"
               (show_all (Some a)) (show_all (Some b));
           let cc = fst(join_cc_function a b) in
           let cmap1, cmap2 = fst(Disequalities.comp_map a.uf), fst(Disequalities.comp_map b.uf)
           in let cc = Option.map (fun cc -> join_bldis a.bldis b.bldis a b cc cmap1 cmap2) cc in
           let cc = Option.bind cc (fun cc -> join_neq a.diseq b.diseq a b cc cmap1 cmap2)
           in reset_normal_form cc)
    in
    if M.tracing then M.tracel "c2po-join" "JOIN. JOIN: %s\n"
        (show_all res);
    res

  let join a b = if GobConfig.get_bool "ana.c2po.precise_join" then
      (if M.tracing then M.trace "c2po-join" "Join Automaton"; join a b join_eq) else (if M.tracing then M.trace "c2po-join" "Join Eq classes"; join a b join_eq_no_automata)

  let join a b = Timing.wrap "join" (join a) b

  let widen_automata a' b' =
    (* we calculate the join and then restrict to the term set of a' *)
    match a',b' with
    | None, b -> b
    | a, None -> a
    | Some a, Some b ->
      match join (Some a) (Some b) with
      | None -> None
      | Some join_result ->
        reset_normal_form @@ remove_terms (fun t -> not @@ SSet.mem t a.set) join_result

  let widen_eq_classes a' b' =
    let res =
      match a',b' with
      | None, b -> b
      | a, None -> a
      | Some a, Some b ->
        if exactly_equal a b then
          a'
        else
          (if M.tracing then M.tracel "c2po-join" "WIDEN. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\n"
               (show_all (Some a)) (show_all (Some b));
           let cc = fst(widen_eq_no_automata a b) in
           let cmap1, cmap2 = fst(Disequalities.comp_map a.uf), fst(Disequalities.comp_map b.uf)
           in let cc = Option.bind cc (fun cc -> join_neq a.diseq b.diseq a b cc cmap1 cmap2) in
           let cc = Option.map (fun cc -> join_bldis a.bldis b.bldis a b cc cmap1 cmap2) cc in
           reset_normal_form cc)
    in
    if M.tracing then M.tracel "c2po-join" "WIDEN. WIDEN: %s\n"
        (show_all res);
    res

  let widen a b = if M.tracing then M.trace "c2po-widen" "WIDEN\n";
    if GobConfig.get_bool "ana.c2po.precise_join" then
      widen_automata a b
    else widen_eq_classes a b

  let meet a' b' =
    if M.tracing then M.trace "c2po-meet" "MEET x= %s; y=%s" (show a') (show b');
    let res = match a',b' with
      | None, _ -> None
      | _, None -> None
      | Some a, Some b ->
        if exactly_equal a b then
          a'
        else
          match get_conjunction a with
          | [] -> b'
          | a_conj -> reset_normal_form (meet_conjs_opt a_conj b')
    in if M.tracing then M.trace "c2po-meet" "MEET RESULT = %s" (show res);
    res

  let leq x y = equal (meet x y) x

  let narrow a' b' =
    let res = match a',b' with
      | None, _ -> None
      | _, None -> None
      | Some a, Some b ->
        if exactly_equal a b then
          a'
        else
          let b_conj = List.filter
              (function | Equal (t1,t2,_)| Nequal (t1,t2,_)| BlNequal (t1,t2) -> SSet.mem t1 a.set && SSet.mem t2 a.set) (get_conjunction b) in
          reset_normal_form (meet_conjs_opt b_conj (Some a))
    in if M.tracing then M.trace "c2po-meet" "NARROW RESULT = %s" (show res);res

  let pretty_diff () (x,y) = Pretty.dprintf ""

  let printXml f x = match x with
    | Some x ->
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\nmin. repr\n</key>\n<value>\n%s</value>\n<key>\ndiseq\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (Some x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.uf)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
        (XmlUtil.escape (Format.asprintf "%s" (show_normal_form x.normal_form)))
        (XmlUtil.escape (Format.asprintf "%s" (Disequalities.show_neq x.diseq)))
    | None ->  BatPrintf.fprintf f "<value>\nbottom\n</value>\n"

  (** Remove terms from the data structure.
      It removes all terms for which "var" is a subterm,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variable var cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_variable: %s\n" (T.show (Addr var));
    Option.bind cc (remove_terms (fun t -> Var.equal (T.get_var t) var))

  (** Remove terms from the data structure.
      It removes all terms which contain one of the "vars",
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^Var.show v) "" vars);
    Option.bind cc (remove_terms (T.contains_variable vars))

  (** Remove terms from the data structure.
      It removes all terms which do not contain one of the "vars",
      except the global vars are also kept (when vglob = true),
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_not_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_not_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^Var.show v) "" vars);
    Option.bind cc (remove_terms (fun t -> (not (Var.to_varinfo (T.get_var t)).vglob) && not (T.contains_variable vars t)))

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms ask s term cc =
    if M.tracing then M.trace "c2po" "remove_may_equal_terms: %s\n" (T.show term);
    let cc = snd (insert cc term) in
    Option.bind cc (remove_terms (MayBeEqual.may_be_equal ask cc s term))

  (** Remove terms from the data structure.
      It removes all terms that may point to one of the tainted addresses.*)
  let remove_tainted_terms ask address cc =
    if M.tracing then M.tracel "c2po-tainted" "remove_tainted_terms: %a\n" MayBeEqual.AD.pretty address;
    Option.bind cc (fun cc -> remove_terms (MayBeEqual.may_point_to_one_of_these_addresses ask address cc) cc)

  (** Remove terms from the data structure.
      It removes all terms that are not in the scope, and also those that are tmp variables.*)
  let remove_vars_not_in_scope scope cc =
    Option.bind cc (fun cc -> remove_terms (fun t ->
        let var = T.get_var t in
        InvariantCil.var_is_tmp (Var.to_varinfo var) || not (InvariantCil.var_is_in_scope scope (Var.to_varinfo var))) cc)
end