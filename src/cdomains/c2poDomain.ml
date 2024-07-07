(** It's the same as wrpointer but less precise and hopefully more efficient? *)

open CongruenceClosure
include CongruenceClosure
open Batteries
open GoblintCil
module Var = CilType.Varinfo

(**Find out if two addresses are not equal by using the MayPointTo query*)
module MayBeEqual = struct

  module AD = Queries.AD
  let dummy_varinfo typ: varinfo = {dummyFunDec.svar with vid=(-1);vtype=typ;vname="c2po__@dummy"}
  let dummy_var var = T.aux_term_of_varinfo (dummy_varinfo var)
  let dummy_lval var = Lval (Var (dummy_varinfo var), NoOffset)

  let return_varinfo typ = {dummyFunDec.svar with vtype=typ;vid=(-2);vname="c2po__@return"}
  let return_var var = T.aux_term_of_varinfo (return_varinfo var)
  let return_lval var = Lval (Var (return_varinfo var), NoOffset)

  let ask_may_point_to (ask: Queries.ask) exp =
    match ask.f (MayPointTo exp) with
    | exception (IntDomain.ArithmeticOnIntegerBot _) -> AD.top ()
    | res -> res

  let may_point_to_all_equal_terms ask exp cc term offset =
    let comp = Disequalities.comp_t cc.uf term in
    let valid_term (t,z) =
      T.is_ptr_type (T.type_of_term t) && (T.get_var t).vid > 0 in
    let equal_terms = List.filter valid_term comp in
    if M.tracing then M.trace "c2po-query" "may-point-to %a -> equal terms: %s"
        d_exp exp (List.fold (fun s (t,z) -> s ^ "(" ^ T.show t ^","^ Z.to_string Z.(z + offset) ^")") "" equal_terms);
    let intersect_query_result res (term,z) =
      let next_query =
        match ask_may_point_to ask (T.to_cil_sum Z.(z + offset) (T.to_cil term)) with
        | exception (T.UnsupportedCilExpression _) -> AD.top()
        | res ->  if AD.is_bot res then AD.top() else res
      in
      AD.meet res next_query in
    List.fold intersect_query_result (AD.top()) equal_terms

  (**Find out if two addresses are possibly equal by using the MayPointTo query. *)
  let may_point_to_address (ask:Queries.ask) adresses t2 off cc =
    match T.to_cil_sum off (T.to_cil t2) with
    | exception (T.UnsupportedCilExpression _) -> true
    | exp2 ->
      let mpt1 = adresses in
      let mpt2 = may_point_to_all_equal_terms ask exp2 cc t2 off in
      let res = not (AD.is_bot (AD.meet mpt1 mpt2)) in
      if M.tracing then M.tracel "c2po-maypointto2" "QUERY MayPointTo. \nres: %a;\nt2: %s; exp2: %a; res: %a; \nmeet: %a; result: %s\n"
          AD.pretty mpt1 (T.show t2) d_plainexp exp2 AD.pretty mpt2 AD.pretty (AD.meet mpt1 mpt2) (string_of_bool res); res

  let may_point_to_same_address (ask:Queries.ask) t1 t2 off cc =
    if T.equal t1 t2 then true else
      let exp1 = T.to_cil t1 in
      let mpt1 = may_point_to_all_equal_terms ask exp1 cc t1 Z.zero in
      let res = may_point_to_address ask mpt1 t2 off cc in
      if M.tracing && res then M.tracel "c2po-maypointto2" "QUERY MayPointTo. \nres: %a;\nt1: %s; exp1: %a;\n"
          AD.pretty mpt1 (T.show t1) d_plainexp exp1; res

  let rec may_be_equal ask cc s t1 t2 =
    let there_is_an_overlap s s' diff =
      if Z.(gt diff zero) then Z.(lt diff s') else Z.(lt (-diff) s)
    in
    match t1, t2 with
    | Deref (t, z,_), Deref (v, z',_) ->
      let (q', z1') = TUF.find_no_pc cc.uf v in
      let (q, z1) = TUF.find_no_pc cc.uf t in
      let s' = T.get_size t2 in
      let diff = Z.(-z' - z1 + z1' + z) in
      (* If they are in the same equivalence class and they overlap, then they are equal *)
      (if T.equal q' q && there_is_an_overlap s s' diff then true
       else
         (* If we have a disequality, then they are not equal *)
       if neq_query (Some cc) (t,v,Z.(z'-z)) then false else
         (* or if we know that they are not equal according to the query MayPointTo*)
       if GobConfig.get_bool "ana.c2po.askbase" then (may_point_to_same_address ask t v Z.(z' - z) cc)
       else true)
      || (may_be_equal ask cc s t1 v)
    | Deref _, _ -> false (* The value of addresses or auxiliaries never change when we overwrite the memory*)
    | Addr _ , _ | Aux _, _ -> T.is_subterm t1 t2

  (**Returns true iff by assigning to t1, the value of t2 could change.
     The parameter s is the size in bits of the variable t1 we are assigning to. *)
  let may_be_equal ask cc s t1 t2 =
    match cc with
    | None -> false
    | Some cc ->
      let res = (may_be_equal ask cc s t1 t2) in
      if M.tracing then M.tracel "c2po-maypointto" "MAY BE EQUAL: %s %s: %b\n" (T.show t1) (T.show t2) res;
      res

  let rec may_point_to_one_of_these_adresses ask adresses cc t2 =
    match t2 with
    | Deref (v, z',_) ->
      (may_point_to_address ask adresses v z' cc)
      || (may_point_to_one_of_these_adresses ask adresses cc v)
    | Addr _ | Aux _ -> false

end

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

  let equal x y =
    if x == y then
      true
    else
      let res =
        match x,y with
        | None, None -> true
        | Some cc1, Some cc2 ->
          if cc1 == cc2 then
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
      in if M.tracing then M.trace "c2po-equal" "equal. %b\nx=\n%s\ny=\n%s" res (show_all x) (show_all y);res

  let empty () = Some {uf = TUF.empty; set = SSet.empty; map = LMap.empty; min_repr = MRMap.empty; diseq = Disequalities.empty; bldis = BlDis.empty}

  let init () = init_congruence []

  let bot () = None
  let is_bot x = Option.is_none x
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.uf

  let join a' b' =
    if  a' == b' then
      a'
    else
      let res =
        match a',b' with
        | None, b -> b
        | a, None -> a
        | Some a, Some b ->
          if  a == b then
            a'
          else
            (if M.tracing then M.tracel "c2po-join" "JOIN. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\n"
                 (show_all (Some a)) (show_all (Some b));
             let cc = fst(join_eq_no_automata a b) in
             let cmap1, cmap2 = Disequalities.comp_map a.uf, Disequalities.comp_map b.uf
             in let cc = join_neq a.diseq b.diseq a b cc cmap1 cmap2 in
             Some (join_bldis a.bldis b.bldis a b cc cmap1 cmap2))
      in
      if M.tracing then M.tracel "c2po-join" "JOIN. JOIN: %s\n"
          (show_all res);
      res

  let widen a b =
    if  a == b then
      a
    else
      let res =
        match a,b with
        | None, b -> b
        | a, None -> a
        | Some a, Some b ->
          if M.tracing then M.tracel "c2po-join" "WIDEN. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\n"
              (show_all (Some a)) (show_all (Some b));
          let cc = fst(widen_eq_no_automata a b) in
          let cmap1, cmap2 = Disequalities.comp_map a.uf, Disequalities.comp_map b.uf
          in let cc = join_neq a.diseq b.diseq a b cc cmap1 cmap2 in
          Some (join_bldis a.bldis b.bldis a b cc cmap1 cmap2)
      in
      if M.tracing then M.tracel "c2po-join" "JOIN. JOIN: %s\n"
          (show_all res);
      res

  let meet a b =
    if a == b then
      a
    else
      match a,b with
      | None, _ -> None
      | _, None -> None
      | Some a, b ->
        match get_conjunction a with
        | [] -> b
        | a_conj -> meet_conjs_opt a_conj b

  let leq x y = equal (meet x y) x

  let narrow a b =
    if a == b then
      a
    else
      match a,b with
      | None, _ -> None
      | _, None -> None
      | Some a, Some b ->
        let b_conj = List.filter
            (function | Equal (t1,t2,_)| Nequal (t1,t2,_)| BlNequal (t1,t2) -> SSet.mem t1 a.set && SSet.mem t2 a.set)
            (get_conjunction b) in
        meet_conjs_opt b_conj (Some a)

  let pretty_diff () (x,y) = Pretty.dprintf ""

  let printXml f x = match x with
    | Some x ->
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\ndiseq\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (Some x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.uf)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
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
    if M.tracing then M.trace "c2po" "remove_terms_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.bind cc (remove_terms (T.contains_variable vars))

  (** Remove terms from the data structure.
      It removes all terms which do not contain one of the "vars",
      except the global vars are also keeped (when vstorage = static),
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_not_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_not_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.bind cc (remove_terms (fun t -> (not (T.get_var t).vglob) && not (T.contains_variable vars t)))

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms ask s term cc =
    if M.tracing then M.trace "c2po" "remove_may_equal_terms: %s\n" (T.show term);
    let cc = snd (insert cc term) in
    Option.bind cc (remove_terms (MayBeEqual.may_be_equal ask cc s term))

  (** Remove terms from the data structure.
      It removes all terms that may point to the same address as "tainted".*)
  let remove_tainted_terms ask address cc =
    if M.tracing then M.tracel "c2po-tainted" "remove_tainted_terms: %a\n" MayBeEqual.AD.pretty address;
    Option.bind cc (fun cc -> remove_terms (MayBeEqual.may_point_to_one_of_these_adresses ask address cc) cc)

end
