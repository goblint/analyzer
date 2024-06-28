(** Domain for weakly relational pointer analysis. *)

open Batteries
open GoblintCil
module Var = CilType.Varinfo
module CC = CongruenceClosure
include CC.CongruenceClosure
module M = Messages
module T = CC.T

(**Find out if two addresses are not equal by using the MayPointTo query*)
module MayBeEqual = struct

  module AD = ValueDomain.AD
  let dummy_varinfo typ: varinfo = {dummyFunDec.svar with vid=(-1);vtype=typ;vname="wrpointer__@dummy"}
  let dummy_var var = T.aux_term_of_varinfo (dummy_varinfo var)
  let dummy_lval var = Lval (Var (dummy_varinfo var), NoOffset)

  let return_varinfo typ = {dummyFunDec.svar with vtype=typ;vid=(-2);vname="wrpointer__@return"}
  let return_var var = T.aux_term_of_varinfo (return_varinfo var)
  let return_lval var = Lval (Var (return_varinfo var), NoOffset)

  (**Find out if two addresses are possibly equal by using the MayPointTo query. *)
  let may_point_to_address (ask:Queries.ask) adresses t2 off =
    match T.to_cil_sum off (T.to_cil t2) with
    | exception (T.UnsupportedCilExpression _) -> true
    | exp2 ->
      let mpt1 = adresses in
      let mpt2 = ask.f (MayPointTo exp2) in
      let res = not (AD.is_bot (AD.meet mpt1 mpt2)) in
      if M.tracing then M.tracel "wrpointer-maypointto2" "QUERY MayPointTo. \nres: %a;\nt2: %s; exp2: %a; res: %a; \nmeet: %a; result: %s\n"
          AD.pretty mpt1 (T.show t2) d_plainexp exp2 AD.pretty mpt2 AD.pretty (AD.meet mpt1 mpt2) (string_of_bool res); res

  let may_point_to_same_address (ask:Queries.ask) t1 t2 off =
    if T.equal t1 t2 then true else
      let exp1 = T.to_cil t1 in
      let mpt1 = ask.f (MayPointTo exp1) in
      may_point_to_address ask mpt1 t2 off

  (**Returns true iff by assigning to t1, the value of t2 could change.
      The parameter s is the size in bits of the variable t1 we are assigning to. *)
  let rec may_be_equal ask cc s t1 t2 =
    match t1, t2 with
    | CC.Deref (t, z,_), CC.Deref (v, z',_) ->
      (* If we have a disequality, then they are not equal *)
      (
        not (neq_query cc (t,v,Z.(z'-z)))
        (* or if we know that they are not equal according to the query MayPointTo*)
        &&
        (may_point_to_same_address ask t v Z.(z' - z))
      )
      || (may_be_equal ask cc s t1 v)
    | CC.Deref _, _ -> false (* The value of addresses or auxiliaries never change when we overwrite the memory*)
    | CC.Addr _ , _ | CC.Aux _, _ -> T.is_subterm t1 t2

  let may_be_equal ask cc s t1 t2 =
    let res = (may_be_equal ask cc s t1 t2) in
    if M.tracing then M.tracel "wrpointer-maypointto" "MAY BE EQUAL: %s %s: %b\n" (T.show t1) (T.show t2) res;
    res

  let rec may_point_to_one_of_these_adresses ask adresses t2 =
    match t2 with
    |  CC.Deref (v, z',_) ->
      (may_point_to_address ask adresses v z')
      || (may_point_to_one_of_these_adresses ask adresses v)
    | CC.Addr _ | CC.Aux _ -> false

end

module D = struct

  include Printable.StdLeaf

  type domain = t option [@@deriving ord, hash]
  type t = domain [@@deriving ord, hash]

  (** Convert to string *)
  let show x = match x with
    | None -> "⊥\n"
    | Some x -> show_conj (get_normal_form x)

  let show_all = function
    | None -> "⊥\n"
    | Some x -> show_all x

  include Printable.SimpleShow(struct type t = domain let show = show end)

  let name () = "wrpointer"

  let equal x y =
    let res = match x, y with
      | Some x, Some y ->
        (T.props_equal (get_normal_form x) (get_normal_form y))
      | None, None -> true
      | _ -> false
    in if M.tracing then M.trace "wrpointer-equal" "equal. %b\nx=\n%s\ny=\n%s" res (show x) (show y);res

  let empty () = Some {uf = TUF.empty; set = SSet.empty; map = LMap.empty; min_repr = MRMap.empty; diseq = Disequalities.empty}

  let init () = init_congruence []

  let bot () = None
  let is_bot x = Option.is_none x
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.uf

  let join a b =
    let res =
      match a,b with
      | None, b -> b
      | a, None -> a
      | Some a, Some b -> let cc = fst(join_eq a b)
        in join_neq a.diseq b.diseq a b cc
    in
    if M.tracing then M.tracel "wrpointer-join" "JOIN. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\nJOIN: %s\n"
        (show a) (show b) (show res);
    res

  let widen a b = if M.tracing then M.trace "wrpointer-join" "WIDEN\n";join a b

  let meet a b = match a,b with
    | None, _ -> None
    | _, None -> None
    | Some a, b ->
      let a_conj = get_normal_form a in
      meet_conjs_opt a_conj b

  let leq x y = equal (meet x y) x

  let narrow = meet

  let pretty_diff () (x,y) = Pretty.dprintf ""

  let printXml f x = match x with
    | Some x ->
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\nmin. repr\n</key>\n<value>\n%s</value>\n<key>\ndiseq\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (Some x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.uf)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
        (XmlUtil.escape (Format.asprintf "%s" (MRMap.show_min_rep x.min_repr)))
        (XmlUtil.escape (Format.asprintf "%s" (Disequalities.show_neq x.diseq)))
    | None ->  BatPrintf.fprintf f "<value>\nbottom\n</value>\n"

  (** Remove terms from the data structure.
      It removes all terms for which "var" is a subterm,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variable var cc =
    if M.tracing then M.trace "wrpointer" "remove_terms_containing_variable: %s\n" (T.show (Addr var));
    Option.bind cc (remove_terms (fun t -> Var.equal (T.get_var t) var))

  (** Remove terms from the data structure.
      It removes all terms which contain one of the "vars",
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variables vars cc =
    if M.tracing then M.trace "wrpointer" "remove_terms_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.bind cc (remove_terms (T.contains_variable vars))

  (** Remove terms from the data structure.
      It removes all terms which do not contain one of the "vars",
      except the global vars are also keeped (when vstorage = static),
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_not_containing_variables vars cc =
    if M.tracing then M.trace "wrpointer" "remove_terms_not_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.bind cc (remove_terms (fun t -> (not (T.get_var t).vglob) && not (T.contains_variable vars t)))

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms ask s term cc =
    if M.tracing then M.trace "wrpointer" "remove_may_equal_terms: %s\n" (T.show term);
    let cc = snd (insert cc term) in
    Option.bind cc (remove_terms (MayBeEqual.may_be_equal ask cc s term))

  (** Remove terms from the data structure.
      It removes all terms that may point to the same address as "tainted".*)
  let remove_tainted_terms ask address cc =
    if M.tracing then M.tracel "wrpointer-tainted" "remove_tainted_terms: %a\n" MayBeEqual.AD.pretty address;
    Option.bind cc (remove_terms (MayBeEqual.may_point_to_one_of_these_adresses ask address))

end
