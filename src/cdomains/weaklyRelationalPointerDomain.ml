(** Domain for weakly relational pointer analysis. *)

open Batteries
open GoblintCil
module Var = CilType.Varinfo
module CC = CongruenceClosure
include CC.CongruenceClosure(Var)
module M = Messages

(**Find out if two addresses are not equal by using the MayPointTo query*)
module Disequalities = struct

  module AD = AddressDomain.AddressSet (PreValueDomain.Mval) (ValueDomain.ID)

  let dummy_varinfo = dummyFunDec.svar
  let dummy_var = CC.Addr dummy_varinfo
  let dummy_lval = AddrOf (Var dummy_varinfo, NoOffset)

  (**Find out if two addresses are possibly equal by using the MayPointTo query*)
  let may_point_to_same_address (ask:Queries.ask) t1 t2 off =
    if T.equal t1 t2 then true else
      (* two local arrays can never point to the same array *)
      let are_different_arrays = match t1, t2 with
        | Deref (Addr x1, z1),  Deref (Addr x2, z2) -> if T.is_array_type x1.vtype && T.is_array_type x2.vtype && not (Var.equal x1 x2) then true else false
        | _ -> false in
      if are_different_arrays || Var.equal dummy_varinfo (T.get_var t1) || Var.equal dummy_varinfo (T.get_var t2) then false else
        let exp1 = T.to_cil ask Z.zero t1 in
        let exp2 = T.to_cil ask off t2 in
        let mpt1 = ask.f (MayPointTo exp1) in
        let mpt2 = ask.f (MayPointTo exp2) in
        let res = not (AD.is_bot (AD.meet mpt1 mpt2)) in
        if M.tracing then M.tracel "wrpointer-maypointto" "QUERY MayPointTo. \nt1: %s; exp1: %a; res: %a; var1: %d;\nt2: %s; exp2: %a; res: %a; var2: %d;\nresult: %s\n"
            (T.show t1) d_plainexp exp1 AD.pretty mpt1 (T.get_var t1).vid (T.show t2) d_plainexp exp2 AD.pretty mpt2 (T.get_var t2).vid (string_of_bool res); res

  (**Returns true iff by assigning to t1, the value of t2 could change. *)
  let rec may_be_equal ask uf t1 t2 =
    match t1, t2 with
    | CC.Deref (t, z), CC.Deref (v, z') ->
      let (q', z1') = TUF.find_no_pc uf v in
      let (q, z1) = TUF.find_no_pc uf t in
      let s = T.get_size_in_bits (T.type_of_term t) in
      let s' = T.get_size_in_bits (T.type_of_term v) in
      let diff = Z.(-z' - z1 + z1' + z) in
      (* If they are in the same equivalence class but with a different offset, then they are not equal *)
      (
        (not (T.equal q' q) || Z.(lt diff s && lt (-s') diff))
        (* or if we know that they are not equal according to the query MayPointTo*)
        &&
        (may_point_to_same_address ask q q' Z.(z' - z + z1 - z1'))
      )
      || (may_be_equal ask uf t1 v)
    | CC.Deref _, _ -> false (*The value of addresses never change when we overwrite the memory*)
    | CC.Addr _ , _ -> T.is_subterm t1 t2

  let may_be_equal ask uf t1 t2 =
    let res = (may_be_equal ask uf t1 t2) in
    if M.tracing then M.tracel "wrpointer-maypointto" "MAY BE EQUAL: %s %s: %b\n" (T.show t1) (T.show t2) res;
    res

  (**Returns true iff by assigning to t1, the value of t2 could change.
      But if we know that t1 and t2 are definitely equal, then it returns false. *)
  let rec may_be_equal_but_not_definitely_equal ask uf t1 t2 =
    match t1, t2 with
    | CC.Deref (t, z), CC.Deref (v, z') ->
      let (q', z1') = TUF.find_no_pc uf v in
      let (q, z1) = TUF.find_no_pc uf t in
      (* If they are in the same equivalence class, then we return false *)
      (
        (not (T.equal q' q))
        (* or if we know that they are not equal according to the query MayPointTo*)
        &&
        (may_point_to_same_address ask q q' Z.(z' - z + z1 - z1'))
      )
      || (may_be_equal ask uf t1 v)
    | CC.Deref _, _ -> false (*The value of addresses never change when we overwrite the memory*)
    | CC.Addr _ , _ -> T.is_subterm t1 t2
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
    | Some x ->  show_all x

  include Printable.SimpleShow(struct type t = domain let show = show end)

  let name () = "wrpointer"

  let equal x y = if M.tracing then M.trace "wrpointer-equal" "equal.\nx=\n%s\ny=\n%s" (show x) (show y);
    match x, y with
    | Some x, Some y ->
      (T.props_equal (get_normal_form x) (get_normal_form y))
    | None, None -> true
    | _ -> false

  let empty () = Some {uf = TUF.empty; set = SSet.empty; map = LMap.empty; min_repr = MRMap.empty}

  let init () = init_congruence []

  let bot () = None
  let is_bot x = Option.is_none x
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.uf

  let join a b = if M.tracing then M.trace "wrpointer" "JOIN\n";a (*TODO implement join*)
  let widen = join

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
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\nmin. repr\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (Some x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.uf)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
        (XmlUtil.escape (Format.asprintf "%s" (MRMap.show_min_rep x.min_repr)))
    | None ->  BatPrintf.fprintf f "<value>\nbottom\n</value>\n"

  (** Remove terms from the data structure.
      It removes all terms for which "var" is a subterm,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variable var cc =
    if M.tracing then M.trace "wrpointer" "remove_terms_containing_variable: %s\n" (T.show var);
    Option.map (remove_terms (fun _ -> T.is_subterm var)) cc

  (** Remove terms from the data structure.
      It removes all terms which contain one of the "vars",
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variables vars cc =
    if M.tracing then M.trace "wrpointer" "remove_terms_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.map (remove_terms (fun _ -> T.contains_variable vars)) cc

  (** Remove terms from the data structure.
      It removes all terms which do not contain one of the "vars",
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_not_containing_variables vars cc =
    if M.tracing then M.trace "wrpointer" "remove_terms_not_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.map (remove_terms (fun _ -> not % T.contains_variable vars)) cc

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms ask term cc =
    if M.tracing then M.trace "wrpointer" "remove_may_equal_terms: %s\n" (T.show term);
    let cc = Option.map (fun cc -> (snd(insert cc term))) cc in
    Option.map (remove_terms (fun uf -> Disequalities.may_be_equal ask uf term)) cc

  (** Remove terms from the data structure and shifts other terms.
      It removes all terms that may be changed after an assignment to "term".
      It shifts all elements that were modified by the asignmnt to "term". *)
  let remove_and_shift_may_equal_terms ask cc t z off =
    let term = CC.Deref (t, z) in
    if M.tracing then M.trace "wrpointer" "remove_and_shift_may_equal_terms: %s. Off: %s\n" (T.show term) (Z.to_string off);
    let cc = Option.map (fun cc -> (snd(insert cc term))) cc in
    Option.map (fun cc -> remove_terms_and_shift (fun uf -> Disequalities.may_be_equal_but_not_definitely_equal ask uf term) cc t z off) cc

end
