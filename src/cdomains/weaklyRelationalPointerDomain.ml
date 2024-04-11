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

  (**Find out if two addresses are definitely not equal by using the MayPointTo query*)
  let may_point_to_same_address (ask:Queries.ask) t1 t2 off =
    let exp1 = T.to_cil Z.zero t1 in
    let exp2 = T.to_cil off t2 in
    let mpt1 = ask.f (MayPointTo exp1) in
    let mpt2 = ask.f (MayPointTo exp2) in
    let res = not (AD.is_bot (AD.meet mpt1 mpt2)) in
    if M.tracing then M.tracel "wrpointer" "QUERY MayPointTo. \nt1: %s; res: %a\nt2: %s; res: %a\nresult: %s\n"
        (T.show t1) AD.pretty mpt1 (T.show t2) AD.pretty mpt2 (string_of_bool res); res

  (**Returns true iff by assigning to t1, the value of t2 could change. *)
  let rec may_be_equal ask part t1 t2 =
    match t1, t2 with
    | CC.Deref (t, z), CC.Deref (v, z') ->
      let (q', z1') = TUF.find_no_pc part v in
      let (q, z1) = TUF.find_no_pc part t in
      (* If they are in the same equivalence class but with a different offset, then they are not equal *)
      (
        (not (T.equal q' q) || Z.(equal z (z' + z1 - z1')))
        (* or if we know that they are not equal according to the query MayPointTo*)
        &&
        (may_point_to_same_address ask q q' Z.(z' - z + z1 - z1'))
      )
      || (may_be_equal ask part t1 v)
    | CC.Deref _, _ -> false (*The value of addresses never change when we overwrite the memory*)
    | CC.Addr _ , _ -> T.is_subterm t1 t2

  let may_be_equal ask part t1 t2 =
    let res = (may_be_equal ask part t1 t2) in
    if M.tracing then M.trace "wrpointer" "MAY BE EQUAL: %s %s: %b\n" (T.show t1) (T.show t2) res;
    res
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
      (get_normal_form x = get_normal_form y)
    | None, None -> true
    | _ -> false

  let empty () = Some {part = TUF.empty; set = SSet.empty; map = LMap.empty; min_repr = MRMap.empty}

  let init () = init_congruence []

  let bot () = None
  let is_bot x = x = None
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.part

  let join a b = if M.tracing then M.trace "wrpointer" "JOIN\n";a (*TODO implement join*)
  let widen = join

  let meet a b = match a,b with
    | None, _ -> None
    | _, None -> None
    | Some a, Some b ->
      let a_conj = get_normal_form a in
      meet_conjs_opt b a_conj

  let leq x y = equal (meet x y) x

  let narrow = meet

  let pretty_diff () (x,y) = Pretty.dprintf ""

  let printXml f x = match x with
    | Some x ->
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\nmin. repr\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (Some x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.part)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
        (XmlUtil.escape (Format.asprintf "%s" (MRMap.show_min_rep x.min_repr)))
    | None ->  BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\ntrue</value>\n</map>\n</value>\n"

  (** Remove terms from the data structure.
      It removes all terms for which "var" is a subterm,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variable cc var =
    remove_terms cc (T.is_subterm var)

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms cc ask term =
    let cc = (snd(insert cc term)) in
    remove_terms cc (Disequalities.may_be_equal ask cc.part term)

end
