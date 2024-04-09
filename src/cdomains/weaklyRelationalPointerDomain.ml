(** Domain for weakly relational pointer analysis. *)

open Batteries
open GoblintCil
module Var = CilType.Varinfo
module CC = CongruenceClosure
open CC.CongruenceClosure(Var)
module M = Messages

(**Find out if two addresses are not equal by using the MayPointTo query*)
module Disequalities = struct

  module AD = AddressDomain.AddressSet (PreValueDomain.Mval) (ValueDomain.ID)

  let query_neq (ask:Queries.ask) t1 t2 =
    let exp1 = T.to_cil t1 in
    let exp2 = T.to_cil t2 in
    let mpt1 = ask.f (MayPointTo exp1) in
    let mpt2 = ask.f (MayPointTo exp2) in
    AD.is_bot (AD.meet mpt1 mpt2)

  (**Find out if two addresses may be equal by using the MayPointTo query*)
  let may_be_equal ask t1 t2 = not (query_neq ask t1 t2)

end

module D = struct

  include Printable.StdLeaf

  type domain = t option
  type t = domain

  (** Convert to string *)
  let show x = match x with
    | None -> "⊥\n"
    | Some x -> show_conj (get_normal_form x)

  let show_all = function
    | None -> "⊥\n"
    | Some x ->  "Union Find partition:\n" ^
                 (TUF.show_uf x.part)
                 ^ "\nSubterm set:\n"
                 ^ (SSet.show_set x.set)
                 ^ "\nLookup map/transitions:\n"
                 ^ (LMap.show_map x.map)
                 ^ "\nMinimal representatives:\n"
                 ^ (MRMap.show_min_rep x.min_repr)

  include Printable.SimpleShow(struct type t = domain let show = show end)

  let name () = "wrpointer"

  let equal x y = match x, y with
    | Some x, Some y ->
      (get_normal_form x = get_normal_form y)
    | None, None -> true
    | _ -> false

  let compare x y = 0 (* How to compare if there is no total order? *)

  let empty () = Some {part = TUF.empty; set = SSet.empty; map = LMap.empty; min_repr = MRMap.empty}

  let init () = init_congruence []

  (** let hash = Hashtbl.hash *)
  let hash x = 1 (* TODO *)
  let bot () = None
  let is_bot x = x = None
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.part

  let join a b = a
  let widen = join

  let meet a b = match a,b with (*TODO put in different file *)
    | None, _ -> None
    | _, None -> None
    | Some a, Some b ->
      let a_conj = get_normal_form a in
      match meet_conjs b a_conj with
      | res -> Some res
      | exception CC.Unsat -> None

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

  let remove_may_equal_terms cc ask term =
    remove_terms cc (Disequalities.may_be_equal ask term)

end
