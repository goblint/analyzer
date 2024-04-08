(** Domain for weakly relational pointer analysis. *)

open Batteries
open GoblintCil
open CongruenceClosure
module Var = CilType.Varinfo

(*
module Var: Val = struct
  type t = varinfo
  let compare = compare (* TODO *)
  let show v = v.vname (* TODO *)
  let hash x = 3 (* TODO *)
  let equal x y = (x = y) (* TODO *)
end *)

module D = struct

  include Printable.StdLeaf
  include CongruenceClosure(Var)

  type domain = t option
  type t = domain

  (** Convert to string *)
  let show x = match x with
    | None -> "⊥"
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
      | exception Unsat -> None

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


end
