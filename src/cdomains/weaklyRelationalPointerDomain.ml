(** Domain for weakly relational pointer analysis. *)

open Batteries
open GoblintCil
open CongruenceClosure

module Var: Val = struct
  type t = varinfo
  let compare = compare (* TODO *)
  let show v = v.vname (* TODO *)
  let hash x = 3 (* TODO *)
  let equal x y = (x = y) (* TODO *)
end

module D : Lattice.S = struct

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
                 ^ (TUF.show_set x.set)
                 ^ "\nLookup map/transitions:\n"
                 ^ (TUF.show_map x.map)
                 ^ "\nMinimal representatives:\n"
                 ^ (show_min_rep x.min_repr)

  include Printable.SimpleShow(struct type t = domain let show = show end)

  let name () = "wrpointer"

  let equal x y = match x, y with
    | Some x, Some y ->
      (get_normal_form x = get_normal_form y)
    | None, None -> true
    | _ -> false

  let compare x y = 0 (* How to compare if there is no total order? *)

  let empty () = Some {part = TMap.empty; set = TSet.empty; map = TMap.empty; min_repr = TMap.empty}

  let init () = congruence []

  (** let hash = Hashtbl.hash *)
  let hash x = 1 (* TODO *)
  let bot () = None
  let is_bot x = x = None
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.part

  let leq x y = false

  let join a b = a
  let widen = join

  let meet a b = match a,b with(*TODO put in different file *)
    | None, _ -> None
    | _, None -> None
    | Some a, Some b ->
      let a_conj = get_normal_form a in
      let b = insert_set b (fst (subterms_of_conj a_conj)) in
      match (closure b (fst (split a_conj))) with
      | res -> Some res
      | exception Unsat -> None


  let narrow = meet

  let pretty_diff () (x,y) = Pretty.dprintf ""

end
