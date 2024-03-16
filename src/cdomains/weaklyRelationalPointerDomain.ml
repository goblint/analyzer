(** Domain for weakly relational pointer analysis. *)

open Batteries
open GoblintCil

module D :Lattice.S = struct
  include Printable.StdLeaf

  type domain = {t: int }
  type t = domain

  (** printing *)
  let show x = ""

  include Printable.SimpleShow(struct type  t = domain let show = show end)

  let name () = "weakly relational pointer analysis"

  (** let equal = Util.equals *)
  let equal x y = true


  (** compare all fields with correspoding compare operators *)
  let compare x y = 0


  (** let hash = Hashtbl.hash *)
  let hash x = 1
  let make tid pred ctx = tid
  let bot () = {t = 0}
  let is_bot x = true
  let any_is_bot x = true
  let top () = {t = 0}
  let is_top x = false

  let leq x y = true

  let op_scheme op1 op2 op3 x y : t = {t = 0}

  let join a b = {t = 0}
  let widen = join
  let meet a b = {t = 0}
  let narrow = meet

  let pretty_diff () (x,y) = Pretty.dprintf ""

end
