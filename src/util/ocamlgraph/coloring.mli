(** Graph coloring.

    See also {!Graph.Coloring}. *)

module Color = Int
(** Colors are {e positive} integers. *)

module ColorSet:
sig
  include Set.S with type elt = Color.t

  val find_unused: t -> elt
end

module Make (G: Graph.Coloring.G):
sig
  module H: Hashtbl.S with type key = G.V.t
  type coloring = Color.t H.t

  val valid_coloring: G.t -> coloring -> bool

  module type Algorithm =
  sig
    val color: G.t -> coloring
  end

  module Greedy: Algorithm
  module Optimal: Algorithm
  module Dsatur: Algorithm
  module Rlf: Algorithm
end
