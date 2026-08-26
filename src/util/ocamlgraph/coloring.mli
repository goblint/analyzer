(** Graph coloring.

    See also {!Graph.Coloring}. *)

module Color = Int
(** Colors are {e positive} integers. *)

module ColorSet: Set.S with type elt = Color.t

module Make (G: Graph.Coloring.G):
sig
  module H: Hashtbl.S with type key = G.V.t
  type coloring = Color.t H.t

  module type Algorithm =
  sig
    val color: G.t -> coloring
  end

  module Greedy: Algorithm
  module Optimal: Algorithm
  module Dsatur: Algorithm
  module Rlf: Algorithm
end
