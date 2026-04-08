(** Trie domains. *)

module Make (Key: Printable.S) (Value: Lattice.S) =
struct
  module rec Trie:
  sig
    type key = Key.t
    type value = Value.t
    include Lattice.S with type t = value * ChildMap.t
  end =
  struct
    type key = Key.t
    type value = Value.t
    include Lattice.Prod (Value) (ChildMap)
  end
  and ChildMap: MapDomain.S with type key = Key.t and type value = Trie.t = MapDomain.MapBot (Key) (Trie)

  include Trie
end
