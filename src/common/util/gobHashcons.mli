(** Hashconsing that is safe for parallel solving when needed.

    {!BatHashcons} is not domain-safe: its tag counter is a plain global counter and its tables are not synchronized,
    so concurrent use can give different values the same tag, while [Lattice.HConsed] relies on equal tags meaning equal values.

    The mode is chosen once, after the configuration is loaded:
    - sequential solvers: a single weak table per [MakeTable], without locking, like {!BatHashcons};
    - parallel solvers ([td_parallel_*]): the table is split into shards by hash code, each with its own lock.

    Tags come from one atomic counter in both modes, so they are unique across all tables. *)

type 'a hobj = private {
  obj: 'a;
  tag: int; (** Unique id for this object. *)
  hcode: int; (** Hash code of [obj]. *)
}

module type Table =
sig
  type key
  type t
  val create: int -> t
  (** [create n] creates a table with at least [n] cells in total. *)

  val hashcons: t -> key -> key hobj
  (** Returns the hashed object of the table that is equal to the key, adding a new one if there is none. *)
end

module MakeTable (HT: Hashtbl.HashedType): Table with type key = HT.t
