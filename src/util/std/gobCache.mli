(** Manual cache interface similar to BatCache.manual_cache *)

type ('a, 'b) manual_cache = {
  get: 'a -> 'b;
  del: 'a -> unit;
  enum: unit -> ('a * 'b) Seq.t;
}

(** Create a hashtable-based cache *)
val make_ht : gen:('a -> 'b) -> init_size:int -> ('a, 'b) manual_cache

(** Create a map-based cache (for single value) *)
val make_map : gen:(unit -> 'a) -> (unit, 'a) manual_cache
