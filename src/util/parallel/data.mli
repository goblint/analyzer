open Batteries

(** A type with a default factory *) 
module type DefaultType = sig
  type t
  val default: unit -> t
  val show: t -> string
end

(** A lock free concurrency safe hashmap *)
module ConcurrentHashmap
    (H : Hashtbl.HashedType)
    (D : DefaultType)
    (HM : Hashtbl.S with type key = H.t) :
sig
  type key = H.t
  type value = D.t Atomic.t
  type t

  val create : unit -> t

  val to_list : t -> (key * value) list
  val to_seq : t -> (key * value) Seq.t
  val to_seq_values : t -> value Seq.t
  val to_hashtbl : t -> D.t HM.t

  val find_option : t -> key -> value option

  (** [find t k] returns the value associated with [k] in [t]. If [k] is not present, it raises Not_found. Use this method if you expect the value to be present in the hashmap *)
  val find : t -> key -> value

  val mem : t -> key -> bool

  (** [find_create t k] returns the value associated with [k] in [t]. If [k] is not present, it creates a new value using the default factory and adds it to [t]. It returns the new value and a boolean indicating whether it was created. *)
  val find_create : t -> key -> value * bool
end

