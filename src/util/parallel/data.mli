open Batteries

(** A type with a default factory *) 
module type DefaultType = sig
  type t
  val default: unit -> t
  val to_string: t -> string
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
  val to_value_seq : t -> value Seq.t
  val to_hashtbl : t -> D.t HM.t

  val find_option : t -> key -> value option
  val find : t -> key -> value
  val mem : t -> key -> bool
  val find_create : t -> key -> value * bool
end

