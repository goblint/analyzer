(** Abstract domains representing structs. *)

open Cil

module type Arg =
sig
  include Lattice.S
  val is_bot_value: t -> bool
  val is_top_value: t -> typ -> bool
end

module type S =
sig
  include Lattice.S
  type field
  (** The abstract representation of field names. *)

  type value
  (** The abstract domain of values stored in the struct. *)

  val create: (field -> value) -> compinfo -> t
  val get: t -> field -> value
  val replace: t -> field -> value -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val map: (value -> value) -> t -> t
  val keys: t -> field list
  val widen_with_fct: (value -> value -> value) -> t -> t -> t
  val join_with_fct: (value -> value -> value) -> t -> t -> t
  val leq_with_fct: (value -> value -> bool) -> t -> t -> bool
  val invariant: value_invariant:(Invariant.context -> value -> Invariant.t) -> Invariant.context -> t -> Invariant.t
end

module Simple (Val: Arg): S with type value = Val.t and type field = fieldinfo
(** Creates a simple structure domain by mapping fieldnames to their values
  * using the {!MapDomain.InfMap} functor *)

module Sets (Val: Arg): S with type value = Val.t and type field = fieldinfo

module KeyedSets (Val: Arg): S with type value = Val.t and type field = fieldinfo

module FlagConfiguredStructDomain (Val: Arg): S with type value = Val.t and type field = fieldinfo
