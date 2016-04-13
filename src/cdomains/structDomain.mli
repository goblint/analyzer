(** Abstract domains representing structs. *)

open Cil

module type S =
sig
  include Lattice.S
  type field (** The abstract representation of field names. *)
  type value (** The abstract domain of values stored in the struct. *)
  val get: t -> field -> value
  val replace: t -> field -> value -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val map: (value -> value) -> t -> t
end

module type Relational =
sig
  include Lattice.S
  type field (** The abstract representation of field names. *)
  type value (** The abstract domain of values stored in the struct. *)
  val add_variable_value_list: (Prelude.Ana.lhost * t) list -> t-> t
  val eval_assert_cil_exp: Cil.exp -> t -> t
  val get: t -> field -> value
  val get_value_of_variable: varinfo -> t -> t
  val get_value_of_variable_and_globals: varinfo -> t -> t
  val fold: (field -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val map: (value -> value) -> t -> t
  val meet_local_and_global_state: t -> t -> t
  val remove_all_local_variables: t -> t
  val remove_all_top_variables: t -> t
  val remove_variable: varinfo -> t -> t
  val replace: t -> field -> value -> t
end


module Simple (Val: Lattice.S): S with type value = Val.t and type field = fieldinfo
(** Creates a simple structure domain by mapping fieldnames to their values
  * using the {!MapDomain.InfMap} functor *)
