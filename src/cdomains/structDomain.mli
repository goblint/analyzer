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

module type RelationalStructDomainSignature =
sig
  include Lattice.S
  type field (** The abstract representation of field names. *)
  type value (** The abstract domain of values stored in the struct. *)
  val add_variable_value_list: (Cil.varinfo option * Cil.varinfo * t) list -> t-> t
  val assign: t -> field -> value -> t
  val eval_assert_cil_exp: Cil.exp -> t -> t
  val eval_cil_exp: Cil.exp -> t -> value
  val get: field -> t -> value
  val get_value_of_variable: varinfo -> t -> t
  val get_value_of_variable_and_globals: varinfo -> t -> t
  val fold: (field -> value -> t -> t) -> t -> t -> t
  val map: (value -> value) -> t -> t
  val meet_local_and_global_state: t -> t -> t
  val remove_all_local_variables: t -> t
  val remove_variable: varinfo -> t -> t
end


module Simple (Val: Lattice.S): S with type value = Val.t and type field = fieldinfo
(** Creates a simple structure domain by mapping fieldnames to their values
  * using the {!MapDomain.InfMap} functor *)


module type StructNameMapSignature =
sig
  type t
  val add: string -> Cil.compinfo -> t -> t
  val empty : t
  val join: t ->  t -> t
  val meet: t ->  t -> t
  val find: string -> t -> Cil.compinfo
  val fold: (string -> Cil.compinfo -> 'b -> 'b) -> t -> 'b -> 'b
  val get_all_fields_of_variable_name: string -> t -> Cil.fieldinfo list
  val get_field_in_compinfo: string -> Cil.compinfo -> Cil.fieldinfo option
  val get_unique_field: Cil.fieldinfo -> string -> t -> Cil.fieldinfo * t
  val mem: string -> t -> bool
  val print: t -> unit
  val remove: string -> t -> t
end

module StructNameMap (Compound: Lattice.S)(Val: Lattice.S with type t = Compound.t * string * bool): StructNameMapSignature with type t = Cil.compinfo Map.Make(String).t
