module ID = IntDomain.IntDomTuple

module type RelationalIntDomainSignature =
sig
  include Lattice.S
  val add_variable_value_list: (Cil.varinfo * ID.t) list -> t -> t
  val eval_assert_cil_exp: Cil.exp -> t -> t
  val eval_assign_int_value: Cil.varinfo -> ID.t -> t -> t
  val eval_assign_cil_exp: Cil.varinfo -> Cil.exp -> t -> t
  val get_value_of_variable: Cil.varinfo -> t -> ID.t
  val meet_local_and_global_state: t -> t -> t
  val remove_all_local_variables:  t -> t
  val remove_all_top_variables:  t -> t
  val remove_variable: Cil.varinfo -> t -> t
end
