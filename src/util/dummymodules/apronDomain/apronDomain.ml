module ApronDomain =
struct
  type t = unit
end

module PolyDomain =
struct
  type t = unit

  let raise_error _ = raise (Invalid_argument "In order to use the apron domain, please install apron and build goblint using 'make poly'")
  let equal _ _ =  raise_error ()
  let hash _ = raise_error ()
  let compare _ _ = raise_error ()
  let short _ _ = raise_error ()
  let isSimple _ = raise_error ()
  let pretty_diff _ _ = raise_error ()
  let toXML _ = raise_error ()
  let pretty_f _ _ _  = raise_error ()
  let pretty _ _ = raise_error ()
  let toXML_f _ _  = raise_error ()
  let printXml _ _  = raise_error ()
  let name _ =  "aprondomain"
  let leq _ _  = raise_error ()
  let join _ _  = raise_error ()
  let meet _ _  = raise_error ()
  let widen _ _ = raise_error ()
  let narrow _ _ = raise_error ()
  let bot _ = raise_error ()
  let top  _ = raise_error ()
  let is_bot _ = raise_error ()
  let is_top _ = raise_error ()
end

module ApronRelationalIntDomain : RelationalIntDomainSignature.RelationalIntDomainSignature =
struct
  type t = unit

  let raise_error _ = raise (Invalid_argument "In order to use the apron domain, please install apron and build goblint using 'make poly'")
  let equal _ _ =  raise_error ()
  let hash _ = raise_error ()
  let compare _ _ = raise_error ()
  let short _ _ = raise_error ()
  let isSimple _ = raise_error ()
  let pretty_diff _ _ = raise_error ()
  let toXML _ = raise_error ()
  let pretty_f _ _ _  = raise_error ()
  let pretty _ _ = raise_error ()
  let toXML_f _ _  = raise_error ()
  let printXml _ _  = raise_error ()
  let name _ =  "aprondomain"
  let leq _ _  = raise_error ()
  let join _ _  = raise_error ()
  let meet _ _  = raise_error ()
  let widen _ _ = raise_error ()
  let narrow _ _ = raise_error ()
  let bot _ = raise_error ()
  let top  _ = raise_error ()
  let is_bot _ = raise_error ()
  let is_top _ = raise_error ()
  let add_variable_value_list _ _ = raise_error ()
  let add_variable_value_pair _ _ = raise_error ()
  let eval_assign_cil_exp _ _ = raise_error ()
  let eval_assert_cil_exp _ _ = raise_error ()
  let eval_assign_int_value _ _ = raise_error ()
  let get_value_of_variable _ _ = raise_error ()
  let meet_local_and_global_state _ _ = raise_error ()
  let remove_all_top_variables _ = raise_error ()
  let remove_all_local_variables _ = raise_error ()
  let remove_variable _ _ = raise_error ()
end

module type Compound =
sig
  include Lattice.S
  val of_int_val: IntDomain.IntDomTuple.t -> t
  val to_int_val: t -> IntDomain.IntDomTuple.t
end

module ApronRelationalStructDomain(Compound: Compound)(EquationField: Equation.GroupableLatticeS with type t = ([`Top | `Bot| `Field of Basetype.VariableFields.t]))  : StructDomain.RelationalStructDomainSignature
  with type t = ApronDomain.t * MapDomain.MapTop_LiftBot(Lattice.Prod(Basetype.Strings)(Basetype.Strings))(EquationField).t
   and type field = EquationField.t
   and type value = Compound.t
=
struct
  type t = ApronDomain.t * MapDomain.MapTop_LiftBot(Lattice.Prod(Basetype.Strings)(Basetype.Strings))(EquationField).t
  type field = EquationField.t
  type value = Compound.t

  let raise_error _ = raise (Invalid_argument "In order to use the apron domain, please install apron and build goblint using 'make poly'")

  let fold _ _ _ = raise_error ()
  let get _ _ =  raise_error ()
  let get_value_of_cil_exp _ _ =  raise_error ()
  let get_value_of_variable_and_globals _ = raise_error ()
  let map _ _ = raise_error ()
  let assign _ _ = raise_error ()

  let equal _ _ =  raise_error ()
  let hash _ = raise_error ()
  let compare _ _ = raise_error ()
  let short _ _ = raise_error ()
  let isSimple _ = raise_error ()
  let pretty_diff _ _ = raise_error ()
  let toXML _ = raise_error ()
  let pretty_f _ _ _  = raise_error ()
  let pretty _ _ = raise_error ()
  let toXML_f _ _  = raise_error ()
  let printXml _ _  = raise_error ()
  let name _ =  "apron"
  let leq _ _  = raise_error ()
  let join _ _  = raise_error ()
  let meet _ _  = raise_error ()
  let widen _ _ = raise_error ()
  let narrow _ _ = raise_error ()
  let bot _ = raise_error ()
  let top  _ = raise_error ()
  let is_bot _ = raise_error ()
  let is_top _ = raise_error ()
  let add_variable_value_list _ _ = raise_error ()
  let add_variable_value_pair _ _ = raise_error ()
  let eval_assign_cil_exp _ _ = raise_error ()
  let eval_assert_cil_exp _ _ = raise_error ()
  let eval_assign_int_value _ _ = raise_error ()
  let get_value_of_variable _ _ = raise_error ()
  let meet_local_and_global_state _ _ = raise_error ()
  let remove_all_top_variables _ = raise_error ()
  let remove_all_local_variables _ = raise_error ()
  let remove_variable _ _ = raise_error ()
end
