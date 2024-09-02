(** Ghost variables for YAML witnesses. *)

let enabled () =
  (YamlWitness.entry_type_enabled YamlWitnessType.GhostVariable.entry_type && YamlWitness.entry_type_enabled YamlWitnessType.GhostUpdate.entry_type) || YamlWitness.entry_type_enabled YamlWitnessType.GhostInstrumentation.entry_type

module Var = WitnessGhostVar

include Var

module Map = RichVarinfo.BiVarinfoMap.Make (Var)

include Map

let variable_entry ~task x =
  let variable = name_varinfo x in
  let type_ = String.trim (CilType.Typ.show (typ x)) in (* CIL printer puts space at the end of some types *)
  let initial = CilType.Exp.show (initial x) in
  YamlWitness.Entry.ghost_variable ~task ~variable ~type_ ~initial

let update_entry ~task ~node x e =
  let loc = Node.location node in
  let location_function = (Node.find_fundec node).svar.vname in
  let location = YamlWitness.Entry.location ~location:loc ~location_function in
  let variable = name_varinfo x in
  let expression = CilType.Exp.show e in
  YamlWitness.Entry.ghost_update ~task ~location ~variable ~expression

let variable' x =
  let variable = name_varinfo x in
  let type_ = String.trim (CilType.Typ.show (typ x)) in (* CIL printer puts space at the end of some types *)
  let initial = CilType.Exp.show (initial x) in
  YamlWitness.Entry.ghost_variable' ~variable ~type_ ~initial

let update' x e =
  let variable = name_varinfo x in
  let expression = CilType.Exp.show e in
  YamlWitness.Entry.ghost_update' ~variable ~expression

let location_update' ~node ~updates =
  let loc = Node.location node in
  let location_function = (Node.find_fundec node).svar.vname in
  let location = YamlWitness.Entry.location ~location:loc ~location_function in
  YamlWitness.Entry.ghost_location_update' ~location ~updates

let instrumentation_entry ~task ~variables ~location_updates =
  YamlWitness.Entry.ghost_instrumentation ~task ~variables ~location_updates
