open GoblintCil

let effects: (string -> Cil.exp list -> (Cil.lval * _) list option) list ref = ref []
let add_effects f = effects := f :: !effects
let effects_for fname args = List.filter_map (fun f -> f fname args) !effects
