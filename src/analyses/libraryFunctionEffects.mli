(* can't use Base.Main.store b/c of circular build - this is painful... *)
open GoblintCil

val add_effects : (string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list option) -> unit
val effects_for : string -> Cil.exp list -> (Cil.lval * ValueDomain.Compound.t) list list
