(** Simplest possible analysis with unit domain ([simplifiedUnit]). *)

open GoblintCil
open SimplifiedAnalysis

module Spec : SimplifiedSpec =
struct
  include SimplifiedAnalysis.DefaultSpec

  let name = "simplifiedUnit"
  module V = Printable.Unit
  module G = Lattice.Unit
  module D = Lattice.Unit
  module C = Printable.Unit

  let startstate = D.bot ()
  let startcontext = ()
  let context man (_, c) f callee_state = c
  let threadenter man state f args = D.top ()
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
