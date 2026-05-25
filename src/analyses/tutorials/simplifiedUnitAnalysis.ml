(** Simplest possible analysis with unit domain ([simplifiedUnit]). *)

open GoblintCil
open SimplifiedAnalysis

module DefaultSpec =
struct
  let query _ _ (type a) (q: a Queries.t) : a Queries.result =
    Queries.Result.top q

  let assign _ state (_: lval) (_: exp) =
    state

  let branch _ state (_: exp) (_: bool) =
    state

  let body _ state (_: fundec) =
    state

  let return _ state (_: exp option) (_: fundec) =
    state

  let enter _ state (_: lval option) (_: fundec) (_: exp list) =
    state

  let combine _ state _ (_: lval option) (_: fundec) (_: exp list) =
    state

  let special _ state (_: lval option) (_: varinfo) (_: exp list) =
    state
end

module Spec : SimplifiedSpec =
struct
  include DefaultSpec

  let name = "simplifiedUnit"
  module V = Printable.Unit
  module G = Lattice.Unit
  module D = Lattice.Unit
  module C = Printable.Unit

  let startstate = D.bot ()
  let startcontext = ()
  let context _ (_, c) _ _ = c
  let threadenter _ _ _ _ = D.top ()
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
