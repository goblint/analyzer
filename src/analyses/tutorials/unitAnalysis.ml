(** Simplest possible analysis with unit domain ([unit]). *)

open GoblintCil
open SimplifiedAnalysis

module Spec : SimplifiedSpec =
struct
  let name = "unit"
  module V = Printable.Unit
  module G = Lattice.Unit
  module D = Lattice.Unit
  module C = Printable.Unit

  (* transfer functions *)
  let query _ _ (type a) (q: a Queries.t) : a Queries.result =
    Queries.Result.top q

  let assign _ state (_: lval) (_: exp) : D.t =
    state

  let branch _ state (_: exp) (_: bool) : D.t =
    state

  let body _ state (_: fundec) : D.t =
    state

  let return _ state (_: exp option) (_: fundec) : D.t =
    state

  let enter _ state (_: lval option) (_: fundec) (_: exp list) : D.t =
    state

  let combine _ state (_: D.t) (_: lval option) (_: fundec) (_: exp list) : D.t =
    state

  let special _ state (_: lval option) (_: varinfo) (_: exp list) : D.t =
    state

  let startcontext = ()
  let startstate = D.bot ()
  let context _ (_, c) _ _ = c
  let threadenter _ _ _ _ = D.top ()
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
