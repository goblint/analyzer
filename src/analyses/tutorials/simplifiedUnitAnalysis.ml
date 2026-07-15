(** Simplest possible analysis with unit domain ([simplifiedUnit]). *)

open GoblintCil
open SimplifiedAnalysis

module DefaultSpec =
struct
  let query man state (type a) (q: a Queries.t) : a Queries.result =
    Queries.Result.top q

  let assign man state (lval: lval) (rval: exp) =
    state

  let branch man state (exp: exp) (tv: bool) =
    state

  let body man state (f: fundec) =
    state

  let return man state (exp: exp option) (f: fundec) =
    state

  let enter man state (lval: lval option) (f: fundec) (args: exp list) =
    state

  let combine man caller_state callee_local (lval: lval option) (f: fundec) (args: exp list) =
    callee_local

  let special man state (lval: lval option) (f: varinfo) (args: exp list) =
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
  let context man (_, c) f callee_state = c
  let threadenter man state f args = D.top ()
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
