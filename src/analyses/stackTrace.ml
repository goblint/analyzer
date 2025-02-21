(** Call stack analyses ([stack_trace], [stack_trace_set], [stack_loc]). *)

open GoblintCil
open Analyses

module Spec (D: StackDomain.S) (N: sig val name : string end)=
struct
  include Analyses.IdentitySpec

  let name () = N.name
  module D = D
  include Analyses.ValueContexts(D)

  (* transfer functions *)

  let body man (f:fundec) : D.t =
    if f.svar.vname = "__goblint_dummy_init" then man.local else D.push f.svar man.local

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* keep local as opposed to IdentitySpec *)

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.bot ()]
  let exitstate  v = D.top ()
end

module SpecLoc =
struct
  include Analyses.IdentitySpec

  let name () = "stack_loc"
  module D = StackDomain.Dom3
  include Analyses.ValueContexts(D)

  (* transfer functions *)

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, D.push !Goblint_tracing.current_loc man.local]

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* keep local as opposed to IdentitySpec *)

  let startstate v = D.bot ()
  let exitstate  v = D.top ()

  let threadenter man ~multiple lval f args =
    [D.push !Goblint_tracing.current_loc man.local]
end


module Spec1 = Spec (StackDomain.Dom1) (struct let name = "stack_trace" end)
module Spec2 = Spec (StackDomain.Dom2) (struct let name = "stack_trace_set" end)
let _ =
  MCP.register_analysis (module SpecLoc : MCPSpec);
  MCP.register_analysis (module Spec1 : MCPSpec);
  MCP.register_analysis (module Spec2 : MCPSpec)
