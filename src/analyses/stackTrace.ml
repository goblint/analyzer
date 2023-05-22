(** Stack-trace "analyses". *)

open GoblintCil
open Analyses
module LF = LibraryFunctions

module Spec (D: StackDomain.S) (N: sig val name : string end)=
struct
  include Analyses.IdentitySpec

  let name () = N.name
  module D = D
  module C = D

  (* transfer functions *)

  let body ctx (f:fundec) : D.t =
    if f.svar.vname = "__goblint_dummy_init" then ctx.local else D.push f.svar ctx.local

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* keep local as opposed to IdentitySpec *)

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let exitstate  v = D.top ()
end

module SpecLoc =
struct
  include Analyses.IdentitySpec

  let name () = "stack_loc"
  module D = StackDomain.Dom3
  module C = StackDomain.Dom3

  (* transfer functions *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.push !Tracing.current_loc ctx.local]

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* keep local as opposed to IdentitySpec *)


  let startstate v = D.bot ()
  let exitstate  v = D.top ()

  let threadenter ctx lval f args =
    [D.push !Tracing.current_loc ctx.local]
end


module Spec1 = Spec (StackDomain.Dom1) (struct let name = "stack_trace" end)
module Spec2 = Spec (StackDomain.Dom2) (struct let name = "stack_trace_set" end)
let _ =
  MCP.register_analysis (module SpecLoc : MCPSpec);
  MCP.register_analysis (module Spec1 : MCPSpec);
  MCP.register_analysis (module Spec2 : MCPSpec)
