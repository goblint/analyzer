(** Stack-trace "analyses". *)

open Prelude.Ana
open Analyses
module LF = LibraryFunctions

module Spec (D: StackDomain.S) (P: sig val name : string end)=
struct
  include Analyses.DefaultSpec

  let name () = P.name
  module D = D
  module C = D
  module G = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    if f.svar.vname = "__goblint_dummy_init" then ctx.local else D.push f.svar ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = D.bot ()
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

module SpecLoc =
struct
  include Analyses.DefaultSpec

  let name () = "stack_loc"
  module D = StackDomain.Dom3
  module C = StackDomain.Dom3
  module G = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.push !Tracing.current_loc ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local


  let startstate v = D.bot ()
  let exitstate  v = D.top ()

  let threadenter ctx lval f args =
    D.push !Tracing.current_loc ctx.local

  let threadspawn ctx lval f args fctx = ctx.local
end


module Spec1 = Spec (StackDomain.Dom1) (struct let name = "stack_trace" end)
module Spec2 = Spec (StackDomain.Dom2) (struct let name = "stack_trace_set" end)
let _ =
  MCP.register_analysis (module SpecLoc : MCPSpec);
  MCP.register_analysis (module Spec1 : MCPSpec);
  MCP.register_analysis (module Spec2 : MCPSpec)
