open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "antConds"
  module D = SetDomain.Make(Printable.Prod (CilType.Exp) (PreValueDomain.IndexDomain))
  module C = D

  let startstate v = D.bot ()
  let exitstate  v = D.bot ()

  let event ctx (event: Events.t) octx =
    match event with
    | ArrayIndex {exp; value} -> D.add (exp, value) ctx.local
    | _ -> ctx.local

end

let _ = MCP.register_analysis (module Spec : MCPSpec)
