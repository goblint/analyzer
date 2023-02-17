open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec
  module VarSet = SetDomain.ToppedSet(CilType.Varinfo) (struct let topname = "All vars" end)

  let name () = "poisonVariables"
  module D = VarSet
  module C = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx ?(longjmpthrough = false) (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let context _ _ = ()

  let event ctx e octx =
    match e with
    | Events.Poison poisoned -> D.join poisoned ctx.local
    | _ -> ctx.local

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
