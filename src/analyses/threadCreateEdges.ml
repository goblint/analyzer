(* threadCreateEdges: Tracks which create edges have been encountered since the beginning of the current function *)

open Prelude.Ana
open Analyses

module Thread = ThreadIdDomain.Thread

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "threadCreateEdges"
  module D = Lattice.LiftTop(Thread.D)
  module C = Lattice.Unit

  let context _ _ = ()

  (* transfer functions *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Entering a function means we start newly with no thread created*)
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    (* after a call, join both both sets of encountered create edges from caller and callee*)
    if M.tracing then M.trace "threadCreateEdges" "caller: %a\ncallee: %a\nboth: %a\n\n" D.pretty ctx.local D.pretty au D.pretty (D.join ctx.local au); 
    D.join ctx.local au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
      | Unknown -> D.top () (*unknown function: Maybe a unknown thread was created*)
      | _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = 
    match ctx.local with
    | `Top -> `Top
    | `Lifted ces -> D.lift (Thread.threadspawn ces ctx.prev_node f)

  let exitstate  v = D.top () (* is this correct? What exactly is exitstate?*)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.CreateEdges -> (ctx.local : D.t)
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
