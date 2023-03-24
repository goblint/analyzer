(** Thread returning analysis. *)

open Prelude.Ana
open Analyses

let is_current (ask: Queries.ask): bool =
  ask.f Queries.MayBeThreadReturn


module Spec : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "threadreturn"
  module D = IntDomain.Booleans
  module C = D

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
    if !Goblintutil.global_initialization then
      (* We are inside enter_with inside a startfun, and thus the current function retruning is the main function *)
      [ctx.local, true]
    else
      [ctx.local, false]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = true
  let threadenter ctx lval f args = [true]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query (ctx: (D.t, _, _, _) ctx) (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.MayBeThreadReturn -> ctx.local
    | _ -> Queries.Result.top x
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
