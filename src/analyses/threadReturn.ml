(** Thread returning analysis using boolean call stack ([threadreturn]). *)

open GoblintCil
open Analyses

let is_current (ask: Queries.ask): bool =
  ask.f Queries.MayBeThreadReturn


module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "threadreturn"
  module D = IntDomain.Booleans
  module C = D

  (* transfer functions *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    if !AnalysisState.global_initialization then
      (* We are inside enter_with inside a startfun, and thus the current function retruning is the main function *)
      [ctx.local, true]
    else
      [ctx.local, false]

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* keep local as opposed to IdentitySpec *)

  let startstate v = true
  let threadenter ctx lval f args = [true]
  let exitstate  v = D.top ()

  let query (ctx: (D.t, _, _, _) ctx) (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.MayBeThreadReturn -> ctx.local
    | _ -> Queries.Result.top x
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
