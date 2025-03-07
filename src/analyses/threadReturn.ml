(** Thread returning analysis which abstracts a thread's call stack by a boolean, indicating whether it is at the topmost call stack frame or not ([threadreturn]). *)

open GoblintCil
open Analyses

let is_current (ask: Queries.ask): bool =
  ask.f Queries.MayBeThreadReturn


module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  let name () = "threadreturn"
  module D = BoolDomain.MayBool
  include Analyses.ValueContexts(D)

  (* transfer functions *)

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    if !AnalysisState.global_initialization then
      (* We are inside enter_with inside a startfun, and thus the current function retruning is the main function *)
      [man.local, true]
    else
      [man.local, false]

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* keep local as opposed to IdentitySpec *)

  let startstate v = true
  let threadenter man ~multiple lval f args = [true]
  let exitstate  v = D.top ()

  let query (man: (D.t, _, _, _) man) (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.MayBeThreadReturn -> man.local
    | _ -> Queries.Result.top x
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
