(* threadCreate: Tracks whether a thread was (maybe) created since the beginning of the current function *)

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "threadCreate"
  module D = BoolDomain.MayBool
  module C = Lattice.Unit

  let context _ _ = ()

  (* transfer functions *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Entering a function means we start newly with no thread created*)
    [ctx.local, D.bot ()]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    (* after a call, the state is (maybe)true, if a thread was created before the call or in the call*)
    ctx.local || au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
      | ThreadCreate _ -> true  (*a thread is created*)
      | Unknown -> D.top () (*unknown function: Maybe a thread was created*)
      | _ -> ctx.local (*other known functions: No thread created*)

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = true
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MayThreadcreate -> (ctx.local : D.t)
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
