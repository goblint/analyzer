(** Analysis of active [longjmp] targets ([activeLongjmp]). *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "activeLongjmp"

  (* The first component are the longjmp targets, the second are the longjmp callers *)
  module D = JmpBufDomain.ActiveLongjmps

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist, f.vname with
    | Longjmp {env; value}, _ ->
      (* Set target to current value of env *)
      let bufs = man.ask (EvalJumpBuf env) in
      bufs, JmpBufDomain.NodeSet.singleton(man.prev_node)
    | _ -> man.local

  (* Initial values don't really matter: overwritten at longjmp call. *)
  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.bot ()]
  let exitstate  v = D.top ()

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | ActiveJumpBuf ->
      (* Does not compile without annotation: "This instance (...) is ambiguous: it would escape the scope of its equation" *)
      (man.local:JmpBufDomain.ActiveLongjmps.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
