(** Thread join analysis. *)
open Prelude.Ana
open Analyses

module TID  = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module MustTIDs = ConcDomain.MustThreadSet

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "threadJoins"
  module D = MustTIDs
  module C = D
  module G = MustTIDs
  module V = TID

  let threadexit ctx =
    (
      match ctx.ask CurrentThreadId with
      | `Lifted tid when ThreadReturn.is_current (Analyses.ask_of_ctx ctx) -> ctx.sideg tid ctx.local
      | _ -> () (* correct? *)
    );
    ctx.local

  (* transfer functions *)
  let return ctx (exp:exp option) (f:fundec) : D.t = threadexit ctx

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | ThreadExit _ ->  threadexit ctx
    | ThreadJoin { thread = id; ret_var } ->
      let threads = ctx.ask (Queries.EvalThread id) in
      if TIDs.is_top threads then
        ctx.local
      else (
        (* elements throws if the thread set is top *)
        let threads = TIDs.elements threads in
        match threads with
        | [tid] when TID.is_unique tid->
          let joined = ctx.global tid in
          D.union (D.add tid ctx.local) joined
        | _ -> ctx.local (* if multiple possible thread ids are joined, none of them is must joined*)
        (* Possible improvement: Do the intersection first, things that are must joined in all possibly joined threads are must-joined *)
      )
    | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustJoinedThreads -> (ctx.local:ConcDomain.MustThreadSet.t) (* type annotation needed to avoid "would escape the scope of its equation" *)
    | _ ->  Queries.Result.top q

  let startstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
