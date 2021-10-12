(** Thread join analysis. *)
open Prelude.Ana
open Analyses

module TID  = ThreadIdDomain.Thread
module TIDs = ConcDomain.ThreadSet
module MustTIDs = ConcDomain.MustThreadSet

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "threadJoins"
  module D = MustTIDs
  module C = D
  module G = MustTIDs

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t =
    (
      match ctx.ask CurrentThreadId with
      | `Lifted tid -> ctx.sideg (TID.to_varinfo tid) ctx.local
      | _ -> () (* correct? *)
    );
    ctx.local
  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t = au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match LibraryFunctions.classify f.vname arglist with
    | `ThreadJoin (id, ret_var) ->
      (
        try
          (* elements throws if the thread set is top *)
          let threads = TIDs.elements (ctx.ask (Queries.EvalThread id)) in
          match threads with
          | [tid] when TID.is_unique tid->
            let joined = ctx.global (TID.to_varinfo tid) in
            D.union (D.add tid ctx.local) joined
          | _ -> ctx.local (* if multiple possible thread ids are joined, none of them is must joined*)
        (* Possible improvement: Do the intersection first, things that are must joined in all possibly joined threads are must-joined *)
        with
          _ -> ctx.local
      )
    | _ -> ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.MustJoinedThreads -> (ctx.local:ConcDomain.MustThreadSet.t) (* type annotation needed to avoid "would escape the scope of its equation" *)
    | _ ->  Queries.Result.top q

  let startstate v = D.top ()
  let threadenter ctx lval f args = [ctx.local]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ = MCP.register_analysis ~dep:["threadid"] (module Spec : MCPSpec)
