(** Current thread ID analysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

module Thread = ConcDomain.Thread
module ThreadLifted = ConcDomain.ThreadLifted

let get_current (ask: Queries.ask): ThreadLifted.t =
  match ask Queries.CurrentThreadId with
  | `Varinfo v -> v
  | `Top -> `Top
  | `Bot -> `Bot
  | _ -> failwith "ThreadId.get_current"

let get_current_unlift ask: Thread.t =
  match get_current ask with
  | `Lifted thread -> thread
  | _ -> failwith "ThreadId.get_current_unlift"


module Spec =
struct
  include Analyses.DefaultSpec

  module D = ThreadLifted
  module C = ThreadLifted
  module G = Lattice.Unit

  let name () = "threadid"

  let startstate v = ThreadLifted.bot ()
  let exitstate  v = `Lifted (Thread.start_thread v)

  let morphstate v _ = `Lifted (Thread.start_thread v)

  let create_tid v =
    let loc = !Tracing.current_loc in
    `Lifted (Thread.spawn_thread loc v)

  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "StartupHook" ->
      (* TODO: is this necessary? *)
      ThreadLifted.top ()
    | _ ->
      ctx.local

  let assign ctx (lval:lval) (rval:exp) : D.t  =
    ctx.local

  let enter ctx lval f args =
    [ctx.local,ctx.local]

  let combine ctx lval fexp f args fc st2 = st2

  let special ctx lval f args =
    ctx.local

  let is_unique ctx =
    match ctx.ask Queries.MustBeUniqueThread with
    | `MustBool true -> true
    | _ -> false

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    if is_unique ctx then
      let tid = ctx.local in
      let tid = ThreadLifted.show tid in
      (Access.LSSSet.singleton es, Access.LSSet.add ("thread",tid) es)
    else
      (Access.LSSSet.singleton es, es)

  let query ctx x =
    match x with
    | Queries.CurrentThreadId -> `Varinfo ctx.local
    | Queries.PartAccess {exp; var_opt; write} ->
      `PartAccessResult (part_access ctx exp var_opt write)
    | _ -> `Top

  let threadenter ctx lval f args =
    [create_tid f]

  let threadspawn ctx lval f args fctx =
    ctx.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
