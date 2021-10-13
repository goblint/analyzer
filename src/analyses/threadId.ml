(** Current thread ID analysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

module Thread = ThreadIdDomain.Thread
module ThreadLifted = ThreadIdDomain.ThreadLifted

let get_current (ask: Queries.ask): ThreadLifted.t =
  ask.f Queries.CurrentThreadId

let get_current_unlift ask: Thread.t =
  match get_current ask with
  | `Lifted thread -> thread
  | _ -> failwith "ThreadId.get_current_unlift"


module Spec =
struct
  include Analyses.DefaultSpec

  module TD = Thread.D

  module D = Lattice.Prod (ThreadLifted) (TD)
  module C = D
  module G = Lattice.Unit

  let name () = "threadid"

  let startstate v = (ThreadLifted.bot (), TD.bot ())
  let exitstate  v = (`Lifted (Thread.threadinit v ~multiple:true), TD.bot ())

  let morphstate v _ = (`Lifted (Thread.threadinit v ~multiple:false), TD.bot ())

  let create_tid (current, td) (node: Node.t) v =
    match current with
    | `Lifted current ->
      `Lifted (Thread.threadenter (current, td) node v)
    | _ ->
      `Lifted (Thread.threadinit v ~multiple:true)

  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "StartupHook" ->
      (* TODO: is this necessary? *)
      (ThreadLifted.top (), TD.bot ()) (* TODO: what should TD be? *)
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
    ctx.ask Queries.MustBeUniqueThread

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    if is_unique ctx then
      let tid = fst ctx.local in
      let tid = ThreadLifted.show tid in
      (Access.LSSSet.singleton es, Access.LSSet.add ("thread",tid) es)
    else
      (Access.LSSSet.singleton es, es)

  let query (ctx: (D.t, _, _) ctx) (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.CurrentThreadId -> fst ctx.local
    | Queries.PartAccess {exp; var_opt; write} ->
      part_access ctx exp var_opt write
    | Queries.MustBeUniqueThread ->
      begin match fst ctx.local with
        | `Lifted tid -> Thread.is_unique tid
        | _ -> Queries.MustBool.top ()
      end
    | _ -> Queries.Result.top x

  let threadenter ctx lval f args =
    [(create_tid ctx.local ctx.node f, TD.bot ())]

  let threadspawn ctx lval f args fctx =
    let (current, td) = ctx.local in
    (current, Thread.threadspawn td ctx.node f)

  type marshal = Thread.marshal * ThreadLifted.marshal

  let init m = match m with
    | Some (t, tl) ->
      Thread.init t;
      ThreadLifted.init tl
    | None -> ()

  let finalize () =
    Thread.marshal (), ThreadLifted.marshal ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
