(** A stand-alone multi-threadedness aanlysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

module Thread = ConcDomain.Thread
module ThreadLifted = ConcDomain.ThreadLifted

let get_current ctx: ThreadLifted.t =
  Obj.obj (List.assoc "threadid" ctx.presub)

let get_current_unlift ctx: Thread.t =
  match get_current ctx with
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

  let query ctx x =
    (* TODO: thread ID query *)
    match x with
    | _ -> `Top

  let is_unique ctx =
    match ctx.ask Queries.IsNotUnique with
    | `Bool false -> true
    | _ -> false

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    if is_unique ctx then
      let tid = ctx.local in
      let tid = ThreadLifted.short 20 tid in
      (Access.LSSSet.singleton es, Access.LSSet.add ("thread",tid) es)
    else
      (Access.LSSSet.singleton es, es)

  let threadenter ctx f args =
    create_tid f

  let threadspawn ctx f args fctx =
    (* TODO: could also be bot? *)
    ctx.local
end

let _ =
  MCP.register_analysis (module Spec : Spec)
