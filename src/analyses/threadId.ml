(** A stand-alone multi-threadedness aanlysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

module Thread = ConcDomain.Thread
module ThreadLifted = ConcDomain.ThreadLifted

let get_current ctx: ThreadLifted.t =
  Obj.obj (List.assoc "threadid" ctx.presub)


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

  (* TODO: move part of threadflag part_access here *)

  let threadenter ctx f args =
    create_tid f

  let threadcombine ctx f args fd =
    (* TODO: could also be bot? *)
    ctx.local
end

let _ =
  MCP.register_analysis (module Spec : Spec)
