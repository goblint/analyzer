(** A stand-alone multi-threadedness aanlysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

let is_multi_ask (ask: Queries.ask): bool =
  match ask Queries.SingleThreaded with
  | `Bool b -> not b
  | `Top -> true
  | _ -> failwith "is_multi_ask"
let is_multi ctx: bool = is_multi_ask ctx.ask


module Spec =
struct
  include Analyses.DefaultSpec

  module Flag = ConcDomain.Simple
  module D = Flag
  module C = Flag
  module G = Lattice.Unit

  let name () = "threadflag"

  let startstate v = Flag.bot ()
  let exitstate  v = Flag.get_multi ()

  let morphstate v _ = Flag.get_single ()

  let create_tid v =
    Flag.get_multi ()

  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      (* TODO: is this necessary? *)
      Flag.join ctx.local (Flag.get_main ())
    | "StartupHook" ->
      (* TODO: is this necessary? *)
      Flag.get_multi ()
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
    match x with
    | Queries.SingleThreaded -> `Bool (Queries.BD.of_bool (not (Flag.is_multi ctx.local)))
    | Queries.IsNotUnique -> `Bool (Flag.is_bad ctx.local)
    (* This used to be in base but also commented out. *)
    (* | Queries.IsPublic _ -> `Bool (Flag.is_multi ctx.local) *)
    | _ -> `Top

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    let fl = ctx.local in
    if Flag.is_multi fl then
      (Access.LSSSet.singleton es, es)
    else
      (* kill access when single threaded *)
      (Access.LSSSet.empty (), es)

  let threadenter ctx f args =
    create_tid f

  let threadcombine ctx f args fctx =
    Flag.join ctx.local (Flag.get_main ())
end

let _ =
  MCP.register_analysis ~dep:["threadid"] (module Spec : Spec)
