(** Multi-threadedness analysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

let is_multi (ask: Queries.ask): bool =
  if !GU.global_initialization || GobConfig.get_bool "ana.library.enabled" then false else
  not (ask.f Queries.MustBeSingleThreaded)


module Spec =
struct
  include Analyses.DefaultSpec

  module Flag = ThreadFlagDomain.Simple
  module D = Flag
  module C = Flag

  let name () = "threadflag"

  let startstate v = Flag.bot ()
  let exitstate  v = Flag.get_multi ()

  let morphstate v _ = Flag.get_single ()

  let create_tid v =
    Flag.get_multi ()

  let should_join = D.equal

  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      (* TODO: is this necessary? *)
      Flag.join ctx.local (Flag.get_main ())
    | _ ->
      ctx.local

  let assign ctx (lval:lval) (rval:exp) : D.t  =
    ctx.local

  let enter ctx lval f args =
    [ctx.local,ctx.local]

  let combine ctx lval fexp f args fc st2 = st2

  let special ctx lval f args =
    ctx.local

  let query ctx (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.MustBeSingleThreaded -> not (Flag.is_multi ctx.local)
    | Queries.MustBeUniqueThread -> not (Flag.is_not_main ctx.local)
    (* This used to be in base but also commented out. *)
    (* | Queries.MayBePublic _ -> Flag.is_multi ctx.local *)
    | _ -> Queries.Result.top x

  module A =
  struct
    include BoolDomain.Bool
    let name () = "multi"
    let may_race m1 m2 = m1 && m2 (* kill access when single threaded *)
    let should_print m = not m
  end
  let access ctx _ =
    is_multi (Analyses.ask_of_ctx ctx)

  let threadenter ctx lval f args =
    if not (is_multi (Analyses.ask_of_ctx ctx)) then
      ctx.emit Events.EnterMultiThreaded;
    [create_tid f]

  let threadspawn ctx lval f args fctx =
    if not (is_multi (Analyses.ask_of_ctx ctx)) then
      ctx.emit Events.EnterMultiThreaded;
    D.join ctx.local (Flag.get_main ())
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
