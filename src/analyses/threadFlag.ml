(** Multi-threadedness analysis. *)

module LF = LibraryFunctions

open GoblintCil
open Analyses

let is_currently_multi (ask: Queries.ask): bool =
  if !AnalysisState.global_initialization then false else
    not (ask.f (Queries.MustBeSingleThreaded {since_start = false}))

let has_ever_been_multi (ask: Queries.ask): bool =
  if !AnalysisState.global_initialization then false else
    not (ask.f (Queries.MustBeSingleThreaded {since_start = true}))

module Spec =
struct
  include Analyses.IdentitySpec

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

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      (* TODO: is this necessary? *)
      Flag.join ctx.local (Flag.get_main ())
    | _ ->
      ctx.local

  let query ctx (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.MustBeSingleThreaded _ -> not (Flag.is_multi ctx.local) (* If this analysis can tell, it is the case since the start *)
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
    is_currently_multi (Analyses.ask_of_ctx ctx)

  let threadenter ctx lval f args =
    if not (has_ever_been_multi (Analyses.ask_of_ctx ctx)) then
      ctx.emit Events.EnterMultiThreaded;
    [create_tid f]

  let threadspawn ctx lval f args fctx =
    if not (has_ever_been_multi (Analyses.ask_of_ctx ctx)) then
      ctx.emit Events.EnterMultiThreaded;
    D.join ctx.local (Flag.get_main ())
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
