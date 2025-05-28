(** Multi-threadedness analysis ([threadflag]). *)

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
  include Analyses.ValueContexts(D)
  module P = IdentityP (D)
  module V = UnitV
  module G = BoolDomain.MayBool

  let name () = "threadflag"


  let startstate v = Flag.bot ()
  let exitstate  v = Flag.get_multi ()

  let morphstate v _ = Flag.get_single ()

  let create_tid v =
    Flag.get_multi ()

  let return man exp fundec  =
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      (* TODO: is this necessary? *)
      Flag.join man.local (Flag.get_main ())
    | _ ->
      man.local

  let query man (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.MustBeSingleThreaded _ -> not (Flag.is_multi man.local) (* If this analysis can tell, it is the case since the start *)
    | Queries.MustBeUniqueThread -> not (Flag.is_not_main man.local)
    | Queries.IsEverMultiThreaded -> (man.global () : bool) (* requires annotation to compile *)
    (* This used to be in base but also commented out. *)
    (* | Queries.MayBePublic _ -> Flag.is_multi man.local *)
    | _ -> Queries.Result.top x

  module A =
  struct
    include Lattice.Prod(Flag)(BoolDomain.MayBool)
    let name () = "threadflag"

    let may_race (m1,b1) (m2,b2) =
      let use_threadflag = GobConfig.get_bool "ana.race.digests.threadflag" in
      let both_mt = Flag.is_multi m1 && Flag.is_multi m2 in
      let one_not_main = Flag.is_not_main m1 || Flag.is_not_main m2 in
      ((not use_threadflag) || (both_mt && one_not_main)) && b1 && b2

      (* kill access when single threaded *)

    let should_print m = true
  end

  let access man _ =
    let st =
      if GobConfig.get_bool "ana.race.digests.join" then
        is_currently_multi (Analyses.ask_of_man man)
      else if GobConfig.get_bool "ana.race.digests.tid" then
        has_ever_been_multi (Analyses.ask_of_man man)
      else
        true
    in
    ((man.local,st):A.t)

  let threadenter man ~multiple lval f args =
    if not (has_ever_been_multi (Analyses.ask_of_man man)) then
      man.emit Events.EnterMultiThreaded;
    [create_tid f]

  let threadspawn man ~multiple lval f args fman =
    man.sideg () true;
    if not (has_ever_been_multi (Analyses.ask_of_man man)) then
      man.emit Events.EnterMultiThreaded;
    D.join man.local (Flag.get_main ())
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
