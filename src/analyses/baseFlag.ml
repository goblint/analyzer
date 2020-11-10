(** A stand-alone multi-threadedness aanlysis. *)

module GU = Goblintutil
module LF = LibraryFunctions

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Flag = BaseDomain.Flag
  module D = Flag
  module C = Flag
  module G = Lattice.Unit

  let name () = "baseflag"

  let startstate v = Flag.bot ()
  let exitstate  v = Flag.start_main v

  let morphstate v _ = Flag.start_single v

  let create_tid v =
    let loc = !Tracing.current_loc in
    Flag.spawn_thread loc v

  let body ctx f = ctx.local

  let branch ctx exp tv = ctx.local

  let return ctx exp fundec  =
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      Flag.make_main ctx.local
    | "StartupHook" ->
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
    | _ -> `Top


  let is_unique ctx fl =
    not (BaseDomain.Flag.is_bad fl) ||
    match ctx.ask Queries.IsNotUnique with
    | `Bool false -> true
    | _ -> false

  (* remove this function and everything related to exp.ignored_threads *)
  let is_special_ignorable_thread = function
    | (_, `Lifted f) ->
      let fs = GobConfig.get_list "exp.ignored_threads" |> List.map Json.string in
      List.mem f.vname fs
    | _ -> false

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    let fl = ctx.local in
    if BaseDomain.Flag.is_multi fl && not (is_special_ignorable_thread fl) then begin
      if is_unique ctx fl then
        let tid = BaseDomain.Flag.short 20 fl in
        (Access.LSSSet.singleton es, Access.LSSet.add ("thread",tid) es)
      else
        (Access.LSSSet.singleton es, es)
    end else
      Access.LSSSet.empty (), es

  let threadenter ctx f args =
    create_tid f

  let threadcombine ctx f args fd =
    Flag.make_main ctx.local
end

let _ =
  MCP.register_analysis (module Spec : Spec)
