(** Analysis for checking whether ghost globals are only accessed by one unique thread ([phaseGhost]). *)

open Analyses
open GoblintCil

module TID = ThreadIdDomain.Thread

module TIDs = ConcDomain.ThreadSet

module Spec =
struct
  module D = Lattice.Unit
  include IdentityUnitContextsSpec

  let name () = "phaseGhost"

  module V = VarinfoV
  module G = TIDs

  let startstate _ = ()
  let exitstate _ = ()

  let tids_of_current_thread man =
    match man.ask Queries.CurrentThreadId with
    | `Lifted tid when TID.is_unique tid -> TIDs.singleton tid
    | _ -> TIDs.top ()

  let event man e oman =
    match e with
    | Events.Access {ad; _} ->
      let tids = tids_of_current_thread man in
      Queries.AD.iter (function
          | Queries.AD.Addr.Addr (var, _) when YamlWitness.VarSet.mem var !(YamlWitness.ghostVars) ->
            man.sideg var tids
          | _ ->
            ()
        ) ad;
      man.local
    | _ ->
      man.local

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.WarnGlobal g ->
      let g: V.t = Obj.obj g in
      let tidset = man.global g in
      if TIDs.is_top tidset then
        M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by a non-unique or unknown thread id" CilType.Varinfo.pretty g
      else
        (match TIDs.elements tidset with
         | [tid] ->
           M.info_noloc ~category:Witness "phaseGhost: global %a is only accessed by unique thread %a" CilType.Varinfo.pretty g TID.pretty tid
         | _ ->
           M.warn_noloc ~category:Witness "phaseGhost: global %a is accessed by multiple unique threads: %a" CilType.Varinfo.pretty g TIDs.pretty tidset
        )
    | _ ->
      Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["access"; "threadid"] (module Spec : MCPSpec)
