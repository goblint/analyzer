(** May-happen-in-parallel (MHP) analysis for memory accesses ([mhp]). *)

open Analyses

module Spec =
struct
  include UnitAnalysis.Spec
  let name () = "mhp"

  module A =
  struct
    include MHP
    let name () = "mhp"
    let may_race = MHP.may_happen_in_parallel
    let should_print {tid; created; must_joined} =
      GobConfig.get_bool "dbg.full-output" ||
      (not (ConcDomain.ThreadSet.is_empty created) ||
       not (ConcDomain.ThreadSet.is_empty must_joined))
  end

  let access ctx _: MHP.t = MHP.current (Analyses.ask_of_ctx ctx)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
