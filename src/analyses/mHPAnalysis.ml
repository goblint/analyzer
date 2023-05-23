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
    let should_print _ = true
  end

  let access ctx _: MHP.t = MHP.current (Analyses.ask_of_ctx ctx)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
