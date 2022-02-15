(** MHP access analysis. *)
open Prelude.Ana
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

  let access ctx e vo w: MHP.t =
    {
      tid = ctx.ask CurrentThreadId;
      created = ctx.ask CreatedThreads;
      must_joined = ctx.ask MustJoinedThreads
    }
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
