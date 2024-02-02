(* Collect the essential information from [Base] start-state. Allows the start state to be queried for a modular analysis to be queried from anyhwere *)
open Analyses

module Spec =
struct
  include UnitAnalysis.Spec
  module D = Lattice.Unit
  module C = Lattice.Unit
  module G = BaseDomain.CPA
  module V = struct
    include CilType.Fundec
    let is_write_only _ = false
  end

  let body ctx f =
    let ask = Analyses.ask_of_ctx ctx in
    let cpa = ask.f Queries.BaseCPA in
    ctx.sideg f cpa;
    ()

  let name () = "startstate"

  let is_modular () = true

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.StartCPA f ->
      let r : G.t = ctx.global f in
      r
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : Analyses.MCPSpec)
