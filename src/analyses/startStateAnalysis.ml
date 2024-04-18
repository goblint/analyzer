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

  module VD = BaseDomain.VD

  let current_function ctx =
    Node.find_fundec ctx.node

  let body ctx f =
    let ask = Analyses.ask_of_ctx ctx in
    let cpa = ask.f Queries.BaseCPA in
    ctx.sideg f cpa;
    if Messages.tracing then Messages.tracel "startstate" "For %a, side-effecting state %a \n" CilType.Fundec.pretty f G.pretty cpa;
    ()

  let name () = "startstate"

  let modular_support () = Modular

  let event ctx e octx = match e with
    | Events.GenerateObject x ->
      if M.tracing then M.tracel "generate_object" "Generating object: %a\n" CilType.Varinfo.pretty x;
      let v, _ = VD.top_value_typed_address_targets x.vtype in
      let cpa_bot = G.bot () in
      let cpa_with_x = G.add x v cpa_bot in
      let current_function = current_function ctx in
      ctx.sideg current_function cpa_with_x;
      ctx.local
    | _ ->
      ctx.local

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.StartCPA f ->
      let r : G.t = ctx.global f in
      if Messages.tracing then Messages.tracel "startstate" "For %a, startstate analysis answering: %a\n" CilType.Fundec.pretty f G.pretty r;
      r
    | _ -> Queries.Result.top q

end

let _ =
  MCP.register_analysis (module Spec : Analyses.MCPSpec)
