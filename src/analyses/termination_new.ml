open Prelude.Ana
open Analyses

module Spec : Analyses.MCPSpec =
struct

  let query ctx (type a) (q: a Queries.t): a Queries.result = ()

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
