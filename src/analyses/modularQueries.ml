open Prelude.Ana
open Analyses

module Spec =
struct
  include UnitAnalysis.Spec

  let name () = "modular_queries"

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | IsMultiple v ->
      TypeVarinfoMap.mem_varinfo v
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
