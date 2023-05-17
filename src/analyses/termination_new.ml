(** Work in progress *)

open Prelude.Ana
open Analyses

let terminates loop = () (* TODO *)

module Spec : Analyses.MCPSpec =
struct

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
      Result.top q (* TODO *)

end

let _ =
  (* Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
