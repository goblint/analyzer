(** Work in progress *)

open Analyses

let terminates loop = () (* TODO *)

module Spec : Analyses.MCPSpec =
struct

  let name () = "termination"

  module D = Lattice.Unit (* TODO *)
  module C = D (* TODO *)

  let startstate _ = D.bot () (* TODO *)
  let exitstate = startstate (* TODO *)

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    Result.top q (* TODO *)

end

let _ =
  (** Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
