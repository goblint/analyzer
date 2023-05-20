(** Work in progress *)

open Analyses
open GoblintCil
open TerminationPreprocessing

let terminates ctx loop exp =
  match ctx.ask (EvalInt exp) with
    _ -> () (* TODO *)

module Spec : Analyses.MCPSpec =
struct

  let name () = "termination"

  module D = Lattice.Unit (* TODO *)
  module C = D (* TODO *)

  let startstate _ = D.bot () (* TODO *)
  let exitstate = startstate (* TODO *)

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let branch ctx (exp:exp) (tv:bool) =
    ctx.local (* TODO *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    Result.top q (* TODO *)

end

let _ =
  (** Register the preprocessing *)
  Cilfacade.register_preprocess (Spec.name ()) (new loopCounterVisitor);
  (** Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
