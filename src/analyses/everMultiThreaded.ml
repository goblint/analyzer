(** Work in progress *)

open Analyses

module UnitV =
struct
  include Printable.Unit
  include StdV
end

module Spec : Analyses.MCPSpec =
struct

  (** Provides some default implementations *)
  include Analyses.IdentitySpec

  let name () = "evermultithreaded"

  module D = Lattice.Unit
  module C = D
  module V = UnitV
  module G = BoolDomain.MayBool

  let startstate _ = ()
  let exitstate = startstate

  (** Sets the global invariant to true when a thread is spawned *)
  let threadspawn ctx lval f args fctx =
    ctx.sideg () true;
    ()

  let query ctx (type a) (q: a Queries.t) : a Queries.result =
    match q with
    | Queries.IsEverMultiThreaded ->
      (*
        ctx.global ()
         *)
      Queries.Result.top q (* TODO *)
    | _ ->
      Queries.Result.top q

end

let () =
  (* Register this analysis within the master control program *)
  MCP.register_analysis (module Spec : MCPSpec)
