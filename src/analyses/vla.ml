(** An analysis to detect if an invocation is in the scope of a variably modified variable. *)

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "vla"
  module D = BoolDomain.MayBool
  module C = Lattice.Unit

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, false]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let vdecl ctx (v:varinfo) : D.t = true

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let exitstate  v = D.top ()
  let context _ _ = ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayBeInVLAScope -> (ctx.local:bool) (* Will not compile without annotation *)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
