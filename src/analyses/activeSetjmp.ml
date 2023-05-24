(** Analysis tracking which setjmp(s) are currently active *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "activeSetjmp"

  module D = JmpBufDomain.JmpBufSet
  module C = JmpBufDomain.JmpBufSet

  let should_join a b = D.equal a b

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask): D.t =
    ctx.local (* keep local as opposed to IdentitySpec *)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Setjmp _ ->
      let entry = (ctx.prev_node, ctx.control_context ()) in
      D.add (Target entry) ctx.local
    | _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.bot ()]
  let exitstate v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | ValidLongJmp -> (ctx.local: D.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
