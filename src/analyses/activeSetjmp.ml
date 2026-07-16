(** Analysis of active [setjmp] buffers ([activeSetjmp]). *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "activeSetjmp"

  module D = JmpBufDomain.JmpBufSet
  include Analyses.ValueContexts(D)
  module P = IdentityP (D)

  let combine_env man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask:Queries.ask): D.t =
    man.local (* keep local as opposed to IdentitySpec *)

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist with
    | Setjmp _ ->
      let entry = (man.prev_node, man.control_context ()) in
      D.add (Target entry) man.local
    | _ -> man.local

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.bot ()]
  let exitstate v = D.top ()

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | ValidLongJmp -> (man.local: D.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
