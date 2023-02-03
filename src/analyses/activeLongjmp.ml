(** Analysis tracking which longjmp is currently active *)

open Prelude.Ana
open Analyses

(* module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Lattice.Unit and type marshal = unit = *)
(* No signature so others can override module G *)
module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "activeLongjmp"
  module D = JmpBufDomain.JmpBufSet
  module C = Lattice.Unit

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    match desc.special arglist, f.vname with
    | Longjmp {env; value; sigrestore}, _ ->
      (* Put current buffer into set *)
      ctx.ask (EvalJumpBuf env)
    | _ -> ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let context _ _ = ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | ActiveJumpBuf ->
      (* Does not compile without annotation: "This instance (...) is ambiguous: it would escape the scope of its equation" *)
      (ctx.local:JmpBufDomain.JmpBufSet.t)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
