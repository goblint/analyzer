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

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match (LibraryFunctions.find f).special arglist with
    | Setjmp _ ->
      (* Checking if this within the scope of an identifier of variably modified type *)
      if ctx.local then
        M.warn "setjmp called within the scope of a variably modified type. If a call to longjmp is made after this scope is left, the behavior is undefined.";
      ctx.local
    | _ ->
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
