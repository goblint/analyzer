(** Analysis of variable-length arrays (VLAs) in scope ([vla]). *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "vla"
  module D = BoolDomain.MayBool
  module C = Lattice.Unit

  let context _ _ = ()

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, false]

  let combine_env ctx lval fexp f args fc au f_ask =
    ctx.local (* keep local as opposed to IdentitySpec *)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match (LibraryFunctions.find f).special arglist with
    | Setjmp _ ->
      (* Checking if this within the scope of an identifier of variably modified type *)
      if ctx.local then
        M.warn ~category:(Behavior (Undefined Other)) "setjmp called within the scope of a variably modified type. If a call to longjmp is made after this scope is left, the behavior is undefined.";
      ctx.local
    | _ ->
      ctx.local

  let vdecl ctx (v:varinfo) : D.t =
    ctx.local || Cilfacade.isVLAType v.vtype

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
