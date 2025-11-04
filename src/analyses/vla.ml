(** Analysis of variable-length arrays (VLAs) in scope ([vla]). *)

open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentityUnitContextsSpec

  let name () = "vla"
  module D = BoolDomain.MayBool

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [man.local, false]

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* keep local as opposed to IdentitySpec *)

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match (LibraryFunctions.find f).special arglist with
    | Setjmp _ ->
      (* Checking if this within the scope of an identifier of variably modified type *)
      if man.local then
        M.warn ~category:(Behavior (Undefined Other)) "setjmp called within the scope of a variably modified type. If a call to longjmp is made after this scope is left, the behavior is undefined.";
      man.local
    | _ ->
      man.local

  let vdecl man (v:varinfo) : D.t =
    man.local || Cilfacade.isVLAType v.vtype

  let startstate v = D.bot ()
  let threadenter man ~multiple lval f args = [D.top ()]
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
