(** May lockset analysis and analysis of double locking. *)

open Analyses

module Arg:LocksetAnalysis.MayArg =
struct
  module D = LockDomain.MayLocksetNoRW
  module G = DefaultSpec.G
  module V = DefaultSpec.V

  let add ctx (l,_) =
    if D.mem l ctx.local then
      (M.warn "double locking"; ctx.local)
    else
      D.add l ctx.local

  let remove ctx l = D.remove l ctx.local
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "maylocks"

  let exitstate  v = D.top () (* TODO: why? *)

  let return ctx exp fundec =
    if not @@ D.is_bot ctx.local && ThreadReturn.is_current (Analyses.ask_of_ctx ctx) then M.warn "Exiting thread while still holding a mutex!";
    ctx.local
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
