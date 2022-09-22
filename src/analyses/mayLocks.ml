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
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
