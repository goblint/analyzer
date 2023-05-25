(** May lockset analysis ([maylocks]). *)

(* TODO: unused *)

open Analyses

module Arg =
struct
  module D = LockDomain.MayLockset
  module G = DefaultSpec.G
  module V = DefaultSpec.V

  let add ctx l =
    D.add l ctx.local

  let remove ctx l =
    D.remove (l, true) (D.remove (l, false) ctx.local)
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "maylocks"

  let exitstate  v = D.top () (* TODO: why? *)
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
