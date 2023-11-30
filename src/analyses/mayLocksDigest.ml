(** May lockset digest analysis ([maylocksdigest]). *)

open Analyses
open GoblintCil
module LF = LibraryFunctions

module Arg:LocksetAnalysis.MayArg =
struct
  module D = LockDomain.MayLocksetNoRW
  module P = IdentityP (D)
  module G = DefaultSpec.G
  module V = DefaultSpec.V

  let add ctx (l,r) =
    D.add l ctx.local

  let remove ctx l =
    ctx.local
end

module Spec =
struct
  include LocksetAnalysis.MakeMay (Arg)
  let name () = "maylocksdigest"

  let threadenter ctx ~multiple lval f args = [ctx.local]
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
