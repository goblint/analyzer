(** May lockset digest analysis ([maylocksdigest]). *)

open Analyses
open GoblintCil
module LF = LibraryFunctions

module Arg =
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

  let query (ctx: (D.t, _, _, _) ctx) (type a) (q: a Queries.t): a Queries.result =
    match q with
    | MayLocksDigest -> ctx.local
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
