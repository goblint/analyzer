(** May lockset digest analysis ([maylocksdigest]). *)

open Analyses
open GoblintCil
module LF = LibraryFunctions

module Arg =
struct
  module D = LockDomain.MayLocksetNoRW
  module P = IdentityP (D)

  module V =
  struct
    include StdV
    include LockDomain.Addr
  end
  module G = SetDomain.Make (D)

  let add (ctx: (D.t, G.t, D.t, V.t) ctx) (l,r) =
    let l's = ctx.global l in
    G.iter (fun l' ->
        if not (D.mem l ctx.local && not (D.mem l l')) then
          ctx.split (D.add l (D.union ctx.local l')) []
      ) l's;
    if D.mem l ctx.local then
      raise Deadcode
    else
      D.add l ctx.local (* for initial empty *)

  let remove (ctx: (D.t, G.t, D.t, V.t) ctx) l =
    ctx.sideg l (G.singleton ctx.local);
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
