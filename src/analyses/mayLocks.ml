(** May-lockset analysis. *)

open Prelude.Ana
open Analyses
open ValueDomain

module Spec =
struct
  include Analyses.IdentitySpec

  let name () = "maylocks"
  module D = LockDomain.MayLockset
  module C = LockDomain.MayLockset

  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  v = D.top ()

  let event ctx e octx =
    match e with
    | Events.Lock l ->
      D.add l ctx.local
    | Events.Unlock l when Addr.equal l (Addr.from_var_offset (dummyFunDec.svar, `NoOffset)) ->
      (* unlock nothing *)
      ctx.local
    | Events.Unlock Addr (v, _) when ctx.ask (IsMultiple v) ->
      (* unlock nothing *)
      ctx.local
    | Events.Unlock l ->
      D.remove (l, true) (D.remove (l, false) ctx.local)
    | _ ->
      ctx.local
end

let _ =
  MCP.register_analysis ~dep:["mutexEvents"] (module Spec : MCPSpec)
