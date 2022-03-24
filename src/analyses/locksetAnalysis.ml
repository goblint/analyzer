(** Basic lockset analyses. *)

open Prelude.Ana
open Analyses
open ValueDomain

module type Arg =
sig
  module D: sig
    include Lattice.S
    val empty: unit -> t
  end

  val add: (D.t, _, D.t, _) ctx -> LockDomain.Lockset.Lock.t -> D.t
  val remove: (D.t, _, D.t, _) ctx -> Addr.t -> D.t
end


module Make (Arg: Arg): MCPSpec with module D = Arg.D and module C = Arg.D =
struct
  include Analyses.IdentitySpec
  let name () = "lockset"

  module D = Arg.D
  module C = D

  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
end


module MakeMay (Arg: Arg): MCPSpec with module D = Arg.D and module C = Arg.D =
struct
  include Make (Arg)
  let name () = "mayLockset"

  let event ctx e octx =
    match e with
    | Events.Lock l ->
      Arg.add ctx l
    | Events.Unlock l when Addr.equal l (Addr.from_var_offset (dummyFunDec.svar, `NoOffset)) ->
      (* unlock nothing *)
      ctx.local
    | Events.Unlock Addr (v, _) when ctx.ask (IsMultiple v) ->
      (* unlock nothing *)
      ctx.local
    | Events.Unlock l ->
      Arg.remove ctx l
    | _ ->
      ctx.local
end
