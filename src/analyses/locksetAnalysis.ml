(** Basic lockset analyses. *)

open Analyses


module type DS =
sig
  include Lattice.S
  val empty: unit -> t
end

module Make (D: DS) =
struct
  include Analyses.IdentitySpec
  let name () = "lockset"

  module D = D
  include Analyses.ValueContexts(D)

  let startstate v = D.empty ()
  let threadenter man ~multiple lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
end


module type MayArg =
sig
  module D: DS
  module G: Lattice.S
  module V: SpecSysVar

  val add: (D.t, G.t, D.t, V.t) man -> LockDomain.AddrRW.t -> D.t
  val remove: (D.t, G.t, D.t, V.t) man -> ValueDomain.Addr.t -> D.t
end

module MakeMay (Arg: MayArg) =
struct
  include Make (Arg.D)
  let name () = "mayLockset"

  module G = Arg.G
  module V = Arg.V

  let event man e oman =
    match e with
    | Events.Lock l ->
      Arg.add man l (* add all locks, including blob and unknown *)
    | Events.Unlock UnknownPtr ->
      man.local (* don't remove any locks, including unknown itself *)
    | Events.Unlock Addr (v, _) when man.ask (IsMultiple v) ->
      man.local (* don't remove non-unique lock *)
    | Events.Unlock l ->
      Arg.remove man l (* remove definite lock or none in parallel if ambiguous *)
    | _ ->
      man.local
end


module type MustArg =
sig
  include MayArg
  val remove_all: (D.t, _, D.t, _) man -> D.t
end

module MakeMust (Arg: MustArg) =
struct
  include Make (Arg.D)
  let name () = "mustLockset"

  module G = Arg.G
  module V = Arg.V

  let event man e oman =
    match e with
    | Events.Lock (UnknownPtr, _) ->
      man.local (* don't add unknown lock *)
    | Events.Lock (Addr (v, _), _) when man.ask (IsMultiple v) ->
      man.local (* don't add non-unique lock *)
    | Events.Lock l ->
      Arg.add man l (* add definite lock or none in parallel if ambiguous *)
    | Events.Unlock UnknownPtr ->
      Arg.remove_all man (* remove all locks *)
    | Events.Unlock l ->
      Arg.remove man l (* remove definite lock or all in parallel if ambiguous (blob lock is never added) *)
    | _ ->
      man.local
end
