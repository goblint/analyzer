(** Basic lockset analyses. *)

open Prelude.Ana
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
  module C = D

  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let exitstate  v = D.empty ()
end


module type MayArg =
sig
  module D: DS
  val add: (D.t, _, D.t, _) ctx -> LockDomain.Lockset.Lock.t -> D.t
  val remove: (D.t, _, D.t, _) ctx -> ValueDomain.Addr.t -> D.t
end

module MakeMay (Arg: MayArg) =
struct
  include Make (Arg.D)
  let name () = "mayLockset"

  let event ctx e octx =
    match e with
    | Events.Lock l ->
      Arg.add ctx l (* add all locks, including blob and unknown *)
    | Events.Unlock UnknownPtr ->
      ctx.local (* don't remove any locks, including unknown itself *)
    | Events.Unlock Addr (v, _) when ctx.ask (IsMultiple v) ->
      ctx.local (* don't remove non-unique lock *)
    | Events.Unlock l ->
      Arg.remove ctx l (* remove definite lock or none in parallel if ambiguous *)
    | _ ->
      ctx.local
end


module type MustArg =
sig
  include MayArg
  val remove_all: (D.t, _, D.t, _) ctx -> D.t
end

module MakeMust (Arg: MustArg) =
struct
  include Make (Arg.D)
  let name () = "mustLockset"

  let event ctx e octx =
    match e with
    | Events.Lock (UnknownPtr, _) ->
      ctx.local (* don't add unknown lock *)
    | Events.Lock (Addr (v, _), _) when ctx.ask (IsMultiple v) ->
      ctx.local (* don't add non-unique lock *)
    | Events.Lock l ->
      Arg.add ctx l (* add definite lock or none in parallel if ambiguous *)
    | Events.Unlock UnknownPtr ->
      Arg.remove_all ctx (* remove all locks *)
    | Events.Unlock l ->
      Arg.remove ctx l (* remove definite lock or all in parallel if ambiguous (blob lock is never added) *)
    | _ ->
      ctx.local
end
