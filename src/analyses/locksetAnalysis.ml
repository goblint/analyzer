(** Basic lockset analyses. *)

open Prelude.Ana
open Analyses
open ValueDomain


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
  val remove: (D.t, _, D.t, _) ctx -> Addr.t -> D.t
end

module MakeMay (Arg: MayArg) =
struct
  include Make (Arg.D)
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
    | Events.Lock ((Addr (v, _) as a, _) as l) when not (Addr.equal a (Addr.from_var_offset (dummyFunDec.svar, `NoOffset))) && not (ctx.ask (IsMultiple v)) ->
      Arg.add ctx l
    | Events.Unlock l when Addr.equal l (Addr.from_var_offset (dummyFunDec.svar, `NoOffset)) ->
      Arg.remove_all ctx
    | Events.Unlock l ->
      Arg.remove ctx l
    | _ ->
      ctx.local
end
