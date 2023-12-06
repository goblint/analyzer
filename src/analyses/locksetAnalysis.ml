(** Basic lockset analyses. *)

open Analyses
open GoblintCil


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
  module G: Lattice.S
  module V: SpecSysVar

  val add: (D.t, G.t, D.t, V.t) ctx -> LockDomain.Lockset.Lock.t -> D.t
  val remove: (D.t, G.t, D.t, V.t) ctx -> ValueDomain.Addr.t -> D.t
  val warn_remove_unknown: bool ref
end

let lock_of_lval (lhost, offset) =
  match lhost with
  | Var varinfo ->
    let lock = ValueDomain.Addr.of_var varinfo in
    let offset = Offset.Exp.of_cil offset in
    let offset = ValueDomain.Offs.of_exp offset in
    let lock = ValueDomain.Addr.add_offset lock offset in
    Some lock
  | Mem exp ->
    match exp with
    | Const (CInt (_, _, Some (str))) ->
      let lock = ValueDomain.Addr.of_string str in
      Some lock
    | _ -> None

module MakeMay (Arg: MayArg) =
struct
  include Make (Arg.D)
  let name () = "mayLockset"

  module G = Arg.G
  module V = Arg.V

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
    | Events.Invalidate {lvals} ->
      Arg.warn_remove_unknown := false;
      let handle_lval ctx lval = 
        match lock_of_lval lval with
        | Some lock -> {ctx with local = Arg.remove ctx lock}
        | None -> ctx
      in
      let ctx = List.fold_left handle_lval ctx lvals in
      Arg.warn_remove_unknown := true;
      ctx.local
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

  module G = Arg.G
  module V = Arg.V

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
    | Events.Invalidate {lvals} ->
      Arg.warn_remove_unknown := false;
      let handle_lval ctx lval = 
        match lock_of_lval lval with
        | Some lock -> {ctx with local = Arg.remove ctx lock}
        | None -> ctx
      in
      let ctx = List.fold_left handle_lval ctx lvals in
      Arg.warn_remove_unknown := true;
      ctx.local
    | _ ->
      ctx.local
end
