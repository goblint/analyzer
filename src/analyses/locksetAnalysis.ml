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
  val is_held: (D.t, G.t, D.t, V.t) ctx -> ValueDomain.Addr.t -> bool
end

let print_addr_set addr_set = Pretty.fprint stdout ~width:39 (ValueDomain.AD.pretty () addr_set)

let addr_set_of_lval (a: Queries.ask) lval =
  let exp = AddrOf lval in
  let addr_set = a.f (Queries.MayPointTo exp) in
  (* this doesn't work as I am forced to use AddrOf, otherwise MayPointTo returns garbage *)
  (* if Queries.VD.is_mutex_type (ValueDomain.AD.type_of addr_set) then None
  else Some addr_set *)
  addr_set

let locks_of_lvals ctx lvals =
  let ask = Analyses.ask_of_ctx ctx in
  let collect_addr_sets locks lval = 
    let addr_set = addr_set_of_lval ask lval in
    ValueDomain.AD.union locks addr_set 
  in
  let addr_set = List.fold_left collect_addr_sets (ValueDomain.AD.empty ()) lvals in
  ValueDomain.AD.elements addr_set
  
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
      let locks = locks_of_lvals ctx lvals in
      let is_held lock = Arg.is_held ctx lock in
      let locks = List.filter is_held locks in
      let remove_lock ctx lock = {ctx with local = Arg.remove ctx lock} in
      let ctx = List.fold_left remove_lock ctx locks in
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
      let locks = locks_of_lvals ctx lvals in
      let is_held lock = Arg.is_held ctx lock in
      let locks = List.filter is_held locks in
      let remove_lock ctx lock = {ctx with local = Arg.remove ctx lock} in
      let ctx = List.fold_left remove_lock ctx locks in
      ctx.local
    | _ ->
      ctx.local
end
