(** Structures for the querying subsystem. *)

open GoblintCil

module GU = Goblintutil
module ID =
struct
  module I = IntDomain.IntDomTuple
  include Lattice.Lift (I) (Printable.DefaultNames)

  let lift op x = `Lifted (op x)
  let unlift op x = match x with
    | `Lifted x -> op x
    | _ -> failwith "Queries.ID.unlift"
  let unlift_opt op x = match x with
    | `Lifted x -> op x
    | _ -> None
  let unlift_is op x = match x with
    | `Lifted x -> op x
    | _ -> false

  let bot_of = lift I.bot_of
  let top_of = lift I.top_of

  let of_int ik = lift (I.of_int ik)
  let of_bool ik = lift (I.of_bool ik)
  let of_interval ?(suppress_ovwarn=false) ik = lift (I.of_interval ~suppress_ovwarn ik)
  let of_excl_list ik = lift (I.of_excl_list ik)
  let of_congruence ik = lift (I.of_congruence ik)
  let starting ?(suppress_ovwarn=false) ik = lift (I.starting ~suppress_ovwarn ik)
  let ending ?(suppress_ovwarn=false) ik = lift (I.ending ~suppress_ovwarn ik)

  let to_int x = unlift_opt I.to_int x
  let to_bool x = unlift_opt I.to_bool x

  let is_top_of ik = unlift_is (I.is_top_of ik)

  let is_bot_ikind = function
    | `Bot -> false
    | `Lifted x -> I.is_bot x
    | `Top -> false
end
module LS = SetDomain.ToppedSet (Lval.CilLval) (struct let topname = "All" end)
module TS = SetDomain.ToppedSet (CilType.Typ) (struct let topname = "All" end)
module ES = SetDomain.Reverse (SetDomain.ToppedSet (CilType.Exp) (struct let topname = "All" end))

module VI = Lattice.Flat (Basetype.Variables) (struct
  let top_name = "Unknown line"
  let bot_name = "Unreachable line"
end)

type iterprevvar = int -> (MyCFG.node * Obj.t * int) -> MyARG.inline_edge -> unit
type itervar = int -> unit
let compare_itervar _ _ = 0
let compare_iterprevvar _ _ = 0

module SD = Basetype.Strings

module MayBool = BoolDomain.MayBool
module MustBool = BoolDomain.MustBool

module Unit = Lattice.Unit

(* Helper definitions for deriving complex parts of Any.compare below. *)
type maybepublic = {global: CilType.Varinfo.t; write: bool} [@@deriving ord, hash]
type maybepublicwithout = {global: CilType.Varinfo.t; write: bool; without_mutex: PreValueDomain.Addr.t} [@@deriving ord, hash]
type mustbeprotectedby = {mutex: PreValueDomain.Addr.t; global: CilType.Varinfo.t; write: bool} [@@deriving ord, hash]
type mustprotectedvars = {mutex: PreValueDomain.Addr.t; write: bool} [@@deriving ord, hash]
type memory_access = {exp: CilType.Exp.t; var_opt: CilType.Varinfo.t option; kind: AccessKind.t} [@@deriving ord, hash]
type access =
  | Memory of memory_access (** Memory location access (race). *)
  | Point (** Program point and state access (MHP), independent of memory location. *)
[@@deriving ord, hash] (* TODO: fix ppx_deriving_hash on variant with inline record *)
type invariant_context = Invariant.context = {
  path: int option;
  lvals: CilLval.Set.t;
}
[@@deriving ord, hash]


(** GADT for queries with specific result type. *)
type _ t =
  | EqualSet: exp -> ES.t t
  | MayPointTo: exp -> LS.t t
  | ReachableFrom: exp -> LS.t t
  | ReachableUkTypes: exp -> TS.t t
  | Regions: exp -> LS.t t
  | MayEscape: varinfo -> MayBool.t t
  | MayBePublic: maybepublic -> MayBool.t t (* old behavior with write=false *)
  | MayBePublicWithout: maybepublicwithout -> MayBool.t t
  | MustBeProtectedBy: mustbeprotectedby -> MustBool.t t
  | MustLockset: LS.t t
  | MustBeAtomic: MustBool.t t
  | MustBeSingleThreaded: MustBool.t t
  | MustBeUniqueThread: MustBool.t t
  | CurrentThreadId: ThreadIdDomain.ThreadLifted.t t
  | MayBeThreadReturn: MayBool.t t
  | EvalFunvar: exp -> LS.t t
  | EvalInt: exp -> ID.t t
  | EvalStr: exp -> SD.t t
  | EvalLength: exp -> ID.t t (* length of an array or string *)
  | BlobSize: exp -> ID.t t (* size of a dynamically allocated `Blob pointed to by exp *)
  | CondVars: exp -> ES.t t
  | PartAccess: access -> Obj.t t (** Only queried by access and deadlock analysis. [Obj.t] represents [MCPAccess.A.t], needed to break dependency cycle. *)
  | IterPrevVars: iterprevvar -> Unit.t t
  | IterVars: itervar -> Unit.t t
  | HeapVar: VI.t t
  | IsHeapVar: varinfo -> MayBool.t t (* TODO: is may or must? *)
  | IsMultiple: varinfo -> MustBool.t t (* Is no other copy of this local variable reachable via pointers? *)
  | EvalThread: exp -> ConcDomain.ThreadSet.t t
  | CreatedThreads: ConcDomain.ThreadSet.t t
  | MustJoinedThreads: ConcDomain.MustThreadSet.t t
  | MustProtectedVars: mustprotectedvars -> LS.t t
  | Invariant: invariant_context -> Invariant.t t
  | InvariantGlobal: Obj.t -> Invariant.t t (** Argument must be of corresponding [Spec.V.t]. *)
  | WarnGlobal: Obj.t -> Unit.t t (** Argument must be of corresponding [Spec.V.t]. *)
  | IterSysVars: VarQuery.t * Obj.t VarQuery.f -> Unit.t t (** [iter_vars] for [Constraints.FromSpec]. [Obj.t] represents [Spec.V.t]. *)
  | MayAccessed: AccessDomain.EventSet.t t

type 'a result = 'a

(** Container for explicitly polymorphic [ctx.ask] function out of [ctx].
    To be used when passing entire [ctx] around seems inappropriate.
    Use [Analyses.ask_of_ctx] to convert [ctx] to [ask]. *)
(* Must be in a singleton record due to second-order polymorphism.
   See https://ocaml.org/manual/polymorphism.html#s%3Ahigher-rank-poly. *)
type ask = { f: 'a. 'a t -> 'a result }

(* Result cannot implement Lattice.S because the function types are different due to GADT. *)
module Result =
struct
  let lattice (type a) (q: a t): (module Lattice.S with type t = a) =
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> (module ES)
    | CondVars _ -> (module ES)
    | MayPointTo _ -> (module LS)
    | ReachableFrom _ -> (module LS)
    | Regions _ -> (module LS)
    | MustLockset -> (module LS)
    | EvalFunvar _ -> (module LS)
    | ReachableUkTypes _ -> (module TS)
    | MayEscape _ -> (module MayBool)
    | MayBePublic _ -> (module MayBool)
    | MayBePublicWithout _ -> (module MayBool)
    | MayBeThreadReturn -> (module MayBool)
    | IsHeapVar _ -> (module MayBool)
    | MustBeProtectedBy _ -> (module MustBool)
    | MustBeAtomic -> (module MustBool)
    | MustBeSingleThreaded -> (module MustBool)
    | MustBeUniqueThread -> (module MustBool)
    | EvalInt _ -> (module ID)
    | EvalLength _ -> (module ID)
    | BlobSize _ -> (module ID)
    | CurrentThreadId -> (module ThreadIdDomain.ThreadLifted)
    | HeapVar -> (module VI)
    | EvalStr _ -> (module SD)
    | IterPrevVars _ -> (module Unit)
    | IterVars _ -> (module Unit)
    | PartAccess _ -> Obj.magic (module Unit: Lattice.S) (* Never used, MCP handles PartAccess specially. Must still return module (instead of failwith) here, but the module is never used. *)
    | IsMultiple _ -> (module MustBool) (* see https://github.com/goblint/analyzer/pull/310#discussion_r700056687 on why this needs to be MustBool *)
    | EvalThread _ -> (module ConcDomain.ThreadSet)
    | CreatedThreads ->  (module ConcDomain.ThreadSet)
    | MustJoinedThreads -> (module ConcDomain.MustThreadSet)
    | MustProtectedVars _ -> (module LS)
    | Invariant _ -> (module Invariant)
    | InvariantGlobal _ -> (module Invariant)
    | WarnGlobal _ -> (module Unit)
    | IterSysVars _ -> (module Unit)
    | MayAccessed -> (module AccessDomain.EventSet)

  (** Get bottom result for query. *)
  let bot (type a) (q: a t): a result =
    let module Result = (val lattice q) in
    Result.bot ()

  (** Get top result for query. *)
  let top (type a) (q: a t): a result =
    (* let module Result = (val lattice q) in
    Result.top () *)
    (* [lattice] and [top] manually inlined to avoid first-class module
       for every unsupported [query] implementation.
       See benchmarks at: https://github.com/goblint/analyzer/pull/221#issuecomment-842351621. *)
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> ES.top ()
    | CondVars _ -> ES.top ()
    | MayPointTo _ -> LS.top ()
    | ReachableFrom _ -> LS.top ()
    | Regions _ -> LS.top ()
    | MustLockset -> LS.top ()
    | EvalFunvar _ -> LS.top ()
    | ReachableUkTypes _ -> TS.top ()
    | MayEscape _ -> MayBool.top ()
    | MayBePublic _ -> MayBool.top ()
    | MayBePublicWithout _ -> MayBool.top ()
    | MayBeThreadReturn -> MayBool.top ()
    | IsHeapVar _ -> MayBool.top ()
    | MustBeProtectedBy _ -> MustBool.top ()
    | MustBeAtomic -> MustBool.top ()
    | MustBeSingleThreaded -> MustBool.top ()
    | MustBeUniqueThread -> MustBool.top ()
    | EvalInt _ -> ID.top ()
    | EvalLength _ -> ID.top ()
    | BlobSize _ -> ID.top ()
    | CurrentThreadId -> ThreadIdDomain.ThreadLifted.top ()
    | HeapVar -> VI.top ()
    | EvalStr _ -> SD.top ()
    | IterPrevVars _ -> Unit.top ()
    | IterVars _ -> Unit.top ()
    | PartAccess _ -> failwith "Queries.Result.top: PartAccess" (* Never used, MCP handles PartAccess specially. *)
    | IsMultiple _ -> MustBool.top ()
    | EvalThread _ -> ConcDomain.ThreadSet.top ()
    | CreatedThreads -> ConcDomain.ThreadSet.top ()
    | MustJoinedThreads -> ConcDomain.MustThreadSet.top ()
    | MustProtectedVars _ -> LS.top ()
    | Invariant _ -> Invariant.top ()
    | InvariantGlobal _ -> Invariant.top ()
    | WarnGlobal _ -> Unit.top ()
    | IterSysVars _ -> Unit.top ()
    | MayAccessed -> AccessDomain.EventSet.top ()
end

(* The type any_query can't be directly defined in Any as t,
   because it also refers to the t from the outer scope. *)
type any_query = Any: 'a t -> any_query

module Any =
struct
  type t = any_query

  (* deriving ord doesn't work for GADTs (t and any_query) so this must be done manually... *)
  let order = function
    | Any (EqualSet _) -> 0
    | Any (MayPointTo _) -> 1
    | Any (ReachableFrom _) -> 2
    | Any (ReachableUkTypes _) -> 3
    | Any (Regions _) -> 4
    | Any (MayEscape _) -> 5
    | Any (MayBePublic _) -> 7
    | Any (MayBePublicWithout _) -> 8
    | Any (MustBeProtectedBy _) -> 9
    | Any MustLockset -> 10
    | Any MustBeAtomic -> 11
    | Any MustBeSingleThreaded -> 12
    | Any MustBeUniqueThread -> 13
    | Any CurrentThreadId -> 14
    | Any MayBeThreadReturn -> 15
    | Any (EvalFunvar _) -> 16
    | Any (EvalInt _) -> 17
    | Any (EvalStr _) -> 18
    | Any (EvalLength _) -> 19
    | Any (BlobSize _) -> 20
    | Any (CondVars _) -> 22
    | Any (PartAccess _) -> 23
    | Any (IterPrevVars _) -> 24
    | Any (IterVars _) -> 25
    | Any HeapVar -> 29
    | Any (IsHeapVar _) -> 30
    | Any (IsMultiple _) -> 31
    | Any (EvalThread _) -> 32
    | Any CreatedThreads -> 33
    | Any MustJoinedThreads -> 34
    | Any (WarnGlobal _) -> 35
    | Any (Invariant _) -> 36
    | Any (IterSysVars _) -> 37
    | Any (InvariantGlobal _) -> 38
    | Any (MustProtectedVars _) -> 39
    | Any MayAccessed -> 40

  let compare a b =
    let r = Stdlib.compare (order a) (order b) in
    if r <> 0 then
      r
    else
      match a, b with
      | Any (EqualSet e1), Any (EqualSet e2) -> CilType.Exp.compare e1 e2
      | Any (MayPointTo e1), Any (MayPointTo e2) -> CilType.Exp.compare e1 e2
      | Any (ReachableFrom e1), Any (ReachableFrom e2) -> CilType.Exp.compare e1 e2
      | Any (ReachableUkTypes e1), Any (ReachableUkTypes e2) -> CilType.Exp.compare e1 e2
      | Any (Regions e1), Any (Regions e2) -> CilType.Exp.compare e1 e2
      | Any (MayEscape vi1), Any (MayEscape vi2) -> CilType.Varinfo.compare vi1 vi2
      | Any (MayBePublic x1), Any (MayBePublic x2) -> compare_maybepublic x1 x2
      | Any (MayBePublicWithout x1), Any (MayBePublicWithout x2) -> compare_maybepublicwithout x1 x2
      | Any (MustBeProtectedBy x1), Any (MustBeProtectedBy x2) -> compare_mustbeprotectedby x1 x2
      | Any (EvalFunvar e1), Any (EvalFunvar e2) -> CilType.Exp.compare e1 e2
      | Any (EvalInt e1), Any (EvalInt e2) -> CilType.Exp.compare e1 e2
      | Any (EvalStr e1), Any (EvalStr e2) -> CilType.Exp.compare e1 e2
      | Any (EvalLength e1), Any (EvalLength e2) -> CilType.Exp.compare e1 e2
      | Any (BlobSize e1), Any (BlobSize e2) -> CilType.Exp.compare e1 e2
      | Any (CondVars e1), Any (CondVars e2) -> CilType.Exp.compare e1 e2
      | Any (PartAccess p1), Any (PartAccess p2) -> compare_access p1 p2
      | Any (IterPrevVars ip1), Any (IterPrevVars ip2) -> compare_iterprevvar ip1 ip2
      | Any (IterVars i1), Any (IterVars i2) -> compare_itervar i1 i2
      | Any (IsHeapVar v1), Any (IsHeapVar v2) -> CilType.Varinfo.compare v1 v2
      | Any (IsMultiple v1), Any (IsMultiple v2) -> CilType.Varinfo.compare v1 v2
      | Any (EvalThread e1), Any (EvalThread e2) -> CilType.Exp.compare e1 e2
      | Any (WarnGlobal vi1), Any (WarnGlobal vi2) -> compare (Hashtbl.hash vi1) (Hashtbl.hash vi2)
      | Any (Invariant i1), Any (Invariant i2) -> compare_invariant_context i1 i2
      | Any (InvariantGlobal vi1), Any (InvariantGlobal vi2) -> compare (Hashtbl.hash vi1) (Hashtbl.hash vi2)
      | Any (IterSysVars (vq1, vf1)), Any (IterSysVars (vq2, vf2)) -> VarQuery.compare vq1 vq2 (* not comparing fs *)
      | Any (MustProtectedVars m1), Any (MustProtectedVars m2) -> compare_mustprotectedvars m1 m2
      (* only argumentless queries should remain *)
      | _, _ -> Stdlib.compare (order a) (order b)

  let equal x y = compare x y = 0

  let hash_arg = function
    | Any (EqualSet e) -> CilType.Exp.hash e
    | Any (MayPointTo e) -> CilType.Exp.hash e
    | Any (ReachableFrom e) -> CilType.Exp.hash e
    | Any (ReachableUkTypes e) -> CilType.Exp.hash e
    | Any (Regions e) -> CilType.Exp.hash e
    | Any (MayEscape vi) -> CilType.Varinfo.hash vi
    | Any (MayBePublic x) -> hash_maybepublic x
    | Any (MayBePublicWithout x) -> hash_maybepublicwithout x
    | Any (MustBeProtectedBy x) -> hash_mustbeprotectedby x
    | Any (EvalFunvar e) -> CilType.Exp.hash e
    | Any (EvalInt e) -> CilType.Exp.hash e
    | Any (EvalStr e) -> CilType.Exp.hash e
    | Any (EvalLength e) -> CilType.Exp.hash e
    | Any (BlobSize e) -> CilType.Exp.hash e
    | Any (CondVars e) -> CilType.Exp.hash e
    | Any (PartAccess p) -> hash_access p
    | Any (IterPrevVars i) -> 0
    | Any (IterVars i) -> 0
    | Any (IsHeapVar v) -> CilType.Varinfo.hash v
    | Any (IsMultiple v) -> CilType.Varinfo.hash v
    | Any (EvalThread e) -> CilType.Exp.hash e
    | Any (WarnGlobal vi) -> Hashtbl.hash vi
    | Any (Invariant i) -> hash_invariant_context i
    | Any (InvariantGlobal vi) -> Hashtbl.hash vi
    | Any (MustProtectedVars m) -> hash_mustprotectedvars m
    (* only argumentless queries should remain *)
    | _ -> 0

  let hash x = 31 * order x + hash_arg x

  let pretty () = function
    | Any (EqualSet e) -> Pretty.dprintf "EqualSet %a" CilType.Exp.pretty e
    | Any (MayPointTo e) -> Pretty.dprintf "MayPointTo %a" CilType.Exp.pretty e
    | Any (ReachableFrom e) -> Pretty.dprintf "ReachableFrom %a" CilType.Exp.pretty e
    | Any (ReachableUkTypes e) -> Pretty.dprintf "ReachableUkTypes %a" CilType.Exp.pretty e
    | Any (Regions e) -> Pretty.dprintf "Regions %a" CilType.Exp.pretty e
    | Any (MayEscape vi) -> Pretty.dprintf "MayEscape %a" CilType.Varinfo.pretty vi
    | Any (MayBePublic x) -> Pretty.dprintf "MayBePublic _"
    | Any (MayBePublicWithout x) -> Pretty.dprintf "MayBePublicWithout _"
    | Any (MustBeProtectedBy x) -> Pretty.dprintf "MustBeProtectedBy _"
    | Any MustLockset -> Pretty.dprintf "MustLockset"
    | Any MustBeAtomic -> Pretty.dprintf "MustBeAtomic"
    | Any MustBeSingleThreaded -> Pretty.dprintf "MustBeSingleThreaded"
    | Any MustBeUniqueThread -> Pretty.dprintf "MustBeUniqueThread"
    | Any CurrentThreadId -> Pretty.dprintf "CurrentThreadId"
    | Any MayBeThreadReturn -> Pretty.dprintf "MayBeThreadReturn"
    | Any (EvalFunvar e) -> Pretty.dprintf "EvalFunvar %a" CilType.Exp.pretty e
    | Any (EvalInt e) -> Pretty.dprintf "EvalInt %a" CilType.Exp.pretty e
    | Any (EvalStr e) -> Pretty.dprintf "EvalStr %a" CilType.Exp.pretty e
    | Any (EvalLength e) -> Pretty.dprintf "EvalLength %a" CilType.Exp.pretty e
    | Any (BlobSize e) -> Pretty.dprintf "BlobSize %a" CilType.Exp.pretty e
    | Any (CondVars e) -> Pretty.dprintf "CondVars %a" CilType.Exp.pretty e
    | Any (PartAccess p) -> Pretty.dprintf "PartAccess _"
    | Any (IterPrevVars i) -> Pretty.dprintf "IterPrevVars _"
    | Any (IterVars i) -> Pretty.dprintf "IterVars _"
    | Any HeapVar -> Pretty.dprintf "HeapVar"
    | Any (IsHeapVar v) -> Pretty.dprintf "IsHeapVar %a" CilType.Varinfo.pretty v
    | Any (IsMultiple v) -> Pretty.dprintf "IsMultiple %a" CilType.Varinfo.pretty v
    | Any (EvalThread e) -> Pretty.dprintf "EvalThread %a" CilType.Exp.pretty e
    | Any CreatedThreads -> Pretty.dprintf "CreatedThreads"
    | Any MustJoinedThreads -> Pretty.dprintf "MustJoinedThreads"
    | Any (MustProtectedVars m) -> Pretty.dprintf "MustProtectedVars _"
    | Any (Invariant i) -> Pretty.dprintf "Invariant _"
    | Any (WarnGlobal vi) -> Pretty.dprintf "WarnGlobal _"
    | Any (IterSysVars _) -> Pretty.dprintf "IterSysVars _"
    | Any (InvariantGlobal i) -> Pretty.dprintf "InvariantGlobal _"
    | Any MayAccessed -> Pretty.dprintf "MayAccessed"
end


let eval_int_binop (module Bool: Lattice.S with type t = bool) binop (ask: ask) e1 e2: Bool.t =
  let e = Cilfacade.makeBinOp binop e1 e2 in
  let i = ask.f (EvalInt e) in
  if ID.is_bot i || ID.is_bot_ikind i then
    Bool.top () (* base returns bot for non-int results, consider unknown *)
  else
    match ID.to_bool i with
    | Some b -> b
    | None -> Bool.top ()

(** Backwards-compatibility for former [MustBeEqual] query. *)
let must_be_equal = eval_int_binop (module MustBool) Eq

(** Backwards-compatibility for former [MayBeEqual] query. *)
let may_be_equal = eval_int_binop (module MayBool) Eq

(** Backwards-compatibility for former [MayBeLess] query. *)
let may_be_less = eval_int_binop (module MayBool) Lt


module Set = BatSet.Make (Any)
module Hashtbl = BatHashtbl.Make (Any)
