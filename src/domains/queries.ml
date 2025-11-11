(** Queries and their result lattices. *)

open GoblintCil

module VDQ = ValueDomainQueries

module ID = VDQ.ID

module LS = VDQ.LS
module TS = SetDomain.ToppedSet (CilType.Typ) (struct let topname = "All" end)
module ES = SetDomain.Reverse (SetDomain.ToppedSet (CilType.Exp) (struct let topname = "All" end))
module VS = SetDomain.ToppedSet (CilType.Varinfo) (struct let topname = "All" end)
module NS = SetDomain.ToppedSet (Node) (struct let topname = "All" end)

module NFL = WrapperFunctionAnalysis0.NodeFlatLattice
module TC = WrapperFunctionAnalysis0.ThreadCreateUniqueCount

module ThreadNodeLattice = Lattice.Prod (NFL) (TC)
module ML = LibraryDesc.MathLifted

module VI = Lattice.Flat (Basetype.Variables)

module VarQuery = Goblint_constraint.VarQuery

type iterprevvar = int -> (MyCFG.node * Obj.t * int) -> MyARG.inline_edge -> unit
type itervar = int -> unit
let compare_itervar _ _ = 0
let compare_iterprevvar _ _ = 0

module FlatYojson = Lattice.Flat (Printable.Yojson)

module SD: Lattice.S with type t = [`Bot | `Lifted of string | `Top] =
  Lattice.Flat (Basetype.RawStrings)
module VD = ValueDomain.Compound
module AD = ValueDomain.AD

module MayBool = BoolDomain.MayBool
module MustBool = BoolDomain.MustBool

module Unit = Lattice.Unit

module ProtectionKind =
struct
  type t = ReadWrite | Write [@@deriving ord, hash]
end

(** Different notions of protection for a global variables g by a mutex m
    m protects g strongly if:
    - whenever g is accessed after the program went multi-threaded for the first time, m is held

    m protects g weakly if:
    - whenever g is accessed and there are any threads other than the main thread that are created but not joined yet, m is held
*)
module Protection = struct
  type t = Strong | Weak [@@deriving ord, hash]
end

module AllocationLocation = struct
  type t = Stack | Heap [@@deriving ord, hash]
end

(* Helper definitions for deriving complex parts of Any.compare below. *)
type maybepublic = {global: CilType.Varinfo.t; kind: ProtectionKind.t; protection: Protection.t} [@@deriving ord, hash]
type maybepublicwithout = {global: CilType.Varinfo.t; kind: ProtectionKind.t; without_mutex: LockDomain.MustLock.t; protection: Protection.t} [@@deriving ord, hash]
type mustbeprotectedby = {mutex: LockDomain.MustLock.t; global: CilType.Varinfo.t; kind: ProtectionKind.t; protection: Protection.t} [@@deriving ord, hash]
type mustprotectedvars = {mutex: LockDomain.MustLock.t; kind: ProtectionKind.t} [@@deriving ord, hash]
type mustprotectinglocks = {global: CilType.Varinfo.t; kind: ProtectionKind.t} [@@deriving ord, hash]
type access =
  | Memory of {exp: CilType.Exp.t; var_opt: CilType.Varinfo.t option; kind: AccessKind.t} (** Memory location access (race). *)
  | Point (** Program point and state access (MHP), independent of memory location. *)
[@@deriving ord, hash]
type invariant_context = Invariant.context = {
  path: int option;
  lvals: Lval.Set.t;
}
[@@deriving ord, hash]

module YS = SetDomain.ToppedSet (YamlWitnessType.Entry) (struct let topname = "Top" end)


(** GADT for queries with specific result type. *)
type _ t =
  | EqualSet: exp -> ES.t t
  | MayPointTo: exp -> AD.t t
  | ReachableFrom: exp -> AD.t t
  | ReachableUkTypes: exp -> TS.t t
  | Regions: exp -> LS.t t
  | MayEscape: varinfo -> MayBool.t t
  | MayBePublic: maybepublic -> MayBool.t t (* old behavior with write=false *)
  | MayBePublicWithout: maybepublicwithout -> MayBool.t t
  | MustBeProtectedBy: mustbeprotectedby -> MustBool.t t
  | MustLockset: LockDomain.MustLockset.t t
  | MustBeAtomic: MustBool.t t
  | MustBeSingleThreaded: {since_start: bool} -> MustBool.t t
  | MustBeUniqueThread: MustBool.t t
  | CurrentThreadId: ThreadIdDomain.ThreadLifted.t t
  | ThreadCreateIndexedNode: ThreadNodeLattice.t t
  | MayBeThreadReturn: MayBool.t t
  | EvalFunvar: exp -> AD.t t
  | EvalInt: exp -> ID.t t
  | EvalStr: exp -> SD.t t
  | EvalLength: exp -> ID.t t (* length of an array or string *)
  | EvalValue: exp -> VD.t t
  | BlobSize: {exp: Cil.exp; base_address: bool} -> ID.t t
  (* Size of a dynamically allocated `Blob pointed to by exp. *)
  (* If the record's second field is set to true, then address offsets are discarded and the size of the `Blob is asked for the base address. *)
  | CondVars: exp -> ES.t t
  | PartAccess: access -> Obj.t t (** Only queried by access and deadlock analysis. [Obj.t] represents [MCPAccess.A.t], needed to break dependency cycle. *)
  | IterPrevVars: iterprevvar -> Unit.t t
  | IterVars: itervar -> Unit.t t
  | PathQuery: int * 'a t -> 'a t (** Query only one path under witness lifter. *)
  | DYojson: FlatYojson.t t (** Get local state Yojson of one path under [PathQuery]. *)
  | AllocVar: AllocationLocation.t -> VI.t t
  (* Create a variable representing a dynamic allocation-site *)
  (* If on_stack is [true], then the dynamic allocation is on the stack (i.e., alloca() or a similar function was called). Otherwise, allocation is on the heap *)
  | IsAllocVar: varinfo -> MayBool.t t (* [true] if variable represents dynamically allocated memory *)
  | IsHeapVar: varinfo -> MayBool.t t (* TODO: is may or must? *)
  | IsMultiple: varinfo -> MustBool.t t
  (* For locals: Is another copy of this local variable reachable via pointers? *)
  (* For dynamically allocated memory: Does this abstract variable corrrespond to a unique heap location? *)
  (* For globals: Is it declared as thread-local? (https://github.com/goblint/analyzer/issues/1072) *)
  | MutexType: Mval.Unit.t -> MutexAttrDomain.t t
  | EvalThread: exp -> ConcDomain.ThreadSet.t t
  | EvalMutexAttr: exp -> MutexAttrDomain.t t
  | EvalJumpBuf: exp -> JmpBufDomain.JmpBufSet.t t
  | ActiveJumpBuf: JmpBufDomain.ActiveLongjmps.t t
  | ValidLongJmp: JmpBufDomain.JmpBufSet.t t
  | CreatedThreads: ConcDomain.ThreadSet.t t
  | MustJoinedThreads: ConcDomain.MustThreadSet.t t
  | ThreadsJoinedCleanly: MustBool.t t
  | MustProtectedVars: mustprotectedvars -> VS.t t
  | MustProtectingLocks: mustprotectinglocks -> LockDomain.MustLockset.t t
  | Invariant: invariant_context -> Invariant.t t
  | InvariantGlobal: Obj.t -> Invariant.t t (** Argument must be of corresponding [Spec.V.t]. *)
  | WarnGlobal: Obj.t -> Unit.t t (** Argument must be of corresponding [Spec.V.t]. *)
  | IterSysVars: VarQuery.t * Obj.t VarQuery.f -> Unit.t t (** [iter_vars] for [Constraints.FromSpec]. [Obj.t] represents [Spec.V.t]. *)
  | MayAccessed: AccessDomain.EventSet.t t
  | MayBeTainted: AD.t t
  | MayBeModifiedSinceSetjmp: JmpBufDomain.BufferEntry.t -> VS.t t
  | MustTermLoop: stmt -> MustBool.t t
  | MustTermAllLoops: MustBool.t t
  | IsEverMultiThreaded: MayBool.t t
  | TmpSpecial: Mval.Exp.t -> ML.t t
  | MaySignedOverflow: exp -> MayBool.t t
  | GasExhausted: CilType.Fundec.t ->  MustBool.t t
  | YamlEntryGlobal: Obj.t * YamlWitnessType.Task.t -> YS.t t (** YAML witness entries for a global unknown ([Obj.t] represents [Spec.V.t]) and YAML witness task. *)
  | GhostVarAvailable: WitnessGhostVar.t -> MayBool.t t
  | InvariantGlobalNodes: NS.t t (** Nodes where YAML witness flow-insensitive invariants should be emitted as location invariants (if [witness.invariant.flow_insensitive-as] is configured to do so). *) (* [Spec.V.t] argument (as [Obj.t]) could be added, if this should be different for different flow-insensitive invariants. *)
  | DescendantThreads: ThreadIdDomain.Thread.t -> ConcDomain.ThreadSet.t t (* TODO consider returning descendants of ego threads only? *)

type 'a result = 'a

(** Container for explicitly polymorphic [man.ask] function out of [man].
    To be used when passing entire [man] around seems inappropriate.
    Use [Analyses.ask_of_man] to convert [man] to [ask]. *)
(* Must be in a singleton record due to second-order polymorphism.
   See https://ocaml.org/manual/polymorphism.html#s%3Ahigher-rank-poly. *)
type ask = { f: 'a. 'a t -> 'a result } [@@unboxed]

(* Result cannot implement Lattice.S because the function types are different due to GADT. *)
module Result =
struct
  let rec lattice: type a. a t -> (module Lattice.S with type t = a) = fun q ->
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> (module ES)
    | CondVars _ -> (module ES)
    | MayPointTo _ -> (module AD)
    | ReachableFrom _ -> (module AD)
    | Regions _ -> (module LS)
    | MustLockset -> (module LockDomain.MustLockset)
    | EvalFunvar _ -> (module AD)
    | ReachableUkTypes _ -> (module TS)
    | MayEscape _ -> (module MayBool)
    | MayBePublic _ -> (module MayBool)
    | MayBePublicWithout _ -> (module MayBool)
    | MayBeThreadReturn -> (module MayBool)
    | IsHeapVar _ -> (module MayBool)
    | IsAllocVar _ -> (module MayBool)
    | MustBeProtectedBy _ -> (module MustBool)
    | MustBeAtomic -> (module MustBool)
    | MustBeSingleThreaded _ -> (module MustBool)
    | MustBeUniqueThread -> (module MustBool)
    | EvalInt _ -> (module ID)
    | EvalLength _ -> (module ID)
    | EvalMutexAttr _ -> (module MutexAttrDomain)
    | EvalValue _ -> (module VD)
    | BlobSize _ -> (module ID)
    | CurrentThreadId -> (module ThreadIdDomain.ThreadLifted)
    | ThreadCreateIndexedNode -> (module ThreadNodeLattice)
    | AllocVar _ -> (module VI)
    | EvalStr _ -> (module SD)
    | IterPrevVars _ -> (module Unit)
    | IterVars _ -> (module Unit)
    | PathQuery (_, q) -> lattice q
    | DYojson -> (module FlatYojson)
    | PartAccess _ -> Obj.magic (module Unit: Lattice.S) (* Never used, MCP handles PartAccess specially. Must still return module (instead of failwith) here, but the module is never used. *)
    | IsMultiple _ -> (module MustBool) (* see https://github.com/goblint/analyzer/pull/310#discussion_r700056687 on why this needs to be MustBool *)
    | MutexType _ -> (module MutexAttrDomain)
    | EvalThread _ -> (module ConcDomain.ThreadSet)
    | EvalJumpBuf _ -> (module JmpBufDomain.JmpBufSet)
    | ActiveJumpBuf -> (module JmpBufDomain.ActiveLongjmps)
    | ValidLongJmp ->  (module JmpBufDomain.JmpBufSet)
    | CreatedThreads ->  (module ConcDomain.ThreadSet)
    | MustJoinedThreads -> (module ConcDomain.MustThreadSet)
    | ThreadsJoinedCleanly -> (module MustBool)
    | MustProtectedVars _ -> (module VS)
    | MustProtectingLocks _ -> (module LockDomain.MustLockset)
    | Invariant _ -> (module Invariant)
    | InvariantGlobal _ -> (module Invariant)
    | WarnGlobal _ -> (module Unit)
    | IterSysVars _ -> (module Unit)
    | MayAccessed -> (module AccessDomain.EventSet)
    | MayBeTainted -> (module AD)
    | MayBeModifiedSinceSetjmp _ -> (module VS)
    | MustTermLoop _ -> (module MustBool)
    | MustTermAllLoops -> (module MustBool)
    | IsEverMultiThreaded -> (module MayBool)
    | TmpSpecial _ -> (module ML)
    | MaySignedOverflow  _ -> (module MayBool)
    | GasExhausted _ -> (module MustBool)
    | YamlEntryGlobal _ -> (module YS)
    | GhostVarAvailable _ -> (module MayBool)
    | InvariantGlobalNodes -> (module NS)
    | DescendantThreads _ -> (module ConcDomain.ThreadSet)

  (** Get bottom result for query. *)
  let bot (type a) (q: a t): a result =
    let module Result = (val lattice q) in
    Result.bot ()

  (** Get top result for query. *)
  let rec top: type a. a t -> a result = fun q ->
    (* let module Result = (val lattice q) in
       Result.top () *)
    (* [lattice] and [top] manually inlined to avoid first-class module
       for every unsupported [query] implementation.
       See benchmarks at: https://github.com/goblint/analyzer/pull/221#issuecomment-842351621. *)
    match q with
    (* Cannot group these GADTs... *)
    | EqualSet _ -> ES.top ()
    | CondVars _ -> ES.top ()
    | MayPointTo _ -> AD.top ()
    | ReachableFrom _ -> AD.top ()
    | Regions _ -> LS.top ()
    | MustLockset -> LockDomain.MustLockset.top ()
    | EvalFunvar _ -> AD.top ()
    | ReachableUkTypes _ -> TS.top ()
    | MayEscape _ -> MayBool.top ()
    | MayBePublic _ -> MayBool.top ()
    | MayBePublicWithout _ -> MayBool.top ()
    | MayBeThreadReturn -> MayBool.top ()
    | IsHeapVar _ -> MayBool.top ()
    | IsAllocVar _ -> MayBool.top ()
    | MutexType _ -> MutexAttrDomain.top ()
    | MustBeProtectedBy _ -> MustBool.top ()
    | MustBeAtomic -> MustBool.top ()
    | MustBeSingleThreaded _ -> MustBool.top ()
    | MustBeUniqueThread -> MustBool.top ()
    | EvalInt _ -> ID.top ()
    | EvalLength _ -> ID.top ()
    | EvalMutexAttr _ -> MutexAttrDomain.top ()
    | EvalValue _ -> VD.top ()
    | BlobSize _ -> ID.top ()
    | CurrentThreadId -> ThreadIdDomain.ThreadLifted.top ()
    | ThreadCreateIndexedNode -> ThreadNodeLattice.top ()
    | AllocVar _ -> VI.top ()
    | EvalStr _ -> SD.top ()
    | IterPrevVars _ -> Unit.top ()
    | IterVars _ -> Unit.top ()
    | PathQuery (_, q) -> top q
    | DYojson -> FlatYojson.top ()
    | PartAccess _ -> failwith "Queries.Result.top: PartAccess" (* Never used, MCP handles PartAccess specially. *)
    | IsMultiple _ -> MustBool.top ()
    | EvalThread _ -> ConcDomain.ThreadSet.top ()
    | EvalJumpBuf _ -> JmpBufDomain.JmpBufSet.top ()
    | ActiveJumpBuf -> JmpBufDomain.ActiveLongjmps.top ()
    | ValidLongJmp -> JmpBufDomain.JmpBufSet.top ()
    | CreatedThreads -> ConcDomain.ThreadSet.top ()
    | MustJoinedThreads -> ConcDomain.MustThreadSet.top ()
    | ThreadsJoinedCleanly -> MustBool.top ()
    | MustProtectedVars _ -> VS.top ()
    | MustProtectingLocks _ -> LockDomain.MustLockset.top ()
    | Invariant _ -> Invariant.top ()
    | InvariantGlobal _ -> Invariant.top ()
    | WarnGlobal _ -> Unit.top ()
    | IterSysVars _ -> Unit.top ()
    | MayAccessed -> AccessDomain.EventSet.top ()
    | MayBeTainted -> AD.top ()
    | MayBeModifiedSinceSetjmp _ -> VS.top ()
    | MustTermLoop _ -> MustBool.top ()
    | MustTermAllLoops -> MustBool.top ()
    | IsEverMultiThreaded -> MayBool.top ()
    | TmpSpecial _ -> ML.top ()
    | MaySignedOverflow _ -> MayBool.top ()
    | GasExhausted _ -> MustBool.top ()
    | YamlEntryGlobal _ -> YS.top ()
    | GhostVarAvailable _ -> MayBool.top ()
    | InvariantGlobalNodes -> NS.top ()
    | DescendantThreads _ -> ConcDomain.ThreadSet.top ()
end

(* The type any_query can't be directly defined in Any as t,
   because it also refers to the t from the outer scope. *)
type any_query = Any: 'a t -> any_query [@@unboxed]

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
    | Any (MustBeSingleThreaded _)-> 12
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
    | Any (AllocVar _) -> 29
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
    | Any MayBeTainted -> 41
    | Any (PathQuery _) -> 42
    | Any DYojson -> 43
    | Any (EvalValue _) -> 44
    | Any (EvalJumpBuf _) -> 45
    | Any ActiveJumpBuf -> 46
    | Any ValidLongJmp -> 47
    | Any (MayBeModifiedSinceSetjmp _) -> 48
    | Any (MutexType _) -> 49
    | Any (EvalMutexAttr _ ) -> 50
    | Any ThreadCreateIndexedNode -> 51
    | Any ThreadsJoinedCleanly -> 52
    | Any (MustTermLoop _) -> 53
    | Any MustTermAllLoops -> 54
    | Any IsEverMultiThreaded -> 55
    | Any (TmpSpecial _) -> 56
    | Any (IsAllocVar _) -> 57
    | Any (MaySignedOverflow _) -> 58
    | Any (GasExhausted _) -> 59
    | Any (YamlEntryGlobal _) -> 60
    | Any (MustProtectingLocks _) -> 61
    | Any (GhostVarAvailable _) -> 62
    | Any InvariantGlobalNodes -> 63
    | Any (DescendantThreads _) -> 64

  let rec compare a b =
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
      | Any (EvalMutexAttr e1), Any (EvalMutexAttr e2) -> CilType.Exp.compare e1 e2
      | Any (EvalValue e1), Any (EvalValue e2) -> CilType.Exp.compare e1 e2
      | Any (BlobSize {exp = e1; base_address = b1}), Any (BlobSize {exp = e2; base_address = b2}) ->
        let r = CilType.Exp.compare e1 e2 in
        if r <> 0 then
          r
        else
          Stdlib.compare b1 b2
      | Any (CondVars e1), Any (CondVars e2) -> CilType.Exp.compare e1 e2
      | Any (PartAccess p1), Any (PartAccess p2) -> compare_access p1 p2
      | Any (IterPrevVars ip1), Any (IterPrevVars ip2) -> compare_iterprevvar ip1 ip2
      | Any (AllocVar location), Any (AllocVar location2) -> AllocationLocation.compare location location2
      | Any (IterVars i1), Any (IterVars i2) -> compare_itervar i1 i2
      | Any (PathQuery (i1, q1)), Any (PathQuery (i2, q2)) ->
        let r = Stdlib.compare i1 i2 in
        if r <> 0 then
          r
        else
          compare (Any q1) (Any q2)
      | Any (IsHeapVar v1), Any (IsHeapVar v2) -> CilType.Varinfo.compare v1 v2
      | Any (IsAllocVar v1), Any (IsAllocVar v2) -> CilType.Varinfo.compare v1 v2
      | Any (IsMultiple v1), Any (IsMultiple v2) -> CilType.Varinfo.compare v1 v2
      | Any (MustTermLoop s1), Any (MustTermLoop s2) -> CilType.Stmt.compare s1 s2
      | Any (EvalThread e1), Any (EvalThread e2) -> CilType.Exp.compare e1 e2
      | Any (EvalJumpBuf e1), Any (EvalJumpBuf e2) -> CilType.Exp.compare e1 e2
      | Any (WarnGlobal vi1), Any (WarnGlobal vi2) -> Stdlib.compare (Hashtbl.hash vi1) (Hashtbl.hash vi2)
      | Any (Invariant i1), Any (Invariant i2) -> compare_invariant_context i1 i2
      | Any (InvariantGlobal vi1), Any (InvariantGlobal vi2) -> Stdlib.compare (Hashtbl.hash vi1) (Hashtbl.hash vi2)
      | Any (YamlEntryGlobal (vi1, task1)), Any (YamlEntryGlobal (vi2, task2)) -> Stdlib.compare (Hashtbl.hash vi1) (Hashtbl.hash vi2) (* TODO: compare task *)
      | Any (IterSysVars (vq1, vf1)), Any (IterSysVars (vq2, vf2)) -> VarQuery.compare vq1 vq2 (* not comparing fs *)
      | Any (MutexType m1), Any (MutexType m2) -> Mval.Unit.compare m1 m2
      | Any (MustProtectedVars m1), Any (MustProtectedVars m2) -> compare_mustprotectedvars m1 m2
      | Any (MustProtectingLocks g1), Any (MustProtectingLocks g2) -> compare_mustprotectinglocks g1 g2
      | Any (MayBeModifiedSinceSetjmp e1), Any (MayBeModifiedSinceSetjmp e2) -> JmpBufDomain.BufferEntry.compare e1 e2
      | Any (MustBeSingleThreaded {since_start=s1;}),  Any (MustBeSingleThreaded {since_start=s2;}) -> Stdlib.compare s1 s2
      | Any (TmpSpecial lv1), Any (TmpSpecial lv2) -> Mval.Exp.compare lv1 lv2
      | Any (MaySignedOverflow e1), Any (MaySignedOverflow e2) -> CilType.Exp.compare e1 e2
      | Any (GasExhausted f1), Any (GasExhausted f2) -> CilType.Fundec.compare f1 f2
      | Any (GhostVarAvailable v1), Any (GhostVarAvailable v2) -> WitnessGhostVar.compare v1 v2
      (* only argumentless queries should remain *)
      | _, _ -> Stdlib.compare (order a) (order b)

  let equal x y = compare x y = 0

  let rec hash_arg = function
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
    | Any (EvalMutexAttr e) -> CilType.Exp.hash e
    | Any (EvalValue e) -> CilType.Exp.hash e
    | Any (BlobSize {exp = e; base_address = b}) -> CilType.Exp.hash e + Hashtbl.hash b
    | Any (CondVars e) -> CilType.Exp.hash e
    | Any (PartAccess p) -> hash_access p
    | Any (IterPrevVars i) -> 0
    | Any (IterVars i) -> 0
    | Any (AllocVar location) -> AllocationLocation.hash location
    | Any (PathQuery (i, q)) -> 31 * i + hash (Any q)
    | Any (IsHeapVar v) -> CilType.Varinfo.hash v
    | Any (MustTermLoop s) -> CilType.Stmt.hash s
    | Any (IsAllocVar v) -> CilType.Varinfo.hash v
    | Any (IsMultiple v) -> CilType.Varinfo.hash v
    | Any (EvalThread e) -> CilType.Exp.hash e
    | Any (EvalJumpBuf e) -> CilType.Exp.hash e
    | Any (WarnGlobal vi) -> Hashtbl.hash vi
    | Any (Invariant i) -> hash_invariant_context i
    | Any (MutexType m) -> Mval.Unit.hash m
    | Any (InvariantGlobal vi) -> Hashtbl.hash vi
    | Any (YamlEntryGlobal (vi, task)) -> Hashtbl.hash vi (* TODO: hash task *)
    | Any (MustProtectedVars m) -> hash_mustprotectedvars m
    | Any (MustProtectingLocks g) -> hash_mustprotectinglocks g
    | Any (MayBeModifiedSinceSetjmp e) -> JmpBufDomain.BufferEntry.hash e
    | Any (MustBeSingleThreaded {since_start}) -> Hashtbl.hash since_start
    | Any (TmpSpecial lv) -> Mval.Exp.hash lv
    | Any (MaySignedOverflow e) -> CilType.Exp.hash e
    | Any (GasExhausted f) -> CilType.Fundec.hash f
    | Any (GhostVarAvailable v) -> WitnessGhostVar.hash v
    (* IterSysVars:                                                                    *)
    (*   - argument is a function and functions cannot be compared in any meaningful way. *)
    (*   - doesn't matter because IterSysVars is always queried from outside of the analysis, so MCP's query caching is not done for it. *)
    (* only argumentless queries should remain *)
    | _ -> 0

  and hash x = 31 * order x + hash_arg x

  let rec pretty () = function
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
    | Any (MustBeSingleThreaded {since_start}) -> Pretty.dprintf "MustBeSingleThreaded since_start=%b" since_start
    | Any MustBeUniqueThread -> Pretty.dprintf "MustBeUniqueThread"
    | Any CurrentThreadId -> Pretty.dprintf "CurrentThreadId"
    | Any ThreadCreateIndexedNode -> Pretty.dprintf "ThreadCreateIndexedNode"
    | Any MayBeThreadReturn -> Pretty.dprintf "MayBeThreadReturn"
    | Any (EvalFunvar e) -> Pretty.dprintf "EvalFunvar %a" CilType.Exp.pretty e
    | Any (EvalInt e) -> Pretty.dprintf "EvalInt %a" CilType.Exp.pretty e
    | Any (EvalStr e) -> Pretty.dprintf "EvalStr %a" CilType.Exp.pretty e
    | Any (EvalLength e) -> Pretty.dprintf "EvalLength %a" CilType.Exp.pretty e
    | Any (EvalValue e) -> Pretty.dprintf "EvalValue %a" CilType.Exp.pretty e
    | Any (BlobSize {exp = e; base_address = b}) -> Pretty.dprintf "BlobSize %a (base_address: %b)" CilType.Exp.pretty e b
    | Any (CondVars e) -> Pretty.dprintf "CondVars %a" CilType.Exp.pretty e
    | Any (PartAccess p) -> Pretty.dprintf "PartAccess _"
    | Any (IterPrevVars i) -> Pretty.dprintf "IterPrevVars _"
    | Any (IterVars i) -> Pretty.dprintf "IterVars _"
    | Any (PathQuery (i, q)) -> Pretty.dprintf "PathQuery (%d, %a)" i pretty (Any q)
    | Any (AllocVar location) -> Pretty.dprintf "AllocVar _"
    | Any (IsHeapVar v) -> Pretty.dprintf "IsHeapVar %a" CilType.Varinfo.pretty v
    | Any (IsAllocVar v) -> Pretty.dprintf "IsAllocVar %a" CilType.Varinfo.pretty v
    | Any (IsMultiple v) -> Pretty.dprintf "IsMultiple %a" CilType.Varinfo.pretty v
    | Any (EvalThread e) -> Pretty.dprintf "EvalThread %a" CilType.Exp.pretty e
    | Any (EvalJumpBuf e) -> Pretty.dprintf "EvalJumpBuf %a" CilType.Exp.pretty e
    | Any ActiveJumpBuf ->  Pretty.dprintf "ActiveJumpBuf"
    | Any ValidLongJmp -> Pretty.dprintf "ValidLongJmp"
    | Any CreatedThreads -> Pretty.dprintf "CreatedThreads"
    | Any MustJoinedThreads -> Pretty.dprintf "MustJoinedThreads"
    | Any ThreadsJoinedCleanly -> Pretty.dprintf "ThreadsJoinedCleanly"
    | Any (MustProtectedVars m) -> Pretty.dprintf "MustProtectedVars _"
    | Any (MustProtectingLocks g) -> Pretty.dprintf "MustProtectingLocks _"
    | Any (Invariant i) -> Pretty.dprintf "Invariant _"
    | Any (WarnGlobal vi) -> Pretty.dprintf "WarnGlobal _"
    | Any (IterSysVars _) -> Pretty.dprintf "IterSysVars _"
    | Any (InvariantGlobal i) -> Pretty.dprintf "InvariantGlobal _"
    | Any (YamlEntryGlobal (i, task)) -> Pretty.dprintf "YamlEntryGlobal _"
    | Any (MutexType (v,o)) ->  Pretty.dprintf "MutexType _"
    | Any (EvalMutexAttr a) ->  Pretty.dprintf "EvalMutexAttr _"
    | Any MayAccessed -> Pretty.dprintf "MayAccessed"
    | Any MayBeTainted -> Pretty.dprintf "MayBeTainted"
    | Any DYojson -> Pretty.dprintf "DYojson"
    | Any MayBeModifiedSinceSetjmp buf -> Pretty.dprintf "MayBeModifiedSinceSetjmp %a" JmpBufDomain.BufferEntry.pretty buf
    | Any (MustTermLoop s) -> Pretty.dprintf "MustTermLoop %a" CilType.Stmt.pretty s
    | Any MustTermAllLoops -> Pretty.dprintf "MustTermAllLoops"
    | Any IsEverMultiThreaded -> Pretty.dprintf "IsEverMultiThreaded"
    | Any (TmpSpecial lv) -> Pretty.dprintf "TmpSpecial %a" Mval.Exp.pretty lv
    | Any (MaySignedOverflow e) -> Pretty.dprintf "MaySignedOverflow %a" CilType.Exp.pretty e
    | Any (GasExhausted f) -> Pretty.dprintf "GasExhausted %a" CilType.Fundec.pretty f
    | Any (GhostVarAvailable v) -> Pretty.dprintf "GhostVarAvailable %a" WitnessGhostVar.pretty v
    | Any InvariantGlobalNodes -> Pretty.dprintf "InvariantGlobalNodes"
    | Any (DescendantThreads t) -> Pretty.dprintf "DescendantThreads"
end

let to_value_domain_ask (ask: ask) =
  let eval_int e = ask.f (EvalInt e) in
  let may_point_to e = ask.f (MayPointTo e) in
  let is_multiple v = ask.f (IsMultiple v) in
  { VDQ.eval_int; may_point_to; is_multiple }

let eval_int_binop (module Bool: Lattice.S with type t = bool) binop (ask: ask) e1 e2: Bool.t =
  let eval_int e = ask.f (EvalInt e) in
  VDQ.eval_int_binop (module Bool) binop eval_int e1 e2

(** Backwards-compatibility for former [MustBeEqual] query. *)
let must_be_equal = eval_int_binop (module MustBool) Eq

(** Backwards-compatibility for former [MayBeEqual] query. *)
let may_be_equal = eval_int_binop (module MayBool) Eq

(** Backwards-compatibility for former [MayBeLess] query. *)
let may_be_less = eval_int_binop (module MayBool) Lt

let eval_bool (ask: ask) e: BoolDomain.FlatBool.t =
  let e' = CastE (TInt (IBool, []), e) in
  match ask.f (EvalInt e') with
  | v when ID.is_bot v || ID.is_bot_ikind v -> `Bot
  | v -> BatOption.map_default (fun b -> `Lifted b) `Top (ID.to_bool v)


module Set = BatSet.Make (Any)
module Hashtbl = BatHashtbl.Make (Any)
