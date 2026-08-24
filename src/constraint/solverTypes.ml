(** Solver signatures *)

open Batteries
open ConstrSys

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericEqSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    (** The hash-map that is the first component of [solve xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs]. *)
    val solve : (S.v*S.d) list -> S.v list -> S.d H.t
  end

(** A solver is something that can translate a system into a solution (hash-table).
    These solver can handle [DemandEqConstrSys] *)
module type DemandEqSolver =
  functor (S:DemandEqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    (** The hash-map that is the first component of [solve xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs]. *)
    val solve : (S.v*S.d) list -> S.v list -> S.d H.t
  end

(** A solver is something that can translate a system into a solution (hash-table).
    Incremental solver has data to be marshaled. *)
module type GenericEqIncrSolverBase =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    type marshal

    val copy_marshal: marshal -> marshal
    val relift_marshal: marshal -> marshal

    (** The hash-map that is the first component of [solve xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].
        As a second component the solver returns data structures for incremental serialization. *)
    val solve : (S.v*S.d) list -> S.v list -> marshal option -> S.d H.t * marshal
  end

(** (Incremental) solver argument, indicating which postsolving should be performed by the solver. *)
module type IncrSolverArg =
sig
  val should_prune: bool
  val should_verify: bool
  val should_warn: bool
  val should_save_run: bool
end

(** An incremental solver takes the argument about postsolving. *)
module type GenericEqIncrSolver =
  functor (Arg: IncrSolverArg) ->
    GenericEqIncrSolverBase

module type DemandEqIncrSolverBase =
  functor (S: DemandEqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    type marshal

    val copy_marshal: marshal -> marshal
    val relift_marshal: marshal -> marshal

    (** The hash-map that is the first component of [solve xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].
        As a second component the solver returns data structures for incremental serialization. *)
    val solve : (S.v*S.d) list -> S.v list -> marshal option -> S.d H.t * marshal
  end

module type DemandEqIncrSolver =
  functor (Arg: IncrSolverArg) ->
    DemandEqIncrSolverBase

(** A solver is something that can translate a system into a solution (hash-table)

*)
module type DemandGlobIncrSolver =
  functor (S:DemandGlobConstrSys) ->
  functor (LH:Hashtbl.S with type key=S.LVar.t) ->
  functor (GH:Hashtbl.S with type key=S.GVar.t) ->
  sig
    type marshal

    val copy_marshal: marshal -> marshal
    val relift_marshal: marshal -> marshal

    (** The hash-map that is the first component of [solve xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs].
        As a second component the solver returns data structures for incremental serialization. *)
    val solve : (S.LVar.t*S.D.t) list -> (S.GVar.t*S.G.t) list -> S.LVar.t list -> marshal option -> (S.D.t LH.t * S.G.t GH.t) * marshal
  end

