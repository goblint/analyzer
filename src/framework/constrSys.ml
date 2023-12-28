(** {{!MonSystem} constraint system} signatures. *)

open Batteries

module type SysVar =
sig
  type t
  val is_write_only: t -> bool
end

module type VarType =
sig
  include Hashtbl.HashedType
  include SysVar with type t := t
  val pretty_trace: unit -> t -> GoblintCil.Pretty.doc
  val compare : t -> t -> int

  val printXml : 'a BatInnerIO.output -> t -> unit
  val var_id   : t -> string
  val node      : t -> MyCFG.node
  val relift    : t -> t (* needed only for incremental+hashcons to re-hashcons contexts after loading *)
end

(** Abstract incremental change to constraint system.
    @param 'v constrain system variable type *)
type 'v sys_change_info = {
  obsolete: 'v list; (** Variables to destabilize. *)
  delete: 'v list; (** Variables to delete. *)
  reluctant: 'v list; (** Variables to solve reluctantly. *)
  restart: 'v list; (** Variables to restart. *)
}

(** A side-effecting system. *)
module type MonSystem =
sig
  type v    (* variables *)
  type d    (* values    *)
  type 'a m (* basically a monad carrier *)

  (** Variables must be hashable, comparable, etc.  *)
  module Var : VarType with type t = v

  (** Values must form a lattice. *)
  module Dom : Lattice.S with type t = d

  (** The system in functional form. *)
  val system : v -> ((v -> d) -> (v -> d -> unit) -> d) m

  val sys_change: (v -> d) -> v sys_change_info
  (** Compute incremental constraint system change from old solution. *)
end

(** Any system of side-effecting equations over lattices. *)
module type EqConstrSys = MonSystem with type 'a m := 'a option

(** A side-effecting system with globals. *)
module type GlobConstrSys =
sig
  module LVar : VarType
  module GVar : VarType

  module D : Lattice.S
  module G : Lattice.S
  val system : LVar.t -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t) option
  val iter_vars: (LVar.t -> D.t) -> (GVar.t -> G.t) -> VarQuery.t -> LVar.t VarQuery.f -> GVar.t VarQuery.f -> unit
  val sys_change: (LVar.t -> D.t) -> (GVar.t -> G.t) -> [`L of LVar.t | `G of GVar.t] sys_change_info
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

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericEqSolver =
  functor (S:EqConstrSys) ->
  functor (H:Hashtbl.S with type key=S.v) ->
  sig
    (** The hash-map that is the first component of [solve xs vs] is a local solution for interesting variables [vs],
        reached from starting values [xs]. *)
    val solve : (S.v*S.d) list -> S.v list -> S.d H.t
  end

(** A solver is something that can translate a system into a solution (hash-table) *)
module type GenericGlobSolver =
  functor (S:GlobConstrSys) ->
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