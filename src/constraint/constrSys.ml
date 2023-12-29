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


(** Combined variables so that we can also use the more common [EqConstrSys]
    that uses only one kind of a variable. *)
module Var2 (LV:VarType) (GV:VarType)
  : VarType
    with type t = [ `L of LV.t  | `G of GV.t ]
=
struct
  type t = [ `L of LV.t  | `G of GV.t ] [@@deriving eq, ord, hash]
  let relift = function
    | `L x -> `L (LV.relift x)
    | `G x -> `G (GV.relift x)

  let pretty_trace () = function
    | `L a -> GoblintCil.Pretty.dprintf "L:%a" LV.pretty_trace a
    | `G a -> GoblintCil.Pretty.dprintf "G:%a" GV.pretty_trace a

  let printXml f = function
    | `L a -> LV.printXml f a
    | `G a -> GV.printXml f a

  let var_id = function
    | `L a -> LV.var_id a
    | `G a -> GV.var_id a

  let node = function
    | `L a -> LV.node a
    | `G a -> GV.node a

  let is_write_only = function
    | `L a -> LV.is_write_only a
    | `G a -> GV.is_write_only a
end


(** Translate a [GlobConstrSys] into a [EqConstrSys] *)
module EqConstrSysFromGlobConstrSys (S:GlobConstrSys)
  : EqConstrSys   with type v = Var2(S.LVar)(S.GVar).t
                   and type d = Lattice.Lift2(S.G)(S.D).t
                   and module Var = Var2(S.LVar)(S.GVar)
                   and module Dom = Lattice.Lift2(S.G)(S.D)
=
struct
  module Var = Var2(S.LVar)(S.GVar)
  module Dom =
  struct
    include Lattice.Lift2 (S.G) (S.D)
    let printXml f = function
      | `Lifted1 a -> S.G.printXml f a
      | `Lifted2 a -> S.D.printXml f a
      | (`Bot | `Top) as x -> printXml f x
  end
  type v = Var.t
  type d = Dom.t

  let getG = function
    | `Lifted1 x -> x
    | `Bot -> S.G.bot ()
    | `Top -> failwith "EqConstrSysFromGlobConstrSys.getG: global variable has top value"
    | `Lifted2 _ -> failwith "EqConstrSysFromGlobConstrSys.getG: global variable has local value"

  let getL = function
    | `Lifted2 x -> x
    | `Bot -> S.D.bot ()
    | `Top -> failwith "EqConstrSysFromGlobConstrSys.getL: local variable has top value"
    | `Lifted1 _ -> failwith "EqConstrSysFromGlobConstrSys.getL: local variable has global value"

  let l, g = (fun x -> `L x), (fun x -> `G x)
  let lD, gD = (fun x -> `Lifted2 x), (fun x -> `Lifted1 x)

  let conv f get set =
    f (getL % get % l) (fun x v -> set (l x) (lD v))
      (getG % get % g) (fun x v -> set (g x) (gD v))
    |> lD

  let system = function
    | `G _ -> None
    | `L x -> Option.map conv (S.system x)

  let sys_change get =
    S.sys_change (getL % get % l) (getG % get % g)
end

(** Splits a [EqConstrSys] solution into a [GlobConstrSys] solution with given [Hashtbl.S] for the [EqConstrSys]. *)
module GlobConstrSolFromEqConstrSolBase (S: GlobConstrSys) (LH: Hashtbl.S with type key = S.LVar.t) (GH: Hashtbl.S with type key = S.GVar.t) (VH: Hashtbl.S with type key = Var2 (S.LVar) (S.GVar).t) =
struct
  let split_solution hm =
    let l' = LH.create 113 in
    let g' = GH.create 113 in
    let split_vars x d = match x with
      | `L x ->
        begin match d with
          | `Lifted2 d -> LH.replace l' x d
          (* | `Bot -> () *)
          (* Since Verify2 is broken and only checks existing keys, add it with local bottom value.
            This works around some cases, where Verify2 would not detect a problem due to completely missing variable. *)
          | `Bot -> LH.replace l' x (S.D.bot ())
          | `Top -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: local variable has top value"
          | `Lifted1 _ -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: local variable has global value"
        end
      | `G x ->
        begin match d with
          | `Lifted1 d -> GH.replace g' x d
          | `Bot -> ()
          | `Top -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: global variable has top value"
          | `Lifted2 _ -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: global variable has local value"
        end
    in
    VH.iter split_vars hm;
    (l', g')
end

(** Splits a [EqConstrSys] solution into a [GlobConstrSys] solution. *)
module GlobConstrSolFromEqConstrSol (S: GlobConstrSys) (LH: Hashtbl.S with type key = S.LVar.t) (GH: Hashtbl.S with type key = S.GVar.t) =
struct
  module S2 = EqConstrSysFromGlobConstrSys (S)
  module VH = Hashtbl.Make (S2.Var)

  include GlobConstrSolFromEqConstrSolBase (S) (LH) (GH) (VH)
end

(** Transforms a [GenericEqIncrSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromEqSolver (Sol:GenericEqIncrSolverBase)
  = functor (S:GlobConstrSys) ->
    functor (LH:Hashtbl.S with type key=S.LVar.t) ->
    functor (GH:Hashtbl.S with type key=S.GVar.t) ->
    struct
      module EqSys = EqConstrSysFromGlobConstrSys (S)

      module VH : Hashtbl.S with type key=EqSys.v = Hashtbl.Make(EqSys.Var)
      module Sol' = Sol (EqSys) (VH)

      module Splitter = GlobConstrSolFromEqConstrSolBase (S) (LH) (GH) (VH) (* reuse EqSys and VH *)

      type marshal = Sol'.marshal

      let copy_marshal = Sol'.copy_marshal
      let relift_marshal = Sol'.relift_marshal

      let solve ls gs l old_data =
        let vs = List.map (fun (x,v) -> `L x, `Lifted2 v) ls
                 @ List.map (fun (x,v) -> `G x, `Lifted1 v) gs in
        let sv = List.map (fun x -> `L x) l in
        let hm, solver_data = Sol'.solve vs sv old_data in
        Splitter.split_solution hm, solver_data
    end


(** [EqConstrSys] where [current_var] indicates the variable whose right-hand side is currently being evaluated. *)
module CurrentVarEqConstrSys (S: EqConstrSys) =
struct
  let current_var = ref None

  module S =
  struct
    include S

    let system x =
      match S.system x with
      | None -> None
      | Some f ->
        let f' get set =
          let old_current_var = !current_var in
          current_var := Some x;
          Fun.protect ~finally:(fun () ->
              current_var := old_current_var
            ) (fun () ->
              f get set
            )
        in
        Some f'
  end
end
