(** {{!EqConstrSys} constraint system} signatures. *)

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

(** Abstract incremental change to constraint system.
    @param 'v constrain system variable type *)
type 'v sys_change_info = {
  obsolete: 'v list; (** Variables to destabilize. *)
  delete: 'v list; (** Variables to delete. *)
  reluctant: 'v list; (** Variables to solve reluctantly. *)
  restart: 'v list; (** Variables to restart. *)
}

(** A side-effecting system. *)
module type EqConstrSys =
sig
  type v    (* variables *)
  type d    (* values    *)

  (** Variables must be hashable, comparable, etc.  *)
  module Var : VarType with type t = v

  (** Values must form a lattice. *)
  module Dom : Lattice.S with type t = d

  (** The system in functional form. *)
  val system : v -> ((v -> d) -> (v -> d -> unit) -> d) option

  (** Compute incremental constraint system change from old solution. *)
  val sys_change: (v -> d) -> v sys_change_info

  (** List of unknowns that should be queried again when the argument unknown has shrunk to bot, to eagerly trigger (analysis-time!) abstract garbage collection idependently of reach-based pruning at the end.
      @see <https://arxiv.org/abs/2504.06026> Stemmler, F., Schwarz, M., Erhard, J., Tilscher, S., Seidl, H. Taking out the Toxic Trash: Recovering Precision in Mixed Flow-Sensitive Static Analyses *)
  val postmortem: v -> v list
end

(** A side-effecting system that supports [demand] calls *)
module type DemandEqConstrSys =
sig
  include EqConstrSys
  val system: v -> ((v -> d) -> (v -> d -> unit) -> (v -> unit) -> d) option
end

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
  val postmortem: LVar.t -> LVar.t list
end

(** A side-effecting system with globals that supports [demand] calls *)
module type DemandGlobConstrSys =
sig
  include GlobConstrSys
  val system: LVar.t -> ((LVar.t -> D.t) -> (LVar.t -> D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> (LVar.t -> unit) -> D.t) option
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
