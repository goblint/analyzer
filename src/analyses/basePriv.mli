open GoblintCil
(* Cannot use local module substitutions because ppx_import is still stuck at 4.07 AST: https://github.com/ocaml-ppx/ppx_import/issues/50#issuecomment-775817579. *)
(* TODO: try again, because ppx_import is now removed *)

module type S =
sig
  module D: Lattice.S
  module G: Lattice.S
  module V: Printable.S

  val startstate: unit -> D.t

  val read_global: Queries.ask -> (V.t -> G.t) -> BaseDomain.BaseComponents (D).t -> varinfo -> BaseDomain.VD.t
  val write_global: ?invariant:bool -> Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> varinfo -> BaseDomain.VD.t -> BaseDomain.BaseComponents (D).t

  val lock: Queries.ask -> (V.t -> G.t) -> BaseDomain.BaseComponents (D).t -> LockDomain.Addr.t -> BaseDomain.BaseComponents (D).t
  val unlock: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> LockDomain.Addr.t -> BaseDomain.BaseComponents (D).t

  val sync: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> BaseDomain.BaseComponents (D).t

  val escape: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> EscapeDomain.EscapedVars.t -> BaseDomain.BaseComponents (D).t
  val enter_multithreaded: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val threadenter: Queries.ask -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val iter_sys_vars: (V.t -> G.t) -> VarQuery.t -> V.t VarQuery.f -> unit (** [Queries.IterSysVars] for base. *)

  val invariant_global: (V.t -> G.t) -> V.t -> Invariant.t
  (** Provides [Queries.InvariantGlobal] result for base.

      Should account for all unprotected/weak values of global variables. *)

  val init: unit -> unit
  val finalize: unit -> unit
end

val get_priv : unit -> (module S)
