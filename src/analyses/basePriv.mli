(** Non-relational thread-modular value analyses for {!Base}.

    @see <https://doi.org/10.1007/978-3-030-88806-0_18> Schwarz, M., Saan, S., Seidl, H., Apinis, K., Erhard, J., Vojdani, V. Improving Thread-Modular Abstract Interpretation. *)

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

  val lock: Queries.ask -> (V.t -> G.t) -> BaseDomain.BaseComponents (D).t -> LockDomain.MustLock.t -> BaseDomain.BaseComponents (D).t
  val unlock: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> LockDomain.MustLock.t -> BaseDomain.BaseComponents (D).t

  val sync: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> [`Normal | `Join | `JoinCall of CilType.Fundec.t | `Return | `Init | `Thread] -> BaseDomain.BaseComponents (D).t

  val escape: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> EscapeDomain.EscapedVars.t -> BaseDomain.BaseComponents (D).t
  val enter_multithreaded: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val threadenter: Queries.ask -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val threadspawn: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val iter_sys_vars: (V.t -> G.t) -> VarQuery.t -> V.t VarQuery.f -> unit (** [Queries.IterSysVars] for base. *)

  val thread_join: ?force:bool -> Queries.ask -> (V.t -> G.t) -> Cil.exp -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val thread_return: Queries.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> ThreadIdDomain.Thread.t -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t

  val invariant_global: Queries.ask -> (V.t -> G.t) -> V.t -> Invariant.t
  (** Provides [Queries.InvariantGlobal] result for base.

      Should account for all unprotected/weak values of global variables. *)

  val invariant_vars: Queries.ask -> (V.t -> G.t) -> BaseDomain.BaseComponents (D).t -> varinfo list
  (** Returns global variables which are privatized. *)

  val init: unit -> unit
  val finalize: unit -> unit
end

val get_priv : unit -> (module S)
