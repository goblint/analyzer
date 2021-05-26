open Prelude.Ana
open Analyses
open GobConfig
open BaseUtil
module Q = Queries

module OctApronComponents = OctApronDomain.OctApronComponents


module type S =
sig
  module D: Lattice.S
  module G: Lattice.S

  val startstate: unit -> D.t

  val read_global: Q.ask -> (varinfo -> G.t) -> OctApronComponents (D).t -> varinfo -> varinfo -> OctApronComponents (D).t

  (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
   * the state when following conditional guards. *)
  val write_global: ?invariant:bool -> Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> varinfo -> varinfo -> OctApronComponents (D).t

  val lock: Q.ask -> (varinfo -> G.t) -> OctApronComponents (D).t -> LockDomain.Addr.t -> OctApronComponents (D).t
  val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> LockDomain.Addr.t -> OctApronComponents (D).t

  val sync: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> OctApronComponents (D).t

  val escape: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> EscapeDomain.EscapedVars.t -> OctApronComponents (D).t
  val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> OctApronComponents (D).t
  val threadenter: Q.ask -> OctApronComponents (D).t -> OctApronComponents (D).t

  val init: unit -> unit
  val finalize: unit -> unit
end


module Dummy: S =
struct
  module D = Lattice.Unit
  module G = Lattice.Unit

  let startstate () = ()

  let read_global ask getg st g x = st
  let write_global ?(invariant=false) ask getg sideg st x g = st

  let lock ask getg st m = st
  let unlock ask getg sideg st m = st

  let sync ask getg sideg st reason = st

  let escape ask getg sideg st escaped = st
  let enter_multithreaded ask getg sideg st = st
  let threadenter ask st = st

  let init () = ()
  let finalize () = ()
end
