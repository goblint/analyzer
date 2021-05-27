open Prelude.Ana
open Analyses
open GobConfig
open BaseUtil
module Q = Queries

module OctApronComponents = OctApronDomain.OctApronComponents

open CommonPriv


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

(** Write-Centered Reading. *)
module WriteCenteredPriv: S =
struct
  open Locksets

  open WriteCenteredD
  module D = Lattice.Prod (W) (P)

  module G = Lattice.Unit

  let startstate () = (W.bot (), P.top ())

  let lockset_init = Lockset.top ()

  (* TODO: distr_init? *)

  let read_global ask getg (st: OctApronComponents (D).t) g x =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let p_g = P.find g p in
    (* TODO: implement *)
    st
  let write_global ?(invariant=false) ask getg sideg (st: OctApronComponents (D).t) x g =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let w' = W.add g (MinLocksets.singleton s) w in
    let p' = P.add g (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    (* TODO: implement *)
    {st with priv = (w', p')}

  let lock ask getg (st: OctApronComponents (D).t) m = st

  let unlock ask getg sideg (st: OctApronComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let (w, p) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    (* TODO: implement *)
    {st with priv = (w, p')}

  let sync ask getg sideg (st: OctApronComponents (D).t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      (* TODO: implement? *)
      begin match ThreadId.get_current ask with
        | `Lifted x (* when CPA.mem x st.cpa *) ->
          st
        | _ ->
          st
      end
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st

  let escape ask getg sideg (st: OctApronComponents (D).t) escaped =
    let s = current_lockset ask in
    EscapeDomain.EscapedVars.fold (fun x acc ->
        let (w, p) = st.priv in
        let p' = P.add x (MinLocksets.singleton s) p in
        (* TODO: implement *)
        {st with priv = (w, p')}
      ) escaped st

  let enter_multithreaded ask getg sideg (st: OctApronComponents (D).t) =
    (* TODO: implement *)
    st

  let threadenter ask (st: OctApronComponents (D).t): OctApronComponents (D).t =
    {oct = OctApronDomain.D.bot (); priv = startstate ()}

  let init () = ()
  let finalize () = ()
end


module TracingPriv (Priv: S): S with module D = Priv.D =
struct
  include Priv

  module OctApronComponents = OctApronComponents (D)

  let read_global ask getg st g x =
    if M.tracing then M.traceli "apronpriv" "read_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = Priv.read_global ask getg st g x in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r

  let write_global ?invariant ask getg sideg st x g =
    if M.tracing then M.traceli "apronpriv" "write_global %a %a\n" d_varinfo x d_varinfo g;
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = write_global ?invariant ask getg sideg st x g in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "apronpriv" "lock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "apronpriv" "unlock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = unlock ask getg sideg st m in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "apronpriv" "enter_multithreaded\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = enter_multithreaded ask getg sideg st in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r

  let threadenter ask st =
    if M.tracing then M.traceli "apronpriv" "threadenter\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let r = threadenter ask st in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "apronpriv" "sync\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = sync ask getg sideg st reason in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" OctApronComponents.pretty r;
    r
end
