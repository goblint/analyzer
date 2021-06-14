open Prelude.Ana
open Analyses
open GobConfig
(* open BaseUtil *)
module Q = Queries

module OctApronComponents = OctApronDomain.OctApronComponents
module AD = OctApronDomain.D2
module A = OctApronDomain.A
module Man = OctApronDomain.Man
open Apron

open CommonPriv


module type S =
sig
  module D: Lattice.S
  module G: Lattice.S

  val startstate: unit -> D.t
  val should_join: OctApronComponents (D).t -> OctApronComponents (D).t -> bool

  val read_global: Q.ask -> (varinfo -> G.t) -> OctApronComponents (D).t -> varinfo -> varinfo -> OctApronComponents (D).t

  (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
   * the state when following conditional guards. *)
  val write_global: ?invariant:bool -> Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> varinfo -> varinfo -> OctApronComponents (D).t

  val lock: Q.ask -> (varinfo -> G.t) -> OctApronComponents (D).t -> LockDomain.Addr.t -> OctApronComponents (D).t
  val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> LockDomain.Addr.t -> OctApronComponents (D).t

  val sync: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> OctApronComponents (D).t

  val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> OctApronComponents (D).t -> OctApronComponents (D).t
  val threadenter: Q.ask -> (varinfo -> G.t) -> OctApronComponents (D).t -> OctApronComponents (D).t

  val init: unit -> unit
  val finalize: unit -> unit
end


module Dummy: S =
struct
  module D = Lattice.Unit
  module G = Lattice.Unit

  let startstate () = ()
  let should_join _ _ = true

  let read_global ask getg st g x = st
  let write_global ?(invariant=false) ask getg sideg st g x = st

  let lock ask getg st m = st
  let unlock ask getg sideg st m = st

  let sync ask getg sideg st reason = st

  let enter_multithreaded ask getg sideg st = st
  let threadenter ask getg st = st

  let init () = ()
  let finalize () = ()
end

module type ProtectionBasedPrivParam =
sig
  (** Whether to be path-sensitive w.r.t. locally written protected globals that have been continuously protected since writing. *)
  val path_sensitive: bool
end

(** Protection-Based Reading. *)
module ProtectionBasedPriv (Param: ProtectionBasedPrivParam): S =
struct
  open Protection

  (** Locally must-written protected globals that have been continuously protected since writing. *)
  module P =
  struct
    include MustVars
    let name () = "P"
  end

  (** Locally may-written protected globals that have been continuously protected since writing. *)
  (* TODO: is this right? *)
  module W =
  struct
    include MayVars
    let name () = "W"
  end

  module D = Lattice.Prod (P) (W)

  module G = AD

  let global_varinfo = RichVarinfo.single ~name:"OCTAPRON_GLOBAL"

  module VM =
  struct
    type t =
      | Local of varinfo
      | Unprot of varinfo
      | Prot of varinfo

    let var_name = function
      | Local g -> g.vname
      | Unprot g -> g.vname ^ "#unprot"
      | Prot g -> g.vname ^ "#prot"
  end
  module V =
  struct
    include OctApronDomain.VarMetadataTbl (VM)
    open VM

    let local g = make_var (Local g)
    let unprot g = make_var (Unprot g)
    let prot g = make_var (Prot g)
  end

  (** Restrict environment to global invariant variables. *)
  let restrict_global oct =
    let (vars, _) = Environment.vars (A.env oct) in (* FIXME: floats *)
    let remove_vars =
      vars
      |> Array.enum
      |> Enum.filter (fun var ->
          match V.find_metadata var with
          | Some (Unprot _ | Prot _) -> false
          | _ -> true
        )
      |> List.of_enum
    in
    AD.remove_vars oct remove_vars

  (** Restrict environment to local variables and still-protected global variables. *)
  let restrict_local is_unprot oct w_remove =
    let (vars, _) = Environment.vars (A.env oct) in (* FIXME: floats *)
    let remove_global_vars =
      vars
      |> Array.enum
      |> Enum.filter (fun var ->
          match V.find_metadata var with
          | Some (Unprot g | Prot g) -> is_unprot g
          | _ -> false
        )
      |> List.of_enum
    in
    let remove_local_vars = List.map V.local (W.elements w_remove) in
    let remove_vars = remove_local_vars @ remove_global_vars in
    AD.remove_vars oct remove_vars

  let startstate () = (P.empty (), W.empty ())

  let should_join (st1: OctApronComponents (D).t) (st2: OctApronComponents (D).t) =
    if Param.path_sensitive then (
      let (p1, _) = st1.priv in
      let (p2, _) = st2.priv in
      P.equal p1 p2
    )
    else
      true

  let read_global ask getg (st: OctApronComponents (D).t) g x =
    let oct = st.oct in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let x_var = Var.of_string x.vname in
    let oct_local =
      if W.mem g w then
        AD.assign_var' oct x_var g_local_var
      else
        AD.bot ()
    in
    let oct_local' =
      if P.mem g p then
        oct_local
      else if is_unprotected ask g then (
        let g_unprot_var = V.unprot g in
        let oct_unprot = AD.add_vars_int oct [g_unprot_var] in
        let oct_unprot = AD.assign_var' oct_unprot x_var g_unprot_var in
        (* let oct_unprot' = AD.join oct_local oct_unprot in
        (* unlock *)
        let oct_unprot' = AD.remove_vars oct_unprot' [g_unprot_var; g_local_var] in
        (* add, assign from, remove is not equivalent to forget if g#unprot already existed and had some relations *)
        (* TODO: why removing g_unprot_var? *)
        oct_unprot' *)
        AD.join oct_local oct_unprot
      )
      else (
        let g_prot_var = V.prot g in
        let oct_prot = AD.add_vars_int oct [g_prot_var] in
        let oct_prot = AD.assign_var' oct_prot x_var g_prot_var in
        AD.join oct_local oct_prot
      )
    in
    let oct_local' = restrict_local (is_unprotected ask) oct_local' (W.empty ()) in
    let oct_local' = AD.meet oct_local' (getg (global_varinfo ())) in
    {st with oct = oct_local'}

  let write_global ?(invariant=false) ask getg sideg (st: OctApronComponents (D).t) g x =
    let oct = st.oct in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let g_unprot_var = V.unprot g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars_int oct [g_local_var] in
    let oct_local = AD.assign_var' oct_local g_local_var x_var in
    let oct_side = AD.add_vars_int oct_local [g_unprot_var] in
    let oct_side = AD.assign_var' oct_side g_unprot_var g_local_var in
    let oct' = oct_side in
    let oct_side = restrict_global oct_side in
    sideg (global_varinfo ()) oct_side;
    let st' =
      (* if is_unprotected ask g then
        st (* add, assign, remove gives original local state *)
      else
        (* restricting g#unprot-s out from oct' gives oct_local *)
        {oct = oct_local; priv = (P.add g p, W.add g w)} *)
      if is_unprotected ask g then
        {st with oct = restrict_local (is_unprotected ask) oct' (W.singleton g)}
      else (
        let p' = P.add g p in
        let w' = W.add g w in
        {oct = restrict_local (is_unprotected ask) oct' (W.empty ()); priv = (p', w')}
      )
    in
    let oct_local' = AD.meet st'.oct (getg (global_varinfo ())) in
    {st' with oct = oct_local'}

  let lock ask getg (st: OctApronComponents (D).t) m = st

  let unlock ask getg sideg (st: OctApronComponents (D).t) m: OctApronComponents (D).t =
    let oct = st.oct in
    let (p, w) = st.priv in
    let (p_remove, p') = P.partition (fun g -> is_unprotected_without ask g m) p in
    let (w_remove, w') = W.partition (fun g -> is_unprotected_without ask g m) w in
    let p_a = P.filter (is_protected_by ask m) p in
    let w_a = W.filter (is_protected_by ask m) (W.diff w p) in
    let big_omega =
      let certain = P.elements p_a in
      let choice = W.elements w_a in
      choice
      |> List.map (fun _ -> [true; false])
      |> List.n_cartesian_product (* TODO: exponential! *)
      |> List.map (fun omega ->
          (* list globals where omega is true *)
          List.fold_left2 (fun acc g omega_g ->
            if omega_g then
              g :: acc
            else
              acc
          ) certain choice omega
        )
    in
    let oct_side = List.fold_left (fun acc omega ->
        let g_prot_vars = List.map V.prot omega in
        let g_local_vars = List.map V.local omega in
        let oct_side1 = AD.add_vars_int oct g_prot_vars in
        let oct_side1 = AD.parallel_assign_vars oct_side1 g_prot_vars g_local_vars in
        AD.join acc oct_side1
      ) (AD.bot ()) big_omega
    in
    let oct' = oct_side in
    let oct_side = restrict_global oct_side in
    sideg (global_varinfo ()) oct_side;
    let oct_local = restrict_local (fun g -> is_unprotected_without ask g m) oct' w_remove in
    let oct_local' = AD.meet oct_local (getg (global_varinfo ())) in
    {oct = oct_local'; priv = (p', w')}

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

  let enter_multithreaded ask getg sideg (st: OctApronComponents (D).t): OctApronComponents (D).t =
    let oct = st.oct in
    let (vars, _) = Environment.vars (A.env oct) in (* FIXME: floats *)
    let (g_vars, gs) =
      vars
      |> Array.enum
      |> Enum.filter_map (fun var ->
          match OctApronDomain.GV.find_metadata var with
          | Some g -> Some (var, g)
          | _ -> None
        )
      |> Enum.uncombine
      |> Tuple2.map List.of_enum List.of_enum
    in
    let g_unprot_vars = List.map V.unprot gs in
    let g_prot_vars = List.map V.prot gs in
    let oct_side = AD.add_vars_int oct (g_unprot_vars @ g_prot_vars) in
    let oct_side = AD.parallel_assign_vars oct_side g_unprot_vars g_vars in
    let oct_side = AD.parallel_assign_vars oct_side g_prot_vars g_vars in
    let oct_side = restrict_global oct_side in
    sideg (global_varinfo ()) oct_side;
    let oct_local = AD.remove_vars oct g_vars in
    let oct_local' = AD.meet oct_local (getg (global_varinfo ())) in
    {oct = oct_local'; priv = startstate ()}

  let threadenter ask getg (st: OctApronComponents (D).t): OctApronComponents (D).t =
    {oct = getg (global_varinfo ()); priv = startstate ()}

  let init () = ()
  let finalize () = ()
end

(** Per-mutex meet. *)
module PerMutexMeetPriv: S =
struct
  open Protection

  module I =
  struct
    include MapDomain.MapBot (LockDomain.Addr) (AD)
    let name () = "I"
  end

  module D = I

  module G = I

  let global_varinfo = RichVarinfo.single ~name:"OCTAPRON_GLOBAL"

  module V = OctApronDomain.GV

  let startstate () = (I.empty ())

  let should_join _ _ = true

  (* TODO: deduplicate with BasePriv *)
  let mutex_global: varinfo -> LockDomain.Addr.t =
    let mutex_global_varinfo: varinfo -> varinfo = RichVarinfo.Variables.map ~name:(fun x -> "MUTEX_GLOBAL_" ^ x.vname) (* explicit type to force call without ?size *)
    in
    fun g ->
      LockDomain.Addr.from_var (mutex_global_varinfo g)

  let read_global ask getg (st: OctApronComponents (D).t) g x =
    let oct = st.oct in
    let i = st.priv in
    let m = mutex_global g in
    (* lock *)
    let oct = AD.meet oct (I.find m i) in
    (* read *)
    let g_var = V.make g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars_int oct [g_var] in
    let oct_local = AD.assign_var' oct_local x_var g_var in
    (* TODO: unlock *)
    {st with oct = oct_local}

  let write_global ?(invariant=false) ask getg sideg (st: OctApronComponents (D).t) g x =
    let oct = st.oct in
    let i = st.priv in
    let m = mutex_global g in
    (* lock *)
    let oct = AD.meet oct (I.find m i) in
    (* write *)
    let g_var = V.make g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars_int oct [g_var] in
    let oct_local = AD.assign_var' oct_local g_var x_var in
    (* TODO: unlock *)
    {st with oct = oct_local}

  let lock ask getg (st: OctApronComponents (D).t) m =
    let oct = st.oct in
    let i = st.priv in
    let oct' = AD.meet oct (I.find m i) in
    {st with oct = oct'}

  let unlock ask getg sideg (st: OctApronComponents (D).t) m: OctApronComponents (D).t =
    let oct = st.oct in
    let i = st.priv in
    let (vars, _) = Environment.vars (A.env oct) in (* FIXME: floats *)
    let keep_vars_side =
      vars
      |> Array.enum
      |> Enum.filter (fun var ->
          match V.find_metadata var with
          | Some g -> is_protected_by ask m g
          | None -> false
        )
      |> List.of_enum
    in
    let oct_side = AD.keep_vars oct keep_vars_side in
    let i_side = I.add m oct_side i in
    sideg (global_varinfo ()) i_side;
    let remove_vars_local =
      vars
      |> Array.enum
      |> Enum.filter (fun var ->
          match V.find_metadata var with
          | Some g -> is_protected_by ask m g && is_unprotected_without ask g m
          | None -> false
        )
      |> List.of_enum
    in
    let oct_local = AD.remove_vars oct remove_vars_local in
    {oct = oct_local; priv = getg (global_varinfo ())}

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

  let enter_multithreaded ask getg sideg (st: OctApronComponents (D).t): OctApronComponents (D).t =
    let oct = st.oct in
    (* TODO: implement *)
    st

  let threadenter ask getg (st: OctApronComponents (D).t): OctApronComponents (D).t =
    {st with priv = startstate ()}

  let init () = ()
  let finalize () = ()
end

(** Write-Centered Reading. *)
module WriteCenteredPriv: S =
struct
  open Locksets

  open WriteCenteredD
  module D = Lattice.Prod (W) (P)

  module G = AD

  let global_varinfo = RichVarinfo.single ~name:"OCTAPRON_GLOBAL"

  let startstate () = (W.bot (), P.top ())

  let should_join _ _ = true

  let lockset_init = Lockset.top ()

  (* TODO: distr_init? *)

  let restrict_globals oct =
    match !MyCFG.current_node with
    | Some node ->
      let fd = MyCFG.getFun node in
      if M.tracing then M.trace "apronpriv" "restrict_globals %s\n" fd.svar.vname;
      (* TODO: avoid *)
      let vars =
        foldGlobals !Cilfacade.current_file (fun acc global ->
          match global with
          | GVar (vi, _, _) ->
            vi :: acc
            (* TODO: what about GVarDecl? *)
          | _ -> acc
        ) []
      in
      let to_keep = List.map (fun v -> v.vname) vars in
      let oct' = A.copy Man.mgr oct in
      AD.remove_all_but_with oct' to_keep;
      oct'
    | None ->
      (* TODO: when does this happen? *)
      if M.tracing then M.trace "apronpriv" "restrict_globals -\n";
      AD.bot ()

  let read_global ask getg (st: OctApronComponents (D).t) g x =
    (* let s = current_lockset ask in *)
    (* let (w, p) = st.priv in *)
    (* let p_g = P.find g p in *)
    (* TODO: implement *)
    let oct' = AD.add_vars st.oct ([g.vname], []) in
    let oct' = A.assign_texpr Man.mgr oct' (Var.of_string x.vname) (Texpr1.var (A.env oct') (Var.of_string g.vname)) None in (* TODO: unsound *)
    {st with oct = oct'}

  let write_global ?(invariant=false) ask getg sideg (st: OctApronComponents (D).t) g x: OctApronComponents (D).t =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let w' = W.add g (MinLocksets.singleton s) w in
    let p' = P.add g (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    (* TODO: implement *)
    let oct' = AD.add_vars st.oct ([g.vname], []) in
    let oct' = A.assign_texpr Man.mgr oct' (Var.of_string g.vname) (Texpr1.var (A.env oct') (Var.of_string x.vname)) None in (* TODO: unsound? *)
    sideg (global_varinfo ()) (restrict_globals oct');
    {oct = oct'; priv = (w', p')}

  let lock ask getg (st: OctApronComponents (D).t) m = st

  let unlock ask getg sideg (st: OctApronComponents (D).t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let (w, p) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    (* TODO: implement *)
    sideg (global_varinfo ()) (restrict_globals st.oct);
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

  let enter_multithreaded ask getg sideg (st: OctApronComponents (D).t) =
    (* TODO: implement *)
    {st with oct = AD.meet st.oct (getg (global_varinfo ()))}

  let threadenter ask getg (st: OctApronComponents (D).t): OctApronComponents (D).t =
    {oct = getg (global_varinfo ()); priv = startstate ()}

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

  let write_global ?invariant ask getg sideg st g x =
    if M.tracing then M.traceli "apronpriv" "write_global %a %a\n" d_varinfo g d_varinfo x;
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
    let r = write_global ?invariant ask getg sideg st g x in
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

  let threadenter ask getg st =
    if M.tracing then M.traceli "apronpriv" "threadenter\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" OctApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = threadenter ask getg st in
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


let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "exp.octapron.privatization" with
        | "dummy" -> (module Dummy: S)
        | "protection" -> (module ProtectionBasedPriv (struct let path_sensitive = false end))
        | "protection-path" -> (module ProtectionBasedPriv (struct let path_sensitive = true end))
        | "mutex-meet" -> (module PerMutexMeetPriv)
        | "write" -> (module WriteCenteredPriv)
        | _ -> failwith "exp.octapron.privatization: illegal value"
      )
    in
    let module Priv = TracingPriv (Priv) in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
