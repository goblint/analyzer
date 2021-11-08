open Prelude.Ana
open Analyses
open GobConfig
(* open BaseUtil *)
module Q = Queries

module A = ApronDomain.A
module ApronComponents = ApronDomain.ApronComponents
open Apron

open CommonPriv


module type S =
  functor (AD: ApronDomain.S2) ->
  sig
    module D: Lattice.S
    module G: Lattice.S

    type apron_components_t := ApronDomain.ApronComponents (AD) (D).t
    val name: unit -> string
    val startstate: unit -> D.t
    val should_join: apron_components_t -> apron_components_t -> bool

    val read_global: Q.ask -> (varinfo -> G.t) -> apron_components_t -> varinfo -> varinfo -> AD.t

    (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
     * the state when following conditional guards. *)
    val write_global: ?invariant:bool -> Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> varinfo -> varinfo -> apron_components_t

    val lock: Q.ask -> (varinfo -> G.t) -> apron_components_t -> LockDomain.Addr.t -> apron_components_t
    val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> LockDomain.Addr.t -> apron_components_t

    val sync: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> [`Normal | `Join | `Return | `Init | `Thread] -> apron_components_t

    val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> apron_components_t
    val threadenter: Q.ask -> (varinfo -> G.t) -> apron_components_t -> apron_components_t

    val init: unit -> unit
    val finalize: unit -> unit
  end


module Dummy: S = functor (AD: ApronDomain.S2) ->
struct
  module AD = AD
  module D = Lattice.Unit
  module G = Lattice.Unit

  let name () = "Dummy"
  let startstate () = ()
  let should_join _ _ = true

  let read_global ask getg st g x = st.ApronDomain.apr
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
module ProtectionBasedPriv (Param: ProtectionBasedPrivParam): S = functor (AD: ApronDomain.S2) ->
struct
  include ConfCheck.RequireMutexActivatedInit
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

  module AD = AD
  module D = Lattice.Prod (P) (W)
  module G = AD

  type apron_components_t = ApronComponents (AD) (D).t
  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"

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
    include ApronDomain.VarMetadataTbl (VM)
    open VM

    let local g = make_var (Local g)
    let unprot g = make_var (Unprot g)
    let prot g = make_var (Prot g)
  end

  let name () = "ProtectionBasedPriv"

  (** Restrict environment to global invariant variables. *)
  let restrict_global apr =
    AD.remove_filter apr (fun var ->
        match V.find_metadata var with
        | Some (Unprot _ | Prot _) -> false
        | _ -> true
      )

  (** Restrict environment to local variables and still-protected global variables. *)
  let restrict_local is_unprot apr w_remove =
    let remove_local_vars = List.map V.local (W.elements w_remove) in
    let apr' = AD.remove_vars apr remove_local_vars in
    (* remove global vars *)
    AD.remove_filter apr' (fun var ->
        match V.find_metadata var with
        | Some (Unprot g | Prot g) -> is_unprot g
        | _ -> false
      )

  let startstate () = (P.empty (), W.empty ())

  let should_join (st1: apron_components_t) (st2: apron_components_t) =
    if Param.path_sensitive then (
      let (p1, _) = st1.priv in
      let (p2, _) = st2.priv in
      P.equal p1 p2
    )
    else
      true

  let read_global ask getg (st: apron_components_t) g x =
    let apr = st.apr in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let x_var = Var.of_string x.vname in
    let apr_local =
      if W.mem g w then
        AD.assign_var apr x_var g_local_var
      else
        AD.bot ()
    in
    let apr_local' =
      if P.mem g p then
        apr_local
      else if is_unprotected ask g then (
        let g_unprot_var = V.unprot g in
        let apr_unprot = AD.add_vars apr [g_unprot_var] in
        let apr_unprot = AD.assign_var apr_unprot x_var g_unprot_var in
        (* let oct_unprot' = AD.join oct_local oct_unprot in
           (* unlock *)
           let oct_unprot' = AD.remove_vars oct_unprot' [g_unprot_var; g_local_var] in
           (* add, assign from, remove is not equivalent to forget if g#unprot already existed and had some relations *)
           (* TODO: why removing g_unprot_var? *)
           oct_unprot' *)
        AD.join apr_local apr_unprot
      )
      else (
        let g_prot_var = V.prot g in
        let apr_prot = AD.add_vars apr [g_prot_var] in
        let apr_prot = AD.assign_var apr_prot x_var g_prot_var in
        AD.join apr_local apr_prot
      )
    in
    let apr_local' = restrict_local (is_unprotected ask) apr_local' (W.empty ()) in
    let apr_local' = AD.meet apr_local' (getg (global_varinfo ())) in
    apr_local'

  let write_global ?(invariant=false) ask getg sideg (st: apron_components_t) g x =
    let apr = st.apr in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let g_unprot_var = V.unprot g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_local_var] in
    let apr_local = AD.assign_var apr_local g_local_var x_var in
    let apr_side = AD.add_vars apr_local [g_unprot_var] in
    let apr_side = AD.assign_var apr_side g_unprot_var g_local_var in
    let apr' = apr_side in
    let apr_side = restrict_global apr_side in
    sideg (global_varinfo ()) apr_side;
    let st' =
      (* if is_unprotected ask g then
         st (* add, assign, remove gives original local state *)
         else
         (* restricting g#unprot-s out from oct' gives oct_local *)
         {oct = oct_local; priv = (P.add g p, W.add g w)} *)
      if is_unprotected ask g then
        {st with apr = restrict_local (is_unprotected ask) apr' (W.singleton g)}
      else (
        let p' = P.add g p in
        let w' = W.add g w in
        {apr = restrict_local (is_unprotected ask) apr' (W.empty ()); priv = (p', w')}
      )
    in
    let apr_local' = AD.meet st'.apr (getg (global_varinfo ())) in
    {st' with apr = apr_local'}

  let lock ask getg (st: apron_components_t) m = st

  let unlock ask getg sideg (st: apron_components_t) m: apron_components_t =
    let apr = st.apr in
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
    let apr_side = List.fold_left (fun acc omega ->
        let g_prot_vars = List.map V.prot omega in
        let g_local_vars = List.map V.local omega in
        let apr_side1 = AD.add_vars apr g_prot_vars in
        let apr_side1 = AD.assign_var_parallel' apr_side1 g_prot_vars g_local_vars in
        AD.join acc apr_side1
      ) (AD.bot ()) big_omega
    in
    let apr' = apr_side in
    let apr_side = restrict_global apr_side in
    sideg (global_varinfo ()) apr_side;
    let apr_local = restrict_local (fun g -> is_unprotected_without ask g m) apr' w_remove in
    let apr_local' = AD.meet apr_local (getg (global_varinfo ())) in
    {apr = apr_local'; priv = (p', w')}

  let sync ask getg sideg (st: apron_components_t) reason =
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

  let enter_multithreaded ask getg sideg (st: apron_components_t): apron_components_t =
    let apr = st.apr in
    let (g_vars, gs) =
      AD.vars apr
      |> List.enum
      |> Enum.filter_map (fun var ->
          match ApronDomain.V.find_metadata var with
          | Some (Global g) -> Some (var, g)
          | _ -> None
        )
      |> Enum.uncombine
      |> Tuple2.map List.of_enum List.of_enum
    in
    let g_unprot_vars = List.map V.unprot gs in
    let g_prot_vars = List.map V.prot gs in
    let apr_side = AD.add_vars apr (g_unprot_vars @ g_prot_vars) in
    let apr_side = AD.assign_var_parallel' apr_side g_unprot_vars g_vars in
    let apr_side = AD.assign_var_parallel' apr_side g_prot_vars g_vars in
    let apr_side = restrict_global apr_side in
    sideg (global_varinfo ()) apr_side;
    let apr_local = AD.remove_vars apr g_vars in
    let apr_local' = AD.meet apr_local (getg (global_varinfo ())) in
    {apr = apr_local'; priv = startstate ()}

  let threadenter ask getg (st: apron_components_t): apron_components_t =
    {apr = getg (global_varinfo ()); priv = startstate ()}

  let finalize () = ()
end

(** Per-mutex meet. *)
module PerMutexMeetPriv : S = functor (AD: ApronDomain.S2) ->
struct
  open Protection
  open ExplicitMutexGlobals

  module AD = AD
  module D = Lattice.Unit
  module G = AD

  type apron_components_t = ApronDomain.ApronComponents (AD) (D).t
  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"

  module V = ApronDomain.V

  let name () = "PerMutexMeetPriv"

  let startstate () = ()

  let should_join _ _ = true

  let mutex_inits = RichVarinfo.single ~name:"MUTEX_INITS"

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (mutex_addr_to_varinfo m) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let get_mutex_inits' = AD.keep_filter get_mutex_inits (fun var ->
        match V.find_metadata var with
        | Some (Global g) -> is_protected_by ask m g
        | _ -> false
      )
    in
    AD.join get_m get_mutex_inits'

  let get_mutex_global_g_with_mutex_inits ask getg g =
    let get_mutex_global_g = getg (mutex_global g) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let g_var = V.global g in
    let get_mutex_inits' = AD.keep_vars get_mutex_inits [g_var] in
    AD.join get_mutex_global_g get_mutex_inits'

  let read_global ask getg (st: apron_components_t) g x: AD.t =
    let apr = st.apr in
    (* lock *)
    let apr = AD.meet apr (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* read *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_var] in
    let apr_local = AD.assign_var apr_local x_var g_var in
    (* unlock *)
    let apr_local' =
      if is_unprotected ask g then
        AD.remove_vars apr_local [g_var]
      else
        apr_local
    in
    apr_local'

  let write_global ?(invariant=false) ask getg sideg (st: apron_components_t) g x: apron_components_t =
    let apr = st.apr in
    (* lock *)
    let apr = AD.meet apr (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* write *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_var] in
    let apr_local = AD.assign_var apr_local g_var x_var in
    (* unlock *)
    let apr_side = AD.keep_vars apr_local [g_var] in
    sideg (mutex_global g) apr_side;
    let apr_local' =
      if is_unprotected ask g then
        AD.remove_vars apr_local [g_var]
      else
        apr_local
    in
    {st with apr = apr_local'}

  let lock ask getg (st: apron_components_t) m =
    let apr = st.apr in
    let get_m = get_m_with_mutex_inits ask getg m in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let get_m = AD.keep_filter get_m (fun var ->
        match V.find_metadata var with
        | Some (Global g) -> is_protected_by ask m g
        | _ -> false
      )
    in
    let apr' = AD.meet apr get_m in
    {st with apr = apr'}

  let unlock ask getg sideg (st: apron_components_t) m: apron_components_t =
    let apr = st.apr in
    let apr_side = AD.keep_filter apr (fun var ->
        match V.find_metadata var with
        | Some (Global g) -> is_protected_by ask m g
        | _ -> false
      )
    in
    sideg (mutex_addr_to_varinfo m) apr_side;
    let apr_local = AD.remove_filter apr (fun var ->
        match V.find_metadata var with
        | Some (Global g) -> is_protected_by ask m g && is_unprotected_without ask g m
        | _ -> false
      )
    in
    {st with apr = apr_local}

  let sync ask getg sideg (st: apron_components_t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      (* TODO: implement? *)
      begin match ThreadId.get_current ask with
        | `Lifted x (* when CPA.mem x st.cpa *) ->
          st
        | _ ->
          st
      end
    | `Join ->
      if (ask.f Q.MustBeSingleThreaded) then
        st
      else
        let apr = st.apr in
        let g_vars = List.filter (fun var ->
            match V.find_metadata var with
            | Some (Global _) -> true
            | _ -> false
          ) (AD.vars apr)
        in
        let apr_side = AD.keep_vars apr g_vars in
        sideg (mutex_inits ()) apr_side;
        let apr_local = AD.remove_filter apr (fun var ->
            match V.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with apr = apr_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let enter_multithreaded ask getg sideg (st: apron_components_t): apron_components_t =
    let apr = st.apr in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match V.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (AD.vars apr)
    in
    let apr_side = AD.keep_vars apr g_vars in
    sideg (mutex_inits ()) apr_side;
    let apr_local = AD.remove_vars apr g_vars in (* TODO: side effect initial values to mutex_globals? *)
    {st with apr = apr_local}

  let threadenter ask getg (st: apron_components_t): apron_components_t =
    {apr = AD.bot (); priv = startstate ()}

  let init () = ()
  let finalize () = ()
end

(** Write-Centered Reading. *)
(* TODO: uncompleted, only W, P components from basePriv *)
module WriteCenteredPriv: S = functor (AD: ApronDomain.S2) ->
struct
  open Locksets
  open WriteCenteredD

  module AD = AD
  module D = Lattice.Prod (W) (P)
  module G = AD

  type apron_components_t =  ApronDomain.ApronComponents (AD) (D).t
  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"

  let name () = "WriteCenteredPriv"

  let startstate () = (W.bot (), P.top ())

  let should_join _ _ = true

  let lockset_init = Lockset.top ()

  (* TODO: distr_init? *)

  let restrict_globals apr =
    match !MyCFG.current_node with
    | Some node ->
      let fd = Node.find_fundec node in
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
      let to_keep = List.map (fun v -> Var.of_string v.vname) vars in
      AD.keep_vars apr to_keep
    | None ->
      (* TODO: when does this happen? *)
      if M.tracing then M.trace "apronpriv" "restrict_globals -\n";
      AD.bot ()

  let read_global ask getg (st: apron_components_t) g x =
    (* let s = current_lockset ask in *)
    (* let (w, p) = st.priv in *)
    (* let p_g = P.find g p in *)
    (* TODO: implement *)
    let apr' = AD.add_vars st.apr [Var.of_string g.vname] in
    let apr' = A.assign_texpr AD.Man.mgr apr' (Var.of_string x.vname) (Texpr1.var (A.env apr') (Var.of_string g.vname)) None in (* TODO: unsound *)
    apr'

  let write_global ?(invariant=false) ask getg sideg (st: apron_components_t) g x: apron_components_t =
    let s = current_lockset ask in
    let (w, p) = st.priv in
    let w' = W.add g (MinLocksets.singleton s) w in
    let p' = P.add g (MinLocksets.singleton s) p in
    let p' = P.map (fun s' -> MinLocksets.add s s') p' in
    (* TODO: implement *)
    let apr' = AD.add_vars st.apr [Var.of_string g.vname] in
    let apr' = A.assign_texpr AD.Man.mgr apr' (Var.of_string g.vname) (Texpr1.var (A.env apr') (Var.of_string x.vname)) None in (* TODO: unsound? *)
    sideg (global_varinfo ()) (restrict_globals apr');
    {apr = apr'; priv = (w', p')}

  let lock ask getg (st: apron_components_t) m = st

  let unlock ask getg sideg (st: apron_components_t) m =
    let s = Lockset.remove m (current_lockset ask) in
    let (w, p) = st.priv in
    let p' = P.map (fun s' -> MinLocksets.add s s') p in
    (* TODO: implement *)
    sideg (global_varinfo ()) (restrict_globals st.apr);
    {st with priv = (w, p')}

  let sync ask getg sideg (st: apron_components_t) reason =
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

  let enter_multithreaded ask getg sideg (st: apron_components_t) =
    (* TODO: implement *)
    {st with apr = AD.meet st.apr (getg (global_varinfo ()))}

  let threadenter ask getg (st: apron_components_t): apron_components_t =
    {apr = getg (global_varinfo ()); priv = startstate ()}

  let init () = ()
  let finalize () = ()
end


module TracingPriv = functor (Priv: S) -> functor (AD: ApronDomain.S2) ->
struct
  module Priv = Priv (AD)
  include Priv

  module D = Priv.D
  module ApronComponents = ApronDomain.ApronComponents (AD) (D)

  let read_global ask getg st g x =
    if M.tracing then M.traceli "apronpriv" "read_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = Priv.read_global ask getg st g x in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" AD.pretty r;
    r

  let write_global ?invariant ask getg sideg st g x =
    if M.tracing then M.traceli "apronpriv" "write_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
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
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "apronpriv" "lock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "apronpriv" "unlock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
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
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "apronpriv" "enter_multithreaded\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
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
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let threadenter ask getg st =
    if M.tracing then M.traceli "apronpriv" "threadenter\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = threadenter ask getg st in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "apronpriv" "sync\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
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
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r
end


let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "exp.apron.privatization" with
         | "dummy" -> (module Dummy : S)
         | "protection" -> (module ProtectionBasedPriv (struct let path_sensitive = false end))
         | "protection-path" -> (module ProtectionBasedPriv (struct let path_sensitive = true end))
         | "mutex-meet" -> (module PerMutexMeetPriv )
         (* | "write" -> (module WriteCenteredPriv) *)
         | _ -> failwith "exp.apron.privatization: illegal value"
      )
    in
    let module Priv = TracingPriv (Priv)  in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
