open Prelude.Ana
open Analyses
open GobConfig
(* open BaseUtil *)
module Q = Queries

module ApronComponents = ApronDomain.ApronComponents
module AD = ApronDomain.D2
module A = ApronDomain.A
module Man = ApronDomain.Man
open Apron

open CommonPriv


module type S =
sig
  module D: Lattice.S
  module G: Lattice.S

  val startstate: unit -> D.t
  val should_join: ApronComponents (D).t -> ApronComponents (D).t -> bool

  val read_global: Q.ask -> (varinfo -> G.t) -> ApronComponents (D).t -> varinfo -> varinfo -> AD.t

  (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
   * the state when following conditional guards. *)
  val write_global: ?invariant:bool -> Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> ApronComponents (D).t -> varinfo -> varinfo -> ApronComponents (D).t

  val lock: Q.ask -> (varinfo -> G.t) -> ApronComponents (D).t -> LockDomain.Addr.t -> ApronComponents (D).t
  val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> ApronComponents (D).t -> LockDomain.Addr.t -> ApronComponents (D).t

  val thread_join: Q.ask -> (varinfo -> G.t) -> exp -> ApronComponents (D).t -> ApronComponents (D).t
  val thread_return: Q.ask -> (varinfo -> G.t) ->  (varinfo -> G.t -> unit) -> ApronComponents (D).t -> ApronComponents (D).t

  val sync: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> ApronComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> ApronComponents (D).t

  val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> ApronComponents (D).t -> ApronComponents (D).t
  val threadenter: Q.ask -> (varinfo -> G.t) -> ApronComponents (D).t -> ApronComponents (D).t

  val init: unit -> unit
  val finalize: unit -> unit
end


module Dummy: S =
struct
  module D = Lattice.Unit
  module G = Lattice.Unit

  let startstate () = ()
  let should_join _ _ = true

  let read_global ask getg st g x = st.ApronDomain.oct
  let write_global ?(invariant=false) ask getg sideg st g x = st

  let lock ask getg st m = st
  let unlock ask getg sideg st m = st

  let thread_join ask getg exp st = st
  let thread_return ask getg sideg st = st

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

  module D = Lattice.Prod (P) (W)

  module G = AD

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

  (** Restrict environment to global invariant variables. *)
  let restrict_global oct =
    AD.remove_filter oct (fun var ->
        match V.find_metadata var with
        | Some (Unprot _ | Prot _) -> false
        | _ -> true
      )

  (** Restrict environment to local variables and still-protected global variables. *)
  let restrict_local is_unprot oct w_remove =
    let remove_local_vars = List.map V.local (W.elements w_remove) in
    let oct' = AD.remove_vars oct remove_local_vars in
    (* remove global vars *)
    AD.remove_filter oct' (fun var ->
        match V.find_metadata var with
        | Some (Unprot g | Prot g) -> is_unprot g
        | _ -> false
      )

  let startstate () = (P.empty (), W.empty ())

  let should_join (st1: ApronComponents (D).t) (st2: ApronComponents (D).t) =
    if Param.path_sensitive then (
      let (p1, _) = st1.priv in
      let (p2, _) = st2.priv in
      P.equal p1 p2
    )
    else
      true

  let read_global ask getg (st: ApronComponents (D).t) g x =
    let oct = st.oct in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let x_var = Var.of_string x.vname in
    let oct_local =
      if W.mem g w then
        AD.assign_var oct x_var g_local_var
      else
        AD.bot ()
    in
    let oct_local' =
      if P.mem g p then
        oct_local
      else if is_unprotected ask g then (
        let g_unprot_var = V.unprot g in
        let oct_unprot = AD.add_vars oct [g_unprot_var] in
        let oct_unprot = AD.assign_var oct_unprot x_var g_unprot_var in
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
        let oct_prot = AD.add_vars oct [g_prot_var] in
        let oct_prot = AD.assign_var oct_prot x_var g_prot_var in
        AD.join oct_local oct_prot
      )
    in
    let oct_local' = restrict_local (is_unprotected ask) oct_local' (W.empty ()) in
    let oct_local' = AD.meet oct_local' (getg (global_varinfo ())) in
    oct_local'

  let write_global ?(invariant=false) ask getg sideg (st: ApronComponents (D).t) g x =
    let oct = st.oct in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let g_unprot_var = V.unprot g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars oct [g_local_var] in
    let oct_local = AD.assign_var oct_local g_local_var x_var in
    let oct_side = AD.add_vars oct_local [g_unprot_var] in
    let oct_side = AD.assign_var oct_side g_unprot_var g_local_var in
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

  let lock ask getg (st: ApronComponents (D).t) m = st

  let unlock ask getg sideg (st: ApronComponents (D).t) m: ApronComponents (D).t =
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
        let oct_side1 = AD.add_vars oct g_prot_vars in
        let oct_side1 = AD.assign_var_parallel' oct_side1 g_prot_vars g_local_vars in
        AD.join acc oct_side1
      ) (AD.bot ()) big_omega
    in
    let oct' = oct_side in
    let oct_side = restrict_global oct_side in
    sideg (global_varinfo ()) oct_side;
    let oct_local = restrict_local (fun g -> is_unprotected_without ask g m) oct' w_remove in
    let oct_local' = AD.meet oct_local (getg (global_varinfo ())) in
    {oct = oct_local'; priv = (p', w')}

  let thread_join ask getg exp st = st
  let thread_return ask getg sideg st = st

  let sync ask getg sideg (st: ApronComponents (D).t) reason =
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

  let enter_multithreaded ask getg sideg (st: ApronComponents (D).t): ApronComponents (D).t =
    let oct = st.oct in
    let (g_vars, gs) =
      AD.vars oct
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
    let oct_side = AD.add_vars oct (g_unprot_vars @ g_prot_vars) in
    let oct_side = AD.assign_var_parallel' oct_side g_unprot_vars g_vars in
    let oct_side = AD.assign_var_parallel' oct_side g_prot_vars g_vars in
    let oct_side = restrict_global oct_side in
    sideg (global_varinfo ()) oct_side;
    let oct_local = AD.remove_vars oct g_vars in
    let oct_local' = AD.meet oct_local (getg (global_varinfo ())) in
    {oct = oct_local'; priv = startstate ()}

  let threadenter ask getg (st: ApronComponents (D).t): ApronComponents (D).t =
    {oct = getg (global_varinfo ()); priv = startstate ()}

  let finalize () = ()
end

module CommonPerMutex =
struct
  include Protection
  module V = ApronDomain.V

  let remove_globals_unprotected_after_unlock ask m oct =
    let newly_unprot var = match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g && is_unprotected_without ask g m
      | _ -> false
    in
    AD.remove_filter oct newly_unprot

  let keep_only_protected_globals ask m oct =
    let protected var =  match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g
      | _ -> false
    in
    AD.keep_filter oct protected
end

(** Per-mutex meet. *)
module PerMutexMeetPriv: S =
struct
  open CommonPerMutex
  open ExplicitMutexGlobals

  module D = Lattice.Unit
  module G = AD

  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"

  module V = ApronDomain.V

  let startstate () = ()

  let should_join _ _ = true

  let mutex_inits = RichVarinfo.single ~name:"MUTEX_INITS"

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (mutex_addr_to_varinfo m) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let get_mutex_inits' = keep_only_protected_globals ask m get_mutex_inits in
    AD.join get_m get_mutex_inits'

  let get_mutex_global_g_with_mutex_inits ask getg g =
    let get_mutex_global_g = getg (mutex_global g) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let g_var = V.global g in
    let get_mutex_inits' = AD.keep_vars get_mutex_inits [g_var] in
    AD.join get_mutex_global_g get_mutex_inits'

  let read_global ask getg (st: ApronComponents (D).t) g x: AD.t =
    let oct = st.oct in
    (* lock *)
    let oct = AD.meet oct (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* read *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars oct [g_var] in
    let oct_local = AD.assign_var oct_local x_var g_var in
    (* unlock *)
    let oct_local' =
      if is_unprotected ask g then
        AD.remove_vars oct_local [g_var]
      else
        oct_local
    in
    oct_local'

  let write_global ?(invariant=false) ask getg sideg (st: ApronComponents (D).t) g x: ApronComponents (D).t =
    let oct = st.oct in
    (* lock *)
    let oct = AD.meet oct (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* write *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars oct [g_var] in
    let oct_local = AD.assign_var oct_local g_var x_var in
    (* unlock *)
    let oct_side = AD.keep_vars oct_local [g_var] in
    sideg (mutex_global g) oct_side;
    let oct_local' =
      if is_unprotected ask g then
        AD.remove_vars oct_local [g_var]
      else
        oct_local
    in
    {st with oct = oct_local'}

  let lock ask getg (st: ApronComponents (D).t) m =
    let oct = st.oct in
    let get_m = get_m_with_mutex_inits ask getg m in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let get_m = keep_only_protected_globals ask m get_m in
    let oct' = AD.meet oct get_m in
    {st with oct = oct'}

  let unlock ask getg sideg (st: ApronComponents (D).t) m: ApronComponents (D).t =
    let oct = st.oct in
    let oct_side = keep_only_protected_globals ask m oct in
    sideg (mutex_addr_to_varinfo m) oct_side;
    let oct_local = remove_globals_unprotected_after_unlock ask m oct in
    {st with oct = oct_local}

  let thread_join ask getg exp st = st
  let thread_return ask getg sideg st = st

  let sync ask getg sideg (st: ApronComponents (D).t) reason =
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
        let oct = st.oct in
        let g_vars = List.filter (fun var ->
            match V.find_metadata var with
            | Some (Global _) -> true
            | _ -> false
          ) (AD.vars oct)
        in
        let oct_side = AD.keep_vars oct g_vars in
        sideg (mutex_inits ()) oct_side;
        let oct_local = AD.remove_filter oct (fun var ->
            match V.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with oct = oct_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let enter_multithreaded ask getg sideg (st: ApronComponents (D).t): ApronComponents (D).t =
    let oct = st.oct in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match V.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (AD.vars oct)
    in
    let oct_side = AD.keep_vars oct g_vars in
    sideg (mutex_inits ()) oct_side;
    let oct_local = AD.remove_vars oct g_vars in (* TODO: side effect initial values to mutex_globals? *)
    {st with oct = oct_local}

  let threadenter ask getg (st: ApronComponents (D).t): ApronComponents (D).t =
    {oct = AD.bot (); priv = startstate ()}

  let init () = ()
  let finalize () = ()
end

(* May written variables *)
module W =
struct
  include MayVars
  let name () = "W"
end

module type ClusterArg =
sig
  module LAD: Lattice.S

  val keep_only_protected_globals: Q.ask -> LockDomain.Addr.t -> LAD.t -> LAD.t
  val keep_global: varinfo -> LAD.t -> LAD.t

  val lock: LAD.t -> LAD.t -> AD.t
  val unlock: W.t -> AD.t -> LAD.t
end

module NoCluster: ClusterArg =
struct
  open CommonPerMutex

  module LAD = AD

  let keep_only_protected_globals = keep_only_protected_globals

  let keep_global g oct =
    let g_var = V.global g in
    AD.keep_vars oct [g_var]

  let lock local_m get_m =
    AD.join local_m get_m

  let unlock w oct_side =
    oct_side
end

module Cluster12: ClusterArg =
struct
  open CommonPerMutex

  module VS =
  struct
    include Printable.Std
    include SetDomain.Make (CilType.Varinfo)
  end
  module LAD = MapDomain.MapBot (VS) (AD)

  let keep_only_protected_globals ask m octs =
    let octs =
      (* must filter by protection to avoid later meeting with non-protecting *)
      LAD.filter (fun gs _ ->
          VS.for_all (is_protected_by ask m) gs (* TODO: is this subset check right? *)
        ) octs
    in
    LAD.map (keep_only_protected_globals ask m) octs (* TODO: is this even necessary if keys are filtered above? *)
    (* octs *)

  let keep_global g octs =
    let g' = VS.singleton g in
    let oct = LAD.find g' octs in
    let g_var = V.global g in
    LAD.singleton g' (AD.keep_vars oct [g_var])

  let lock local_m get_m =
    let joined = LAD.join local_m get_m in
    if M.tracing then M.traceli "apronpriv" "cluster12 lock:\n  local=%a\n  get=%a\n  joined=%a\n" LAD.pretty local_m LAD.pretty get_m LAD.pretty joined;
    let r = LAD.fold (fun _ -> AD.meet) joined (AD.bot ()) in (* bot is top with empty env *)
    if M.tracing then M.traceu "apronpriv" "-> %a\n" AD.pretty r;
    r

  let unlock w oct_side =
    let vars = AD.vars oct_side in
    let gs = List.map (fun var -> match V.find_metadata var with
        | Some (Global g) -> g
        | _ -> assert false (* oct_side should only contain (protected) globals *)
      ) vars
    in
    let clusters =
      List.cartesian_product gs gs
      |> List.filter (fun (g1, g2) -> CilType.Varinfo.compare g1 g2 <= 0) (* filter flipped ordering, keep equals for next step *)
      |> List.map (fun (g1, g2) -> VS.of_list [g1; g2]) (* if g1 = g2, then we get a singleton cluster *)
      |> List.filter (VS.exists (fun g -> W.mem g w)) (* cluster intersection w is non-empty *)
    in
    let oct_side_cluster gs =
      AD.keep_vars oct_side (gs |> VS.elements |> List.map V.global)
    in
    LAD.add_list_fun clusters oct_side_cluster (LAD.empty ())
end

(** Per-mutex meet with TIDs. *)
module PerMutexMeetPrivTID (Cluster: ClusterArg): S =
struct
  open CommonPerMutex
  open ExplicitMutexGlobals
  include ConfCheck.RequireThreadFlagPathSensInit

  module LAD = Cluster.LAD

  (* Map from locks to last written values thread-locally *)
  module L = MapDomain.MapBot_LiftTop(Basetype.Variables)(LAD)

  module LMust = struct include MustVars let name () = "LMust" end

  module D = Lattice.Prod3 (W) (LMust) (L)
  module GMutex = MapDomain.MapBot_LiftTop(ThreadIdDomain.ThreadLifted)(LAD)
  module GThread = Lattice.Prod (LMust) (L)
  module G = Lattice.Lift2(GMutex)(GThread)(struct let bot_name = "bot" let top_name = "top" end)

  module V = ApronDomain.V
  module TID = ThreadIdDomain.Thread

  let compatible (ask:Q.ask) current must_joined other =
    match current, other with
    | `Lifted current, `Lifted other ->
      let not_self_read = (not (TID.is_unique current)) || (not (TID.equal current other)) in
      let may_be_running () =
        if (not (TID.is_must_parent current other)) then
          true
        else
          let created = ask.f Q.CreatedThreads in
          let ident_or_may_be_created creator = TID.equal creator other || TID.may_create creator other in
          if ConcDomain.ThreadSet.is_top created then
            true
          else
            ConcDomain.ThreadSet.exists (ident_or_may_be_created) created
      in
      let may_not_be_joined () =
        try
          not @@ List.mem other (ConcDomain.ThreadSet.elements must_joined)
        with _ -> true
      in
      not_self_read && (not (GobConfig.get_bool "exp.apron.priv.not-started") || (may_be_running ())) && (not (GobConfig.get_bool "exp.apron.priv.must-joined") || (may_not_be_joined ()))
    | _ -> true

  let get_relevant_writes (ask:Q.ask) m v =
    let current = ask.f Queries.CurrentThreadId in
    let must_joined = ask.f Queries.MustJoinedThreads in
    let compats = List.filter (fun (k,v) -> compatible ask current must_joined k) (GMutex.bindings v) in
    List.fold_left (fun acc (k,v) -> LAD.join acc (Cluster.keep_only_protected_globals ask m v)) (LAD.bot ()) compats

  let get_relevant_writes_nofilter (ask:Q.ask) v =
    let current = ask.f Queries.CurrentThreadId in
    let must_joined = ask.f Queries.MustJoinedThreads in
    let compats = List.filter (fun (k,v) -> compatible ask current must_joined k) (GMutex.bindings v) in
    List.fold_left (fun acc (k,v) -> LAD.join acc v) (LAD.bot ()) compats

  let merge_all v =
    let bs = List.map snd (GMutex.bindings v) in
    List.fold_left LAD.join (LAD.bot ()) bs

  let remove_globals_unprotected_after_unlock ask m oct =
    let newly_unprot var = match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g && is_unprotected_without ask g m
      | _ -> false
    in
    AD.remove_filter oct newly_unprot

  let keep_only_protected_globals ask m oct =
    let protected var =  match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g
      | _ -> false
    in
    AD.keep_filter oct protected

  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"


  let startstate () = W.bot (), LMust.top (), L.bot ()

  let should_join _ _ = true

  let mutex_inits = RichVarinfo.single ~name:"MUTEX_INITS"

  let get_m_with_mutex_inits inits ask getg_mutex m =
    let vi = mutex_addr_to_varinfo m in
    let get_m = get_relevant_writes ask m (getg_mutex vi) in
    if M.tracing then M.traceli "apronpriv" "get_m_with_mutex_inits %a\n  get=%a\n" LockDomain.Addr.pretty m LAD.pretty get_m;
    let r =
    if not inits then
      get_m
    else
      let get_mutex_inits = merge_all @@ getg_mutex (mutex_inits ()) in
      let get_mutex_inits' = Cluster.keep_only_protected_globals ask m get_mutex_inits in
      if M.tracing then M.trace "apronpriv" "inits=%a\n  inits'=%a\n" LAD.pretty get_mutex_inits LAD.pretty get_mutex_inits';
      LAD.join get_m get_mutex_inits'
    in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" LAD.pretty r;
    r

  let get_mutex_global_g_with_mutex_inits inits ask getg_mutex g =
    let vi = mutex_global g in
    let get_mutex_global_g = get_relevant_writes_nofilter ask @@ getg_mutex vi in
    if M.tracing then M.traceli "apronpriv" "get_mutex_global_g_with_mutex_inits %a\n  get=%a\n" CilType.Varinfo.pretty g LAD.pretty get_mutex_global_g;
    let r =
    if not inits then
      get_mutex_global_g
    else
      let get_mutex_inits = merge_all @@ getg_mutex (mutex_inits ()) in
      let get_mutex_inits' = Cluster.keep_global g get_mutex_inits in
      if M.tracing then M.trace "apronpriv" "inits=%a\n  inits'=%a\n" LAD.pretty get_mutex_inits LAD.pretty get_mutex_inits';
      LAD.join get_mutex_global_g get_mutex_inits'
    in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" LAD.pretty r;
    r

  let read_global ask getg (st: ApronComponents (D).t) g x: AD.t =
    let _,lmust,l = st.priv in
    let oct = st.oct in
    let mg = mutex_global g in
    (* lock *)
    let tmp = (get_mutex_global_g_with_mutex_inits (not (LMust.mem mg lmust)) ask getg g) in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt (mg) l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let tmp = Cluster.lock local_m tmp in
    let oct = AD.meet oct tmp in
    (* read *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars oct [g_var] in
    let oct_local = AD.assign_var oct_local x_var g_var in
    (* unlock *)
    let oct_local' =
      if is_unprotected ask g then
        AD.remove_vars oct_local [g_var]
      else
        oct_local
    in
    oct_local'

  let write_global ?(invariant=false) (ask:Q.ask) getg sideg (st: ApronComponents (D).t) g x: ApronComponents (D).t =
    let w,lmust,l = st.priv in
    let mg = mutex_global g in
    let oct = st.oct in
    (* lock *)
    let tmp = (get_mutex_global_g_with_mutex_inits (not (LMust.mem mg lmust)) ask getg g) in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt (mg) l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let tmp = Cluster.lock local_m tmp in
    let oct = AD.meet oct tmp in
    (* write *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let oct_local = AD.add_vars oct [g_var] in
    let oct_local = AD.assign_var oct_local g_var x_var in
    (* unlock *)
    let oct_side = AD.keep_vars oct_local [g_var] in
    let oct_side = Cluster.unlock (W.singleton g) oct_side in
    let tid = ask.f Queries.CurrentThreadId in
    let sidev = GMutex.singleton tid oct_side in
    sideg mg sidev;
    let l' = L.add mg oct_side l in
    let oct_local' =
      if is_unprotected ask g then
        AD.remove_vars oct_local [g_var]
      else
        oct_local
    in
    {oct = oct_local'; priv = (W.add g w,LMust.add mg lmust,l')}

  let lock ask getg (st: ApronComponents (D).t) m =
    let oct = st.oct in
    let _,lmust,l = st.priv in
    let m_v = (mutex_addr_to_varinfo m) in
    let get_m = get_m_with_mutex_inits (not (LMust.mem m_v lmust)) ask getg m in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt m_v l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let local_m = Cluster.keep_only_protected_globals ask m local_m in
    let r = Cluster.lock local_m get_m in
    if not (AD.is_bot r) then
      let oct' = AD.meet oct r in
      {st with oct = oct'}
    else
      st

  let unlock ask getg sideg (st: ApronComponents (D).t) m: ApronComponents (D).t =
    let oct = st.oct in
    let w,lmust,l = st.priv in
    let oct_local = remove_globals_unprotected_after_unlock ask m oct in
    let w' = W.filter (fun v -> not (is_unprotected_without ask v m)) w in
    let side_needed = W.exists (fun v -> is_protected_by ask m v) w in
    if not side_needed then
      {oct = oct_local; priv = (w',lmust,l)}
    else
      let oct_side = keep_only_protected_globals ask m oct in
      let oct_side = Cluster.unlock w oct_side in
      let tid = ask.f Queries.CurrentThreadId in
      let sidev = GMutex.singleton tid oct_side in
      let vi = mutex_addr_to_varinfo m in
      sideg (mutex_addr_to_varinfo m) sidev;
      let l' = L.add vi oct_side l in
      {oct = oct_local; priv = (w',LMust.add (mutex_addr_to_varinfo m) lmust,l')}

  let thread_join (ask:Q.ask) getg exp (st: ApronComponents (D).t) =
    let w,lmust,l = st.priv in
    try
      (* elements throws if the thread set is top *)
      let tids = ConcDomain.ThreadSet.elements (ask.f (Q.EvalThread exp)) in
      match tids with
      | [tid] ->
        let lmust',l' = getg tid in
        {st with priv = (w, LMust.union lmust' lmust, L.join l l')}
      | _ ->
        (* To match the paper more closely, one would have to join in the non-definite case too *)
        (* Given how we handle lmust (for initialization), doing this might actually be beneficial given that it grows lmust *)
        st
    with
    | _ -> st

  let thread_return ask getg sideg (st: ApronComponents (D).t) =
    (
      match ThreadId.get_current ask with
      | `Lifted tid when ThreadReturn.is_current ask ->
        let _,lmust,l = st.priv in
        sideg tid (lmust,l)
      | _ -> ()
    );
    st

  let sync (ask:Q.ask) getg sideg (st: ApronComponents (D).t) reason =
    match reason with
    | `Return -> st (* TODO: implement? *)
    | `Join ->
      if (ask.f Q.MustBeSingleThreaded) then
        st
      else
        let oct = st.oct in
        (* There can be no branched going multi-threaded here *)
        (* TODO: Do we need to remove no longer protected variables here? *)
        (* TODO: Is not potentially even unsound to do so?! *)
        let oct_local = AD.remove_filter oct (fun var ->
            match V.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with oct = oct_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let enter_multithreaded (ask:Q.ask) getg sideg (st: ApronComponents (D).t): ApronComponents (D).t =
    let oct = st.oct in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match V.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (AD.vars oct)
    in
    let oct_side = AD.keep_vars oct g_vars in
    let oct_side = Cluster.unlock (W.top ()) oct_side in
    let tid = ask.f Queries.CurrentThreadId in
    let sidev = GMutex.singleton tid oct_side in
    let vi = mutex_inits () in
    sideg vi sidev;
    (* Introduction into local state not needed, will be read via initializer *)
    (* Also no side-effetc to mutex globals needed, the value here will either by read via the initializer, *)
    (* or it will be locally overwitten and in LMust in which case these values are irrelevant anyway *)
    let oct_local = AD.remove_vars oct g_vars in
    {st with oct = oct_local}

  let threadenter ask getg (st: ApronComponents (D).t): ApronComponents (D).t =
    let _,lmust,l = st.priv in
    {oct = AD.bot (); priv = (W.bot (),lmust,l)}

  let finalize () = ()

  (* All that follows is stupid boilerplate to give each of these functions the getg and sideg that only deals with TIDs or Mutexes *)

  let sideg_mutex (sideg: varinfo -> G.t -> unit) (g:varinfo) (v:GMutex.t):unit =
    sideg g (`Lifted1 v)

  let sideg_tid (sideg:varinfo -> G.t -> unit) (tid:TID.t) (v:GThread.t):unit =
    sideg (TID.to_varinfo tid) (`Lifted2 v)

  let getg_mutex getg g = match getg g with
    | `Lifted1 v -> v
    | `Bot -> GMutex.bot ()
    | _ -> failwith "wrong either argument"

  let getg_tid getg tid = match getg (TID.to_varinfo tid) with
    | `Lifted2 v -> v
    | `Bot -> GThread.bot ()
    | _ -> failwith "wrong either argument"

  let patch_getside_mutex fn ask getg sideg = fn ask (getg_mutex getg) (sideg_mutex sideg)
  let patch_getside_tid fn ask getg sideg = fn ask (getg_tid getg) (sideg_tid sideg)

  let patch_get_mutex fn ask getg = fn ask (getg_mutex getg)
  let patch_get_tid fn ask getg = fn ask (getg_tid getg)

  let read_global = patch_get_mutex read_global
  let write_global ?(invariant=false) (ask:Q.ask) getg sideg = write_global ~invariant ask (getg_mutex getg) (sideg_mutex sideg)
  let lock = patch_get_mutex lock
  let unlock = patch_getside_mutex unlock
  let thread_join = patch_get_tid thread_join
  let thread_return = patch_getside_tid thread_return
  let sync = patch_getside_mutex sync
  let enter_multithreaded = patch_getside_mutex enter_multithreaded
  let threadenter = patch_get_mutex threadenter
end

module TracingPriv (Priv: S): S with module D = Priv.D =
struct
  include Priv

  module ApronComponents = ApronComponents (D)

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
         | "dummy" -> (module Dummy: S)
         | "protection" -> (module ProtectionBasedPriv (struct let path_sensitive = false end))
         | "protection-path" -> (module ProtectionBasedPriv (struct let path_sensitive = true end))
         | "mutex-meet" -> (module PerMutexMeetPriv)
         | "mutex-meet-tid" -> (module PerMutexMeetPrivTID (NoCluster))
         | "mutex-meet-tid-cluster12" -> (module PerMutexMeetPrivTID (Cluster12))
         | _ -> failwith "exp.apron.privatization: illegal value"
      )
    in
    let module Priv = TracingPriv (Priv) in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
