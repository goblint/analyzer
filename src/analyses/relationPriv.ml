open Prelude.Ana
open Analyses
open RelationDomain
open GobConfig
(* open BaseUtil *)
module Q = Queries

module RelationComponents = RelationDomain.RelComponents

open CommonPriv


module type S =
  functor (RD: RelationDomain.RD) ->
  sig
    module D: Lattice.S
    module G: Lattice.S
    module V: Printable.S
    type relation_components_t := RelationDomain.RelComponents (RD) (D).t
    val name: unit -> string
    val startstate: unit -> D.t
    val should_join: relation_components_t -> relation_components_t -> bool

    val read_global: Q.ask -> (V.t -> G.t) -> relation_components_t -> varinfo -> varinfo -> RD.t

    (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
      * the state when following conditional guards. *)
    val write_global: ?invariant:bool -> Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> varinfo -> varinfo -> relation_components_t

    val lock: Q.ask -> (V.t -> G.t) -> relation_components_t -> LockDomain.Addr.t -> relation_components_t
    val unlock: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> LockDomain.Addr.t -> relation_components_t

    val sync: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> [`Normal | `Join | `Return | `Init | `Thread] -> relation_components_t

    val escape: Node.t -> Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> EscapeDomain.EscapedVars.t -> relation_components_t
    val enter_multithreaded: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> relation_components_t
    val threadenter: Q.ask -> (V.t -> G.t) -> relation_components_t -> relation_components_t

    val thread_join: ?force:bool -> Q.ask -> (V.t -> G.t) -> Cil.exp -> apron_components_t -> apron_components_t
    val thread_return: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> ThreadIdDomain.Thread.t -> apron_components_t -> apron_components_t
    val iter_sys_vars: (V.t -> G.t) -> VarQuery.t -> V.t VarQuery.f -> unit (** [Queries.IterSysVars] for apron. *)

    val init: unit -> unit
    val finalize: unit -> unit
  end

(** Top privatization, which doesn't track globals at all.
    This is unlike base's "none" privatization. which does track globals, but doesn't privatize them. *)
module Top: S = functor (RD: RelationDomain.RD) ->
struct
  module D = Lattice.Unit
  module G = Lattice.Unit
  module V = EmptyV
  module AV = RD.V

  type relation_components_t = RelComponents (RD) (D).t

  let name () = "top"
  let startstate () = ()
  let should_join _ _ = true

  let read_global ask getg (st: relation_components_t) g x =
    let rel = st.rel in
    assert (not (RD.mem_var rel (AV.global g)));
    rel

  let write_global ?(invariant=false) ask getg sideg (st: relation_components_t) g x: relation_components_t =
    let rel = st.rel in
    assert (not (RD.mem_var rel (AV.global g)));
    st

    let lock ask getg st m = st
    let unlock ask getg sideg st m = st

  let thread_join ?(force=false) ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let escape node ask getg sideg (st:relation_components_t) escaped:relation_components_t =
    let rel = st.rel in
    let esc_vars = List.filter (fun var -> match AV.find_metadata var with
        | Some (Global _) -> false
        | Some Local ->
          (let fundec = Node.find_fundec node in
           let r = AV.to_cil_varinfo fundec var in
           match r with
           | Some r -> EscapeDomain.EscapedVars.mem r escaped
           | _ -> false)
        | _ -> false
      ) (RD.vars rel)
    in
    let apr_local = RD.remove_vars rel esc_vars in
    { st with rel = apr_local }

  let sync (ask: Q.ask) getg sideg (st: relation_components_t) reason =
    match reason with
    | `Join ->
      if (ask.f Q.MustBeSingleThreaded) then
        st
      else
        (* must be like enter_multithreaded *)
        let rel = st.rel in
        let rel_local = RD.remove_filter rel (fun var ->
            match AV.find_metadata var with
            | Some (Global _) -> true
            | _ -> false
          )
        in
        {st with rel = rel_local}
    | `Normal
    | `Init
    | `Thread
    | `Return ->
      st

  let enter_multithreaded ask getg sideg (st: relation_components_t): relation_components_t =
    let rel = st.rel in
    let apr_local = RD.remove_filter rel (fun var ->
        match AV.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      )
    in
    {st with rel = apr_local}

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    {rel = RD.bot (); priv = startstate ()}

  let iter_sys_vars getg vq vf = ()

  let init () = ()
  let finalize () = ()
end

module type ProtectionBasedPrivParam =
sig
  (** Whether to be path-sensitive w.r.t. locally written protected globals that have been continuously protected since writing. *)
  val path_sensitive: bool
end

(** Protection-Based Reading. Is unsound w.r.t. to locals escaping and becoming public. *)
module ProtectionBasedPriv (Param: ProtectionBasedPrivParam): S = functor (RD: RelationDomain.RD) ->
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
  module G = RD
  module V = Printable.UnitConf (struct let name = "global" end)

  type relation_components_t = RelationComponents (RD) (D).t

  module VM =
  struct
    type t =
      | Local of varinfo
      | Unprot of varinfo
      | Prot of varinfo

    let var_name = function
      | Local x -> x.vname ^ "#" ^ string_of_int(x.vid)
      | Unprot g -> g.vname ^ "#unprot"
      | Prot g -> g.vname ^ "#prot"
  end
  module AV =
  struct
    include RelationDomain.VarMetadataTbl (VM)(RD.Var)
    

    let local g = make_var (Local g)
    let unprot g = make_var (Unprot g)
    let prot g = make_var (Prot g)
  end

  let name () = "ProtectionBasedPriv"

  (** Restrict environment to global invariant variables. *)
  let restrict_global rel =
    RD.remove_filter rel (fun var ->
        match AV.find_metadata var with
        | Some (Unprot _ | Prot _) -> false
        | _ -> true
      )

  (** Restrict environment to local variables and still-protected global variables. *)
  let restrict_local is_unprot rel w_remove =
    let remove_local_vars = List.map AV.local (W.elements w_remove) in
    let rel' = RD.remove_vars rel remove_local_vars in
    (* remove global vars *)
    RD.remove_filter rel' (fun var ->
        match AV.find_metadata var with
        | Some (Unprot g | Prot g) -> is_unprot g
        | _ -> false
      )

  let startstate () = (P.empty (), W.empty ())

  let should_join (st1: relation_components_t) (st2: relation_components_t) =
    if Param.path_sensitive then (
      let (p1, _) = st1.priv in
      let (p2, _) = st2.priv in
      P.equal p1 p2
    )
    else
      true

  let read_global ask getg (st: relation_components_t) g x =
    let rel = st.rel in
    let (p, w) = st.priv in
    let g_local_var = AV.local g in
    let x_var = AV.local x in
    let rel_local =
      if W.mem g w then
        RD.assign_var rel x_var g_local_var
      else
        RD.bot ()
    in
    let rel_local' =
      if P.mem g p then
        rel_local
      else if is_unprotected ask g then (
        let g_unprot_var = AV.unprot g in
        let rel_unprot = RD.add_vars rel [g_unprot_var] in
        let rel_unprot = RD.assign_var rel_unprot x_var g_unprot_var in
        (* let oct_unprot' = RD.D2.join oct_local oct_unprot in
           (* unlock *)
           let oct_unprot' = RD.D2.remove_vars oct_unprot' [g_unprot_var; g_local_var] in
           (* add, assign from, remove is not equivalent to forget if g#unprot already existed and had some relations *)
           (* TODO: why removing g_unprot_var? *)
           oct_unprot' *)
        RD.join rel_local rel_unprot
      )
      else (
        let g_prot_var = AV.prot g in
        let rel_prot = RD.add_vars rel [g_prot_var] in
        let rel_prot = RD.assign_var rel_prot x_var g_prot_var in
        RD.join rel_local rel_prot
      )
    in
    let rel_local' = restrict_local (is_unprotected ask) rel_local' (W.empty ()) in
    let rel_local' = RD.meet rel_local' (getg ()) in
    rel_local'

  let write_global ?(invariant=false) ask getg sideg (st: relation_components_t) g x =
    let rel = st.rel in
    let (p, w) = st.priv in
    let g_local_var = AV.local g in
    let g_unprot_var = AV.unprot g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_local_var] in
    let rel_local = RD.assign_var rel_local g_local_var x_var in
    let rel_side = RD.add_vars rel_local [g_unprot_var] in
    let rel_side = RD.assign_var rel_side g_unprot_var g_local_var in
    let rel' = rel_side in
    let rel_side = restrict_global rel_side in
    sideg () rel_side;
    let st' =
      (* if is_unprotected ask g then
         st (* add, assign, remove gives original local state *)
         else
         (* restricting g#unprot-s out from oct' gives oct_local *)
         {oct = oct_local; priv = (P.add g p, W.add g w)} *)
      if is_unprotected ask g then
        {st with rel = restrict_local (is_unprotected ask) rel' (W.singleton g)}
      else (
        let p' = P.add g p in
        let w' = W.add g w in
        {rel = restrict_local (is_unprotected ask) rel' (W.empty ()); priv = (p', w')}
      )
    in
    let rel_local' = RD.meet st'.rel (getg ()) in
    {st' with rel = rel_local'}

  let lock ask getg (st: relation_components_t) m = st

  let unlock ask getg sideg (st: relation_components_t) m: relation_components_t =
    let rel = st.rel in
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
    let rel_side = List.fold_left (fun acc omega ->
        let g_prot_vars = List.map AV.prot omega in
        let g_local_vars = List.map AV.local omega in
        let rel_side1 = RD.add_vars rel g_prot_vars in
        let rel_side1 = RD.assign_var_parallel' rel_side1 g_prot_vars g_local_vars in
        RD.join acc rel_side1
      ) (RD.bot ()) big_omega
    in
    let rel' = rel_side in
    let rel_side = restrict_global rel_side in
    sideg () rel_side;
    let rel_local = restrict_local (fun g -> is_unprotected_without ask g m) rel' w_remove in
    let rel_local' = RD.meet rel_local (getg ()) in
    {rel = rel_local'; priv = (p', w')}


  let thread_join ?(force=false) ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let sync ask getg sideg (st: relation_components_t) reason =
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
        (* must be like enter_multithreaded *)
        let rel = st.rel in
        let (g_vars, gs) =
          RD.vars rel
          |> List.enum
          |> Enum.filter_map (fun var ->
              match RD.V.find_metadata var with
              | Some (Global g) -> Some (var, g)
              | _ -> None
            )
          |> Enum.uncombine
          |> Tuple2.map List.of_enum List.of_enum
        in
        let g_unprot_vars = List.map AV.unprot gs in
        let g_prot_vars = List.map AV.prot gs in
        let rel_side = RD.add_vars rel (g_unprot_vars @ g_prot_vars) in
        let rel_side = RD.assign_var_parallel' rel_side g_unprot_vars g_vars in
        let rel_side = RD.assign_var_parallel' rel_side g_prot_vars g_vars in
        let rel_side = restrict_global rel_side in
        sideg () rel_side;
        (* TODO: why not remove at all? should only remove unprotected? *)
        (* let rel_local = RD.remove_vars rel g_vars in
        let rel_local' = RD.meet rel_local (getg ()) in
        {st with rel = rel_local'} *)
        st
    | `Normal
    | `Init
    | `Thread ->
      st

  let escape node ask getg sideg st escaped = (* TODO: Implement *) st

  let enter_multithreaded ask getg sideg (st: relation_components_t): relation_components_t =
    let rel = st.rel in
    let (g_vars, gs) =
      RD.vars rel
      |> List.enum
      |> Enum.filter_map (fun var ->
          match RD.V.find_metadata var with
          | Some (Global g) -> Some (var, g)
          | _ -> None
        )
      |> Enum.uncombine
      |> Tuple2.map List.of_enum List.of_enum
    in
    let g_unprot_vars = List.map AV.unprot gs in
    let g_prot_vars = List.map AV.prot gs in
    let rel_side = RD.add_vars rel (g_unprot_vars @ g_prot_vars) in
    let rel_side = RD.assign_var_parallel' rel_side g_unprot_vars g_vars in
    let rel_side = RD.assign_var_parallel' rel_side g_prot_vars g_vars in
    let rel_side = restrict_global rel_side in
    sideg () rel_side;
    let rel_local = RD.remove_vars rel g_vars in
    let rel_local' = RD.meet rel_local (getg ()) in
    {rel = rel_local'; priv = startstate ()}

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    {rel = getg (); priv = startstate ()}

  let iter_sys_vars getg vq vf = () (* TODO: or report singleton global for any Global query? *)

  let finalize () = ()
end

module CommonPerMutex = functor(RD: RelationDomain.RD) ->
struct
  include Protection
  module V = RD.V

  let remove_globals_unprotected_after_unlock ask m oct =
    let newly_unprot var = match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g && is_unprotected_without ask g m
      | _ -> false
    in
    RD.remove_filter oct newly_unprot

  let keep_only_protected_globals ask m oct =
    let protected var = match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g
      | _ -> false
    in
    AD.keep_filter oct protected
end

(** Per-mutex meet. *)
module PerMutexMeetPriv : S = functor (RD: RelationDomain.RD) ->
struct
  open CommonPerMutex(RD)
  include MutexGlobals

  module D = Lattice.Unit
  module G = RD

  type relation_components_t = RelationDomain.RelComponents (RD) (D).t

  module AV = RD.V

  let name () = "PerMutexMeetPriv"

  let startstate () = ()

  let should_join _ _ = true

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (V.mutex m) in
    let get_mutex_inits = getg V.mutex_inits in
    let get_mutex_inits' = keep_only_protected_globals ask m get_mutex_inits in
    RD.join get_m get_mutex_inits'

  let get_mutex_global_g_with_mutex_inits ask getg g =
    let get_mutex_global_g = getg (V.global g) in
    let get_mutex_inits = getg V.mutex_inits in
    let g_var = AV.global g in
    let get_mutex_inits' = RD.keep_vars get_mutex_inits [g_var] in
    RD.join get_mutex_global_g get_mutex_inits'

  let read_global ask getg (st: relation_components_t) g x: RD.t =
    let rel = st.rel in
    (* lock *)
    let rel = RD.meet rel (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* read *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local x_var g_var in
    (* unlock *)
    let rel_local' =
      if is_unprotected ask g then
        RD.remove_vars rel_local [g_var]
      else
        rel_local
    in
    rel_local'

    let write_global ?(invariant=false) ask getg sideg (st: relation_components_t) g x: relation_components_t =
      let apr = st.rel in
      (* lock *)
      let apr = RD.meet apr (get_mutex_global_g_with_mutex_inits ask getg g) in
      (* write *)
      let g_var = AV.global g in
      let x_var = AV.local x in
      let apr_local = RD.add_vars apr [g_var] in
      let apr_local = RD.assign_var apr_local g_var x_var in
      (* unlock *)
      let apr_side = RD.keep_vars apr_local [g_var] in
      sideg (V.global g) apr_side;
      let apr_local' =
        if is_unprotected ask g then
          RD.remove_vars apr_local [g_var]
        else
          apr_local
      in
      {st with rel = apr_local'}

    let lock ask getg (st: relation_components_t) m =
      (* TODO: somehow actually unneeded here? *)
      if Locksets.(not (Lockset.mem m (current_lockset ask))) then (
        let rel = st.rel in
        let get_m = get_m_with_mutex_inits ask getg m in
        (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
        let get_m = keep_only_protected_globals ask m get_m in
        let rel' = RD.meet rel get_m in
        {st with rel = rel'}
      )
      else
        st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: relation_components_t) m: relation_components_t =
    let rel = st.rel in
    let rel_side = keep_only_protected_globals ask m rel in
    sideg (V.mutex m) rel_side;
    let rel_local = remove_globals_unprotected_after_unlock ask m rel in
    {st with rel = rel_local}

  let thread_join ?(force=false) ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let sync ask getg sideg (st: relation_components_t) reason =
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
        let rel = st.rel in
        let g_vars = List.filter (fun var ->
            match AV.find_metadata var with
            | Some (Global _) -> true
            | _ -> false
          ) (RD.vars rel)
        in
        let rel_side = RD.keep_vars rel g_vars in
        sideg V.mutex_inits rel_side;
        let rel_local = RD.remove_filter rel (fun var ->
            match AV.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with rel = rel_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let enter_multithreaded ask getg sideg (st: relation_components_t): relation_components_t =
    let rel = st.rel in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match AV.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (RD.vars rel)
    in
    let rel_side = RD.keep_vars rel g_vars in
    sideg V.mutex_inits rel_side;
    let rel_local = RD.remove_vars rel g_vars in (* TODO: side effect initial values to mutex_globals? *)
    {st with rel = rel_local}

  let escape node ask getg sideg (st:relation_components_t) escaped : relation_components_t =
    let esc_vars = EscapeDomain.EscapedVars.elements escaped in
    let esc_vars = List.filter (fun v -> not v.vglob && RD.varinfo_tracked v && RD.mem_var st.rel (AV.local v)) esc_vars in
    let escape_one (x:varinfo) st = write_global ask getg sideg st x x in
    List.fold_left (fun st v -> escape_one v st) st esc_vars

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    {rel = RD.bot (); priv = startstate ()}

  let init () = ()
  let finalize () = ()
end

(** May written variables. *)
module W =
struct
  include MayVars
  let name () = "W"
end

module type ClusterArg = functor (RD: RelationDomain.RD) ->
sig
  module LAD: Lattice.S

  val keep_only_protected_globals: Q.ask -> LockDomain.Addr.t -> LAD.t -> LAD.t
  val keep_global: varinfo -> LAD.t -> LAD.t

  val lock: RD.t -> LAD.t -> LAD.t -> RD.t
  val unlock: W.t -> RD.t -> LAD.t

  val name: unit -> string
end

(** No clustering. *)
module NoCluster:ClusterArg = functor (RD: RelationDomain.RD) ->
struct
  module AD = RD
  open CommonPerMutex(RD)
  module LAD = AD

  let keep_only_protected_globals = keep_only_protected_globals

  let keep_global g oct =
    let g_var = V.global g in
    RD.keep_vars oct [g_var]

  let lock oct local_m get_m =
    RD.meet oct (RD.join local_m get_m)

  let unlock w oct_side =
    oct_side

  let name () = "no-clusters"
end

module type ClusteringArg =
sig
  val generate: varinfo list -> varinfo list list
  val name: unit -> string
end

(** All clusters of size 1 and 2. *)
module Clustering12: ClusteringArg =
struct
  let generate gs =
    List.cartesian_product gs gs
    |> List.filter (fun (g1, g2) -> CilType.Varinfo.compare g1 g2 <= 0) (* filter flipped ordering, keep equals for next step *)
    |> List.map (fun (g1, g2) -> [g1; g2]) (* if g1 = g2, then we get a singleton cluster *)

  let name () = "cluster12"
end

(** All clusters of size 2. *)
module Clustering2: ClusteringArg =
struct
  let generate gs =
    match gs with
    | [_] -> [gs] (* use clusters of size 1 if only 1 variable *)
    | _ ->
      List.cartesian_product gs gs
      |> List.filter (fun (g1, g2) -> CilType.Varinfo.compare g1 g2 < 0) (* filter flipped ordering, forbid equals for just clusters of size 2 *)
      |> List.map (fun (g1, g2) -> [g1; g2])

  let name () = "cluster2"
end

(** All subset clusters. *)
module ClusteringPower: ClusteringArg =
struct
  let generate gs =
    gs
    |> List.map (fun _ -> [true; false])
    |> List.n_cartesian_product (* TODO: exponential! *)
    |> List.map (fun bs ->
        (* list globals where omega is true *)
        List.fold_left2 (fun acc g b ->
            if b then
              g :: acc
            else
              acc
          ) [] gs bs
      )

  let name () = "clusterPow"
end

(** One maximum cluster. *)
module ClusteringMax: ClusteringArg =
struct
  let generate gs =
    [gs]

  let name () = "clusterMax"
end


(** Clusters when clustering is downward-closed. *)
module DownwardClosedCluster (ClusteringArg: ClusteringArg) =  functor (RD: RelationDomain.RD) ->
struct
  open CommonPerMutex(RD)

  module VS =
  struct
    include Printable.Std
    include SetDomain.Make (CilType.Varinfo)
  end
  module LAD = MapDomain.MapBot (VS) (RD)

  let keep_only_protected_globals ask m octs =
    (* normal (strong) mapping: contains only still fully protected *)
    (* must filter by protection to avoid later meeting with non-protecting *)
    LAD.filter (fun gs _ ->
        VS.for_all (is_protected_by ask m) gs
      ) octs

  let keep_global g octs =
    let g_var = V.global g in
    (* normal (strong) mapping: contains only still fully protected *)
    let g' = VS.singleton g in
    let oct = LAD.find g' octs in
    LAD.singleton g' (RD.keep_vars oct [g_var])

  let lock_get_m oct local_m get_m =
    let joined = LAD.join local_m get_m in
    if M.tracing then M.traceli "relationpriv" "lock_get_m:\n  get=%a\n  joined=%a\n" LAD.pretty get_m LAD.pretty joined;
    let r = LAD.fold (fun _ -> RD.meet) joined (RD.bot ()) in (* bot is top with empty env *)
    if M.tracing then M.trace "relationpriv" "meet=%a\n" RD.pretty r;
    let r = RD.meet oct r in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RD.pretty r;
    r

  let lock oct local_m get_m =
    if M.tracing then M.traceli "relationpriv" "cluster lock: local=%a\n" LAD.pretty local_m;
    let r = lock_get_m oct local_m get_m in
    (* is_bot check commented out because it's unnecessarily expensive *)
    (* if RD.is_bot_env r then
       failwith "DownwardClosedCluster.lock: not downward closed?"; *)
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RD.pretty r;
    r

  let unlock w oct_side =
    let vars = RD.vars oct_side in
    let gs = List.map (fun var -> match V.find_metadata var with
        | Some (Global g) -> g
        | _ -> assert false (* oct_side should only contain (protected) globals *)
      ) vars
    in
    let clusters =
      ClusteringArg.generate gs
      |> List.map VS.of_list
      |> List.filter (VS.exists (fun g -> W.mem g w)) (* cluster intersection w is non-empty *)
    in
    let oct_side_cluster gs =
      RD.keep_vars oct_side (gs |> VS.elements |> List.map V.global)
    in
    LAD.add_list_fun clusters oct_side_cluster (LAD.empty ())

  let name = ClusteringArg.name
end

(** Clusters when clustering is arbitrary (not necessarily downward-closed). *)
module ArbitraryCluster (ClusteringArg: ClusteringArg): ClusterArg = functor (RD: RelationDomain.RD) ->
struct
  module DCCluster = (DownwardClosedCluster(ClusteringArg))(RD)

  open CommonPerMutex(RD)

  module VS = DCCluster.VS
  module LAD1 = DCCluster.LAD
  module LAD = Lattice.Prod (LAD1) (LAD1) (* second component is only used between keep_* and lock for additional weak mapping *)

  let name = ClusteringArg.name

  let filter_map' f m =
    LAD1.fold (fun k v acc ->
        match f k v with
        | Some (k', v') ->
          LAD1.add k' (RD.join (LAD1.find k' acc) v') acc
        | None ->
          acc
      ) m (LAD1.empty ())

  let keep_only_protected_globals ask m (octs, _) =
    let lad = DCCluster.keep_only_protected_globals ask m octs in
    let lad_weak =
      (* backup (weak) mapping: contains any still intersecting with protected, needed for decreasing protecting locksets *)
      filter_map' (fun gs oct ->
          (* must filter by protection to avoid later meeting with non-protecting *)
          let gs' = VS.filter (is_protected_by ask m) gs in
          if VS.is_empty gs' then
            None
          else
            (* must restrict cluster down to protected (join) *)
            Some (gs', keep_only_protected_globals ask m oct)
        ) octs
    in
    (lad, lad_weak)

  let keep_global g (octs, _) =
    let g_var = V.global g in
    let lad = DCCluster.keep_global g octs in
    let lad_weak =
      (* backup (weak) mapping: contains any still intersecting with protected, needed for decreasing protecting locksets *)
      filter_map' (fun gs oct ->
          (* must filter by protection to avoid later meeting with non-protecting *)
          if VS.mem g gs then
            (* must restrict cluster down to m_g (join) *)
            Some (VS.singleton g, RD.keep_vars oct [g_var])
          else
            None
        ) octs
    in
    (lad, lad_weak)

  let lock oct (local_m, _) (get_m, get_m') =
    if M.tracing then M.traceli "relationpriv" "cluster lock: local=%a\n" LAD1.pretty local_m;
    let r =
      let locked = DCCluster.lock_get_m oct local_m get_m in
      if RD.is_bot_env locked then (
        let locked' = DCCluster.lock_get_m oct local_m get_m' in
        if RD.is_bot_env locked' then
          raise Deadcode
        else
          locked'
      )
      else
        locked
    in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RD.pretty r;
    r

  let unlock w oct_side =
    (DCCluster.unlock w oct_side, LAD1.bot ())
end

(** Per-mutex meet with TIDs. *)
module PerMutexMeetPrivTID (Cluster: ClusterArg): S  = functor (RD: RelationDomain.RD) ->
struct
  open CommonPerMutex(RD)
  include MutexGlobals
  include ConfCheck.RequireThreadFlagPathSensInit

  module NC = Cluster(RD)
  module Cluster = NC
  module LAD = NC.LAD

  module LLock =
  struct
    include Printable.Either (Locksets.Lock) (CilType.Varinfo)
    let mutex m = `Left m
    let global x = `Right x
  end

  (* Map from locks to last written values thread-locally *)
  module L = MapDomain.MapBot_LiftTop (LLock) (LAD)

  module LMust = struct
    include SetDomain.Reverse (SetDomain.ToppedSet (LLock) (struct let topname = "All locks" end))
    let name () = "LMust"
  end

  module D = Lattice.Prod3 (W) (LMust) (L)
  module GMutex = MapDomain.MapBot_LiftTop (ThreadIdDomain.ThreadLifted) (LAD)
  module GThread = Lattice.Prod (LMust) (L)
  module G =
  struct
    include Lattice.Lift2 (GMutex) (GThread) (Printable.DefaultNames)

    let mutex = function
      | `Bot -> GMutex.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "PerMutexMeetPrivTID.mutex"
    let thread = function
      | `Bot -> GThread.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "PerMutexMeetPrivTID.thread"
    let create_mutex mutex = `Lifted1 mutex
    let create_global global = `Lifted1 global
    let create_thread thread = `Lifted2 thread
  end

  module AV = RD.V
  module TID = ThreadIdDomain.Thread

  module V =
  struct
    include Printable.Either (MutexGlobals.V) (TID)
    let mutex x = `Left (MutexGlobals.V.mutex x)
    let mutex_inits = `Left MutexGlobals.V.mutex_inits
    let global x = `Left (MutexGlobals.V.global x)
    let thread x = `Right x
  end

  let name () = "PerMutexMeetPrivTID(" ^ (Cluster.name ()) ^ (if GobConfig.get_bool "ana.relation.priv.must-joined" then  ",join"  else "") ^ ")"


  let compatible (ask:Q.ask) current must_joined other =
    match current, other with
    | `Lifted current, `Lifted other ->
      if (TID.is_unique current) && (TID.equal current other) then
        false (* self-read *)
      else if GobConfig.get_bool "ana.relation.priv.not-started" && MHP.definitely_not_started (current, ask.f Q.CreatedThreads) other then
        false (* other is not started yet *)
      else if GobConfig.get_bool "ana.relation.priv.must-joined" && MHP.must_be_joined other must_joined then
        false (* accounted for in local information *)
      else
        true
    | _ -> true

  let get_relevant_writes (ask:Q.ask) m v =
    let current = ThreadId.get_current ask in
    let must_joined = ask.f Queries.MustJoinedThreads in
    GMutex.fold (fun k v acc ->
        if compatible ask current must_joined k then
          LAD.join acc (Cluster.keep_only_protected_globals ask m v)
        else
          acc
      ) v (LAD.bot ())

  let get_relevant_writes_nofilter (ask:Q.ask) v =
    let current = ThreadId.get_current ask in
    let must_joined = ask.f Queries.MustJoinedThreads in
    GMutex.fold (fun k v acc ->
        if compatible ask current must_joined k then
          LAD.join acc v
        else
          acc
      ) v (LAD.bot ())

  let merge_all v =
    GMutex.fold (fun _ v acc -> LAD.join acc v) v (LAD.bot ())

  type relation_components_t =  RelationDomain.RelComponents (RD) (D).t


  let startstate () = W.bot (), LMust.top (), L.bot ()

  let should_join _ _ = true

  let get_m_with_mutex_inits inits ask getg m =
    let get_m = get_relevant_writes ask m (G.mutex @@ getg (V.mutex m)) in
    if M.tracing then M.traceli "relationpriv" "get_m_with_mutex_inits %a\n  get=%a\n" LockDomain.Addr.pretty m LAD.pretty get_m;
    let r =
      if not inits then
        get_m
      else
        let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
        let get_mutex_inits' = Cluster.keep_only_protected_globals ask m get_mutex_inits in
        if M.tracing then M.trace "relationpriv" "inits=%a\n  inits'=%a\n" LAD.pretty get_mutex_inits LAD.pretty get_mutex_inits';
        LAD.join get_m get_mutex_inits'
    in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" LAD.pretty r;
    r

  let get_mutex_global_g_with_mutex_inits inits ask getg g =
    let get_mutex_global_g = get_relevant_writes_nofilter ask @@ G.mutex @@ getg (V.global g) in
    if M.tracing then M.traceli "relationpriv" "get_mutex_global_g_with_mutex_inits %a\n  get=%a\n" CilType.Varinfo.pretty g LAD.pretty get_mutex_global_g;
    let r =
      if not inits then
        get_mutex_global_g
      else
        let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
        let get_mutex_inits' = Cluster.keep_global g get_mutex_inits in
        if M.tracing then M.trace "relationpriv" "inits=%a\n  inits'=%a\n" LAD.pretty get_mutex_inits LAD.pretty get_mutex_inits';
        LAD.join get_mutex_global_g get_mutex_inits'
    in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" LAD.pretty r;
    r

  let read_global ask getg (st: relation_components_t) g x: RD.t =
    let _,lmust,l = st.priv in
    let rel = st.rel in
    let lm = LLock.global g in
    (* lock *)
    let tmp = get_mutex_global_g_with_mutex_inits (not (LMust.mem lm lmust)) ask getg g in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt lm l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let rel = Cluster.lock rel local_m tmp in
    (* read *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local x_var g_var in
    (* unlock *)
    let rel_local' =
      if is_unprotected ask g then
        RD.remove_vars rel_local [g_var]
      else
        rel_local
    in
    rel_local'

  let write_global ?(invariant=false) (ask:Q.ask) getg sideg (st: relation_components_t) g x: relation_components_t =
    let w,lmust,l = st.priv in
    let lm = LLock.global g in
    let rel = st.rel in
    (* lock *)
    let tmp = get_mutex_global_g_with_mutex_inits (not (LMust.mem lm lmust)) ask getg g in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt lm l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let rel = Cluster.lock rel local_m tmp in
    (* write *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local g_var x_var in
    (* unlock *)
    let rel_side = RD.keep_vars rel_local [g_var] in
    let rel_side = Cluster.unlock (W.singleton g) rel_side in
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid rel_side in
    sideg (V.global g) (G.create_global sidev);
    let l' = L.add lm rel_side l in
    let rel_local' =
      if is_unprotected ask g then
        RD.remove_vars rel_local [g_var]
      else
        rel_local
    in
    {rel = rel_local'; priv = (W.add g w,LMust.add lm lmust,l')}

  let lock ask getg (st: relation_components_t) m =
    if Locksets.(not (Lockset.mem m (current_lockset ask))) then (
      let rel = st.rel in
      let _,lmust,l = st.priv in
      let lm = LLock.mutex m in
      let get_m = get_m_with_mutex_inits (not (LMust.mem lm lmust)) ask getg m in
      let local_m = BatOption.default (LAD.bot ()) (L.find_opt lm l) in
      (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
      let local_m = Cluster.keep_only_protected_globals ask m local_m in
      let rel = Cluster.lock rel local_m get_m in
      {st with rel = rel}
    )
    else
      st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: relation_components_t) m: relation_components_t =
    let rel = st.rel in
    let w,lmust,l = st.priv in
    let rel_local = remove_globals_unprotected_after_unlock ask m rel in
    let w' = W.filter (fun v -> not (is_unprotected_without ask v m)) w in
    let side_needed = W.exists (fun v -> is_protected_by ask m v) w in
    if not side_needed then
      {rel = rel_local; priv = (w',lmust,l)}
    else
      let rel_side = keep_only_protected_globals ask m rel in
      let rel_side = Cluster.unlock w rel_side in
      let tid = ThreadId.get_current ask in
      let sidev = GMutex.singleton tid rel_side in
      sideg (V.mutex m) (G.create_mutex sidev);
      let lm = LLock.mutex m in
      let l' = L.add lm rel_side l in
      {rel = rel_local; priv = (w',LMust.add lm lmust,l')}

  let thread_join ?(force=false) (ask:Q.ask) getg exp (st: apron_components_t) =
    let w,lmust,l = st.priv in
    let tids = ask.f (Q.EvalThread exp) in
    if force then (
      if ConcDomain.ThreadSet.is_top tids then (
        M.info ~category:Unsound "Unknown thread ID assume-joined, Apron privatization unsound"; (* TODO: something more sound *)
        st (* cannot find all thread IDs to join them all *)
      )
      else (
        (* fold throws if the thread set is top *)
        let tids' = ConcDomain.ThreadSet.diff tids (ask.f Q.MustJoinedThreads) in (* avoid unnecessary imprecision by force joining already must-joined threads, e.g. 46-apron2/04-other-assume-inprec *)
        let (lmust', l') = ConcDomain.ThreadSet.fold (fun tid (lmust, l) ->
            let lmust',l' = G.thread (getg (V.thread tid)) in
            (LMust.union lmust' lmust, L.join l l')
          ) tids' (lmust, l)
        in
        {st with priv = (w, lmust', l')}
      )
    )
    else (
      if ConcDomain.ThreadSet.is_top tids then
        st (* TODO: why needed? *)
      else (
        (* elements throws if the thread set is top *)
        let tids = ConcDomain.ThreadSet.elements tids in
        match tids with
        | [tid] ->
          let lmust',l' = G.thread (getg (V.thread tid)) in
          {st with priv = (w, LMust.union lmust' lmust, L.join l l')}
        | _ ->
          (* To match the paper more closely, one would have to join in the non-definite case too *)
          (* Given how we handle lmust (for initialization), doing this might actually be beneficial given that it grows lmust *)
          st
      )
    )

  let thread_return ask getg sideg tid (st: relation_components_t) =
    let _,lmust,l = st.priv in
    sideg (V.thread tid) (G.create_thread (lmust,l));
    st

  let sync (ask:Q.ask) getg sideg (st: relation_components_t) reason =
    match reason with
    | `Return -> st (* TODO: implement? *)
    | `Join ->
      if (ask.f Q.MustBeSingleThreaded) then
        st
      else
        let rel = st.rel in
        (* There can be no branched going multi-threaded here *)
        (* TODO: Do we need to remove no longer protected variables here? *)
        (* TODO: Is not potentially even unsound to do so?! *)
        let rel_local = RD.remove_filter rel (fun var ->
            match AV.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with rel = rel_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let escape node ask getg sideg (st: relation_components_t) escaped: relation_components_t =
    let esc_vars = EscapeDomain.EscapedVars.elements escaped in
    let esc_vars = List.filter (fun v -> not v.vglob && RD.varinfo_tracked v && RD.mem_var st.rel (AV.local v)) esc_vars in
    let escape_one (x:varinfo) st = write_global ask getg sideg st x x in
    List.fold_left (fun st v -> escape_one v st) st esc_vars

  let enter_multithreaded (ask:Q.ask) getg sideg (st: relation_components_t): relation_components_t =
    let rel = st.rel in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match AV.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (RD.vars rel)
    in
    let rel_side = RD.keep_vars rel g_vars in
    let rel_side = Cluster.unlock (W.top ()) rel_side in (* top W to avoid any filtering *)
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid rel_side in
    sideg V.mutex_inits (G.create_mutex sidev);
    (* Introduction into local state not needed, will be read via initializer *)
    (* Also no side-effect to mutex globals needed, the value here will either by read via the initializer, *)
    (* or it will be locally overwitten and in LMust in which case these values are irrelevant anyway *)
    let rel_local = RD.remove_vars rel g_vars in
    {st with rel = rel_local}

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    let _,lmust,l = st.priv in
    {rel = RD.bot (); priv = (W.bot (),lmust,l)}

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g -> vf (V.global g)
    | _ -> ()

  let finalize () = ()
end

module TracingPriv = functor (Priv: S) -> functor (RD: RelationDomain.RD) ->
struct
  module Priv = Priv (RD)
  include Priv

  module D = Priv.D
  module RelComponents = RelationDomain.RelComponents (RD) (D)

  let read_global ask getg st g x =
    if M.tracing then M.traceli "relationpriv" "read_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let r = Priv.read_global ask getg st g x in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RD.pretty r;
    r

  let write_global ?invariant ask getg sideg st g x =
    if M.tracing then M.traceli "relationpriv" "write_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = write_global ?invariant ask getg sideg st g x in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RelComponents.pretty r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "relationpriv" "lock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RelComponents.pretty r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "relationpriv" "unlock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = unlock ask getg sideg st m in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RelComponents.pretty r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "relationpriv" "enter_multithreaded\n";
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = enter_multithreaded ask getg sideg st in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RelComponents.pretty r;
    r

  let threadenter ask getg st =
    if M.tracing then M.traceli "relationpriv" "threadenter\n";
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let r = threadenter ask getg st in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RelComponents.pretty r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "relationpriv" "sync\n";
    if M.tracing then M.trace "relationpriv" "st: %a\n" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a\n" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a\n" V.pretty x G.pretty v;
      sideg x v
    in
    let r = sync ask getg sideg st reason in
    if M.tracing then M.traceu "relationpriv" "-> %a\n" RelComponents.pretty r;
    r
end

let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "ana.relation.privatization" with
         | "top" -> (module Top : S)
         | "protection" -> (module ProtectionBasedPriv (struct let path_sensitive = false end))
         | "protection-path" -> (module ProtectionBasedPriv (struct let path_sensitive = true end))
         | "mutex-meet" -> (module PerMutexMeetPriv)
         | "mutex-meet-tid" -> (module PerMutexMeetPrivTID (NoCluster))
         | "mutex-meet-tid-cluster12" -> (module PerMutexMeetPrivTID (DownwardClosedCluster (Clustering12)))
         | "mutex-meet-tid-cluster2" -> (module PerMutexMeetPrivTID (ArbitraryCluster (Clustering2)))
         | "mutex-meet-tid-cluster-max" -> (module PerMutexMeetPrivTID (ArbitraryCluster (ClusteringMax)))
         | "mutex-meet-tid-cluster-power" -> (module PerMutexMeetPrivTID (DownwardClosedCluster (ClusteringPower)))
         | _ -> failwith "ana.relation.privatization: illegal value"
      )
    in
    let module Priv = TracingPriv (Priv)  in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
