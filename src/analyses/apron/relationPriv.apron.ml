(** Relational thread-modular value analyses for {!RelationAnalysis}, i.e. {!ApronAnalysis} and {!AffineEqualityAnalysis}.

    @see <https://doi.org/10.1007/978-3-031-30044-8_2> Schwarz, M., Saan, S., Seidl, H., Erhard, J., Vojdani, V. Clustered Relational Thread-Modular Abstract Interpretation with Local Traces. *)

open Batteries
open GoblintCil
open Analyses
open RelationDomain
open GobConfig
(* open BaseUtil *)
module Q = Queries

module RelationComponents = RelationDomain.RelComponents

module VarQuery = Goblint_constraint.VarQuery

open CommonPriv


module type S =
  functor (RD: RelationDomain.RD) ->
  sig
    module D: Lattice.S
    module G: Lattice.S
    module V: Printable.S
    module P: DisjointDomain.Representative with type elt := D.t (** Path-representative. *)

    type relation_components_t := RelationDomain.RelComponents (RD) (D).t
    val name: unit -> string
    val startstate: unit -> D.t

    val read_global: Q.ask -> (V.t -> G.t) -> relation_components_t -> varinfo -> varinfo -> RD.t

    (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
       the state when following conditional guards. *)
    val write_global: ?invariant:bool -> Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> varinfo -> varinfo -> relation_components_t

    val lock: Q.ask -> (V.t -> G.t) -> relation_components_t -> LockDomain.MustLock.t -> relation_components_t
    val unlock: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> LockDomain.MustLock.t -> relation_components_t

    val sync: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> [`Normal | `Join | `JoinCall of CilType.Fundec.t | `Return | `Init | `Thread] -> relation_components_t

    val escape: Node.t -> Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> EscapeDomain.EscapedVars.t -> relation_components_t
    val enter_multithreaded: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> relation_components_t -> relation_components_t
    val threadenter: Q.ask -> (V.t -> G.t) -> relation_components_t -> relation_components_t

    val thread_join: ?force:bool -> Q.ask -> (V.t -> G.t) -> Cil.exp -> relation_components_t -> relation_components_t
    val thread_return: Q.ask -> (V.t -> G.t) -> (V.t -> G.t -> unit) -> ThreadIdDomain.Thread.t -> relation_components_t -> relation_components_t
    val iter_sys_vars: (V.t -> G.t) -> VarQuery.t -> V.t VarQuery.f -> unit (** [Queries.IterSysVars] for apron. *)

    val invariant_global: Q.ask -> (V.t -> G.t) -> V.t -> Invariant.t
    (** Returns flow-insensitive invariant for global unknown. *)

    val invariant_vars: Q.ask -> (V.t -> G.t) -> relation_components_t -> varinfo list
    (** Returns global variables which are privatized. *)

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
  module P = UnitP

  type relation_components_t = RelComponents (RD) (D).t

  let name () = "top"
  let startstate () = ()

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
    let rel_local = RD.remove_filter rel (fun var ->
        match AV.find_metadata var with
        | Some (Global _) -> false
        | Some (Local r) -> EscapeDomain.EscapedVars.mem r escaped
        | _ -> false
      )
    in
    { st with rel = rel_local }

  let sync (ask: Q.ask) getg sideg (st: relation_components_t) reason =
    let branched_sync () =
      if ask.f (Q.MustBeSingleThreaded {since_start = true}) then
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
    in
    match reason with
    | `Join when ConfCheck.branched_thread_creation () ->
      branched_sync ()
    | `JoinCall _ when ConfCheck.branched_thread_creation () ->
      branched_sync ()
    | `Join
    | `JoinCall _
    | `Normal
    | `Init
    | `Thread
    | `Return ->
      st

  let enter_multithreaded ask getg sideg (st: relation_components_t): relation_components_t =
    let rel = st.rel in
    let rel_local = RD.remove_filter rel (fun var ->
        match AV.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      )
    in
    {st with rel = rel_local}

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    {rel = RD.top (); priv = startstate ()}

  let iter_sys_vars getg vq vf = ()
  let invariant_global ask getg g = Invariant.none
  let invariant_vars ask getg st = []

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
  open struct
    module P =
    struct
      include MustVars
      let name () = "P"
    end
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
  module PS =
  struct
    include Printable.Option (P) (struct let name = "None" end)

    let of_elt (p, _) =
      if Param.path_sensitive then
        Some p
      else
        None
  end

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
    include RelationDomain.VarMetadataTbl (VM)

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

  let sync (ask:Q.ask) getg sideg (st: relation_components_t) reason =
    let branched_sync () =
      if ask.f (Q.MustBeSingleThreaded { since_start= true }) then
        st
      else
        (* must be like enter_multithreaded *)
        let rel = st.rel in
        let (g_vars, gs) =
          RD.vars rel
          |> List.to_seq
          |> Seq.filter_map (fun var ->
              match RD.V.find_metadata var with
              | Some (Global g) -> Some (var, g)
              | _ -> None
            )
          |> Seq.unzip
          |> Tuple2.map List.of_seq List.of_seq
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
    in
    match reason with
    | `Return -> (* required for thread return *)
      (* TODO: implement? *)
      begin match ThreadId.get_current ask with
        | `Lifted x (* when CPA.mem x st.cpa *) ->
          st
        | _ ->
          st
      end
    | `Join when ConfCheck.branched_thread_creation () ->
      branched_sync ()
    | `JoinCall f when ConfCheck.branched_thread_creation_at_call ask f ->
      branched_sync ()
    | `Join
    | `JoinCall _
    | `Normal
    | `Init
    | `Thread ->
      st

  let escape node ask getg sideg st escaped = (* TODO: Implement *) st

  let enter_multithreaded ask getg sideg (st: relation_components_t): relation_components_t =
    let rel = st.rel in
    let (g_vars, gs) =
      RD.vars rel
      |> List.to_seq
      |> Seq.filter_map (fun var ->
          match RD.V.find_metadata var with
          | Some (Global g) -> Some (var, g)
          | _ -> None
        )
      |> Seq.unzip
      |> Tuple2.map List.of_seq List.of_seq
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
  let invariant_global ask getg g = Invariant.none
  let invariant_vars ask getg st = protected_vars ask ~kind:Write (* TODO: is this right? *)

  let finalize () = ()

  module P = PS
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
    RD.keep_filter oct protected
end

module PerMutexMeetPrivBase (RD: RelationDomain.RD) =
struct
  let invariant_vars ask getg (st: (RD.t, _) RelationDomain.relcomponents_t) =
    (* Mutex-meet local states contain precisely the protected global variables,
       so we can do fewer queries than {!protected_vars}. *)
    RD.vars st.rel
    |> List.filter_map (fun var ->
        match RD.V.find_metadata var with
        | Some (Global g) -> Some g
        | _ -> None
      )
end

(** Per-mutex meet. *)
module PerMutexMeetPriv (Param: AtomicParam) : S = functor (RD: RelationDomain.RD) ->
struct
  open CommonPerMutex(RD)
  include MutexGlobals
  include PerMutexMeetPrivBase (RD)

  module D = Lattice.Unit
  module G = RD
  module P = UnitP

  type relation_components_t = RelationDomain.RelComponents (RD) (D).t

  module AV = RD.V

  let name () = "PerMutexMeetPriv"

  let startstate () = ()

  let atomic_mutex = LockDomain.MustLock.of_var LibraryFunctions.verifier_atomic_var

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (V.mutex m) in
    let get_mutex_inits = getg V.mutex_inits in
    let get_mutex_inits' = keep_only_protected_globals ask m get_mutex_inits in
    RD.join get_m get_mutex_inits'

  let get_mutex_global_g_with_mutex_inits ask getg g =
    let g_var = AV.global g in
    let get_mutex_global_g =
      if Param.handle_atomic then (
        (* Unprotected invariant is one big relation. *)
        RD.keep_vars (getg (V.mutex atomic_mutex)) [g_var]
      )
      else
        getg (V.global g)
    in
    let get_mutex_inits = getg V.mutex_inits in
    let get_mutex_inits' = RD.keep_vars get_mutex_inits [g_var] in
    if RD.mem_var get_mutex_global_g g_var && not (RD.mem_var get_mutex_inits' g_var) then (* TODO: is this just a workaround for an escape bug? https://github.com/goblint/analyzer/pull/1354/files#r1498882657 *)
      (* This is an escaped variable whose value was never side-effected to get_mutex_inits' *)
      get_mutex_global_g
    else
      RD.join get_mutex_global_g get_mutex_inits'

  let get_mutex_global_g_with_mutex_inits_atomic ask getg =
    (* Unprotected invariant is one big relation. *)
    let get_mutex_global_g = getg (V.mutex atomic_mutex) in
    let get_mutex_inits = getg V.mutex_inits in
    RD.join get_mutex_global_g get_mutex_inits

  let read_global (ask: Q.ask) getg (st: relation_components_t) g x: RD.t =
    let atomic = Param.handle_atomic && ask.f MustBeAtomic in
    let rel = st.rel in
    (* lock *)
    let rel =
      if atomic && RD.mem_var rel (AV.global g) then
        rel (* Read previous unpublished unprotected write in current atomic section. *)
      else if atomic then
        RD.meet rel (get_mutex_global_g_with_mutex_inits_atomic ask getg) (* Read unprotected invariant as full relation. *)
      else
        RD.meet rel (get_mutex_global_g_with_mutex_inits ask getg g)
    in
    (* read *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local x_var g_var in
    (* unlock *)
    if not atomic then (
      let rel_local' =
        if is_unprotected ask g then
          RD.remove_vars rel_local [g_var]
        else
          rel_local
      in
      rel_local'
    )
    else
      rel_local (* Keep write local as if it were protected by the atomic section. *)

  (** Like write global but has option to skip meet with current value, as the value will not have been side-effected to a useful location thus far *)
  let write_global_internal ?(skip_meet=false)  ?(invariant=false) (ask: Q.ask) getg sideg (st: relation_components_t) g x: relation_components_t =
    let atomic = Param.handle_atomic && ask.f MustBeAtomic in
    let rel = st.rel in
    (* lock *)
    let rel =
      if skip_meet then
        rel
      else if atomic && RD.mem_var rel (AV.global g) then
        rel (* Read previous unpublished unprotected write in current atomic section. *)
      else if atomic then
        RD.meet rel (get_mutex_global_g_with_mutex_inits_atomic ask getg) (* Read unprotected invariant as full relation. *)
      else
        RD.meet rel (get_mutex_global_g_with_mutex_inits ask getg g)
    in
    (* write *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local g_var x_var in
    (* unlock *)
    if not atomic then (
      let rel_side = RD.keep_vars rel_local [g_var] in
      if Param.handle_atomic then
        sideg (V.mutex atomic_mutex) rel_side (* Unprotected invariant is one big relation. *)
      else
        sideg (V.global g) rel_side;
      let rel_local' =
        if is_unprotected ask g then
          RD.remove_vars rel_local [g_var]
        else
          rel_local
      in
      {st with rel = rel_local'}
    )
    else
      (* Delay publishing unprotected write in the atomic section. *)
      {st with rel = rel_local} (* Keep write local as if it were protected by the atomic section. *)


  let write_global = write_global_internal ~skip_meet:false
  let write_escape = write_global_internal ~skip_meet:true

  let lock ask getg (st: relation_components_t) m =
    let atomic = Param.handle_atomic && LockDomain.MustLock.equal m atomic_mutex in
    (* TODO: somehow actually unneeded here? *)
    if not atomic && Locksets.(not (MustLockset.mem m (current_lockset ask))) then (
      let rel = st.rel in
      let get_m = get_m_with_mutex_inits ask getg m in
      (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
      let get_m = keep_only_protected_globals ask m get_m in
      let rel' = RD.meet rel get_m in
      {st with rel = rel'}
    )
    else
      (* Atomic section locking is recursive. *)
      st (* sound w.r.t. recursive lock *)

  let unlock ask getg sideg (st: relation_components_t) m: relation_components_t =
    let atomic = Param.handle_atomic && LockDomain.MustLock.equal m atomic_mutex in
    let rel = st.rel in
    if not atomic then (
      let rel_side = keep_only_protected_globals ask m rel in
      sideg (V.mutex m) rel_side;
      let rel_local = remove_globals_unprotected_after_unlock ask m rel in
      {st with rel = rel_local}
    )
    else (
      (* Publish delayed unprotected write as if it were protected by the atomic section. *)
      let rel_side = RD.keep_filter rel (fun var ->
          match AV.find_metadata var with
          | Some (Global g) -> true
          | _ -> false
        )
      in
      (* Unprotected invariant is one big relation. *)
      (* If no globals are contained here, none need to be published *)
      (* https://github.com/goblint/analyzer/pull/1354 *)
      if RD.vars rel_side <> [] then
        sideg (V.mutex atomic_mutex) rel_side;
      let rel_local =
        let newly_unprot var = match AV.find_metadata var with
          | Some (Global g) -> is_unprotected_without ask g atomic_mutex
          | _ -> false
        in
        RD.remove_filter rel newly_unprot
      in
      {st with rel = rel_local}
    )

  let thread_join ?(force=false) ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let sync (ask:Q.ask) getg sideg (st: relation_components_t) reason =
    let branched_sync () =
      if ask.f (Q.MustBeSingleThreaded {since_start = true}) then
        st
      else
        let rel = st.rel in
        (* Replace with remove_filter once issues are fixed *)
        let g_vars = List.filter (fun var ->
            match AV.find_metadata var with
            | Some (Global _) -> true
            | _ -> false
          ) (RD.vars rel)
        in
        let rel_side = RD.keep_vars rel g_vars in
        (* If no globals are contained here, none need to be published *)
        (* https://github.com/goblint/analyzer/pull/1354 *)
        if g_vars <> [] then sideg V.mutex_inits rel_side;
        let rel_local = RD.remove_filter rel (fun var ->
            match AV.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with rel = rel_local}
    in
    match reason with
    | `Return -> (* required for thread return *)
      (* TODO: implement? *)
      begin match ThreadId.get_current ask with
        | `Lifted x (* when CPA.mem x st.cpa *) ->
          st
        | _ ->
          st
      end
    | `Join when ConfCheck.branched_thread_creation () ->
      branched_sync ()
    | `JoinCall f when ConfCheck.branched_thread_creation_at_call ask f ->
      branched_sync ()
    | `Join
    | `JoinCall _
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
    let esc_vars = List.filter (fun v -> not v.vglob && RD.Tracked.varinfo_tracked v && RD.mem_var st.rel (AV.local v)) esc_vars in
    let escape_one (x:varinfo) st = write_escape ask getg sideg st x x in
    List.fold_left (fun st v -> escape_one v st) st esc_vars

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    {rel = RD.top (); priv = startstate ()}

  let init () = ()
  let finalize () = ()

  let invariant_global (ask: Q.ask) (getg: V.t -> G.t): V.t -> Invariant.t = function
    | `Left m' -> (* mutex *)
      let atomic = LockDomain.MustLock.equal m' (LockDomain.MustLock.of_var LibraryFunctions.verifier_atomic_var) in
      if atomic || ask.f (GhostVarAvailable (Locked m')) then (
        (* filters like query_invariant *)
        let one_var = GobConfig.get_bool "ana.relation.invariant.one-var" in
        let exact = GobConfig.get_bool "witness.invariant.exact" in

        let rel = keep_only_protected_globals ask m' (get_m_with_mutex_inits ask getg m') in (* Could be more precise if mutex_inits invariant is added by disjunction instead of joining abstract values. *)
        let inv =
          RD.invariant rel
          |> List.to_seq
          |> Seq.filter_map (fun (lincons1: Apron.Lincons1.t) ->
              (* filter one-vars and exact *)
              (* RD.invariant simplifies two octagon SUPEQ constraints to one EQ, so exact works *)
              if (one_var || GobApron.Lincons1.num_vars lincons1 >= 2) && (exact || Apron.Lincons1.get_typ lincons1 <> EQ) then
                RD.cil_exp_of_lincons1 lincons1
                |> Option.filter (InvariantCil.exp_is_suitable ?scope:None)
              else
                None
            )
          |> Seq.fold_left (fun acc x -> Invariant.(acc && of_exp x)) Invariant.none
        in
        if atomic then
          inv
        else (
          let var = WitnessGhost.to_varinfo (Locked m') in
          Invariant.(of_exp (Lval (GoblintCil.var var)) || inv) [@coverage off] (* bisect_ppx cannot handle redefined (||) *)
        )
      )
      else
        Invariant.none
    | g -> (* global *)
      Invariant.none (* Could output unprotected one-variable (so non-relational) invariants, but probably not very useful. [BasePriv] does those anyway. *)
end

(** May written variables. *)
module W =
struct
  include MayVars
  let name () = "W"
end

module type ClusterArg = functor (RD: RelationDomain.RD) ->
sig
  module LRD: Lattice.S

  module Cluster: Printable.S

  val keep_only_protected_globals: Q.ask -> LockDomain.MustLock.t -> LRD.t -> LRD.t
  val keep_global: varinfo -> LRD.t -> LRD.t

  val lock: RD.t -> LRD.t -> LRD.t -> RD.t
  val unlock: W.t -> RD.t -> LRD.t * (Cluster.t list)

  val filter_clusters: (Cluster.t -> bool) -> LRD.t -> LRD.t

  val name: unit -> string
end

(** No clustering. *)
module NoCluster:ClusterArg = functor (RD: RelationDomain.RD) ->
struct
  open CommonPerMutex(RD)
  module LRD = RD
  module Cluster = Printable.Unit

  let keep_only_protected_globals = keep_only_protected_globals

  let keep_global g oct =
    let g_var = V.global g in
    RD.keep_vars oct [g_var]

  let lock oct local_m get_m =
    RD.meet oct (RD.join local_m get_m)

  let unlock w oct_side =
    oct_side, [()]

  let filter_clusters f oct =
    if f () then
      oct
    else
      RD.bot ()

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

  module VS = SetDomain.Make (CilType.Varinfo)
  module LRD = MapDomain.MapBot (VS) (RD)

  module Cluster = VS

  let keep_only_protected_globals ask m octs =
    (* normal (strong) mapping: contains only still fully protected *)
    (* must filter by protection to avoid later meeting with non-protecting *)
    LRD.filter (fun gs _ ->
        VS.for_all (is_protected_by ask m) gs
      ) octs

  let keep_global g octs =
    let g_var = V.global g in
    (* normal (strong) mapping: contains only still fully protected *)
    let g' = VS.singleton g in
    (* If there is no map entry yet which contains the global, default to top rather than bot *)
    (* Happens e.g. in 46/86 because of escape *)
    let oct = Option.default (RD.top ()) (LRD.find_opt g' octs) in
    LRD.singleton g' (RD.keep_vars oct [g_var])

  let lock_get_m oct local_m get_m =
    let joined = LRD.join local_m get_m in
    if M.tracing then M.traceli "relationpriv" "lock_get_m:\n  get=%a\n  joined=%a" LRD.pretty get_m LRD.pretty joined;
    let r = LRD.fold (fun _ -> RD.meet) joined (RD.top ()) in
    if M.tracing then M.trace "relationpriv" "meet=%a" RD.pretty r;
    let r = RD.meet oct r in
    if M.tracing then M.traceu "relationpriv" "-> %a" RD.pretty r;
    r

  let lock oct local_m get_m =
    if M.tracing then M.traceli "relationpriv" "cluster lock: local=%a" LRD.pretty local_m;
    let r = lock_get_m oct local_m get_m in
    (* is_bot check commented out because it's unnecessarily expensive *)
    (* if RD.is_bot_env r then
       failwith "DownwardClosedCluster.lock: not downward closed?"; *)
    if M.tracing then M.traceu "relationpriv" "-> %a" RD.pretty r;
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
    (LRD.add_list_fun clusters oct_side_cluster (LRD.empty ()), clusters)

  let filter_clusters f oct =
    LRD.filter (fun gs _ -> f gs) oct

  let name = ClusteringArg.name
end

(** Clusters when clustering is arbitrary (not necessarily downward-closed). *)
module ArbitraryCluster (ClusteringArg: ClusteringArg): ClusterArg = functor (RD: RelationDomain.RD) ->
struct
  module DCCluster = (DownwardClosedCluster(ClusteringArg))(RD)

  open CommonPerMutex(RD)

  module VS = DCCluster.VS
  module LRD1 = DCCluster.LRD
  module LRD = Lattice.Prod (LRD1) (LRD1) (* second component is only used between keep_* and lock for additional weak mapping *)

  module Cluster = DCCluster.Cluster

  let name = ClusteringArg.name

  let filter_map' f m =
    LRD1.fold (fun k v acc ->
        match f k v with
        | Some (k', v') ->
          LRD1.add k' (RD.join (LRD1.find k' acc) v') acc
        | None ->
          acc
      ) m (LRD1.empty ())

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
    if M.tracing then M.traceli "relationpriv" "cluster lock: local=%a" LRD1.pretty local_m;
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
    if M.tracing then M.traceu "relationpriv" "-> %a" RD.pretty r;
    r

  let unlock w oct_side =
    let lad, clusters = DCCluster.unlock w oct_side in
    ((lad, LRD1.bot ()), clusters)

  let filter_clusters f (lad,lad') =
    (LRD1.filter (fun gs _ -> f gs) lad, LRD1.filter (fun gs _ -> f gs) lad')
end

(** Per-mutex meet with TIDs. *)
module PerMutexMeetPrivTID (Param: AtomicParam) (Digest: Digest) (Cluster: ClusterArg): S  = functor (RD: RelationDomain.RD) ->
struct
  open CommonPerMutex(RD)
  include MutexGlobals
  include PerMutexMeetPrivBase (RD)

  module NC = Cluster(RD)
  module Cluster = NC
  module LRD = NC.LRD

  include PerMutexTidCommon (Digest) (LRD) (NC.Cluster)

  module AV = RD.V
  module P = UnitP

  let name () = "PerMutexMeetPrivTID(" ^ (Cluster.name ()) ^ (if GobConfig.get_bool "ana.relation.priv.must-joined" then  ",join"  else "") ^ ")"

  let get_relevant_writes (ask:Q.ask) m v =
    let current = Digest.current ask in
    GMutex.fold (fun k v acc ->
        if not (Digest.accounted_for ask ~current ~other:k) then
          LRD.join acc (Cluster.keep_only_protected_globals ask m v)
        else
          acc
      ) v (LRD.bot ())

  type relation_components_t =  RelationDomain.RelComponents (RD) (D).t

  let get_m_with_mutex_inits inits ask getg m =
    let get_m = get_relevant_writes ask m (G.mutex @@ getg (V.mutex m)) in
    if M.tracing then M.traceli "relationpriv" "get_m_with_mutex_inits %a\n  get=%a" LockDomain.MustLock.pretty m LRD.pretty get_m;
    let r =
      let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
      let get_mutex_inits' = Cluster.keep_only_protected_globals ask m get_mutex_inits in
      let get_mutex_inits' = Cluster.filter_clusters inits get_mutex_inits' in
      if M.tracing then M.trace "relationpriv" "inits=%a\n  inits'=%a" LRD.pretty get_mutex_inits LRD.pretty get_mutex_inits';
      LRD.join get_m get_mutex_inits'
    in
    if M.tracing then M.traceu "relationpriv" "-> %a" LRD.pretty r;
    r

  let atomic_mutex = LockDomain.MustLock.of_var LibraryFunctions.verifier_atomic_var

  let get_mutex_global_g_with_mutex_inits inits ask getg g =
    let get_mutex_global_g =
      if Param.handle_atomic then (
        (* Unprotected invariant is one big relation. *)
        get_relevant_writes_nofilter ask @@ G.mutex @@ getg (V.mutex atomic_mutex)
        |> Cluster.keep_global g
      )
      else
        get_relevant_writes_nofilter ask @@ G.mutex @@ getg (V.global g)
    in
    if M.tracing then M.traceli "relationpriv" "get_mutex_global_g_with_mutex_inits %a\n  get=%a" CilType.Varinfo.pretty g LRD.pretty get_mutex_global_g;
    let r =
      let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
      let get_mutex_inits' = Cluster.keep_global g get_mutex_inits in
      let get_mutex_inits' = Cluster.filter_clusters inits get_mutex_inits' in
      if M.tracing then M.trace "relationpriv" "inits=%a\n  inits'=%a" LRD.pretty get_mutex_inits LRD.pretty get_mutex_inits';
      LRD.join get_mutex_global_g get_mutex_inits'
    in
    if M.tracing then M.traceu "relationpriv" "-> %a" LRD.pretty r;
    r

  let get_mutex_global_g_with_mutex_inits_atomic inits ask getg =
    (* Unprotected invariant is one big relation. *)
    let get_mutex_global_g = get_relevant_writes_nofilter ask @@ G.mutex @@ getg (V.mutex atomic_mutex) in
    let get_mutex_inits = merge_all @@ G.mutex @@ getg V.mutex_inits in
    let get_mutex_inits' = Cluster.filter_clusters inits get_mutex_inits in
    LRD.join get_mutex_global_g get_mutex_inits'

  let read_global (ask: Q.ask) getg (st: relation_components_t) g x: RD.t =
    let atomic = Param.handle_atomic && ask.f MustBeAtomic in
    let _,lmust,l = st.priv in
    let rel = st.rel in
    let lm = LLock.global g in
    (* lock *)
    let local_m = BatOption.default (LRD.bot ()) (L.find_opt lm l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let rel =
      if atomic && RD.mem_var rel (AV.global g) then
        rel (* Read previous unpublished unprotected write in current atomic section. *)
      else if atomic then
        Cluster.lock rel local_m (get_mutex_global_g_with_mutex_inits_atomic (fun c -> (not (LMust.mem (lm,c) lmust))) ask getg) (* Read unprotected invariant as full relation. *)
      else
        Cluster.lock rel local_m (get_mutex_global_g_with_mutex_inits (fun c -> (not (LMust.mem (lm,c) lmust))) ask getg g)
    in
    (* read *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local x_var g_var in
    (* unlock *)
    if not atomic then (
      let rel_local' =
        if is_unprotected ask g then
          RD.remove_vars rel_local [g_var]
        else
          rel_local
      in
      rel_local'
    )
    else
      rel_local (* Keep write local as if it were protected by the atomic section. *)

  let write_global ?(invariant=false) (ask:Q.ask) getg sideg (st: relation_components_t) g x: relation_components_t =
    let atomic = Param.handle_atomic && ask.f MustBeAtomic in
    let w,lmust,l = st.priv in
    let lm = LLock.global g in
    let rel = st.rel in
    (* lock *)
    let local_m = BatOption.default (LRD.bot ()) (L.find_opt lm l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let rel =
      if atomic && RD.mem_var rel (AV.global g) then
        rel (* Read previous unpublished unprotected write in current atomic section. *)
      else if atomic then
        Cluster.lock rel local_m (get_mutex_global_g_with_mutex_inits_atomic (fun c -> (not (LMust.mem (lm,c) lmust))) ask getg) (* Read unprotected invariant as full relation. *)
      else
        Cluster.lock rel local_m (get_mutex_global_g_with_mutex_inits (fun c -> (not (LMust.mem (lm,c) lmust))) ask getg g)
    in
    (* write *)
    let g_var = AV.global g in
    let x_var = AV.local x in
    let rel_local = RD.add_vars rel [g_var] in
    let rel_local = RD.assign_var rel_local g_var x_var in
    (* unlock *)
    if not atomic then (
      let rel_side = RD.keep_vars rel_local [g_var] in
      let rel_side, clusters = Cluster.unlock (W.singleton g) rel_side in
      let digest = Digest.current ask in
      let sidev = GMutex.singleton digest rel_side in
      if Param.handle_atomic then
        sideg (V.mutex atomic_mutex) (G.create_global sidev) (* Unprotected invariant is one big relation. *)
      else
        sideg (V.global g) (G.create_global sidev);
      let l' = L.add lm rel_side l in
      let rel_local' =
        if is_unprotected ask g then
          RD.remove_vars rel_local [g_var]
        else
          rel_local
      in
      let lmust' = List.fold (fun a c -> LMust.add (lm,c) a) lmust clusters in
      {rel = rel_local'; priv = (W.add g w,lmust',l')}
    )
    else
      (* Delay publishing unprotected write in the atomic section. *)
      {rel = rel_local; priv = (W.add g w,lmust,l)} (* Keep write local as if it were protected by the atomic section. *)

  let lock ask getg (st: relation_components_t) m =
    let atomic = Param.handle_atomic && LockDomain.MustLock.equal m atomic_mutex in
    if not atomic && Locksets.(not (MustLockset.mem m (current_lockset ask))) then (
      let rel = st.rel in
      let _,lmust,l = st.priv in
      let lm = LLock.mutex m in
      let get_m = get_m_with_mutex_inits (fun c -> (not (LMust.mem (lm,c) lmust))) ask getg m in
      let local_m = BatOption.default (LRD.bot ()) (L.find_opt lm l) in
      (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
      let local_m = Cluster.keep_only_protected_globals ask m local_m in
      let rel = Cluster.lock rel local_m get_m in
      {st with rel}
    )
    else
      (* Atomic section locking is recursive. *)
      st (* sound w.r.t. recursive lock *)

  let keep_only_globals ask m oct =
    let protected var = match AV.find_metadata var with
      | Some (Global g) -> true
      | _ -> false
    in
    RD.keep_filter oct protected

  let unlock ask getg sideg (st: relation_components_t) m: relation_components_t =
    let atomic = Param.handle_atomic && LockDomain.MustLock.equal m atomic_mutex in
    let rel = st.rel in
    let w,lmust,l = st.priv in
    if not atomic then (
      let rel_local = remove_globals_unprotected_after_unlock ask m rel in
      let w' = W.filter (fun v -> not (is_unprotected_without ask v m)) w in
      let side_needed = W.exists (fun v -> is_protected_by ask m v) w in
      if not side_needed then
        {rel = rel_local; priv = (w',lmust,l)}
      else
        let rel_side = keep_only_protected_globals ask m rel in
        let rel_side, clusters = Cluster.unlock w rel_side in
        let digest = Digest.current ask in
        let sidev = GMutex.singleton digest rel_side in
        sideg (V.mutex m) (G.create_mutex sidev);
        let lm = LLock.mutex m in
        let l' = L.add lm rel_side l in
        let lmust' = List.fold (fun a c -> LMust.add (lm,c) a) lmust clusters in
        {rel = rel_local; priv = (w',lmust',l')}
    )
    else (
      (* Publish delayed unprotected write as if it were protected by the atomic section. *)
      let rel_local = remove_globals_unprotected_after_unlock ask m rel in
      let w' = W.filter (fun v -> not (is_unprotected_without ask v m)) w in
      let side_needed = not (W.is_empty w) in
      if not side_needed then
        {rel = rel_local; priv = (w',lmust,l)}
      else
        let rel_side = keep_only_globals ask m rel in
        let rel_side, clusters = Cluster.unlock w rel_side in
        let digest = Digest.current ask in
        let sidev = GMutex.singleton digest rel_side in
        (* Unprotected invariant is one big relation. *)
        sideg (V.mutex atomic_mutex) (G.create_mutex sidev);
        let (lmust', l') = W.fold (fun g (lmust, l) ->
            let lm = LLock.global g in
            let lmust'' = List.fold (fun a c -> LMust.add (lm,c) a) lmust clusters in
            (lmust'', L.add lm rel_side l)
          ) w (lmust, l)
        in
        {rel = rel_local; priv = (w',lmust',l')}
    )

  let thread_join ?(force=false) (ask:Q.ask) getg exp (st: relation_components_t) =
    let w,lmust,l = st.priv in
    let tids = ask.f (Q.EvalThread exp) in
    if force then (
      if ConcDomain.ThreadSet.is_top tids then (
        M.info ~category:Unsound "Unknown thread ID assume-joined, relation privatization unsound"; (* TODO: something more sound *)
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
        st
      else
        match ConcDomain.ThreadSet.elements tids with
        | [tid] ->
          let lmust',l' = G.thread (getg (V.thread tid)) in
          {st with priv = (w, LMust.union lmust' lmust, L.join l l')}
        | _ ->
          (* To match the paper more closely, one would have to join in the non-definite case too *)
          (* Given how we handle lmust (for initialization), doing this might actually be beneficial given that it grows lmust *)
          st
    )

  let thread_return ask getg sideg tid (st: relation_components_t) =
    let _,lmust,l = st.priv in
    sideg (V.thread tid) (G.create_thread (lmust,l));
    st

  let sync (ask:Q.ask) getg sideg (st: relation_components_t) reason =
    let branched_sync () =
      if ask.f (Q.MustBeSingleThreaded {since_start = true}) then
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
    in
    match reason with
    | `Return -> st (* TODO: implement? *)
    | `Join when ConfCheck.branched_thread_creation () ->
      branched_sync ()
    | `JoinCall f when ConfCheck.branched_thread_creation_at_call ask f ->
      branched_sync ()
    | `Join
    | `JoinCall _
    | `Normal
    | `Init
    | `Thread ->
      st

  let escape node ask getg sideg (st: relation_components_t) escaped: relation_components_t =
    let esc_vars = EscapeDomain.EscapedVars.elements escaped in
    let esc_vars = List.filter (fun v -> not v.vglob && RD.Tracked.varinfo_tracked v && RD.mem_var st.rel (AV.local v)) esc_vars in
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
    let rel_side, clusters = Cluster.unlock (W.top ()) rel_side in (* top W to avoid any filtering *)
    let digest = Digest.current ask in
    let sidev = GMutex.singleton digest rel_side in
    sideg V.mutex_inits (G.create_mutex sidev);
    (* Introduction into local state not needed, will be read via initializer *)
    (* Also no side-effect to mutex globals needed, the value here will either by read via the initializer, *)
    (* or it will be locally overwitten and in LMust in which case these values are irrelevant anyway *)
    let rel_local = RD.remove_vars rel g_vars in
    {st with rel = rel_local}

  let threadenter ask getg (st: relation_components_t): relation_components_t =
    let _,lmust,l = st.priv in
    {rel = RD.top (); priv = (W.bot (),lmust,l)}

  let iter_sys_vars getg vq vf =
    match vq with
    | VarQuery.Global g -> vf (V.global g)
    | _ -> ()

  let finalize () = ()

  let invariant_global ask getg g = Invariant.none
end

module TracingPriv = functor (Priv: S) -> functor (RD: RelationDomain.RD) ->
struct
  module Priv = Priv (RD)
  include Priv

  module D = Priv.D
  module RelComponents = RelationDomain.RelComponents (RD) (D)

  let read_global ask getg st g x =
    if M.tracing then M.traceli "relationpriv" "read_global %a %a" CilType.Varinfo.pretty g CilType.Varinfo.pretty x;
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let r = Priv.read_global ask getg st g x in
    if M.tracing then M.traceu "relationpriv" "-> %a" RD.pretty r;
    r

  let write_global ?invariant ask getg sideg st g x =
    if M.tracing then M.traceli "relationpriv" "write_global %a %a" CilType.Varinfo.pretty g CilType.Varinfo.pretty x;
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a" V.pretty x G.pretty v;
      sideg x v
    in
    let r = write_global ?invariant ask getg sideg st g x in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "relationpriv" "lock %a" LockDomain.MustLock.pretty m;
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "relationpriv" "unlock %a" LockDomain.MustLock.pretty m;
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a" V.pretty x G.pretty v;
      sideg x v
    in
    let r = unlock ask getg sideg st m in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "relationpriv" "enter_multithreaded";
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a" V.pretty x G.pretty v;
      sideg x v
    in
    let r = enter_multithreaded ask getg sideg st in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let threadenter ask getg st =
    if M.tracing then M.traceli "relationpriv" "threadenter";
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let r = threadenter ask getg st in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "relationpriv" "sync";
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a" V.pretty x G.pretty v;
      sideg x v
    in
    let r = sync ask getg sideg st reason in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let escape node ask getg sideg st vs =
    if M.tracing then M.traceli "relationpriv" "escape";
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a" V.pretty x G.pretty v;
      sideg x v
    in
    let r = escape node ask getg sideg st vs in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let thread_join ?force ask getg e st =
    if M.tracing then M.traceli "relationpriv" "thread_join";
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let r = thread_join ?force ask getg e st in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r

  let thread_return ask getg sideg tid st =
    if M.tracing then M.traceli "relationpriv" "thread_return";
    if M.tracing then M.trace "relationpriv" "st: %a" RelComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "relationpriv" "getg %a -> %a" V.pretty x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "relationpriv" "sideg %a %a" V.pretty x G.pretty v;
      sideg x v
    in
    let r = thread_return ask getg sideg tid st in
    if M.tracing then M.traceu "relationpriv" "-> %a" RelComponents.pretty r;
    r
end


let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "ana.relation.privatization" with
         | "top" -> (module Top : S)
         | "protection" -> (module ProtectionBasedPriv (struct let path_sensitive = false end))
         | "protection-path" -> (module ProtectionBasedPriv (struct let path_sensitive = true end))
         | "mutex-meet" -> (module PerMutexMeetPriv (NoAtomic))
         | "mutex-meet-atomic" -> (module PerMutexMeetPriv (struct let handle_atomic = true end)) (* experimental *)
         | "mutex-meet-tid" -> (module PerMutexMeetPrivTID (NoAtomic) (ThreadDigest) (NoCluster))
         | "mutex-meet-tid-atomic" -> (module PerMutexMeetPrivTID (struct let handle_atomic = true end) (ThreadDigest) (NoCluster)) (* experimental *)
         | "mutex-meet-tid-cluster12" -> (module PerMutexMeetPrivTID (NoAtomic) (ThreadDigest) (DownwardClosedCluster (Clustering12)))
         | "mutex-meet-tid-cluster2" -> (module PerMutexMeetPrivTID (NoAtomic) (ThreadDigest) (ArbitraryCluster (Clustering2)))
         | "mutex-meet-tid-cluster-max" -> (module PerMutexMeetPrivTID (NoAtomic) (ThreadDigest) (ArbitraryCluster (ClusteringMax)))
         | "mutex-meet-tid-cluster-power" -> (module PerMutexMeetPrivTID (NoAtomic) (ThreadDigest) (DownwardClosedCluster (ClusteringPower)))
         | _ -> failwith "ana.relation.privatization: illegal value"
      )
    in
    let module Priv = TracingPriv (Priv)  in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
