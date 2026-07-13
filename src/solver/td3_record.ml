(** Incremental/interactive terminating top-down solver, like [td3] but with consolidated per-unknown solver data ([td3_record]).

    Functionally identical to [td3], but all per-unknown solver state (value, stability, widening gas, influences, dependencies, ...)
    is stored in a single hashmap of records instead of many separate hashmaps, reducing the number of hashtable lookups.
    This is analogous to what [td_simplified_ref] does for [td_simplified].

    Intentional differences to [td3]:
    - [solvers.td3.narrow-globs.enabled] is not supported, because [Td3UpdateRule.Narrow] requires td3's separate hashmaps. Fails fast.
    - [solvers.td3.side_widen = unstable_called] is not supported, because it iterates td3's separate called hashmap. Fails fast.
    - The marshaled incremental data format differs, so incremental runs must save and load with [td3_record] on both sides.
    - Pruning of incremental data also removes [sides] and [weak_dep] of deleted/unreachable unknowns, which td3 keeps around.

    @see <https://doi.org/10.1017/S0960129521000499> Seidl, H., Vogler, R. Three improvements to the top-down solver.
    @see <https://arxiv.org/abs/2209.10445> Interactive Abstract Interpretation: Reanalyzing Whole Programs for Cheap. *)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes
open Goblint_constraint.Translators
open Messages

module M = Messages

module type Hooks =
sig
  module S: DemandEqConstrSys
  module HM: Hashtbl.S with type key = S.v

  val print_data: unit -> unit
  (** Print additional solver data statistics. *)

  val system: S.v -> ((S.v -> S.d) -> (S.v -> S.d -> unit) -> (S.v -> unit) -> S.d) option
  (** Wrap [S.system]. Always use this hook instead of [S.system]! *)

  val delete_marked: S.v list -> unit
  (** Incrementally delete additional solver data. *)

  val stable_remove: S.v -> unit
  (** Remove additional solver data when variable removed from [stable]. *)

  val prune: reachable:unit HM.t -> unit
  (** Prune unreachable additional solver data. *)
end

module Base =
  functor (Arg: IncrSolverArg) ->
  functor (S:DemandEqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (Hooks: Hooks with module S = S and module HM = HM) ->
  functor (UpdateRule: Td3UpdateRule.S) ->
  struct
    open SolverBox.Warrow (S.Dom)
    module EqS0 = EqConstrSysFromDemandConstrSys (S)
    include Generic.SolverStats (EqS0) (HM)
    module VS = Set.Make (S.Var)

    module UpdateRule = UpdateRule(EqS0) (HM) (VS)

    let assert_can_receive_side x =
      if Hooks.system x <> None then (
        failwith ("side-effect to unknown w/ rhs: " ^ GobPretty.sprint S.Var.pretty_trace x);
      )

    (** All solver data for a single unknown. Replaces td3's separate hashmaps, such that one lookup gives access to everything. *)
    type var_data = {
      mutable value: S.Dom.t; (** Analogous to td3's [rho] value. Only meaningful if [in_rho]. *)
      mutable in_rho: bool; (** Whether the unknown would be in td3's [rho]. Only differs from mere record existence in space mode, where values of evaluated non-widening-points are not kept. *)
      mutable stable: bool;
      mutable superstable: bool; (** In incremental load, initially stable and never destabilized. Analogous to td3's [superstable]. These don't have to be re-verified and warnings can be reused. *)
      mutable called: bool;
      mutable wpoint_gas: int option; (** [None] if not a widening point, otherwise the remaining widening gas. Tracks the gas of both side-effected and non-side-effected variables. Although they may have different gas budgets, they can share the field since no side-effected variable may ever have a rhs. *)
      mutable infl: VS.t;
      mutable sides: VS.t;
      mutable side_dep: VS.t; (** Dependencies of side-effected variables. Knowing these allows restarting them and re-triggering all side effects. *)
      mutable side_infl: VS.t; (** Influences to side-effected variables. Not normally in [infl], but used for restarting them. *)
      mutable dep: VS.t; (** Dependencies of variables. Inverse of [infl]. Used for fast pre-reachable pruning in incremental postsolving. *)
      mutable weak_dep: VS.t; (** Weak dependencies of variables via [demand] (if enabled). *)
    }

    let create_var_data () = {
      value = S.Dom.bot ();
      in_rho = false;
      stable = false;
      superstable = false;
      called = false;
      wpoint_gas = None;
      infl = VS.empty;
      sides = VS.empty;
      side_dep = VS.empty;
      side_infl = VS.empty;
      dep = VS.empty;
      weak_dep = VS.empty;
    }

    let copy_var_data r = {
      value = r.value;
      in_rho = r.in_rho;
      stable = r.stable;
      superstable = r.superstable;
      called = r.called;
      wpoint_gas = r.wpoint_gas;
      infl = r.infl;
      sides = r.sides;
      side_dep = r.side_dep;
      side_infl = r.side_infl;
      dep = r.dep;
      weak_dep = r.weak_dep;
    }

    type solver_data = {
      st: (S.Var.t * S.Dom.t) list; (* needed to destabilize start functions if their start state changed because of some changed global initializer *)
      data: var_data HM.t; (** All per-unknown solver data. *)
      update_rule_data: UpdateRule.data;
      var_messages: Message.t HM.t; (** Messages from right-hand sides of variables. Used for incremental postsolving. Contains intentional duplicate keys, so kept as separate multimap. *)
      rho_write: S.Dom.t HM.t HM.t; (** Side effects from variables to write-only variables with values. Used for fast incremental restarting of write-only variables. Inner maps contain intentional duplicate keys. *)
    }

    type marshal = solver_data

    let create_empty_data () = {
      st = [];
      data = HM.create 10;
      update_rule_data = UpdateRule.create_empty_data ();
      var_messages = HM.create 10;
      rho_write = HM.create 10;
    }

    let print_data data =
      let (rho_n, stable_n, wpoint_n) =
        HM.fold (fun _ r (rho_n, stable_n, wpoint_n) ->
            ((if r.in_rho then rho_n + 1 else rho_n),
             (if r.stable then stable_n + 1 else stable_n),
             (if r.wpoint_gas <> None then wpoint_n + 1 else wpoint_n))
          ) data.data (0, 0, 0)
      in
      Logs.debug "|data|=%d" (HM.length data.data);
      Logs.debug "|rho|=%d" rho_n;
      Logs.debug "|stable|=%d" stable_n;
      Logs.debug "|wpoint_gas|=%d" wpoint_n;
      Logs.debug "|var_messages|=%d" (HM.length data.var_messages);
      Logs.debug "|rho_write|=%d" (HM.length data.rho_write);
      Hooks.print_data ()

    let print_data_verbose data str =
      if Logs.Level.should_log Debug then (
        Logs.debug "%s:" str;
        print_data data
      )

    let verify_data data =
      if GobConfig.get_bool "solvers.td3.verify" then (
        (* every variable in (pruned) rho should be stable *)
        HM.iter (fun x r ->
            if r.in_rho && not r.stable then (
              Logs.warn "unstable in rho: %a" S.Var.pretty_trace x;
              assert false
            )
          ) data.data
        (* vice versa doesn't currently hold, because stable is not pruned *)
      )

    let copy_marshal (data: marshal): marshal =
      {
        st = data.st; (* data.st is immutable *)
        data = HM.map (fun _ r -> copy_var_data r) data.data;
        update_rule_data = UpdateRule.copy_marshal data.update_rule_data;
        var_messages = HM.copy data.var_messages;
        rho_write = HM.map (fun x w -> HM.copy w) data.rho_write; (* map copies outer HM *)
      }

    (* The following hack is for fixing hashconsing.
       If hashcons is enabled now, then it also was for the loaded values (otherwise it would crash). If it is off, we don't need to do anything.
       HashconsLifter uses BatHashcons.hashcons on Lattice operations like join, so we call join (with bot) to make sure that the old values will populate the empty hashcons table via side-effects and at the same time get new tags that are conform with its state.
       The tags are used for `equals` and `compare` to avoid structural comparisons. TODO could this be replaced by `==` (if values are shared by hashcons they should be physically equal)?
       We have to replace all tags since they are not derived from the value (like hash) but are incremented starting with 1, i.e. dependent on the order in which lattice operations for different values are called, which will very likely be different for an incremental run.
       If we didn't do this, during solve, a rhs might give the same value as from the old rho but it wouldn't be detected as equal since the tags would be different.
       In the worst case, every rhs would yield the same value, but we would destabilize for every var in rho until we replaced all values (just with new tags).
       The other problem is that we would likely use more memory since values from old rho would not be shared with the same values in the hashcons table. So we would keep old values in memory until they are replace in rho and eventually garbage collected. *)
    (* Another problem are the tags for the context part of a S.Var.t.
       This will cause problems when old and new vars interact or when new S.Dom values are used as context:
       - reachability is a problem since it marks vars reachable with a new tag, which will remove vars with the same context but old tag from rho.
       - If we destabilized a node with a call, we will also destabilize all vars of the called function. However, if we end up with the same state at the caller node, without hashcons we would only need to go over all vars in the function once to restabilize them since we have
         the old values, whereas with hashcons, we would get a context with a different tag, could not find the old value for that var, and have to recompute all vars in the function (without access to old values). *)
    let relift_var_data r = {
      value = S.Dom.relift r.value;
      in_rho = r.in_rho;
      stable = r.stable;
      superstable = r.superstable;
      called = r.called;
      wpoint_gas = r.wpoint_gas;
      infl = VS.map S.Var.relift r.infl;
      sides = VS.map S.Var.relift r.sides;
      side_dep = VS.map S.Var.relift r.side_dep;
      side_infl = VS.map S.Var.relift r.side_infl;
      dep = VS.map S.Var.relift r.dep;
      weak_dep = VS.map S.Var.relift r.weak_dep;
    }

    let relift_marshal (data: marshal): marshal =
      let data' = HM.create (HM.length data.data) in
      HM.iter (fun k r ->
          (* call hashcons on contexts and abstract values; results in new tags *)
          HM.replace data' (S.Var.relift k) (relift_var_data r)
        ) data.data;
      let update_rule_data = UpdateRule.relift_marshal data.update_rule_data in
      let st = List.map (fun (k, v) -> S.Var.relift k, S.Dom.relift v) data.st in
      let var_messages = HM.create (HM.length data.var_messages) in
      HM.iter (fun k v ->
          HM.add var_messages (S.Var.relift k) v (* var_messages contains duplicate keys, so must add not replace! *)
        ) data.var_messages;
      let rho_write = HM.create (HM.length data.rho_write) in
      HM.iter (fun x w ->
          let w' = HM.create (HM.length w) in
          HM.iter (fun y d ->
              HM.add w' (S.Var.relift y) (S.Dom.relift d) (* w contains duplicate keys, so must add not replace! *)
            ) w;
          HM.replace rho_write (S.Var.relift x) w';
        ) data.rho_write;
      {st; data = data'; update_rule_data; var_messages; rho_write}

    type phase = Widen | Narrow [@@deriving show] (* used in inner solve *)

    module CurrentVarS = Goblint_constraint.ConstrSys.CurrentVarDemandEqConstrSys (S)
    module S = CurrentVarS.S
    module EqS = EqConstrSysFromDemandConstrSys (S) (* new S, so must construct new EqS *)

    let solve st vs marshal =
      if GobConfig.get_bool "solvers.td3.narrow-globs.enabled" then
        failwith "solver td3_record does not support solvers.td3.narrow-globs (use td3)";
      (match GobConfig.get_string "solvers.td3.side_widen" with
       | "unstable_called" | "unstable-called" -> failwith "solver td3_record does not support solvers.td3.side_widen unstable_called (use td3)"
       | _ -> ());

      let reuse_stable = GobConfig.get_bool "incremental.stable" in
      let reuse_wpoint = GobConfig.get_bool "incremental.wpoint" in
      let solver_data =
        match marshal with
        | Some solver_data ->
          if not reuse_stable then (
            Logs.info "Destabilizing everything!";
            HM.iter (fun _ r ->
                r.stable <- false;
                r.infl <- VS.empty
              ) solver_data.data
          );
          if not reuse_wpoint then (
            HM.iter (fun _ r ->
                r.wpoint_gas <- None;
                r.sides <- VS.empty
              ) solver_data.data
          );
          solver_data
        | None ->
          create_empty_data ()
      in

      let term  = GobConfig.get_bool "solvers.td3.term" in
      let default_side_widen_gas = GobConfig.get_int "solvers.td3.side_widen_gas" in
      let default_widen_gas = GobConfig.get_int "solvers.td3.widen_gas" in
      let space = GobConfig.get_bool "solvers.td3.space" in
      let cache = GobConfig.get_bool "solvers.td3.space_cache" in

      let data = solver_data.data in
      let update_rule_data = solver_data.update_rule_data in

      let narrow_reuse = GobConfig.get_bool "solvers.td3.narrow-reuse" in
      let remove_wpoint = GobConfig.get_bool "solvers.td3.remove-wpoint" in
      let weak_deps_handling = GobConfig.get_string "solvers.td3.weak-deps" in

      let restart_sided = GobConfig.get_bool "incremental.restart.sided.enabled" in
      let restart_vars = GobConfig.get_string "incremental.restart.sided.vars" in

      let restart_wpoint = GobConfig.get_bool "solvers.td3.restart.wpoint.enabled" in
      let restart_once = GobConfig.get_bool "solvers.td3.restart.wpoint.once" in
      let restarted_wpoint = HM.create 10 in

      let incr_verify = GobConfig.get_bool "incremental.postsolver.enabled" in
      let consider_superstable_reached = GobConfig.get_bool "incremental.postsolver.superstable-reached" in
      (* In incremental load, initially stable nodes, which are never destabilized.
         These don't have to be re-verified and warnings can be reused. (td3: superstable = HM.copy stable) *)
      HM.iter (fun _ r -> r.superstable <- r.stable) data;

      let reluctant = GobConfig.get_bool "incremental.reluctant.enabled" in

      let var_messages = solver_data.var_messages in
      let rho_write = solver_data.rho_write in

      (* dep is only needed for some incremental pruning. *)
      let collect_dep = GobConfig.get_bool "incremental.load" || GobConfig.get_bool "incremental.save" in

      let (module WPS) = SideWPointSelect.choose_impl () in
      let module WPS = struct
        include WPS (EqS) (HM) (VS)
      end in

      print_solver_stats := (fun () ->
          print_data solver_data;
          Logs.info "|called|=%d" (HM.fold (fun _ r c -> if r.called then c + 1 else c) data 0);
          print_context_stats data
        );

      if GobConfig.get_bool "incremental.load" then (
        print_data_verbose solver_data "Loaded data for incremental analysis";
        verify_data solver_data
      );

      let cache_sizes = ref [] in

      let find_or_create x =
        match HM.find_option data x with
        | Some r -> r
        | None ->
          let r = create_var_data () in
          HM.replace data x r;
          r
      in
      let is_stable x = GobOption.exists (fun r -> r.stable) (HM.find_option data x) in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a" S.Var.pretty_trace y S.Var.pretty_trace x;
        let yr = find_or_create y in
        yr.infl <- VS.add x yr.infl;
        if collect_dep then (
          let xr = find_or_create x in
          xr.dep <- VS.add y xr.dep
        )
      in
      let add_sides y x =
        let yr = find_or_create y in
        yr.sides <- VS.add x yr.sides
      in

      let destabilize_ref: (S.v -> unit) ref = ref (fun _ -> failwith "no destabilize yet") in
      let destabilize x = !destabilize_ref x in (* must be eta-expanded to use changed destabilize_ref *)

      let pretty_wpoint () x =
        match HM.find_option data x with
        | Some {wpoint_gas = Some gas; _} -> Pretty.dprintf "true (gas: %d)" gas
        | _ -> Pretty.text "false"
      in
      let mark_wpoint r default_gas =
        if r.wpoint_gas = None then r.wpoint_gas <- Some default_gas in
      let reduce_gas x r = (* x is only used for tracing *)
        match r.wpoint_gas with
        | Some old_gas ->
          let decremented_gas = old_gas - 1 in
          if decremented_gas >= 0 then (
            if tracing then trace "widengas" "reducing gas of %a: %d -> %d" S.Var.pretty_trace x old_gas decremented_gas;
            r.wpoint_gas <- Some decremented_gas
          )
        | None -> ((* Not a widening point *)) in
      let should_widen r = r.wpoint_gas = Some 0 in
      let wps_data = WPS.create_data is_stable add_infl in

      (* Dummies for interfaces that structurally require td3's separate hashmaps.
         Only the strategies which actually read them are rejected above/in after_config. *)
      let dummy_called: unit HM.t = HM.create 0 in (* only iterated by side_widen unstable_called *)
      let dummy_stable: unit HM.t = HM.create 0 in (* only used by Td3UpdateRule.Narrow *)
      let dummy_sides: VS.t HM.t = HM.create 0 in (* only used by Td3UpdateRule.Narrow *)
      let dummy_rho: S.Dom.t HM.t = HM.create 0 in (* only used by Td3UpdateRule.Narrow *)

      (* Same as destabilize, but returns true if it destabilized a called var, or a var in vs which was stable. *)
      let rec destabilize_vs x = (* TODO remove? Only used for side_widen cycle. *)
        if tracing then trace "sol2" "destabilize_vs %a" S.Var.pretty_trace x;
        match HM.find_option data x with
        | None -> false
        | Some xr ->
          let w = xr.infl in
          xr.infl <- VS.empty;
          VS.fold (fun y b ->
              let yr = HM.find_option data y in
              let was_stable = GobOption.exists (fun r -> r.stable) yr in
              Option.may (fun r ->
                  r.stable <- false;
                  r.superstable <- false
                ) yr;
              Hooks.stable_remove y;
              if not (GobOption.exists (fun r -> r.called) yr) then
                destabilize_vs y || b || was_stable && List.mem_cmp S.Var.compare y vs
              else
                true
            ) w false (* nosemgrep: fold-exists *) (* does side effects *)
      and eq_wrapper x eqx  = ((UpdateRule.get_wrapper ~solve_widen:(fun x-> solve x Widen) ~init:(fun x -> ignore (init x)) ~stable:dummy_stable ~data:update_rule_data ~sides:dummy_sides ~add_sides ~rho:dummy_rho ~destabilize ~side ~assert_can_receive_side):UpdateRule.eq_wrapper) x eqx
      and solve ?reuse_eq x phase =
        let xr = init x in
        if tracing then trace "sol2" "solve %a, phase: %s, called: %b, stable: %b, wpoint: %a" S.Var.pretty_trace x (show_phase phase) xr.called xr.stable pretty_wpoint x;
        assert (Hooks.system x <> None);
        if not (xr.called || xr.stable) then (
          if tracing then trace "sol2" "stable add %a" S.Var.pretty_trace x;
          xr.stable <- true;
          xr.called <- true;
          (* Here we cache should_widen x before eq. If during eq eval makes x wpoint (with config widen_gas = 0), then be still don't apply widening the first time, but just overwrite.
             It means that the first iteration at wpoint is still precise.
             This doesn't matter during normal solving (?), because old would be bot.
             This matters during incremental loading, when wpoints have been removed (or not marshaled) and are redetected.
             Then the previous local wpoint value is discarded automagically and not joined/widened, providing limited restarting of local wpoints. (See eval for more complete restarting.) *)
          let wp = should_widen xr in (* if x becomes a wpoint (with gas = 0) during eq, checking this will delay widening until next solve *)
          let l = HM.create 10 in (* local cache *)
          let eqd = (* d from equation/rhs *)
            match reuse_eq with
            | Some d when narrow_reuse ->
              (* Do not reset deps for reuse of eq *)
              if tracing then trace "sol2" "eq reused %a" S.Var.pretty_trace x;
              incr SolverStats.narrow_reuses;
              d
            | _ ->
              (* The RHS is re-evaluated, all deps are re-trigerred *)
              if collect_dep then
                xr.dep <- VS.empty;
              eq_wrapper x (fun side -> eq x (eval l x) side (demand l x))
          in
          xr.called <- false;
          let old = xr.value in (* d from older solve *) (* find old value after eq since wpoint restarting in eq/eval might have changed it meanwhile *)

          (* if value has changed, reduce gas (only applies to marked widening points) *)
          if not (term && phase = Narrow) && not (S.Dom.equal eqd old) then reduce_gas x xr;

          let wpd = (* d after widen/narrow (if wp) *)
            if not wp then eqd
            else if term then
              match phase with
              | Widen -> S.Dom.widen old (S.Dom.join old eqd)
              | Narrow when GobConfig.get_bool "exp.no-narrow" -> old (* no narrow *)
              | Narrow ->
                (* assert S.Dom.(leq eqd old || not (leq old eqd)); (* https://github.com/goblint/analyzer/pull/490#discussion_r875554284 *) *)
                S.Dom.narrow old eqd
            else
              box old eqd
          in
          if tracing then trace "sol" "Var: %a (wp: %b)\nOld value: %a\nEqd: %a\nNew value: %a" S.Var.pretty_trace x wp S.Dom.pretty old S.Dom.pretty eqd S.Dom.pretty wpd;
          if cache then (
            if tracing then trace "cache" "cache size %d for %a" (HM.length l) S.Var.pretty_trace x;
            cache_sizes := HM.length l :: !cache_sizes;
          );
          if not (Timing.wrap "S.Dom.equal" (fun () -> S.Dom.equal old wpd) ()) then ( (* value changed *)
            if tracing then trace "sol" "Changed";
            (* if tracing && not (S.Dom.is_bot old) && wp then trace "solchange" "%a (wpx: %a): %a -> %a" S.Var.pretty_trace x pretty_wpoint x S.Dom.pretty old S.Dom.pretty wpd; *)
            if tracing && not (S.Dom.is_bot old) && wp then trace "solchange" "%a (wpx: %a): %a" S.Var.pretty_trace x pretty_wpoint x S.Dom.pretty_diff (wpd, old);
            update_var_event x old wpd;
            xr.value <- wpd;
            destabilize x;
            (solve[@tailcall]) x phase
          ) else (
            (* TODO: why non-equal and non-stable checks in switched order compared to TD3 paper? *)
            if not xr.stable then ( (* value unchanged, but not stable, i.e. destabilized itself during rhs *)
              if tracing then trace "sol2" "solve still unstable %a" S.Var.pretty_trace x;
              (solve[@tailcall]) x Widen
            ) else (
              if term && phase = Widen && xr.wpoint_gas <> None then ( (* TODO: or use wp? *)
                if tracing then trace "sol2" "solve switching to narrow %a" S.Var.pretty_trace x;
                if tracing then trace "sol2" "stable remove %a" S.Var.pretty_trace x;
                xr.stable <- false;
                xr.superstable <- false;
                Hooks.stable_remove x;
                (solve[@tailcall]) ~reuse_eq:eqd x Narrow
              ) else if remove_wpoint && not space && (not term || phase = Narrow) then ( (* this makes e.g. nested loops precise, ex. tests/regression/34-localization/01-nested.c - if we do not remove wpoint, the inner loop head will stay a wpoint and widen the outer loop variable. *)
                if tracing then trace "sol2" "solve removing wpoint %a (%a)" S.Var.pretty_trace x pretty_wpoint x;
                xr.wpoint_gas <- None
              )
            )
          )
        )
      and eq x get set demand =
        if tracing then trace "sol2" "eq %a" S.Var.pretty_trace x;
        match Hooks.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set demand
      and simple_solve l x y =
        if tracing then trace "sol2" "simple_solve %a (rhs: %b)" S.Var.pretty_trace y (Hooks.system y <> None);
        if Hooks.system y = None then (let yr = init y in yr.stable <- true; yr.value) else
          (* TODO: should td_space store information for widening points with remaining gas? *)
        if not space || GobOption.exists (fun r -> r.wpoint_gas <> None) (HM.find_option data y) then (solve y Widen; (HM.find data y).value) else
        if GobOption.exists (fun r -> r.called) (HM.find_option data y) then (let yr = init y in HM.remove l y; yr.value) else (* TODO: [called y] is not in the TD3 paper, what is it for? optimization? *)
          (* if HM.mem called y then (init y; let y' = HM.find_default l y (S.Dom.bot ()) in HM.replace rho y y'; HM.remove l y; y') else *)
        if cache && HM.mem l y then HM.find l y
        else (
          let yr = find_or_create y in
          yr.called <- true;
          let eqd =
            (* We check in maingoblint that `solvers.td3.space` and `solvers.td3.narrow-globs.enabled` are not on at the same time *)
            (* Narrowing on for globals ('solvers.td3.narrow-globs.enabled') would require enhancing this to work withe Narrow update rule *)
            eq y (eval l x) (side ~x) (demand l x)
          in
          yr.called <- false;
          if yr.wpoint_gas <> None then (HM.remove l y; solve y Widen; (HM.find data y).value)
          else (if cache then HM.replace l y eqd; eqd)
        )
      and eval l x y =
        if tracing then trace "sol2" "eval %a ## %a" S.Var.pretty_trace x S.Var.pretty_trace y;
        get_var_event y;
        (match HM.find_option data y with
         | Some yr when yr.called ->
           if restart_wpoint && yr.wpoint_gas = None then (
             (* Even though solve cleverly restarts redetected wpoints during incremental load, the loop body would be calculated based on the old wpoint value.
                The loop body might then side effect the old value, see tests/incremental/06-local-wpoint-read.
                Here we avoid this, by setting it to bottom for the loop body eval. *)
             if not (restart_once && HM.mem restarted_wpoint y) then (
               if tracing then trace "sol2" "wpoint restart %a ## %a" S.Var.pretty_trace y S.Dom.pretty (if yr.in_rho then yr.value else S.Dom.bot ());
               yr.value <- S.Dom.bot ();
               yr.in_rho <- true;
               if restart_once then (* avoid populating hashtable unnecessarily *)
                 HM.replace restarted_wpoint y ();
             )
           );
           if tracing then trace "sol2" "eval adding wpoint %a from %a" S.Var.pretty_trace y S.Var.pretty_trace x;
           mark_wpoint yr default_widen_gas
         | _ -> ());
        let tmp = simple_solve l x y in
        if GobOption.exists (fun r -> r.in_rho) (HM.find_option data y) then add_infl y x;
        if tracing then trace "sol2" "eval %a ## %a -> %a" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty tmp;
        tmp
      and side ?x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
        if tracing then trace "sol2" "side to %a (wpx: %a) from %a ## value: %a" S.Var.pretty_trace y pretty_wpoint y (Pretty.docOpt (S.Var.pretty_trace ())) x S.Dom.pretty d;
        assert_can_receive_side y;
        let yr = init y in

        WPS.notify_side wps_data x y;

        let widen a b =
          if M.tracing then M.traceli "sol2" "side widen %a %a" S.Dom.pretty a S.Dom.pretty b;
          let r = S.Dom.widen a (S.Dom.join a b) in
          if M.tracing then M.traceu "sol2" "-> %a" S.Dom.pretty r;
          r
        in
        let old_sides = yr.sides in
        let vetoed_widen = WPS.veto_widen wps_data dummy_called old_sides x y in
        let op a b = (* If y still has widening gas, widening will not be performed. *)
          if vetoed_widen || not (should_widen yr) then S.Dom.join a b else widen a b
        in
        let old = yr.value in
        let tmp = op old d in
        if tracing then trace "sol2" "stable add %a" S.Var.pretty_trace y;
        yr.stable <- true;
        if not (S.Dom.leq tmp old) then (
          if tracing && not (S.Dom.is_bot old) then trace "solside" "side to %a (wpx: %a) from %a: %a -> %a" S.Var.pretty_trace y pretty_wpoint y (Pretty.docOpt (S.Var.pretty_trace ())) x S.Dom.pretty old S.Dom.pretty tmp;
          if tracing && not (S.Dom.is_bot old) then trace "solchange" "side to %a (wpx: %a) from %a: %a" S.Var.pretty_trace y pretty_wpoint y (Pretty.docOpt (S.Var.pretty_trace ())) x S.Dom.pretty_diff (tmp, old);

          (match x with
           | Some x ->
             if not (VS.mem x old_sides) then add_sides y x;
           | None -> ());

          yr.value <- tmp;
          let destabilized_vs: bool option = if WPS.record_destabilized_vs then (
              destabilize y;
              None
            ) else
              Some (destabilize_vs y) in

          (* make y a widening point if ... This will only matter for the next side _ y.  *)
          if WPS.should_mark_wpoint wps_data dummy_called old_sides x y destabilized_vs then (
            if tracing then trace "sol2" "side adding wpoint %a from %a" S.Var.pretty_trace y (Pretty.docOpt (S.Var.pretty_trace ())) x;
            mark_wpoint yr default_side_widen_gas
          );

          (* y has grown. Reduce widening gas! *)
          if not vetoed_widen then reduce_gas y yr;
        )
      and demand l x y =
        if tracing then trace "sol2" "demand weak dep %a from %a" S.Var.pretty_trace y S.Var.pretty_trace x;
        match weak_deps_handling with
        | "none" -> ignore (eval l x y)
        | "eager" ->
          let xr = find_or_create x in
          xr.weak_dep <- VS.add y xr.weak_dep;
          solve y Widen
        | "lazy" ->
          let xr = find_or_create x in
          xr.weak_dep <- VS.add y xr.weak_dep
        | _ -> assert false
      and init x =
        if tracing then trace "sol2" "init %a" S.Var.pretty_trace x;
        let r = find_or_create x in
        if not r.in_rho then (
          new_var_event x;
          r.value <- S.Dom.bot ();
          r.in_rho <- true
        );
        r
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a" S.Var.pretty_trace x S.Dom.pretty d;
        let xr = init x in
        UpdateRule.register_start update_rule_data x d;
        xr.value <- d;
        xr.stable <- true
        (* solve x Widen *)
      in

      let rec destabilize_normal x =
        if tracing then trace "sol2" "destabilize %a" S.Var.pretty_trace x;
        match HM.find_option data x with
        | None -> ()
        | Some xr ->
          let w = xr.infl in
          xr.infl <- VS.empty;
          VS.iter (fun y ->
              if tracing then trace "sol2" "stable remove %a" S.Var.pretty_trace y;
              let yr = HM.find_option data y in
              Option.may (fun r ->
                  r.stable <- false;
                  r.superstable <- false
                ) yr;
              Hooks.stable_remove y;
              if not (GobOption.exists (fun r -> r.called) yr) then destabilize_normal y
            ) w
      in

      start_event ();

      (* reluctantly unchanged return nodes to additionally query for postsolving to get warnings, etc. *)
      let reluctant_vs: S.Var.t list ref = ref [] in

      let restart_write_only = GobConfig.get_bool "incremental.restart.write-only" in

      if GobConfig.get_bool "incremental.load" then (

        let restart_leaf x =
          if tracing then trace "sol2" "Restarting to bot %a" S.Var.pretty_trace x;
          Logs.debug "Restarting to bot %a" S.Var.pretty_trace x;
          let xr = find_or_create x in
          xr.value <- S.Dom.bot ();
          xr.in_rho <- true;
          xr.wpoint_gas <- None; (* otherwise gets immediately widened during resolve *)
          xr.sides <- VS.empty; (* just in case *)

          (* immediately redo "side effect" from st *)
          match GobList.assoc_eq_opt S.Var.equal x st with
          | Some d ->
            xr.value <- d
          | None ->
            ()
        in

        let restart_fuel_only_globals = GobConfig.get_bool "incremental.restart.sided.fuel-only-global" in

        (* destabilize which restarts side-effected vars *)
        (* side_fuel specifies how many times (in recursion depth) to destabilize side_infl, None means infinite *)
        let rec destabilize_with_side ~side_fuel x =
          if tracing then trace "sol2" "destabilize_with_side %a %a" S.Var.pretty_trace x (Pretty.docOpt (Pretty.dprintf "%d")) side_fuel;
          match HM.find_option data x with
          | None -> ()
          | Some xr ->
            (* retrieve and remove (side-effect) dependencies/influences *)
            let w_side_dep = xr.side_dep in
            xr.side_dep <- VS.empty;
            let w_infl = xr.infl in
            xr.infl <- VS.empty;
            let w_side_infl = xr.side_infl in
            xr.side_infl <- VS.empty;

            let should_restart =
              match restart_write_only, S.Var.is_write_only x with
              | true, true -> false (* prefer efficient write-only restarting during postsolving *)
              | _, is_write_only ->
                match restart_vars with
                | "all" -> true
                | "global" -> Node.equal (S.Var.node x) (Function GoblintCil.dummyFunDec) (* non-function entry node *)
                | "write-only" -> is_write_only
                | _ -> assert false
            in

            (* is side-effected var (global/function entry)? *)
            if not (VS.is_empty w_side_dep) && should_restart then (
              (* restart side-effected var *)
              restart_leaf x;

              (* destabilize side dep to redo side effects *)
              VS.iter (fun y ->
                  if tracing then trace "sol2" "destabilize_with_side %a side_dep %a" S.Var.pretty_trace x S.Var.pretty_trace y;
                  if tracing then trace "sol2" "stable remove %a" S.Var.pretty_trace y;
                  Option.may (fun r ->
                      r.stable <- false;
                      r.superstable <- false
                    ) (HM.find_option data y);
                  Hooks.stable_remove y;
                  destabilize_with_side ~side_fuel y
                ) w_side_dep;
            );

            (* destabilize eval infl *)
            VS.iter (fun y ->
                if tracing then trace "sol2" "destabilize_with_side %a infl %a" S.Var.pretty_trace x S.Var.pretty_trace y;
                if tracing then trace "sol2" "stable remove %a" S.Var.pretty_trace y;
                Option.may (fun r ->
                    r.stable <- false;
                    r.superstable <- false
                  ) (HM.find_option data y);
                Hooks.stable_remove y;
                destabilize_with_side ~side_fuel y
              ) w_infl;

            (* destabilize side infl *)
            if side_fuel <> Some 0 then ( (* non-0 or infinite fuel is fine *)
              let side_fuel' =
                if not restart_fuel_only_globals || Node.equal (S.Var.node x) (Function GoblintCil.dummyFunDec) then
                  Option.map Int.pred side_fuel
                else
                  side_fuel (* don't decrease fuel for function entry side effect *)
              in
              (* TODO: should this also be conditional on restart_only_globals? right now goes through function entry side effects, but just doesn't restart them *)
              VS.iter (fun y ->
                  if tracing then trace "sol2" "destabilize_with_side %a side_infl %a" S.Var.pretty_trace x S.Var.pretty_trace y;
                  if tracing then trace "sol2" "stable remove %a" S.Var.pretty_trace y;
                  Option.may (fun r ->
                      r.stable <- false;
                      r.superstable <- false
                    ) (HM.find_option data y);
                  Hooks.stable_remove y;
                  destabilize_with_side ~side_fuel:side_fuel' y
                ) w_side_infl
            )
        in

        destabilize_ref :=
          if restart_sided then (
            let side_fuel =
              match GobConfig.get_int "incremental.restart.sided.fuel" with
              | fuel when fuel >= 0 -> Some fuel
              | _ -> None (* infinite *)
            in
            destabilize_with_side ~side_fuel
          )
          else
            destabilize_normal;

        let sys_change = S.sys_change (fun v ->
            match HM.find_option data v with
            | Some r when r.in_rho -> r.value
            | _ -> S.Dom.bot ()
          ) in

        let old_ret = HM.create 103 in
        if reluctant then (
          (* save entries of changed functions in rho for the comparison whether the result has changed after a function specific solve *)
          List.iter (fun k ->
              match HM.find_option data k with
              | Some r when r.in_rho ->
                HM.replace old_ret k (r.value, r.infl)
              | _ -> ()
            ) sys_change.reluctant;
        );

        if sys_change.obsolete <> [] then
          Logs.debug "Destabilizing changed functions and primary old nodes ...";
        List.iter (fun k ->
            if is_stable k then
              destabilize k
          ) sys_change.obsolete;

        (* We remove all unknowns for program points in changed or removed functions from rho, stable, infl and wpoint *)
        Logs.debug "Removing data for changed and removed functions...";
        (* Clear the fields td3 deletes before destabilizing sides (rho, infl, wpoint_gas, dep);
           the records themselves are removed further below, where td3 deletes stable, side_dep and side_infl. *)
        List.iter (fun k ->
            Option.may (fun r ->
                r.in_rho <- false;
                r.infl <- VS.empty;
                r.wpoint_gas <- None;
                r.dep <- VS.empty
              ) (HM.find_option data k)
          ) sys_change.delete;
        Hooks.delete_marked sys_change.delete;

        (* destabilize_with_side doesn't have all infl to follow anymore, so should somewhat work with reluctant *)
        if restart_sided then (
          (* restarts old copies of functions and their (removed) side effects *)
          Logs.debug "Destabilizing sides of changed functions, primary old nodes and removed functions ...";
          List.iter (fun k ->
              if is_stable k then (
                Logs.debug "marked %a" S.Var.pretty_trace k;
                destabilize k
              )
            ) sys_change.delete
        );

        (* [destabilize_leaf] is meant for restarting of globals selected by the user. *)
        (* Must be called on a leaf! *)
        let destabilize_leaf (x : S.v) =
          let destab_side_dep (x : S.v) =
            match HM.find_option data x with
            | None -> ()
            | Some xr ->
              let w = xr.side_dep in
              if not (VS.is_empty w) then (
                xr.side_dep <- VS.empty;
                (* destabilize side dep to redo side effects *)
                VS.iter (fun y ->
                    if tracing then trace "sol2" "destabilize_leaf %a side_dep %a" S.Var.pretty_trace x S.Var.pretty_trace y;
                    if tracing then trace "sol2" "stable remove %a" S.Var.pretty_trace y;
                    Option.may (fun r ->
                        r.stable <- false;
                        r.superstable <- false
                      ) (HM.find_option data y);
                    Hooks.stable_remove y;
                    destabilize_normal y
                  ) w
              )
          in
          restart_leaf x;
          destab_side_dep x;
          destabilize_normal x

        in

        List.iter (fun v ->
            if Hooks.system v <> None then
              Logs.warn "Trying to restart non-leaf unknown %a. This has no effect." S.Var.pretty_trace v
            else if is_stable v then
              destabilize_leaf v
          ) sys_change.restart;

        let restart_and_destabilize x = (* destabilize_with_side doesn't restart x itself *)
          restart_leaf x;
          destabilize x
        in

        let should_restart_start = restart_sided && restart_vars <> "write-only" in (* assuming start vars are not write-only *)
        (* TODO: should this distinguish non-global (function entry) and global (earlyglobs) start vars? *)

        (* Call side on all globals and functions in the start variables to make sure that changes in the initializers are propagated.
         * This also destabilizes start functions if their start state changes because of globals that are neither in the start variables nor in the contexts *)
        List.iter (fun (v,d) ->
            if should_restart_start then (
              match GobList.assoc_eq_opt S.Var.equal v solver_data.st with
              | Some old_d when not (S.Dom.equal old_d d) ->
                Logs.debug "Destabilizing and restarting changed start var %a" S.Var.pretty_trace v;
                restart_and_destabilize v (* restart side effect from start *)
              | _ ->
                (* don't restart unchanged start global *)
                (* no need to restart added start global (implicit bot before) *)
                (* restart removed start global below *)
                ()
            );
            side v d
          ) st;

        if should_restart_start then (
          List.iter (fun (v, _) ->
              match GobList.assoc_eq_opt S.Var.equal v st with
              | None ->
                (* restart removed start global to allow it to be pruned from incremental solution *)
                (* this gets rid of its warnings and makes comparing with from scratch sensible *)
                Logs.debug "Destabilizing and restarting removed start var %a" S.Var.pretty_trace v;
                restart_and_destabilize v
              | _ ->
                ()
            ) solver_data.st
        );

        (* Remove the records of deleted unknowns entirely (td3: delete_marked stable, side_dep, side_infl, superstable).
           Unlike td3, this also drops their sides and weak_dep. *)
        List.iter (HM.remove data) sys_change.delete;

        (* delete from incremental postsolving/warning structures to remove spurious warnings *)
        List.iter (HM.remove var_messages) sys_change.delete;

        if restart_write_only then (
          (* restart write-only *)
          (* before delete_marked because we also want to restart write-only side effects from deleted nodes *)
          HM.iter (fun x w ->
              HM.iter (fun y d ->
                  Logs.debug "Restarting write-only to bot %a" S.Var.pretty_trace y;
                  let yr = find_or_create y in
                  yr.value <- S.Dom.bot ();
                  yr.in_rho <- true
                ) w
            ) rho_write
        );
        List.iter (HM.remove rho_write) sys_change.delete;
        HM.iter (fun x w -> List.iter (HM.remove w) sys_change.delete) rho_write;

        print_data_verbose solver_data "Data after clean-up";

        (* TODO: reluctant doesn't call destabilize on removed functions or old copies of modified functions (e.g. after removing write), so those globals don't get restarted *)

        if reluctant then (
          (* solve on the return node of changed functions. Only destabilize the function's return node if the analysis result changed *)
          Logs.debug "Separately solving changed functions...";
          HM.iter (fun x (old_rho, old_infl) ->
              let xr = find_or_create x in
              xr.value <- old_rho;
              xr.in_rho <- true;
              xr.infl <- old_infl
            ) old_ret;
          HM.iter (fun x (old_rho, old_infl) ->
              Logs.debug "test for %a" Node.pretty_trace (S.Var.node x);
              solve x Widen;
              if not (S.Dom.equal (HM.find data x).value old_rho) then (
                Logs.debug "Further destabilization happened ...";
              )
              else (
                Logs.debug "Destabilization not required...";
                reluctant_vs := x :: !reluctant_vs
              )
            ) old_ret;

          Logs.debug "Final solve..."
        );
      ) else (
        List.iter set_start st;
      );

      destabilize_ref := destabilize_normal; (* always use normal destabilize during actual solve *)

      List.iter (fun x -> ignore (init x)) vs;
      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we side some global which v1 depends on with a new value. Then v1 is no longer stable and we have to solve it again. *)
      let i = ref 0 in
      let rec solver () = (* as while loop in paper *)
        incr i;
        let weak_dep_vs =
          HM.fold (fun _ r acc -> VS.fold List.cons r.weak_dep acc) data []
        in
        let all_vs = vs @ weak_dep_vs in (* vs is singleton for us, so it's cheap to prepend *)
        let unstable_vs = List.filter (fun x -> not (is_stable x)) all_vs in
        if unstable_vs <> [] then (
          if Logs.Level.should_log Debug then (
            if !i = 1 then Logs.newline ();
            Logs.debug "Unstable solver start vars in %d. phase:" !i;
            List.iter (fun v -> Logs.debug "\t%a" S.Var.pretty_trace v) unstable_vs;
            Logs.newline ();
            flush_all ();
          );
          List.iter (fun x -> solve x Widen) unstable_vs;
          solver ();
        )
      in
      solver ();
      (* Before we solved all unstable vars in rho with a rhs in a loop. This is unneeded overhead since it also solved unreachable vars (reachability only removes those from rho further down). *)
      (* After termination, only those variables are stable which are
       * - reachable from any of the queried variables vs, or
       * - effected by side-effects and have no constraints on their own (this should be the case for all of our analyses). *)

      (* verifies values at widening points and adds values for variables in-between *)
      let visited = HM.create 10 in
      let check_side x y d =
        HM.replace visited y ();
        let y_in_rho = GobOption.exists (fun r -> r.in_rho) (HM.find_option data y) in
        let d' = match HM.find_option data y with
          | Some r when r.in_rho -> r.value
          | _ -> S.Dom.bot ()
        in
        if not (S.Dom.leq d d') then Logs.error "TDFP Fixpoint not reached in restore step at side-effected variable (mem: %b) %a from %a: %a not leq %a" y_in_rho S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty d S.Dom.pretty d'
      in
      let rec eq check x =
        HM.replace visited x ();
        match Hooks.system x with
        | None ->
          (match HM.find_option data x with
           | Some r when r.in_rho -> r.value
           | _ -> Logs.warn "TDFP Found variable %a w/o rhs and w/o value in rho" S.Var.pretty_trace x; S.Dom.bot ())
        | Some f -> f (get ~check) (check_side x) (demand ~check)
      and get ?(check=false) x =
        if HM.mem visited x then (
          (HM.find data x).value
        ) else (
          match HM.find_option data x with
          | Some r when r.in_rho -> (* `vs` are in `rho`, so to restore others we need to skip to `eq`. *)
            let d1 = r.value in
            let d2 = eq check x in (* just to reach unrestored variables *)
            if check then (
              if not r.stable && Hooks.system x <> None then Logs.error "TDFP Found an unknown in rho that should be stable: %a" S.Var.pretty_trace x;
              if not (S.Dom.leq d2 d1) then
                Logs.error "TDFP Fixpoint not reached in restore step at %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" S.Var.pretty_trace x S.Dom.pretty d1 S.Dom.pretty d2 S.Dom.pretty_diff (d1,d2);
            );
            d1
          | r_opt ->
            let d = eq check x in
            let r = match r_opt with
              | Some r -> r
              | None -> find_or_create x
            in
            r.value <- d;
            r.in_rho <- true;
            d
        )
      and demand ?check x =
        ignore (get ?check x)
      in
      (* restore values for non-widening-points *)
      if space && GobConfig.get_bool "solvers.td3.space_restore" then (
        Logs.debug "Restoring missing values.";
        let restore () =
          let get x =
            let d = get ~check:true x in
            if tracing then trace "sol2" "restored var %a ## %a" S.Var.pretty_trace x S.Dom.pretty d
          in
          List.iter get vs;
          (* drop values of unknowns not visited (td3: prune rho to visited) *)
          HM.iter (fun x r -> if not (HM.mem visited x) then r.in_rho <- false) data
        in
        Timing.wrap "restore" restore ();
        Logs.debug "Solved %d vars. Total of %d vars after restore." !SolverStats.vars (HM.fold (fun _ r n -> if r.in_rho then n + 1 else n) data 0);
        let avg xs = if List.is_empty !cache_sizes then 0.0 else float_of_int (BatList.sum xs) /. float_of_int (List.length xs) in
        if tracing && cache then trace "cache" "#caches: %d, max: %d, avg: %.2f" (List.length !cache_sizes) (List.max !cache_sizes) (avg !cache_sizes);
      );

      stop_event ();
      print_data_verbose solver_data "Data after solve completed";

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Logs.newline ();
        Logs.debug "Widening points:";
        HM.iter (fun k r ->
            match r.wpoint_gas with
            | Some gas -> Logs.debug "%a (gas: %d)" S.Var.pretty_trace k gas
            | None -> ()
          ) data;
        Logs.newline ();
      );

      (* Materialize rho for the postsolver, which requires a plain value hashmap it can read and mutate.
         Mutations (write-only restarting, pruning) are transformed back into data afterwards. *)
      let rho = HM.create (HM.length data) in
      HM.iter (fun x r -> if r.in_rho then HM.replace rho x r.value) data;

      let module S = EqS in (* TODO: expose demand to postsolvers? *)

      (* Prune other data structures than rho with reachable.
         These matter for the incremental data. *)
      let module IncrPrune: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Unit (S) (HM)

        let finalize ~vh ~reachable =
          (* Analogous to td3 filtering stable and the keys and value sets of infl, side_infl, side_dep and dep. *)
          HM.iter (fun x r ->
              if VH.mem reachable x then (
                r.infl <- VS.filter (VH.mem reachable) r.infl;
                r.side_infl <- VS.filter (VH.mem reachable) r.side_infl;
                r.side_dep <- VS.filter (VH.mem reachable) r.side_dep;
                r.dep <- VS.filter (VH.mem reachable) r.dep
              )
              else (
                r.stable <- false;
                r.infl <- VS.empty;
                r.side_infl <- VS.empty;
                r.side_dep <- VS.empty;
                r.dep <- VS.empty
              )
            ) data;

          VH.filteri_inplace (fun x w ->
              if VH.mem reachable x then (
                VH.filteri_inplace (fun y _ ->
                    VH.mem reachable y
                  ) w;
                true
              )
              else
                false
            ) rho_write

        (* TODO: prune other data structures? *)
      end
      in

      (* postsolver also populates side_dep, side_infl, and dep *)
      let module SideInfl: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Unit (S) (HM)

        (* TODO: We should be able to reset side_infl before executing the RHS, as all relevant side-effects should happen here again *)
        (* However, this currently breaks some tests https://github.com/goblint/analyzer/pull/713#issuecomment-1114764937 *)
        let one_side ~vh ~x ~y ~d =
          (* Also record side-effects caused by post-solver *)
          let yr = find_or_create y in
          yr.side_dep <- VS.add x yr.side_dep;
          let xr = find_or_create x in
          xr.side_infl <- VS.add y xr.side_infl
      end
      in

      let stable_reluctant_vs =
        List.filter is_stable !reluctant_vs
      in
      let reachable_and_superstable =
        if incr_verify && not consider_superstable_reached then
          (* Perform reachability on whole constraint system, but cheaply by using logged dependencies *)
          (* This only works if the other reachability has been performed before, so dependencies created only during postsolve are recorded *)
          let reachable' = HM.create (HM.length data) in
          let reachable_and_superstable = HM.create (HM.length data) in
          let rec one_var' x =
            if not (HM.mem reachable' x) then (
              match HM.find_option data x with
              | Some r ->
                if r.superstable then HM.replace reachable_and_superstable x ();
                HM.replace reachable' x ();
                VS.iter one_var' r.dep;
                VS.iter one_var' r.side_infl
              | None ->
                HM.replace reachable' x ()
            )
          in
          (Timing.wrap "cheap_full_reach" (List.iter one_var')) (vs @ stable_reluctant_vs);

          reachable_and_superstable (* consider superstable reached if it is still reachable: stop recursion (evaluation) and keep from being pruned *)
        else if incr_verify then (
          let superstable = HM.create (HM.length data) in
          HM.iter (fun x r -> if r.superstable then HM.replace superstable x ()) data;
          superstable
        )
        else
          HM.create 0 (* doesn't matter, not used *)
      in

      if incr_verify then (
        HM.filteri_inplace (fun x _ -> HM.mem reachable_and_superstable x) var_messages;
        HM.filteri_inplace (fun x _ -> HM.mem reachable_and_superstable x) rho_write
      )
      else (
        HM.clear var_messages;
        HM.clear rho_write
      );

      let init_reachable = reachable_and_superstable in

      let module IncrWarn: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Warn (S) (HM)

        let init () =
          init (); (* enable warning like standard Warn *)

          (* replay superstable messages from unknowns that are still reachable *)
          if incr_verify then (
            HM.iter (fun _ m ->
                Messages.add m
              ) var_messages;
          );

          (* hook to collect new messages *)
          Messages.Table.add_hook := (fun m ->
              match !CurrentVarS.current_var with
              | Some x -> HM.add var_messages x m
              | None -> ()
            )

        let finalize ~vh ~reachable =
          finalize ~vh ~reachable; (* disable warning like standard Warn *)

          (* unhook to avoid accidental var_messages modifications *)
          Messages.Table.add_hook := (fun _ -> ())
      end
      in

      (** Incremental write-only side effect restart handling:
          retriggers superstable ones (after restarting above) and collects new (non-superstable) ones. *)
      let module IncrWrite: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Unit (S) (HM)

        let init () =
          (* retrigger superstable side writes from unknowns that are still reachable *)
          if incr_verify then (
            HM.iter (fun x w ->
                HM.iter (fun y d ->
                    let old_d = try HM.find rho y with Not_found -> S.Dom.bot () in
                    (* Logs.debug "rho_write retrigger %a %a %a %a" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty old_d S.Dom.pretty d; *)
                    HM.replace rho y (S.Dom.join old_d d);
                    HM.replace init_reachable y ();
                    (find_or_create y).stable <- true (* make stable just in case, so following incremental load would have in superstable *)
                  ) w
              ) rho_write
          )

        let one_side ~vh ~x ~y ~d =
          if S.Var.is_write_only y then (
            (* Logs.debug "rho_write collect %a %a %a" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty d; *)
            (find_or_create y).stable <- true; (* make stable just in case, so following incremental load would have in superstable *)
            let w =
              try
                VH.find rho_write x
              with Not_found ->
                let w = VH.create 1 in (* only create on demand, modify_def would eagerly allocate *)
                VH.replace rho_write x w;
                w
            in
            VH.add w y d (* intentional add *)
          )
      end
      in

      let module MakeIncrListArg =
      struct
        module Arg =
        struct
          include Arg
          let should_warn = false (* disable standard Warn in favor of IncrWarn *)
        end
        include PostSolver.ListArgFromStdArg (S) (HM) (Arg)

        (* Only put postsolvers defined in here with [S] from [CurrentVarEqConstrSys]! *)
        let postsolvers = (module IncrPrune: M) :: (module SideInfl: M) :: (module IncrWrite: M) :: (module IncrWarn: M) :: postsolvers

        let init_reachable ~vh =
          if incr_verify then
            init_reachable
          else
            HM.create (HM.length vh)
      end
      in

      let module Post = PostSolver.MakeIncrList (MakeIncrListArg) in
      Post.post st (stable_reluctant_vs @ vs) rho;

      (* Transform the postsolved rho back into data: values may have changed (write-only restarting)
         and unknowns may have been pruned (reachability). *)
      HM.filteri_inplace (fun x r ->
          match HM.find_option rho x with
          | Some v ->
            r.value <- v;
            r.in_rho <- true;
            true
          | None ->
            false
        ) data;
      HM.iter (fun x v ->
          if not (HM.mem data x) then (
            let r = create_var_data () in
            r.value <- v;
            r.in_rho <- true;
            r.stable <- true;
            HM.replace data x r
          )
        ) rho;

      let solver_data = {solver_data with st} in
      print_data_verbose solver_data "Data after postsolve";

      verify_data solver_data;
      (rho, solver_data)
  end

(** TD3 with consolidated data and no hooks. *)
module Basic(UpdateRule: Td3UpdateRule.S): DemandEqIncrSolver =
  functor (Arg: IncrSolverArg) ->
  functor (S:DemandEqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v)->
  struct
    include Generic.SolverStats (EqConstrSysFromDemandConstrSys (S)) (HM)

    module Hooks =
    struct
      module S = S
      module HM = HM

      let print_data () = ()

      let system x =
        match S.system x with
        | None -> None
        | Some f ->
          let f' get set demand =
            eval_rhs_event x;
            f get set demand
          in
          Some f'

      let delete_marked _ = ()
      let stable_remove _ = ()
      let prune ~reachable = ()
    end

    include Base (Arg) (S) (HM) (Hooks) (UpdateRule)
  end

(** TD3 with consolidated data and eval skipping using [dep_vals]. *)
module DepVals(UpdateRule: Td3UpdateRule.S): DemandEqIncrSolver =
  functor (Arg: IncrSolverArg) ->
  functor (S:DemandEqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    include Generic.SolverStats (EqConstrSysFromDemandConstrSys (S)) (HM)

    (* TODO: more efficient inner data structure than assoc list, https://github.com/goblint/analyzer/pull/738#discussion_r876016079 *)
    type dep_vals = (S.Dom.t * (S.Var.t * S.Dom.t) list) HM.t

    let current_dep_vals: dep_vals ref = ref (HM.create 0)
    (** Reference to current [dep_vals] in hooks. *)

    module Hooks =
    struct
      module S = S
      module HM = HM

      let print_data () =
        Logs.debug "|dep_vals|=%d" (HM.length !current_dep_vals)

      let system x =
        match S.system x with
        | None -> None
        | Some f ->
          let dep_vals = !current_dep_vals in
          let f' get set demand =
            let all_deps_unchanged =
              match HM.find_option dep_vals x with
              | None -> None
              | Some (oldv, deps) ->
                (* TODO: is this reversal necessary? https://github.com/goblint/analyzer/pull/738#discussion_r876703516 *)
                let deps_inorder = List.rev deps in
                if List.for_all (fun (var, value) -> S.Dom.equal (get var) value) deps_inorder then
                  Some oldv
                else
                  None
            in
            match all_deps_unchanged with
            | Some oldv ->
              if M.tracing then M.trace "sol2" "All deps unchanged for %a, not evaluating RHS" S.Var.pretty_trace x;
              oldv
            | None ->
              (* This needs to be done here as a local wrapper around get to avoid polluting dep_vals during earlier checks *)
              let get y =
                let tmp = get y in
                let (oldv,curr_dep_vals) = HM.find dep_vals x in
                HM.replace dep_vals x (oldv,((y,tmp) :: curr_dep_vals));
                tmp
              in
              eval_rhs_event x;
              (* Reset dep_vals to [] *)
              HM.replace dep_vals x (S.Dom.bot (),[]);
              let res = f get set demand in (* TODO: also need to wrap demand? *)
              (* Insert old value of last RHS evaluation *)
              HM.replace dep_vals x (res, snd (HM.find dep_vals x));
              res
          in
          Some f'

      let delete_marked delete =
        (* very basic fix for incremental runs with aborting such that unknowns of function
           return nodes with changed rhs but same id are actually evaluated and not looked up
           (this is probably not sufficient / desirable for inefficient matchings) *)
        List.iter (HM.remove !current_dep_vals) delete

      let stable_remove x =
        HM.remove !current_dep_vals x

      let prune ~reachable =
        HM.filteri_inplace (fun x _ ->
            HM.mem reachable x
          ) !current_dep_vals
    end

    module Base = Base (Arg) (S) (HM) (Hooks) (UpdateRule)

    type marshal = {
      base: Base.marshal;
      dep_vals: dep_vals; (** Dependencies of variables and values encountered at last eval of RHS. *)
    }

    let copy_marshal {base; dep_vals} =
      {
        base = Base.copy_marshal base;
        dep_vals = HM.copy dep_vals;
      }

    let relift_marshal {base; dep_vals} =
      let base' = Base.relift_marshal base in
      let dep_vals' = HM.create (HM.length dep_vals) in
      HM.iter (fun k (value,deps) ->
          HM.replace dep_vals' (S.Var.relift k) (S.Dom.relift value, List.map (fun (var,value) -> (S.Var.relift var,S.Dom.relift value)) deps)
        ) dep_vals;
      {base = base'; dep_vals = dep_vals'}

    let solve st vs marshal =
      let base_marshal = match marshal with
        | Some {base; dep_vals} ->
          current_dep_vals := dep_vals;
          Some base
        | None ->
          current_dep_vals := HM.create 10;
          None
      in
      let (rho, base_marshal') = Base.solve st vs base_marshal in
      (rho, {base = base_marshal'; dep_vals = !current_dep_vals})
  end

let after_config () =
  let restart_sided = GobConfig.get_bool "incremental.restart.sided.enabled" in
  let restart_wpoint = GobConfig.get_bool "solvers.td3.restart.wpoint.enabled" in
  let restart_once = GobConfig.get_bool "solvers.td3.restart.wpoint.once" in
  let skip_unchanged_rhs = GobConfig.get_bool "solvers.td3.skip-unchanged-rhs" in
  (* Only the Inactive update rule is supported: Td3UpdateRule.Narrow (narrow-globs) requires td3's separate hashmaps.
     solve fails fast if narrow-globs is enabled. *)
  let module UpdateRule = Td3UpdateRule.Inactive in
  if skip_unchanged_rhs then (
    if restart_sided || restart_wpoint || restart_once then
      (* restarting active, skip-unchanged-rhs is ignored (td3's after_config already warns about this combination) *)
      (* TODO: fix DepVals with restarting, https://github.com/goblint/analyzer/pull/738#discussion_r876005821 *)
      Selector.add_solver ("td3_record", (module Basic(UpdateRule): DemandEqIncrSolver))
    else
      Selector.add_solver ("td3_record", (module DepVals(UpdateRule): DemandEqIncrSolver))
  )
  else
    Selector.add_solver ("td3_record", (module Basic(UpdateRule): DemandEqIncrSolver))

let () =
  AfterConfig.register after_config
