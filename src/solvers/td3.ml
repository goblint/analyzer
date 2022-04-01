(** Incremental terminating top down solver that optionally only keeps values at widening points and restores other values afterwards. *)
(* Incremental: see paper 'Incremental Abstract Interpretation' https://link.springer.com/chapter/10.1007/978-3-030-41103-9_5 *)
(* TD3: see paper 'Three Improvements to the Top-Down Solver' https://dl.acm.org/doi/10.1145/3236950.3236967
 * Option solvers.td3.* (default) ? true : false (solver in paper):
 * - term (true) ? use phases for widen+narrow (TDside) : use box (TDwarrow)
 * - space (false) ? only keep values at widening points (TDspace + side) in rho : keep all values in rho
 * - space_cache (true) ? local cache l for eval calls in each solve (TDcombined) : no cache
 * - space_restore (true) ? eval each rhs and store all in rho : do not restore missing values
 * For simpler (but unmaintained) versions without the incremental parts see the paper or topDown{,_space_cache_term}.ml.
 *)

open Prelude
open Analyses
open Constraints
open Messages
open CompareCIL
open Cil

module WP =
  functor (Arg: IncrSolverArg) ->
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    type solver_data = {
      mutable st: (S.Var.t * S.Dom.t) list; (* needed to destabilize start functions if their start state changed because of some changed global initializer *)
      mutable infl: VS.t HM.t;
      mutable sides: VS.t HM.t;
      mutable rho: S.Dom.t HM.t;
      mutable wpoint: unit HM.t;
      mutable stable: unit HM.t;
      mutable side_dep: VS.t HM.t; (** Dependencies of side-effected variables. Knowing these allows restarting them and re-triggering all side effects. *)
      mutable side_infl: VS.t HM.t; (** Influences to side-effected variables. Not normally in [infl], but used for restarting them. *)
      mutable var_messages: Message.t HM.t;
      mutable rho_write: S.Dom.t HM.t HM.t;
    }

    type marshal = solver_data

    let create_empty_data () = {
      st = [];
      infl = HM.create 10;
      sides = HM.create 10;
      rho = HM.create 10;
      wpoint = HM.create 10;
      stable = HM.create 10;
      side_dep = HM.create 10;
      side_infl = HM.create 10;
      var_messages = HM.create 10;
      rho_write = HM.create 10;
    }

    let print_data data str =
      if GobConfig.get_bool "dbg.verbose" then
        Printf.printf "%s:\n|rho|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n|side_dep|=%d\n|side_infl|=%d\n"
          str (HM.length data.rho) (HM.length data.stable) (HM.length data.infl) (HM.length data.wpoint) (HM.length data.side_dep) (HM.length data.side_infl)

    let exists_key f hm = HM.fold (fun k _ a -> a || f k) hm false

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq, hash]
    end

    module HPM = Hashtbl.Make (P)

    type phase = Widen | Narrow

    let current_var = ref None

    module S =
    struct
      include S

      let system x =
        match S.system x with
        | None -> None
        | Some f ->
          let f' get set =
            let old_current_var = !current_var in
            current_var := Some x;
            Fun.protect ~finally:(fun () ->
                current_var := old_current_var
              ) (fun () ->
                f get set
              )
          in
          Some f'
    end

    exception AbortEq

    let solve box st vs data =
      let term  = GobConfig.get_bool "solvers.td3.term" in
      let side_widen = GobConfig.get_string "solvers.td3.side_widen" in
      let space = GobConfig.get_bool "solvers.td3.space" in
      let cache = GobConfig.get_bool "solvers.td3.space_cache" in
      let called = HM.create 10 in
      let called_changed = HM.create 10 in

      let infl = data.infl in
      let sides = data.sides in
      let rho = data.rho in
      let wpoint = data.wpoint in
      let stable = data.stable in

      let narrow_reuse = GobConfig.get_bool "solvers.td3.narrow-reuse" in
      let narrow_reuse_verify = GobConfig.get_bool "solvers.td3.narrow-reuse-verify" in

      let side_dep = data.side_dep in
      let side_infl = data.side_infl in
      (* If true, incremental destabilized side-effected vars will be restarted.
         If false, they are not. *)
      let restart_sided = GobConfig.get_bool "incremental.restart.sided.enabled" in
      (* If true, incremental side-effected var restart will only restart destabilized globals (using hack).
         If false, it will restart all destabilized side-effected vars. *)
      let restart_only_globals = GobConfig.get_bool "incremental.restart.sided.only-global" in

      (* If true, wpoint will be restarted to bot when added.
         This allows incremental to avoid reusing and republishing imprecise local values due to globals (which get restarted). *)
      let restart_wpoint = GobConfig.get_bool "solvers.td3.restart.wpoint.enabled" in
      (* If true, each wpoint will be restarted once when added.
         If false, it will be restarted each time it is added again (wpoints are removed after Narrow). *)
      let restart_once = GobConfig.get_bool "solvers.td3.restart.wpoint.once" in
      let restarted_wpoint = HM.create 10 in

      let incr_verify = GobConfig.get_bool "incremental.verify" in
      (* In incremental load, initially stable nodes, which are never destabilized.
         These don't have to be re-verified and warnings can be reused. *)
      let superstable = HM.copy stable in

      let var_messages = data.var_messages in
      let rho_write = data.rho_write in

      let abort = GobConfig.get_bool "solvers.td3.abort" in
      let destab_infl = HM.create 10 in
      let destab_front = HM.create 10 in
      let destab_dep = HM.create 10 in

      let abort_verify = GobConfig.get_bool "solvers.td3.abort-verify" in
      let prev_dep_vals = HM.create 10 in

      let () = print_solver_stats := fun () ->
        Printf.printf "|rho|=%d\n|called|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n|side_dep|=%d\n|side_infl|=%d\n"
          (HM.length rho) (HM.length called) (HM.length stable) (HM.length infl) (HM.length wpoint) (HM.length side_dep) (HM.length side_infl);
        print_context_stats rho
      in

      if GobConfig.get_bool "incremental.load" then print_data data "Loaded data for incremental analysis";

      let cache_sizes = ref [] in

      let trace_called () =
        if tracing then (
          let called_pretty () called =
            HM.fold (fun x _ acc -> Pretty.dprintf "%a\n  %a" S.Var.pretty_trace x Pretty.insert acc) called Pretty.nil
          in
          trace "sol2" "called:\n  %a\n" called_pretty called
        )
      in
      let vs_pretty () vs =
        VS.fold (fun x acc -> Pretty.dprintf "%a, %a" S.Var.pretty_trace x Pretty.insert acc) vs Pretty.nil
      in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty));
      in
      let add_sides y x = HM.replace sides y (VS.add x (try HM.find sides y with Not_found -> VS.empty)) in

      let destabilize_ref: (?front:bool -> S.v -> unit) ref = ref (fun ?front _ -> failwith "no destabilize yet") in
      let destabilize x = !destabilize_ref x in (* must be eta-expanded to use changed destabilize_ref *)

      let rec destabilize_vs x = (* TODO remove? Only used for side_widen cycle. *)
        if tracing then trace "sol2" "destabilize_vs %a\n" S.Var.pretty_trace x;
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.fold (fun y b ->
            let was_stable = HM.mem stable y in
            HM.remove stable y;
            HM.remove superstable y;
            HM.mem called y || destabilize_vs y || b || was_stable && List.mem y vs
          ) w false
      and solve ?reuse_eq ?(abortable=true) x phase (changed: bool): bool =
        if tracing then trace "sol2" "solve %a, phase: %s, changed: %b, abortable: %b, called: %b, stable: %b\n" S.Var.pretty_trace x (match phase with Widen -> "Widen" | Narrow -> "Narrow") changed abortable (HM.mem called x) (HM.mem stable x);
        trace_called ();
        init x;
        assert (S.system x <> None);
        if not (HM.mem called x || HM.mem stable x) then (
          if tracing then trace "sol2" "stable add %a\n" S.Var.pretty_trace x;
          HM.replace stable x ();
          HM.replace called x ();
          (* Here we cache HM.mem wpoint x before eq. If during eq eval makes x wpoint, then be still don't apply widening the first time, but just overwrite.
             It means that the first iteration at wpoint is still precise.
             This doesn't matter during normal solving (?), because old would be bot.
             This matters during incremental loading, when wpoints have been removed (or not marshaled) and are redetected.
             Then the previous local wpoint value is discarded automagically and not joined/widened, providing limited restarting of local wpoints. (See eval for more complete restarting.) *)
          let wp = HM.mem wpoint x in
          let l = HM.create 10 in
          let prev_dep_vals_x = HM.find_default prev_dep_vals x (HM.create 0) in (* used by abort_verify *)
          let eval' =
            if tracing then trace "sol2" "eval' %a abortable=%b destab_dep=%b\n" S.Var.pretty_trace x abortable (HM.mem destab_dep x);
            if abort && abortable && HM.mem destab_dep x then (
              let unasked_dep_x = ref (HM.find destab_dep x) in
              if tracing then trace "sol2" "eval' %a dep=%a\n" S.Var.pretty_trace x vs_pretty !unasked_dep_x;
              let all_dep_x_unchanged = ref true in
              let all_dep_x_unchanged_verify = ref true in
              fun y ->
                let (d, changed) = eval l x y in
                if tracing then trace "sol2" "eval' %a asked %a changed=%b mem=%b\n" S.Var.pretty_trace x S.Var.pretty_trace y changed (VS.mem y !unasked_dep_x);
                if abort_verify then (
                  let prev_d = HM.find_default prev_dep_vals_x y (S.Dom.bot ()) in
                  if not (S.Dom.equal prev_d d) then (
                    (* TODO: this is not a bug? change might be covered by destab_front *)
                    (* if not changed then (
                      ignore (Pretty.eprintf "not changed did change: eval %a %a: \nold=%a\n new=%a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty prev_d S.Dom.pretty d)
                    ); *)
                    all_dep_x_unchanged_verify := false;
                    if tracing then trace "sol2" "eval' %a asked %a abort %B verify\n  prev=%a\n   now=%a\n" S.Var.pretty_trace x S.Var.pretty_trace y (HM.mem prev_dep_vals_x y) S.Dom.pretty prev_d S.Dom.pretty d;
                  )
                );
                if VS.mem y !unasked_dep_x then (
                  unasked_dep_x := VS.remove y !unasked_dep_x;
                  if changed then
                    all_dep_x_unchanged := false;
                  if tracing then trace "sol2" "eval' %a asked %a checking abort unasked=%a all_unchanged=%b front=%b\n" S.Var.pretty_trace x S.Var.pretty_trace y vs_pretty !unasked_dep_x !all_dep_x_unchanged (HM.mem destab_front x);
                  let should_abort = VS.is_empty !unasked_dep_x && !all_dep_x_unchanged && not (HM.mem destab_front x) in (* must check front here, because each eval might change it for x *)
                  let should_abort_verify = !all_dep_x_unchanged_verify in
                  if should_abort then (
                    if abort_verify && not should_abort_verify then (
                      failwith (Pretty.sprint ~width:max_int (Pretty.dprintf "TD3 abort verify: should not abort %a" S.Var.pretty_trace x));
                    );
                    raise AbortEq
                  )
                );
                d
            )
            else
              fun y ->
                let (d, changed) = eval l x y in
                d
          in
          let (tmp, aborted) =
            match reuse_eq with
            | Some d when narrow_reuse && not narrow_reuse_verify ->
              if tracing then trace "sol2" "eq reused %a\n" S.Var.pretty_trace x;
              incr Goblintutil.narrow_reuses;
              (d, false)
            | _ ->
              try
                if abort && abort_verify then (
                  (* collect dep vals for x *)
                  let new_dep_vals_x = HM.create (HM.length prev_dep_vals_x) in
                  let eval' y =
                    let d = eval' y in
                    HM.replace new_dep_vals_x y d;
                    d
                  in
                  let tmp = eq x eval' (side ~x) in
                  HM.replace prev_dep_vals x new_dep_vals_x;
                  (tmp, false)
                )
                else
                  (eq x eval' (side ~x), false)
              with AbortEq ->
                abort_rhs_event x;
                if tracing then trace "sol2" "eq aborted %a\n" S.Var.pretty_trace x;
                HM.remove destab_dep x; (* TODO: safe to remove here? doesn't prevent some aborts? *)
                (* prev_dep_vals remain the same *)
                (HM.find rho x, true) (* old *)
          in
          begin match reuse_eq with
            | Some reuse_eq when narrow_reuse_verify && not (S.Dom.equal tmp reuse_eq) ->
              if M.tracing then trace "sol2" "reuse eq neq %a: %a %a\n" S.Var.pretty_trace x S.Dom.pretty reuse_eq S.Dom.pretty tmp;
              failwith (Pretty.sprint ~width:max_int (Pretty.dprintf "TD3 narrow reuse verify: should not reuse %a" S.Var.pretty_trace x))
            | _ ->
              ()
          end;
          let new_eq = tmp in
          (* let tmp = if GobConfig.get_bool "ana.opt.hashcons" then S.Dom.join (S.Dom.bot ()) tmp else tmp in (* Call hashcons via dummy join so that the tag of the rhs value is up to date. Otherwise we might get the same value as old, but still with a different tag (because no lattice operation was called after a change), and since Printable.HConsed.equal just looks at the tag, we would unnecessarily destabilize below. Seems like this does not happen. *) *)
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          HM.remove called x;
          HM.remove called_changed x;
          let old = HM.find rho x in (* find old value after eq since wpoint restarting in eq/eval might have changed it meanwhile *)
          let tmp =
            if not wp then tmp
            else
              if term then
                match phase with Widen -> S.Dom.widen old (S.Dom.join old tmp) | Narrow -> S.Dom.narrow old tmp
              else
                box x old tmp
          in
          if tracing then trace "sol" "Old value:%a\n" S.Dom.pretty old;
          if tracing then trace "sol" "New Value:%a\n" S.Dom.pretty tmp;
          if tracing then trace "cache" "cache size %d for %a\n" (HM.length l) S.Var.pretty_trace x;
          cache_sizes := HM.length l :: !cache_sizes;
          if not (Stats.time "S.Dom.equal" (fun () -> S.Dom.equal old tmp) ()) then (
            if tracing then trace "sol" "Changed\n";
            update_var_event x old tmp;
            HM.replace rho x tmp;
            HM.replace called_changed x ();
            if abort then (
              if HM.mem destab_front x then (
                if HM.mem stable x then (
                  (* If some side during eq made x unstable, then it should remain in destab_front.
                    Otherwise recursive solve might prematurely abort it. *)
                  if tracing then trace "sol2" "front remove %a\n" S.Var.pretty_trace x;
                  HM.remove destab_front x;
                );
                if HM.mem destab_infl x then (
                  VS.iter (fun y ->
                      if tracing then trace "sol2" "pushing front from %a to %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
                      if tracing then trace "sol2" "front add %a\n" S.Var.pretty_trace y;
                      HM.replace destab_front y ()
                    ) (HM.find destab_infl x)
                );
                HM.remove destab_infl x
              )
            );
            destabilize x;
            (solve[@tailcall]) x phase true
          ) else (
            (* TODO: why non-equal and non-stable checks in switched order compared to TD3 paper? *)
            if not (HM.mem stable x) then (
              (* If some side during eq made x unstable, then it should remain in destab_front.
                 Otherwise recursive solve might prematurely abort it. *)
              if tracing then trace "sol2" "solve still unstable %a\n" S.Var.pretty_trace x;
              (solve[@tailcall]) x Widen changed
            ) else (
              if abort && HM.mem destab_front x then (
                if tracing then trace "sol2" "front remove %a\n" S.Var.pretty_trace x;
                HM.remove destab_front x;
                if tracing then trace "sol2" "not pushing front from %a\n" S.Var.pretty_trace x;
                (* don't push front here *)
                HM.remove destab_infl x
              );
              if term && phase = Widen && HM.mem wpoint x then ( (* TODO: or use wp? *)
                if tracing then trace "sol2" "solve switching to narrow %a\n" S.Var.pretty_trace x;
                if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace x;
                HM.remove stable x;
                HM.remove superstable x;
                let reuse_eq = if not aborted then
                    Some new_eq
                  else
                    None (* TODO: for some reason cannot reuse aborted rhs, see 34-localwn_restart/02-hybrid *)
                in
                (solve[@tailcall]) ?reuse_eq ~abortable:false x Narrow changed
              ) else if not space && (not term || phase = Narrow) then ( (* this makes e.g. nested loops precise, ex. tests/regression/34-localization/01-nested.c - if we do not remove wpoint, the inner loop head will stay a wpoint and widen the outer loop variable. *)
                if tracing then trace "sol2" "solve removing wpoint %a (%b)\n" S.Var.pretty_trace x (HM.mem wpoint x);
                HM.remove wpoint x;
                changed
              )
              else
                changed
            )
          )
        )
        else if HM.mem called x then
          changed || HM.mem called_changed x
        else
          changed
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pretty_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      and simple_solve l x y: S.d * bool =
        if tracing then trace "sol2" "simple_solve %a (rhs: %b)\n" S.Var.pretty_trace y (S.system y <> None);
        if S.system y = None then (init y; (HM.find rho y, true (* TODO: ??? *))) else
        if HM.mem rho y || not space then (let changed = solve y Widen false in (HM.find rho y, changed)) else
        if abort then failwith "space abort unimplemented" else
        if HM.mem called y then (init y; HM.remove l y; (HM.find rho y, true (* TODO: ??? *))) else
        (* if HM.mem called y then (init y; let y' = HM.find_default l y (S.Dom.bot ()) in HM.replace rho y y'; HM.remove l y; y') else *)
        if cache && HM.mem l y then (HM.find l y, true (* TODO: ??? *))
        else (
          HM.replace called y ();
          (* TODO: abort? *)
          let tmp = eq y (fun z -> fst (eval l x z)) (side ~x) in
          HM.remove called y;
          if HM.mem rho y then (HM.remove l y; ignore (solve y Widen false); (HM.find rho y, true (* TODO: ??? *)))
          else (if cache then HM.replace l y tmp; (tmp, true (* TODO: ??? *)))
        )
      and eval l x y: S.d * bool =
        if tracing then trace "sol2" "eval %a ## %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
        get_var_event y;
        if HM.mem called y then (
          if restart_wpoint && not (HM.mem wpoint y) then (
            (* Even though solve cleverly restarts redetected wpoints during incremental load, the loop body would be calculated based on the old wpoint value.
               The loop body might then side effect the old value, see tests/incremental/06-local-wpoint-read.
               Here we avoid this, by setting it to bottom for the loop body eval. *)
            if not (restart_once && HM.mem restarted_wpoint y) then (
              if tracing then trace "sol2" "wpoint restart %a ## %a\n" S.Var.pretty_trace y S.Dom.pretty (HM.find_default rho y (S.Dom.bot ()));
              HM.replace rho y (S.Dom.bot ());
              HM.replace called_changed y (); (* just in case *)
              (* required for abort (front), for 34-localwn_restart/21-restart_abort_aget *)
              HM.remove stable y;
              HM.remove superstable y;
              destabilize y;
              if restart_once then (* avoid populating hashtable unnecessarily *)
                HM.replace restarted_wpoint y ();
            )
          );
          HM.replace wpoint y ();
        );
        let tmp = simple_solve l x y in
        if HM.mem rho y then add_infl y x;
        if tracing then trace "sol2" "eval %a ## %a -> %a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty (fst tmp);
        tmp
      and side ?x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
        if tracing then trace "sol2" "side to %a (wpx: %b) from %a ## value: %a\n" S.Var.pretty_trace y (HM.mem wpoint y) (Pretty.docOpt (S.Var.pretty_trace ())) x S.Dom.pretty d;
        if S.system y <> None then (
          ignore @@ Pretty.printf "side-effect to unknown w/ rhs: %a, contrib: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
        );
        assert (S.system y = None);
        init y;
        (match x with None -> () | Some x -> if side_widen = "unstable_self" then add_infl x y);
        let op =
          if HM.mem wpoint y then fun a b ->
            if M.tracing then M.traceli "sol2" "side widen %a %a\n" S.Dom.pretty a S.Dom.pretty b;
            let r = S.Dom.widen a (S.Dom.join a b) in
            if M.tracing then M.traceu "sol2" "-> %a\n" S.Dom.pretty r;
            r
          else S.Dom.join
        in
        let old = HM.find rho y in
        let tmp = op old d in
        if tracing then trace "sol2" "stable add %a\n" S.Var.pretty_trace y;
        HM.replace stable y ();
        if not (S.Dom.leq tmp old) then (
          (* if there already was a `side x y d` that changed rho[y] and now again, we make y a wpoint *)
          let old_sides = HM.find_default sides y VS.empty in
          let sided = match x with
            | Some x ->
              let sided = VS.mem x old_sides in
              if not sided then add_sides y x;
              sided
            | None -> false
          in
          (* HM.replace rho y ((if HM.mem wpoint y then S.Dom.widen old else identity) (S.Dom.join old d)); *)
          HM.replace rho y tmp;
          if side_widen <> "cycle" then destabilize y;
          (* make y a widening point if ... This will only matter for the next side _ y.  *)
          let wpoint_if e = if e then HM.replace wpoint y () in
          match side_widen with
          | "always" -> (* Any side-effect after the first one will be widened which will unnecessarily lose precision. *)
            wpoint_if true
          | "never" -> (* On side-effect cycles, this should terminate via the outer `solver` loop. TODO check. *)
            wpoint_if false
          | "sides" -> (* x caused more than one update to y. >=3 partial context calls will be precise since sides come from different x. TODO this has 8 instead of 5 phases of `solver` for side_cycle.c *)
            wpoint_if sided
          | "sides-pp" ->
            (match x with
            | Some x ->
              let n = S.Var.node x in
              let sided = VS.exists (fun v -> Node.equal (S.Var.node v) n) old_sides in
              wpoint_if sided
            | None -> ())
          | "cycle" -> (* destabilized a called or start var. Problem: two partial context calls will be precise, but third call will widen the state. *)
            (* if this side destabilized some of the initial unknowns vs, there may be a side-cycle between vs and we should make y a wpoint *)
            let destabilized_vs = destabilize_vs y in
            wpoint_if destabilized_vs
          (* TODO: The following two don't check if a vs got destabilized which may be a problem. *)
          | "unstable_self" -> (* TODO test/remove. Side to y destabilized itself via some infl-cycle. The above add_infl is only required for this option. Check for which examples this is problematic! *)
            wpoint_if @@ not (HM.mem stable y)
          | "unstable_called" -> (* TODO test/remove. Widen if any called var (not just y) is no longer stable. Expensive! *)
            wpoint_if @@ exists_key (neg (HM.mem stable)) called (* this is very expensive since it folds over called! see https://github.com/goblint/analyzer/issues/265#issuecomment-880748636 *)
          | x -> failwith ("Unknown value '" ^ x ^ "' for option solvers.td3.side_widen!")
        )
      and init x =
        if tracing then trace "sol2" "init %a\n" S.Var.pretty_trace x;
        if not (HM.mem rho x) then (
          new_var_event x;
          HM.replace rho x (S.Dom.bot ())
        )
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d;
        init x;
        HM.replace rho x d;
        HM.replace stable x ();
        (* solve x Widen *)
      in

      let destabilize_front ~front x w =
        if abort then (
          if front then (
            VS.iter (fun y ->
                if tracing then trace "sol2" "front add %a (infl)\n" S.Var.pretty_trace y;
                HM.replace destab_front y ()
              ) w;
            (* Also add front via destab_infl in case infl has already been removed by previous destabilize.
               This fixes 29-svcomp/27-td3-front-via-destab-infl. *)
            VS.iter (fun y ->
                if tracing then trace "sol2" "front add %a (destab_infl)\n" S.Var.pretty_trace y;
                HM.replace destab_front y ()
              ) (HM.find_default destab_infl x VS.empty)
          )
          else (
            HM.replace destab_infl x (VS.union w (HM.find_default destab_infl x VS.empty));
            VS.iter (fun y ->
                HM.replace destab_dep y (VS.add x (try HM.find destab_dep y with Not_found -> VS.empty))
              ) w
          )
        )
      in

      let rec destabilize_normal ?(front=true) x =
        if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace x;
        trace_called ();
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        destabilize_front ~front x w;
        VS.iter (fun y ->
            if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace y;
            HM.remove stable y;
            HM.remove superstable y;
            if not (HM.mem called y) then destabilize_normal ~front:false y
          ) w
      in

      start_event ();

      if GobConfig.get_bool "incremental.load" then (
        let c = S.increment.changes in
        List.(Printf.printf "change_info = { unchanged = %d; changed = %d; added = %d; removed = %d }\n" (length c.unchanged) (length c.changed) (length c.added) (length c.removed));

        let restart_leaf x =
          if tracing then trace "sol2" "Restarting to bot %a\n" S.Var.pretty_trace x;
          ignore (Pretty.printf "Restarting to bot %a\n" S.Var.pretty_trace x);
          HM.replace rho x (S.Dom.bot ());
          (* HM.remove rho x; *)
          HM.remove wpoint x; (* otherwise gets immediately widened during resolve *)
          HM.remove sides x; (* just in case *)

          (* immediately redo "side effect" from st *)
          match GobList.assoc_eq_opt S.Var.equal x st with
          | Some d ->
            HM.replace rho x d;
          | None ->
            ()
        in

        (* destabilize which restarts side-effected vars *)
        let rec destabilize_with_side ?(front=true) x =
          if tracing then trace "sol2" "destabilize_with_side %a\n" S.Var.pretty_trace x;

          (* is side-effected var (global/function entry)? *)
          let w = HM.find_default side_dep x VS.empty in
          HM.remove side_dep x;

          if not (VS.is_empty w) && (not restart_only_globals || Node.equal (S.Var.node x) (Function Cil.dummyFunDec)) && not (S.Var.is_write_only x) then (
            (* restart side-effected var *)
            restart_leaf x;

            (* add side_dep to front to prevent them from being aborted *)
            destabilize_front ~front:true x w;

            (* destabilize side dep to redo side effects *)
            VS.iter (fun y ->
                if tracing then trace "sol2" "destabilize_with_side %a side_dep %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
                if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace y;
                HM.remove stable y;
                HM.remove superstable y;
                destabilize_with_side ~front:false y
              ) w
          );

          (* destabilize eval infl *)
          let w = HM.find_default infl x VS.empty in
          HM.replace infl x VS.empty;
          destabilize_front ~front x w;
          VS.iter (fun y ->
              if tracing then trace "sol2" "destabilize_with_side %a infl %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
              if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace y;
              HM.remove stable y;
              HM.remove superstable y;
              destabilize_with_side ~front:false y
            ) w;

          (* destabilize side infl *)
          let w = HM.find_default side_infl x VS.empty in
          HM.remove side_infl x;
          (* TODO: should this also be conditional on restart_only_globals? right now goes through function entry side effects, but just doesn't restart them *)
          VS.iter (fun y ->
              if tracing then trace "sol2" "destabilize_with_side %a side_infl %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
              if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace y;
              HM.remove stable y;
              HM.remove superstable y;
              destabilize_with_side ~front:false y
            ) w
        in

        destabilize_ref :=
          if restart_sided then
            destabilize_with_side
          else
            destabilize_normal;

        let changed_funs = List.filter_map (function
            | {old = GFun (f, _); diff = None; _} ->
              print_endline ("Completely changed function: " ^ f.svar.vname);
              Some f
            | _ -> None
          ) S.increment.changes.changed
        in
        let part_changed_funs = List.filter_map (function
            | {old = GFun (f, _); diff = Some nd; _} ->
              print_endline ("Partially changed function: " ^ f.svar.vname);
              Some (f, nd.primObsoleteNodes, nd.unchangedNodes)
            | _ -> None
          ) S.increment.changes.changed
        in
        let removed_funs = List.filter_map (function
            | GFun (f, _) ->
              print_endline ("Removed function: " ^ f.svar.vname);
              Some f
            | _ -> None
          ) S.increment.changes.removed
        in

        let mark_node hm f node =
          let get x = try HM.find rho x with Not_found -> S.Dom.bot () in
          S.iter_vars get (Node {node; fundec = Some f}) (fun v ->
              HM.replace hm v ()
            )
        in

        let reluctant = GobConfig.get_bool "incremental.reluctant.on" in
        let reanalyze_entry f =
          (* destabilize the entry points of a changed function when reluctant is off,
             or the function is to be force-reanalyzed  *)
          (not reluctant) || CompareCIL.VarinfoSet.mem f.svar S.increment.changes.exclude_from_rel_destab
        in
        let obsolete_ret = HM.create 103 in
        let obsolete_entry = HM.create 103 in
        let obsolete_prim = HM.create 103 in

        (* When reluctant is on:
           Only add function entry nodes to obsolete_entry if they are in force-reanalyze *)
        List.iter (fun f ->
            if reanalyze_entry f then
              (* collect function entry for eager destabilization *)
              mark_node obsolete_entry f (FunctionEntry f)
            else
              (* collect function return for reluctant analysis *)
              mark_node obsolete_ret f (Function f)
          ) changed_funs;
        (* Unknowns from partially changed functions need only to be collected for eager destabilization when reluctant is off *)
        (* We utilize that force-reanalyzed functions are always considered as completely changed (and not partially changed) *)
        if not reluctant then (
          List.iter (fun (f, pn, _) ->
              List.iter (fun n ->
                  mark_node obsolete_prim f n
                ) pn;
              mark_node obsolete_ret f (Function f);
            ) part_changed_funs;
        );

        let old_ret = HM.create 103 in
        if reluctant then (
          (* save entries of changed functions in rho for the comparison whether the result has changed after a function specific solve *)
          HM.iter (fun k v ->
              if HM.mem rho k then (
                let old_rho = HM.find rho k in
                let old_infl = HM.find_default infl k VS.empty in
                HM.replace old_ret k (old_rho, old_infl)
              )
            ) obsolete_ret;
        );

        if not (HM.is_empty obsolete_entry) || not (HM.is_empty obsolete_prim) then
          print_endline "Destabilizing changed functions and primary old nodes ...";
        HM.iter (fun k _ ->
            if HM.mem stable k then
              destabilize k
          ) obsolete_entry;
        HM.iter (fun k _ ->
            if HM.mem stable k then
              destabilize k
          ) obsolete_prim;

        (* We remove all unknowns for program points in changed or removed functions from rho, stable, infl and wpoint *)
        let marked_for_deletion = HM.create 103 in

        let dummy_pseudo_return_node f =
          (* not the same as in CFG, but compares equal because of sid *)
          Node.Statement ({Cil.dummyStmt with sid = CfgTools.get_pseudo_return_id f})
        in
        let add_nodes_of_fun (functions: fundec list) (withEntry: fundec -> bool) =
          let add_stmts (f: fundec) =
            List.iter (fun s ->
                mark_node marked_for_deletion f (Statement s)
              ) f.sallstmts
          in
          List.iter (fun f ->
              if withEntry f then
                mark_node marked_for_deletion f (FunctionEntry f);
              mark_node marked_for_deletion f (Function f);
              add_stmts f;
              mark_node marked_for_deletion f (dummy_pseudo_return_node f)
            ) functions;
        in

        add_nodes_of_fun changed_funs reanalyze_entry;
        add_nodes_of_fun removed_funs (fun _ -> true);
        (* it is necessary to remove all unknowns for changed pseudo-returns because they have static ids *)
        let add_pseudo_return f un =
          let pseudo = dummy_pseudo_return_node f in
          if not (List.exists (Node.equal pseudo % fst) un) then
            mark_node marked_for_deletion f (dummy_pseudo_return_node f)
        in
        List.iter (fun (f,_,un) ->
            mark_node marked_for_deletion f (Function f);
            add_pseudo_return f un
          ) part_changed_funs;

        print_endline "Removing data for changed and removed functions...";
        let delete_marked s = HM.iter (fun k _ -> HM.remove s k) marked_for_deletion in
        delete_marked rho;
        delete_marked infl;
        delete_marked wpoint;

        (* destabilize_with_side doesn't have all infl to follow anymore, so should somewhat work with reluctant *)
        if restart_sided then (
          (* restarts old copies of functions and their (removed) side effects *)
          print_endline "Destabilizing sides of changed functions, primary old nodes and removed functions ...";
          HM.iter (fun k _ ->
              if HM.mem stable k then (
                ignore (Pretty.printf "marked %a\n" S.Var.pretty_trace k);
                destabilize k
              )
            ) marked_for_deletion
        );

        (* [destabilize_leaf] is meant for restarting of globals selected by the user. *)
        (* Must be called on a leaf! *)
        let destabilize_leaf (x : S.v) =
          let destab_side_dep (x : S.v) =
            let w = HM.find_default side_dep x VS.empty in
            if not (VS.is_empty w) then (
              HM.remove side_dep x;
              (* add side_dep to front to prevent them from being aborted *)
              destabilize_front ~front:true x w;
              (* destabilize side dep to redo side effects *)
              VS.iter (fun y ->
                  if tracing then trace "sol2" "destabilize_leaf %a side_dep %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
                  if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace y;
                  HM.remove stable y;
                  HM.remove superstable y;
                  destabilize_normal y
                ) w
            )
          in
          restart_leaf x;
          destab_side_dep x;
          destabilize_normal x

        in
        let globals_to_restart = S.increment.restarting in
        let get x = try HM.find rho x with Not_found -> S.Dom.bot () in

        List.iter
          (fun g ->
             S.iter_vars get g
               (fun v ->
                  if S.system v <> None then
                    ignore @@ Pretty.printf "Trying to restart non-leaf unknown %a. This has no effect.\n" S.Var.pretty_trace v
                  else if HM.mem stable v then
                    destabilize_leaf v)
          )
          globals_to_restart;

        let restart_and_destabilize x = (* destabilize_with_side doesn't restart x itself *)
          restart_leaf x;
          destabilize x
        in

        (* Call side on all globals and functions in the start variables to make sure that changes in the initializers are propagated.
         * This also destabilizes start functions if their start state changes because of globals that are neither in the start variables nor in the contexts *)
        List.iter (fun (v,d) ->
            if restart_sided then (
              match GobList.assoc_eq_opt S.Var.equal v data.st with
              | Some old_d when not (S.Dom.equal old_d d) ->
                ignore (Pretty.printf "Destabilizing and restarting changed start var %a\n" S.Var.pretty_trace v);
                restart_and_destabilize v (* restart side effect from start *)
              | _ ->
                (* don't restart unchanged start global *)
                (* no need to restart added start global (implicit bot before) *)
                (* restart removed start global below *)
                ()
            );
            side v d
          ) st;

        if restart_sided then (
          List.iter (fun (v, _) ->
              match GobList.assoc_eq_opt S.Var.equal v st with
              | None ->
                (* restart removed start global to allow it to be pruned from incremental solution *)
                (* this gets rid of its warnings and makes comparing with from scratch sensible *)
                ignore (Pretty.printf "Destabilizing and restarting removed start var %a\n" S.Var.pretty_trace v);
                restart_and_destabilize v
              | _ ->
                ()
            ) data.st
        );

        delete_marked stable;
        delete_marked side_dep;
        delete_marked side_infl;
        print_data data "Data after clean-up";

        (* TODO: reluctant doesn't call destabilize on removed functions or old copies of modified functions (e.g. after removing write), so those globals don't get restarted *)

        if reluctant then (
          (* solve on the return node of changed functions. Only destabilize the function's return node if the analysis result changed *)
          print_endline "Separately solving changed functions...";
          let op = if GobConfig.get_string "incremental.reluctant.compare" = "leq" then S.Dom.leq else S.Dom.equal in
          HM.iter (fun x (old_rho, old_infl) ->
              ignore @@ Pretty.printf "test for %a\n" Node.pretty_trace (S.Var.node x);
              ignore (solve x Widen false); (* TODO: use returned changed for comparison? *)
              if not (op (HM.find rho x) old_rho) then (
                print_endline "Destabilization required...";
                HM.replace infl x old_infl;
                destabilize x;
                HM.replace stable x ()
              )
            ) old_ret;

          print_endline "Final solve..."
        );

        (* reachability will populate these tables for incremental global restarting *)
        HM.clear side_dep;
        HM.clear side_infl;
      ) else (
        List.iter set_start st;
      );

      destabilize_ref := destabilize_normal; (* always use normal destabilize during actual solve *)

      List.iter init vs;
      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we side some global which v1 depends on with a new value. Then v1 is no longer stable and we have to solve it again. *)
      let i = ref 0 in
      let rec solver () = (* as while loop in paper *)
        incr i;
        let unstable_vs = List.filter (neg (HM.mem stable)) vs in
        if unstable_vs <> [] then (
          if GobConfig.get_bool "dbg.verbose" then (
            if !i = 1 then print_newline ();
            Printf.printf "Unstable solver start vars in %d. phase:\n" !i;
            List.iter (fun v -> ignore @@ Pretty.printf "\t%a\n" S.Var.pretty_trace v) unstable_vs;
            print_newline ();
            flush_all ();
          );
          List.iter (fun x -> ignore (solve x Widen false)) unstable_vs;
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
        let mem = HM.mem rho y in
        let d' = try HM.find rho y with Not_found -> S.Dom.bot () in
        if not (S.Dom.leq d d') then ignore @@ Pretty.printf "TDFP Fixpoint not reached in restore step at side-effected variable (mem: %b) %a from %a: %a not leq %a\n" mem S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty d S.Dom.pretty d'
      in
      let rec eq check x =
        HM.replace visited x ();
        match S.system x with
        | None -> if HM.mem rho x then HM.find rho x else (ignore @@ Pretty.printf "TDFP Found variable %a w/o rhs and w/o value in rho\n" S.Var.pretty_trace x; S.Dom.bot ())
        | Some f -> f (get ~check) (check_side x)
      and get ?(check=false) x =
        if HM.mem visited x then (
          HM.find rho x
        ) else if HM.mem rho x then ( (* `vs` are in `rho`, so to restore others we need to skip to `eq`. *)
          let d1 = HM.find rho x in
          let d2 = eq check x in (* just to reach unrestored variables *)
          if check then (
            if not (HM.mem stable x) && S.system x <> None then ignore @@ Pretty.printf "TDFP Found an unknown in rho that should be stable: %a\n" S.Var.pretty_trace x;
            if not (S.Dom.leq d2 d1) then
              ignore @@ Pretty.printf "TDFP Fixpoint not reached in restore step at %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" S.Var.pretty_trace x S.Dom.pretty d1 S.Dom.pretty d2 S.Dom.pretty_diff (d1,d2);
          );
          d1
        ) else (
          let d = eq check x in
          HM.replace rho x d;
          d
        )
      in
      (* restore values for non-widening-points *)
      if space && GobConfig.get_bool "solvers.td3.space_restore" then (
        if GobConfig.get_bool "dbg.verbose" then
          print_endline ("Restoring missing values.");
        let restore () =
          let get x =
            let d = get ~check:true x in
            if tracing then trace "sol2" "restored var %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d
          in
          List.iter get vs;
          HM.filteri_inplace (fun x _ -> HM.mem visited x) rho
        in
        Stats.time "restore" restore ();
        if GobConfig.get_bool "dbg.verbose" then ignore @@ Pretty.printf "Solved %d vars. Total of %d vars after restore.\n" !Goblintutil.vars (HM.length rho);
        let avg xs = if List.is_empty !cache_sizes then 0.0 else float_of_int (BatList.sum xs) /. float_of_int (List.length xs) in
        if tracing then trace "cache" "#caches: %d, max: %d, avg: %.2f\n" (List.length !cache_sizes) (List.max !cache_sizes) (avg !cache_sizes);
      );

      stop_event ();
      print_data data "Data after solve completed";

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Printf.printf "\nWidening points:\n";
        HM.iter (fun k () -> ignore @@ Pretty.printf "%a\n" S.Var.pretty_trace k) wpoint;
        print_newline ();
      );

      (* Prune other data structures than rho with reachable.
         These matter for the incremental data. *)
      let module IncrPrune: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Unit (S) (HM)

        let finalize ~vh ~reachable =
          VH.filteri_inplace (fun x _ ->
              VH.mem reachable x
            ) stable;

          (* filter both keys and value sets of a VS.t HM.t *)
          let filter_vs_hm hm =
            VH.filter_map_inplace (fun x vs ->
                if VH.mem reachable x then
                  Some (VS.filter (VH.mem reachable) vs)
                else
                  None
              ) hm
          in
          filter_vs_hm infl;
          filter_vs_hm side_infl;
          filter_vs_hm side_dep;

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

      (* postsolver also populates side_dep and side_infl *)
      let module SideInfl: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Unit (S) (HM)

        let one_side ~vh ~x ~y ~d =
          HM.replace side_dep y (VS.add x (try HM.find side_dep y with Not_found -> VS.empty));
          HM.replace side_infl x (VS.add y (try HM.find side_infl x with Not_found -> VS.empty));
      end
      in

      (* restart write-only *)
      HM.iter (fun x w ->
          HM.iter (fun y d ->
              HM.replace rho y (S.Dom.bot ());
            ) w
        ) rho_write;

      if incr_verify then (
        HM.filteri_inplace (fun x _ -> HM.mem superstable x) var_messages;
        HM.filteri_inplace (fun x _ -> HM.mem superstable x) rho_write
      )
      else (
        HM.clear var_messages;
        HM.clear rho_write
      );

      let init_reachable =
        if incr_verify then
          HM.copy superstable (* consider superstable reached: stop recursion (evaluation) and keep from being pruned *)
        else
          HM.create 0 (* doesn't matter, not used *)
      in

      let module IncrWarn: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Warn (S) (HM)

        let init () =
          init (); (* enable warning like standard Warn *)

          (* replay superstable messages *)
          if incr_verify then (
            HM.iter (fun _ m ->
                Messages.add m
              ) var_messages;
          );

          (* hook to collect new messages *)
          Messages.Table.add_hook := (fun m ->
            match !current_var with
            | Some x -> HM.add var_messages x m
            | None -> ()
          );
      end
      in

      (** Incremental write-only side effect restart handling:
          retriggers superstable ones (after restarting above) and collects new (non-superstable) ones. *)
      let module IncrWrite: PostSolver.S with module S = S and module VH = HM =
      struct
        include PostSolver.Unit (S) (HM)

        let init () =
          (* retrigger superstable side writes *)
          if incr_verify then (
            HM.iter (fun x w ->
                HM.iter (fun y d ->
                    let old_d = try HM.find rho y with Not_found -> S.Dom.bot () in
                    (* ignore (Pretty.printf "rho_write retrigger %a %a %a %a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty old_d S.Dom.pretty d); *)
                    HM.replace rho y (S.Dom.join old_d d);
                    HM.replace init_reachable y ();
                    HM.replace stable y (); (* make stable just in case, so following incremental load would have in superstable *)
                  ) w
              ) rho_write
          )

        let one_side ~vh ~x ~y ~d =
          if S.Var.is_write_only y then (
            (* ignore (Pretty.printf "rho_write collect %a %a %a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty d); *)
            HM.replace stable y (); (* make stable just in case, so following incremental load would have in superstable *)
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

        let postsolvers = (module IncrPrune: M) :: (module SideInfl: M) :: (module IncrWrite: M) :: (module IncrWarn: M) :: postsolvers

        let init_reachable ~vh =
          if incr_verify then
            init_reachable
          else
            HM.create (HM.length vh)
      end
      in

      let module Post = PostSolver.MakeIncrList (MakeIncrListArg) in

      Post.post st vs rho;

      print_data data "Data after postsolve";

      {st; infl; sides; rho; wpoint; stable; side_dep; side_infl; var_messages; rho_write}

    let solve box st vs =
      let reuse_stable = GobConfig.get_bool "incremental.stable" in
      let reuse_wpoint = GobConfig.get_bool "incremental.wpoint" in
      if GobConfig.get_bool "incremental.load" then (
        let loaded, data = match S.increment.old_data with
          | Some d -> true, Obj.obj d.solver_data
          | _ -> false, create_empty_data ()
        in
        (* This hack is for fixing hashconsing.
         * If hashcons is enabled now, then it also was for the loaded values (otherwise it would crash). If it is off, we don't need to do anything.
         * HashconsLifter uses BatHashcons.hashcons on Lattice operations like join, so we call join (with bot) to make sure that the old values will populate the empty hashcons table via side-effects and at the same time get new tags that are conform with its state.
         * The tags are used for `equals` and `compare` to avoid structural comparisons. TODO could this be replaced by `==` (if values are shared by hashcons they should be physically equal)?
         * We have to replace all tags since they are not derived from the value (like hash) but are incremented starting with 1, i.e. dependent on the order in which lattice operations for different values are called, which will very likely be different for an incremental run.
         * If we didn't do this, during solve, a rhs might give the same value as from the old rho but it wouldn't be detected as equal since the tags would be different.
         * In the worst case, every rhs would yield the same value, but we would destabilize for every var in rho until we replaced all values (just with new tags).
         * The other problem is that we would likely use more memory since values from old rho would not be shared with the same values in the hashcons table. So we would keep old values in memory until they are replace in rho and eventually garbage collected.
         *)
        (* Another problem are the tags for the context part of a S.Var.t.
         * This will cause problems when old and new vars interact or when new S.Dom values are used as context:
         * - reachability is a problem since it marks vars reachable with a new tag, which will remove vars with the same context but old tag from rho.
         * - If we destabilized a node with a call, we will also destabilize all vars of the called function. However, if we end up with the same state at the caller node, without hashcons we would only need to go over all vars in the function once to restabilize them since we have
         *   the old values, whereas with hashcons, we would get a context with a different tag, could not find the old value for that var, and have to recompute all vars in the function (without access to old values).
         *)
        if loaded && GobConfig.get_bool "ana.opt.hashcons" then (
          let rho' = HM.create (HM.length data.rho) in
          HM.iter (fun k v ->
            (* call hashcons on contexts and abstract values; results in new tags *)
            let k' = S.Var.relift k in
            let v' = S.Dom.relift v in
            HM.replace rho' k' v';
          ) data.rho;
          data.rho <- rho';
          let stable' = HM.create (HM.length data.stable) in
          HM.iter (fun k v ->
            HM.replace stable' (S.Var.relift k) v
          ) data.stable;
          data.stable <- stable';
          let wpoint' = HM.create (HM.length data.wpoint) in
          HM.iter (fun k v ->
            HM.replace wpoint' (S.Var.relift k) v
          ) data.wpoint;
          data.wpoint <- wpoint';
          let infl' = HM.create (HM.length data.infl) in
          HM.iter (fun k v ->
            HM.replace infl' (S.Var.relift k) (VS.map S.Var.relift v)
          ) data.infl;
          data.infl <- infl';
          let side_infl' = HM.create (HM.length data.side_infl) in
          HM.iter (fun k v ->
            HM.replace side_infl' (S.Var.relift k) (VS.map S.Var.relift v)
          ) data.side_infl;
          data.side_infl <- side_infl';
          let side_dep' = HM.create (HM.length data.side_dep) in
          HM.iter (fun k v ->
            HM.replace side_dep' (S.Var.relift k) (VS.map S.Var.relift v)
          ) data.side_dep;
          data.side_dep <- side_dep';
          data.st <- List.map (fun (k, v) -> S.Var.relift k, S.Dom.relift v) data.st;
          let var_messages' = HM.create (HM.length data.var_messages) in
          HM.iter (fun k v ->
              HM.add var_messages' (S.Var.relift k) v (* var_messages contains duplicate keys, so must add not replace! *)
            ) data.var_messages;
          data.var_messages <- var_messages';
          let rho_write' = HM.create (HM.length data.rho_write) in
          HM.iter (fun x w ->
              let w' = HM.create (HM.length w) in
              HM.iter (fun y d ->
                  HM.add w' (S.Var.relift y) (S.Dom.relift d) (* w contains duplicate keys, so must add not replace! *)
                ) w;
              HM.replace rho_write' (S.Var.relift x) w';
            ) data.rho_write;
          data.rho_write <- rho_write';
        );
        if not reuse_stable then (
          print_endline "Destabilizing everything!";
          data.stable <- HM.create 10;
          data.infl <- HM.create 10
        );
        if not reuse_wpoint then data.wpoint <- HM.create 10;
        let result = solve box st vs data in
        result.rho, result
      )
      else (
        let data = create_empty_data () in
        let result = solve box st vs data in
        result.rho, result
      )
  end

let _ =
  Selector.add_solver ("td3", (module WP : GenericEqBoxIncrSolver));
