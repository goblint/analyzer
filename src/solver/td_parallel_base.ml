(** Terminating, parallelized top-down solver with side effects. ([td_parallel_base]). *)

(** Top-down solver that is parallelised with fine-grain-locked shared data 
  * 
  * The solver consists of multiple threads, that operate on the same data. 
  * The solvers starts with a single thread, and starts a new one at every `create` call it encounters.
  * Create nodes are created by the analysis. For the purposes of this solver, they can be placed anywhere,
  * however the solver benefits from having those at points where the analysis branches into mostly 
  * disjunt parts, such as thread creation in the analysed program.
  * The starting points of the threads are memorized. If such a point is destabilized after thread 
  * termination, the thread is restarted. 
*)
(* Options:
 * - solvers.td3.parallel_domains (default: 0 - automatic selection): Maximal number of Domains that the solver can use in parallel.
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes
open Goblint_parallel
open Saturn
open Messages


module Base : DemandEqSolver = 
  functor (S: DemandEqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S.Dom)
    module VS = Set.Make (S.Var)

    (* TODO Introduce this module *)
    open ParallelStats.ParallelSolverStats

    exception CasFailException

    (* Same as [Atomic.compare_and_set] but raises an exception on failure. *)
    let cas r seen v =
      if (Atomic.compare_and_set r seen v) then (
        cas_success_event (); 
      ) else (
        cas_fail_event ();
        raise CasFailException
      )
    (* if not (Atomic.compare_and_set r seen v) then raise CasFailException *)

    (** State for each unkown and a default factory. *)
    module DefaultState = struct
      type t = {
        value: S.Dom.t;
        infl: (S.Var.t, unit) Htbl.t;  (** Unknowns influenced this unknown *)
        wpoint: bool;    (** Unkown is a widening point *)
        stable: bool;    (** Unknown is stable, i.e. its value is known unless its dependencies change *)
        called: bool;    (** Unknown is currently being solved by any thread *)
        top_level: bool; (** Unknown is the starting point of a solver thread *)
      }
      let default () = {
        value = S.Dom.bot ();
        infl = Htbl.create ~hashed_type:(module S.Var) ();
        called = false;
        stable = false;
        wpoint = false;
        top_level = false;
      }
      let show s = 
        Printf.sprintf "{value: %s; infl: %d; wpoint: %b; stable: %b; called: %b; top_level: %b}" 
          (S.Dom.show s.value) (Htbl.length s.infl) s.wpoint s.stable s.called s.top_level
    end

    (** Concurrency safe hashmap for the state of the unknowns. *)
    module CM = Data.ConcurrentHashmap (S.Var) (DefaultState) (HM)
    (* We need to keep track of this to avoid queueing multiple jobs for the same unknown. *)
    let unknowns_with_running_jobs = Htbl.create ()
    let job_id_counter = (Atomic.make 1)

    let solve st vs =
      solver_start_event ();
      let nr_domains = GobConfig.get_int "solvers.td3.parallel_domains" in
      let nr_domains = if nr_domains = 0 then (Domain.recommended_domain_count ()) else nr_domains in

      (* The argument of threadpool.create is the number of additional domains, hence -1 *)
      (* This comes from domainslib *)
      let pool = Threadpool.create (nr_domains-1) in

      let data = CM.create ()
      in

      (** Initialize or get the state for an unknown. 
          @param x The unknown to get the state for.
          @param thread_id The id of the thread that is initializing the unknown.
          @return true if the unknown was created, false otherwise.
      *)
      let init x =
        let value, was_created = CM.find_create data x in
        value
      in

      (** Get the right-hand-side for an unknown.
          @param x The unknown to get the rhs for.
          @param get Function to return values for unknowns.
          @param set Function to set values for unknowns.
          @param create Function to handle create nodes/ initialize new solver threads.
          @return The rhs for the unknown.
      *)
      let eq x get set create =
        if tracing then trace "eq" "eq %a" S.Var.pretty_trace x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set create
      in

      (** Check if the unknown is a global. 
          @param x The unknown to check.
          @return true if the unknown is a global, false otherwise.
      *)
      let is_global x = S.system x = None in

      (** destabilizes vars from outer_w and their infl recursively. 
          If a variable was the root of a solver thread, a new thread is started for the variable. 
          @param prom The promises list to add new threads to.
          @param outer_w The set of variables to destabilize.
      *)
      let rec destabilize prom outer_w =
        let rec destab_single y =
          try (
            let y_atom = CM.find data y in
            let y_state = Atomic.get y_atom in
            if not y_state.stable then (
              ()
            ) else if y_state.called then (
              (* If y is called, we do not need to destabilize, as it will happen after the value change anyway. *)
              cas y_atom y_state {y_state with stable = false};
              if tracing then trace "destab" "stable remove %a (top_level:%b, called:%b)" S.Var.pretty_trace y y_state.top_level y_state.called;
            ) else (
              let inner_w = y_state.infl in
              cas y_atom y_state {y_state with stable = false};
              if tracing then trace "destab" "stable remove %a (top_level:%b, called:%b)" S.Var.pretty_trace y y_state.top_level y_state.called;
              if y_state.top_level then create_task prom y; 
              destabilize prom inner_w
            )) with CasFailException -> destab_single y
        in
        Htbl.remove_all outer_w |> Seq.map fst |> 
        Seq.iter destab_single

      (** Creates a task to solve for y 
          @param outer_prom The promises list to add the new task to.
          @param y The variable to solve for.
      *)
      and create_task outer_prom y =
        let work_fun () =
          let job_id = Atomic.fetch_and_add job_id_counter 1 in
          let y_atom = init y in
          let s = Atomic.get y_atom in
          if s.called then (
            (* instant_return_event () *)
          ) else (
            let success = Atomic.compare_and_set y_atom s {s with called = true; stable = true; top_level = true} in 
            if success then (
              if tracing then trace "thread_pool" "starting task %d to iterate %a" job_id S.Var.pretty_trace y;
              (* thread_starts_solve_event job_id; *)
              let inner_prom = ref [] in
              iterate None inner_prom y job_id y_atom;
              (* This is safe to ignore, as it can only be removed by someone else. *)
              (* For it to be added by another thread, it needs to be removed first *)
              Htbl.try_remove unknowns_with_running_jobs y |> ignore;
              (* thread_ends_solve_event job_id; *)
              Threadpool.await_all pool (!inner_prom)
            ) 
            (* Nothing to do if CAS fails, another thread is already working on this variable. *)
          )
        in
        if not (Htbl.mem unknowns_with_running_jobs y) then (
          if tracing then trace "create" "create_task %a" S.Var.pretty_trace y;
          (* Here a failure can be ignored, as it is very rare and
             a missing entry causes recalculation, not a unsound result. *)
          Htbl.try_set unknowns_with_running_jobs y () |> ignore;
          outer_prom := Threadpool.add_work pool work_fun :: (!outer_prom)
        )

      (** Iterates to solve for x (invoked from query to orig if present) 
          @param orig The variable whose query led to the iteration of x.
          @param prom The promises list to add new tasks to.
          @param x The variable to solve for.
          @param job_id The id of the thread that is solving for x.
          @param x_atom The atomic reference to the state of x, to prevent unnecessary lookups.
      *)
      and iterate orig prom x job_id x_atom = (* ~(inner) solve in td3*)

        (** Get the value for y, triggering an iteration if necessary, and performing a lookup otherwise. 
            @param x The unknown whose query led to the query for y, so that the infl of y can be updated.
            @param y The unknown to get the value for.
            @return The value of y.
        *)
        let rec query x y = (* ~eval in td3 *)
          (* Query with atomics: if anything is changed, query is repeated and the initial call *)
          (* has no side effects. Thus, imitating that the query just happend in a later point in time.*)
          try (
            let y_atom = init y in
            (* get_var_event y; *)
            let y_state = Atomic.get y_atom in
            if tracing then trace "query" "%d entering query for %a; stable %b; called %b" job_id S.Var.pretty_trace y y_state.stable y_state.called;
            ignore @@ Htbl.try_add y_state.infl x ();

            if y_state.called then (
              if tracing then trace "infl" "add_infl %a %a" S.Var.pretty_trace y S.Var.pretty_trace x;
              cas y_atom y_state {y_state with wpoint=true};
              y_state.value
            ) 
            else if y_state.stable then (
              (Atomic.get y_atom).value
            ) else (
              if is_global y then (
                if tracing then trace "infl" "add_infl %a %a" S.Var.pretty_trace y S.Var.pretty_trace x;
                cas y_atom y_state {y_state with stable = true};
                y_state.value
              ) else (
                if tracing then trace "infl" "add_infl %a %a" S.Var.pretty_trace y S.Var.pretty_trace x;
                cas y_atom y_state {y_state with stable = true; called=true};
                if tracing then trace "called" "called %a" S.Var.pretty_trace y;
                iterate (Some x) prom y job_id y_atom;
                (Atomic.get y_atom).value
              )
            ) ) with CasFailException -> query x y
        in

        (** Apply a side effect to y
            @param x The variable that caused the side effect.
            @param y The variable to side-effect.
            @param d The value to side-effect y with.
        *)
        let rec side x y d =
          assert (is_global y);
          try (
            let y_atom = init y in
            let s = Atomic.get y_atom in
            if tracing then trace "side" "%d side to %a from %a" job_id S.Var.pretty_trace y S.Var.pretty_trace x;
            if tracing then trace "side-v" "%d side to %a (wpx: %b) from %a ## value: %a" job_id S.Var.pretty_trace y s.wpoint S.Var.pretty_trace x S.Dom.pretty d;
            let old = s.value in
            if S.Dom.leq d old then (
              ()
            ) else (
              let widen a b =
                if tracing then trace "sidew" "%d side widen %a" job_id S.Var.pretty_trace y;
                S.Dom.widen a (S.Dom.join a b)
              in 
              if tracing then trace "update" "%d side update %a with \n\t%a" job_id S.Var.pretty_trace x S.Dom.pretty (widen old d);
              let w = s.infl in
              let new_s = {s with value = (widen old d); stable = true} in
              cas y_atom s new_s;
              if tracing then trace "destab" "destabilize %a" S.Var.pretty_trace y;
              (* update_var_event job_id y old (new_s.value); *)
              destabilize prom w
            )) with CasFailException -> side x y d
        in

        (** Handle create nodes
            @param x The variable that caused the create node.
            @param y The variable to create a solver task for.
        *)
        let create x y = (* create called from x on y *)
          if tracing then trace "create" "create from td_parallel_base was executed from %a on %a" S.Var.pretty_trace x S.Var.pretty_trace y;
          create_task prom y
        in

        (* begining of iteration to update the value for x *)
        start_iterate_event job_id;
        assert (not @@ is_global x);
        let x_state = Atomic.get x_atom in

        if tracing then trace "iter" "%d iterate %a, stable: %b, wpoint: %b" job_id S.Var.pretty_trace x x_state.stable x_state.wpoint;
        let x_is_widening_point = x_state.wpoint in (* if x becomes a wpoint during eq, checking this will delay widening until next iterate *)
        (* eval_rhs_event job_id x; *)
        let value_from_rhs = eq x (query x) (side x) (create x) in
        let x_state = Atomic.get x_atom in
        let old_value = x_state.value in
        let new_value = (* value after box operator (if wp: widening) *)
          if not x_is_widening_point then 
            value_from_rhs
          else (if tracing then trace "wpoint" "box widening %a" S.Var.pretty_trace x; box old_value value_from_rhs)
        in

        if S.Dom.equal new_value old_value then (
          if x_state.stable then (
            (match orig with 
             | Some z -> ignore @@ Htbl.try_add x_state.infl z ()
             | None -> ());
            let x_state_new = {x_state with called = false; wpoint = false} in
            try (cas x_atom x_state x_state_new;
                 if tracing then trace "called" "uncalled (A) %a" S.Var.pretty_trace x;
                ) with CasFailException -> (iterate[@tailcall]) orig prom x job_id x_atom;
          ) else (
            let x_state_new = {x_state with stable = true} in
            (* No need to track cas success, as we will iterate again anyway. *)
            ignore @@ Atomic.compare_and_set x_atom x_state x_state_new;
            if tracing then trace "iter" "iterate still unstable %a" S.Var.pretty_trace x;
            (iterate[@tailcall]) orig prom x job_id x_atom
          )
        ) else (
          (* value has changed *)
          if tracing then trace "update" "%d iterate update %a with \n\t%a" job_id S.Var.pretty_trace x S.Dom.pretty new_value;
          let x_state_new = {x_state with value = new_value} in
          try (
            cas x_atom x_state x_state_new;
            if tracing then trace "destab" "destabilize %a" S.Var.pretty_trace x;
            (* update_var_event job_id x old_value new_value; *)
            destabilize prom x_state.infl;

            let rec finalize () =
              let x_state = Atomic.get x_atom in

              if x_state.stable then (
                (match orig with
                 | Some z -> ignore @@ Htbl.try_add x_state.infl z ()
                 | None -> ());
                let new_s = {x_state with called = false} in
                try (cas x_atom x_state new_s;
                     if tracing then trace "called" "uncalled (B) %a" S.Var.pretty_trace x;
                    )
                with CasFailException -> (finalize[@tailcall]) ()
              ) else (
                let new_s = {x_state with stable = true} in
                (* Here we cannot use the exception, because the handling would *)
                (* break the tail-recursion in the call to iterate *)
                let success = Atomic.compare_and_set x_atom x_state new_s in 
                if success then (
                  if tracing then trace "iter" "iterate changed %a" S.Var.pretty_trace x;
                  (iterate[@tailcall]) orig prom x job_id x_atom
                ) else (finalize[@tailcall]) ()
              ) in
            finalize ();
          ) with CasFailException -> (iterate[@tailcall]) orig prom x job_id x_atom;
        ) in

      let set_start (x,d) =
        let x_atom = init x in
        let s = Atomic.get x_atom in
        Atomic.set x_atom {s with value = d; stable = true}
      in

      (* beginning of main solve *)

      (* start_event (); *)
      List.iter set_start st;

      List.iter (fun x -> ignore @@ init x ) vs;
      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we 
         side some global which v1 depends on with a new value. 
         Then v1 is no longer stable and we have to solve it again. *)
      let i = ref 0 in
      let rec solver () = 
        incr i;
        let unstable_vs = List.filter (fun v -> not (Atomic.get @@ CM.find data v).stable) vs in
        if unstable_vs <> [] then (
          if Logs.Level.should_log Debug then (
            if !i = 1 then Logs.newline ();
            Logs.debug "Unstable solver start vars in %d. phase:" !i;
            List.iter (fun v -> Logs.debug "\t%a" S.Var.pretty_trace v) unstable_vs;
            Logs.newline ();
            flush_all ();
          );
          List.iter (fun x -> 
              if tracing then trace "multivar" "solving for %a" S.Var.pretty_trace x;
              Threadpool.run pool (fun () -> 
                  let promises = ref [] in
                  create_task promises x;
                  Threadpool.await_all pool (!promises)
                );
            ) unstable_vs;
          solver ();
        )
      in
      solver ();
      Threadpool.finished_with pool;
      solver_end_event ();

      (* After termination, only those variables are stable which are
       * - reachable from any of the queried variables vs, or
       * - effected by side-effects and have no constraints on their own (this should be the case for all of our analyses). *)
      (* print_stats (); *)
      (* stop_event (); *)

      let data_ht = CM.to_hashtbl data in
      let wpoint = HM.map (fun _ (s: DefaultState.t) -> s.wpoint) data_ht in

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Logs.newline ();
        Logs.debug "Widening points:";
        HM.iter (fun k wp -> if wp then Logs.debug "%a" S.Var.pretty_trace k) wpoint;
        Logs.newline ();
      );

      print_stats ();
      (* TODO reenable *)
      (* if GobConfig.get_bool "dbg.timing.enabled" then LHM.print_stats data; *)

      let solution = HM.map (fun _ (s: DefaultState.t) -> s.value) data_ht in
      Logs.info "Solver finished with %d unknowns." (HM.length solution);
      Logs.info "Number of jobs: %d" (Atomic.get job_id_counter);
      solution
  end

let () =
  Selector.add_solver ("td_parallel_base", (module PostSolver.DemandEqIncrSolverFromDemandEqSolver (Base)))
