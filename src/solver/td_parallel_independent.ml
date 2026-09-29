(** Terminating, parallelized top-down solver with side effects ([td_parallel_independent]).

    The independent approach of
    {{:https://doi.org/10.1007/978-3-032-22749-2_9} Kocal et al., Same Engine, Multiple Gears: Parallelizing Fixpoint Iteration at Different Granularities (TACAS 2026)}:
    tasks share as little data as possible, each has its own copy of the unknowns' data.
    The solver starts with a single task, and starts a new one at every [create] call it encounters.
    Create calls are issued by the analysis. For the correctness of this solver, they can be placed anywhere,
    however the solver benefits from having them at points where the analysis branches into mostly
    disjoint parts, such as thread creation in the analysed program.
    Changes to global unknowns are published to a publish/subscribe queue, which every subscribed task
    consumes after every RHS evaluation.
    If a task that has finished receives an update it depends on, it is revived. *)
(* Options:
 * - solvers.td_parallel.domains (default: -1 - value of jobs; 0 - automatic selection based on available cores): Maximal number of Domains that the solver can use in parallel.
 * The solvers.td3 options are not read: side-effects to globals are always widened (as TD3 with solvers.td3.side_widen = always).
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes
open Goblint_parallel
open Messages

module Htbl = Saturn.Htbl
module Stack = Saturn.Stack

module type Key = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

module type MessageQueueParams = sig
  module Subscriber: Key
  type message
  module Topic: Key
end

module MessageQueue (X: MessageQueueParams) = struct
  type subscriber = X.Subscriber.t
  type message = X.message
  type topic = X.Topic.t

  type t = {
    subscriptions: (topic, (subscriber Stack.t)) Htbl.t;
    messages: (subscriber, (message Stack.t)) Htbl.t;
    messages_by_topic: (topic, (message Stack.t)) Htbl.t;
  }

  let create () = {
    subscriptions = Htbl.create ~hashed_type:(module X.Topic) ();
    messages = Htbl.create ~hashed_type:(module X.Subscriber) ();
    messages_by_topic = Htbl.create ~hashed_type:(module X.Topic) ()
  }

  (* The stack stored for [key] in [tbl], adding an empty one if there is none. *)
  let find_or_add tbl key =
    let stack = Stack.create () in
    if Htbl.try_add tbl key stack then stack else Htbl.find_exn tbl key

  let subscribe mq topic subscriber =
    Stack.push (find_or_add mq.subscriptions topic) subscriber;

    let queue_for_subscriber = find_or_add mq.messages subscriber in
    let unread_messages = match (Htbl.find_opt mq.messages_by_topic topic) with
      | None -> Seq.empty
      | Some s -> Stack.to_seq s in
    Seq.iter (Stack.push queue_for_subscriber) unread_messages


  let push (mq : t) (topic : topic) (message : message): unit =
    Stack.push (find_or_add mq.messages_by_topic topic) message;


    let subs: subscriber Seq.t = match Htbl.find_opt mq.subscriptions topic with
      | Some l -> Stack.to_seq l
      | None -> Seq.empty in

    let push_to_subscriber (subscriber : subscriber): unit =
      Stack.push (find_or_add mq.messages subscriber) message
    in
    Seq.iter push_to_subscriber subs

  let consume mq subscriber f =
    let rec consume_queue q =
      match Stack.pop_opt q with
      | None -> ()
      | Some v -> (f v; consume_queue q) in
    let q = Htbl.find_opt mq.messages subscriber in
    match q with
      None -> if tracing then trace "sub" "is none"
    | Some q -> consume_queue q

  let is_empty mq subscriber = Option.map_default Stack.is_empty true (Htbl.find_opt mq.messages subscriber)
end

module Base : DemandEqSolver =
  functor (S: DemandEqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  struct
    open SolverBox.Warrow (S.Dom)

    module VS = Set.Make (S.Var)

    open ParallelStats.ParallelSolverStats

    module Sides = struct
      module MQ = MessageQueue (struct
          module Subscriber = S.Var (* identify threads by their root unknown *)
          type message = S.Var.t * S.Dom.t
          module Topic = S.Var
        end)
      let mq = MQ.create ()
      type remaining_status = NewSide | Fin

      let process_updates thread_id thread_root_var f =
        if tracing then trace "process" "process begin";
        let f_log s = if tracing then trace "process" "from: %d, on: %a" thread_id S.Var.pretty_trace (fst s); f s in
        MQ.consume mq thread_root_var f_log;
        if tracing then trace "process" "process end"

      let add_side thread_id resolve ((v,d) as side) =
        if tracing then trace "handle" "Adding %a with %a" S.Var.pretty_trace v S.Dom.pretty d;
        MQ.push mq v side; resolve ()
      let subscribe v thread_root_var =
        if tracing then trace "sub" "%a: subscription to %a" S.Var.pretty_trace thread_root_var S.Var.pretty_trace v;
        MQ.subscribe mq v thread_root_var

      let updates_or_fin thread_id thread_root_var mark_prelim =
        if MQ.is_empty mq thread_root_var then (mark_prelim (); Fin) else NewSide
    end

    type unknown_data = {
      infl: VS.t;
      rho: S.Dom.t;
      wpoint: bool;
      stable: bool;
      called: bool
    }

    let create_unknown_data () = {
      infl = VS.empty;
      rho = S.Dom.bot ();
      wpoint = false;
      stable = false;
      called = false
    }

    type solver_data = {
      unknowns: unknown_data ref HM.t;
      subscriptions: (S.Var.t, unit) Htbl.t;
    }

    let create_empty_data () = {
      unknowns = HM.create 10;
      subscriptions = Htbl.create ~hashed_type:(module S.Var)();
    }

    let init (unknowns : unknown_data ref HM.t) x =
      let found_data = HM.find_option unknowns x in
      match found_data with
      | Some data -> data
      | None ->
        let data = ref @@ create_unknown_data () in
        if tracing then trace "init" "init %a" S.Var.pretty_trace x;
        HM.replace unknowns x data;
        data

    let create_start_data st =
      let data = create_empty_data () in
      (* Start variables are provided as pairs of variable and value to the solver *)
      (* The following block brings the data into the format that the solver expects *)
      let set_start (x,d) =
        let new_ref = init data.unknowns x in
        new_ref := {!new_ref with rho = d; stable = true};
      in
      List.iter set_start st;
      data


    let solve st vs =
      let nr_domains = match GobConfig.get_int "solvers.td_parallel.domains" with
        | -1 -> GobConfig.get_int "jobs"
        | n -> n
      in
      let nr_domains = if nr_domains <= 0 then Domain.recommended_domain_count () else nr_domains in

      (* As in domainslib, the argument of Threadpool.create is the number of additional domains, hence -1 *)
      let pool = Threadpool.create (nr_domains - 1) in

      (* Promises keep track of the threads that are possibly still running *)
      (* We only add to this when processing create and reviving *)
      let promises = ref [] in
      (* TODO: Again, a thread safe data structure would be better *)
      (* Even if we end-up using mutexes, it is simpler to use *)
      let prom_mutex = GobMutex.create () in

      (* Unknowns a task has been created for by [create], so that none is created twice.
         Revived tasks do not go through it. Protected by prom_mutex, as [create] runs on any domain. *)
      let created_vars = HM.create 10 in
      (* Suspended tasks: stopped, but not necessarily final, as updates can still arrive and revive them.
         Protected by prom_mutex, as suspending and reviving run on any domain. *)
      let prelim_vars = ref [] in

      let job_id_counter = (Atomic.make 1) in

      solver_start_event ();

      (** solves for a single point-of-interest variable (x_poi) *)
      (* primary means user is interested in the result *)
      let rec solve_single is_primary x_poi sd job_id =
        if tracing then trace "handle" "solving for: %a" S.Var.pretty_trace x_poi;
        let unknowns = sd.unknowns in
        let subs = sd.subscriptions in

        let add_infl y x =
          if tracing then trace "infl" "%d add %a influences %a" job_id S.Var.pretty_trace y S.Var.pretty_trace x;
          let y_ref = init unknowns y in
          y_ref := {!y_ref with infl = VS.add x !y_ref.infl}
        in

        let eq x get set create =
          if tracing then trace "eq" "eq %a" S.Var.pretty_trace x;
          match S.system x with
          | None -> S.Dom.bot ()
          | Some f -> f get set create
        in

        let rec destabilize outer_w =
          VS.iter (fun y ->
              let y_ref = HM.find unknowns y in
              if not (!y_ref.stable) then
                ()
              else if !y_ref.called then (
                if tracing then trace "destab" "%d stable remove %a" job_id S.Var.pretty_trace y;
                y_ref := {!y_ref with stable = false};
              ) else (
                let inner_w = !y_ref.infl in
                if tracing then trace "destab" "%d stable remove %a" job_id S.Var.pretty_trace y;
                y_ref := {!y_ref with infl = VS.empty; stable = false};
                destabilize inner_w
              )
            ) outer_w
        in

        (** iterates to solve for x *)
        let rec iterate orig x = (* ~(inner) solve in td3*)
          let query x y = (* ~eval in td3 *)
            let y_ref = init unknowns y in
            if tracing then trace "sol_query" "%d query for %a from %a; stable %b; called %b" job_id S.Var.pretty_trace y S.Var.pretty_trace x (!y_ref.stable) (!y_ref.called);
            if !y_ref.stable || !y_ref.called then (
              if !y_ref.called then (y_ref := {!y_ref with wpoint = true});
              add_infl y x
            ) else (
              if S.system y = None then (
                y_ref := {!y_ref with stable = true};
                if not (Htbl.mem subs y) then (
                  ignore @@ Htbl.try_add subs y ();
                  if tracing then trace "sub" "%a subscribed to %a" S.Var.pretty_trace x_poi S.Var.pretty_trace y;
                  Sides.subscribe y x_poi);
                (* Normally, we process updates in iterate *)
                (* For vars without constraints, we need to handle sides here *)
                (* as they do not result in an iterate call *)
                if tracing then trace "process" "from query";
                Sides.process_updates job_id x_poi handle_side;
                add_infl y x
              ) else (
                y_ref := {!y_ref with called = true; stable = true};
                iterate (Some x) y
                (* Infl will be added in iterate *)
              )
            );
            let value = !y_ref.rho in
            if tracing then trace "answer" "exiting query for %a\nanswer: %a" S.Var.pretty_trace y S.Dom.pretty value;
            value
          in

          let side x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
            let y_ref = init unknowns y in
            if tracing then trace "doside" "update to %a; value %a" S.Var.pretty_trace y S.Dom.pretty d;
            if tracing then trace "side" "%d side to %a (wpx: %b) from %a" job_id S.Var.pretty_trace y (!y_ref.wpoint) S.Var.pretty_trace x;
            (* Globals are not updated in rho here: this happens in handle_side, also for the publishing task itself. *)
            publish_side y d
          in

          let create x y = (* create called from x on y *)
            if tracing then trace "create" "create from td_parallel_independent is being executed from %a on %a" S.Var.pretty_trace x S.Var.pretty_trace y;
            GobMutex.lock prom_mutex;
            if HM.mem created_vars y then
              ()
            else (
              HM.replace created_vars y ();
              (* Solve single does not create its data, but expects it to be created before the call *)
              (* At least st must be passed to create start data *)

              (* We can possibly reuse some data, but it is not happening yet *)
              let new_sd = create_start_data st in
              let new_id = Atomic.fetch_and_add job_id_counter 1 in
              if tracing then trace "thread_pool" "%d adding job %d to solve for %a(%d)" job_id new_id S.Var.pretty_trace y (S.Var.hash y);
              (* TODO: are all primaries surely started or at least added to created_vars before? *)
              promises := (Threadpool.add_work pool (fun () -> solve_single false y new_sd new_id))::!promises
            );
            GobMutex.unlock prom_mutex
          in

          (* beginning of iterate *)
          start_iterate_event job_id;
          assert (S.system x <> None);
          let x_ref = init unknowns x in
          if tracing then trace "sol2" "iterate %a, called: %b, stable: %b, wpoint: %b" S.Var.pretty_trace x (!x_ref.called) (!x_ref.stable) (!x_ref.wpoint);
          let x_is_widening_point = !x_ref.wpoint in (* if x becomes a wpoint during eq, checking this will delay widening until next iterate *)
          let value_from_rhs = eq x (query x) (side x) (create x) in
          (* Process updates after every rhs evaluation *)
          Sides.process_updates job_id x_poi handle_side;
          let old_value = !x_ref.rho in
          let new_value = (* value after box operator (if wp: widening) *)
            if not x_is_widening_point then value_from_rhs
            else box old_value value_from_rhs
          in
          (* TODO: wrap S.Dom.equal in timing if a reasonable threadsafe timing becomes available *)
          if S.Dom.equal old_value new_value then (
            (* old_value = new_value*)
            if !x_ref.stable then (
              Option.may (add_infl x) orig;
              x_ref := {!x_ref with wpoint = false; called = false};
            ) else (
              x_ref := {!x_ref with stable = true};
              (iterate[@tailcall]) orig x
            )
          ) else (
            (* old_value != new_value*)
            if tracing then trace "update" "%d set %a value: %a" job_id S.Var.pretty_trace x S.Dom.pretty new_value;
            let w = !x_ref.infl in
            x_ref := {!x_ref with rho = new_value; infl = VS.empty};
            destabilize w;
            if !x_ref.stable then (
              Option.may (add_infl x) orig;
              x_ref := {!x_ref with called = false};
            ) else (
              x_ref := {!x_ref with stable = true};
              (iterate[@tailcall]) orig x
            )
          )
        and publish_side y d =
          let revive_suspended () =
            (* Preliminary results must be revaluated *)
            if tracing then trace "revive" "revive called";
            GobMutex.lock prom_mutex;
            let should_not_revive, should_revive = List.partition (fun (_, _, rsd, _) ->
                let y_infl = HM.find_option rsd.unknowns y |> Option.map_default (fun unknown -> !unknown.infl) VS.empty in
                VS.is_empty y_infl
              ) !prelim_vars in
            prelim_vars := should_not_revive;
            GobMutex.unlock prom_mutex;
            List.iter (fun (is_primary, z, rsd, id) ->
                if tracing then trace "revive" "reviving job %d solving for %a (after side to %a)" id S.Var.pretty_trace z S.Var.pretty_trace y;
                let new_id = Atomic.fetch_and_add job_id_counter 1 in
                let promise = Threadpool.add_work pool (fun () -> solve_single is_primary z rsd new_id) in
                GobMutex.lock prom_mutex;
                promises := promise :: !promises;
                GobMutex.unlock prom_mutex
              ) should_revive
          in
          if not (Htbl.mem subs y) then (
            ignore @@ Htbl.try_add subs y ();
            Sides.subscribe y x_poi
          );
          Sides.add_side job_id revive_suspended (y, d)
        and handle_side (y, v) =
          if tracing then trace "handle" "handling side to %a" S.Var.pretty_trace y;
          let y_ref = init unknowns y in
          let old_v = !y_ref.rho in
          if S.Dom.leq v old_v then
            ()
          else (
            let new_v = S.Dom.widen old_v (S.Dom.join old_v v) in
            if tracing then trace "handle" "%d side set %a value: %a" job_id S.Var.pretty_trace y S.Dom.pretty new_v;
            y_ref := {!y_ref with rho = new_v};
            (* TODO: should this happen via side or here and traced differently *)
            publish_side y new_v;
            let w = !y_ref.infl in
            y_ref := {!y_ref with stable = true; infl = VS.empty};
            destabilize w
          )
        in

        (* beginning of solve_single *)
        let x_poi_ref = init unknowns x_poi in
        if (not (!x_poi_ref.stable)) then (
          x_poi_ref := {!x_poi_ref with stable = true; called = true};
          iterate None x_poi
        );

        (* Does not block: processes pending updates, and suspends the task once there are none.
           A suspended task keeps its data in prelim_vars; reviving it starts a new task with that data. *)
        let rec wait () =
          (* Called with prom_mutex held. *)
          let suspend () =
            if tracing then trace "suspend" "suspending job %d solving for %a (suspended_vars: %d)" job_id S.Var.pretty_trace x_poi (List.length !prelim_vars);
            prelim_vars := (is_primary, x_poi, sd, job_id)::!prelim_vars
          in
          (* Checking for updates and suspending must be atomic w.r.t. revive_suspended,
             otherwise a side published in between finds nobody to revive. *)
          GobMutex.lock prom_mutex;
          let status = Sides.updates_or_fin job_id x_poi suspend in
          GobMutex.unlock prom_mutex;
          match status with
          | Sides.NewSide -> (
              if tracing then trace "wait" "%d processing new sides" job_id;
              if tracing then trace "process" "from newside";
              Sides.process_updates job_id x_poi handle_side;
              if !x_poi_ref.stable then
                wait ()
              else (
                x_poi_ref := {!x_poi_ref with stable = true; called = true};
                iterate None x_poi;
                wait ())
            )
          | Sides.Fin -> if tracing then trace "wait" "%d all sides processed -> suspended" job_id
        in
        wait ()
      in


      (* beginning of main solve (initial mapping set above) *)
      let start_data = create_start_data st in
      List.iter (fun v -> ignore @@ init start_data.unknowns v) vs;

      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we side some global which v1 depends on with a new value. Then v1 is no longer stable and we have to solve it again. *)
      let phase = ref 0 in
      let rec solver () = (* as while loop in paper *)
        incr phase;
        let is_stable x = !(HM.find start_data.unknowns x).stable in
        let unstable_vs = List.filter (neg is_stable) vs in
        if unstable_vs <> [] then (
          if Logs.Level.should_log Debug then (
            if !phase = 1 then Logs.newline ();
            Logs.debug "Unstable solver start vars in %d. phase:" !phase;
            List.iter (fun v -> Logs.debug "\t%a" S.Var.pretty_trace v) unstable_vs;
            Logs.newline ();
            flush_all ();
          );
          List.iter (fun x ->
              if tracing then trace "multivar" "solving for %a" S.Var.pretty_trace x;
              Threadpool.run pool (fun () ->
                  let first_id = Atomic.fetch_and_add job_id_counter 1 in
                  solve_single true x start_data first_id;
                  (* make sure, everything is awaited, since promises could change during await_all *)
                  let rec await_changing_list () =
                    GobMutex.lock prom_mutex;
                    let current_proms = !promises in
                    promises := [];
                    GobMutex.unlock prom_mutex;
                    Threadpool.await_all pool current_proms;
                    let promises_empty = begin
                      GobMutex.lock prom_mutex;
                      let is_empty = List.is_empty !promises in
                      GobMutex.unlock prom_mutex;
                      is_empty
                    end
                    in
                    if not promises_empty then await_changing_list ()
                  in
                  await_changing_list ();
                  if tracing then trace "dbg_para" "promises: %d" (List.length !promises)
                )
            ) unstable_vs;
          solver ();
        )
      in
      solver ();
      Threadpool.finished_with pool;
      (* After termination, only those variables are stable which are
         * - reachable from any of the queried variables vs, or
         * - affected by side-effects and have no constraints on their own (this should be the case for all of our analyses). *)

      solver_end_event ();
      print_stats ();

      if GobConfig.get_bool "dbg.print_wpoints" then (
        (* Each thread keeps its own widening points, an unknown is one if any thread marked it. *)
        let wpoint = HM.create 10 in
        let add_wpoints sd = HM.iter (fun k v -> if !v.wpoint then HM.replace wpoint k ()) sd.unknowns in
        add_wpoints start_data;
        List.iter (fun (_, _, sd, _) -> add_wpoints sd) !prelim_vars;
        Logs.newline ();
        Logs.debug "Widening points:";
        HM.iter (fun k () -> Logs.debug "%a" S.Var.pretty_trace k) wpoint;
        Logs.newline ();
      );

      (* TODO: make a better merge here*)
      if tracing then trace "dbg_para" "suspended_vars: %d" (List.length !prelim_vars);
      let unknowns_to_rho u = HM.map (fun _ v -> !v.rho) u in
      let start_rho = unknowns_to_rho start_data.unknowns in
      let nr_inconsistent = ref 0 in
      let final_rho = List.fold (fun acc (_,_,sd,job_id) ->
          if tracing then trace "dbg_para" "merging rho from job %d" job_id;
          HM.merge (
            fun k ao bo ->
              match ao, bo with
              | None, None -> None
              | Some a, None -> ao
              | None, Some b -> bo
              | Some a, Some b -> if S.Dom.equal a b then (if tracing then trace "dbg_para" "found both";ao)
                else (incr nr_inconsistent; if tracing then trace "dbg_para" "Inconsistent data for %a:\n left: %a\n right (%d): %a" S.Var.pretty_trace k S.Dom.pretty a job_id S.Dom.pretty b; Some (S.Dom.join a b))
          ) acc (unknowns_to_rho sd.unknowns)
        ) start_rho !prelim_vars in
      if !nr_inconsistent > 0 then
        Logs.debug "For %d locals, solver threads found different values. The results are merged, they will be sound, but may not be a fixpoint." !nr_inconsistent;
      if tracing then trace "dbg_para" "final_rho len: %d" (HM.length final_rho);
      final_rho
  end

let () =
  Selector.add_solver ("td_parallel_independent", (module PostSolver.DemandEqIncrSolverFromDemandEqSolver (Base)))
