(* GENERATED FILE. DO NOT EDIT.
   Control: extraction/goblint_control_inlined.ml
   Variant: Simple
   Configuration: first-discovery instance of the policy-parametric Simple algorithm
   Adapter: extraction/goblint_qsolver_simple.ml.in
   Regenerate with: make goblint-solvers *)
(**
   Goblint callback adapter for the Simple QSolver.

   This is a mutable implementation of the algorithm in
   [simple/QSOLVER_SIMPLE.md].  The control operations at [@@CONTROL@@] are
   supplied either by Rocq extraction or by the explicitly inlined benchmark
   variant.

   The state and evaluation frame deliberately contain only data used by the
   Simple algorithm.  In particular, they have no reset registrations,
   dead-side sets, read-dependency rows, or grow/narrow episode data.

   Current incremental-solver limitation: each call starts from fresh state
   and ignores [old_data].
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes

open QS_update

module Control = struct
  let[@inline always] solve_one
      bottom start_evaluation get side spawn set_value drain_owner system owner =
    start_evaluation owner;
    let direct =
      match system owner with
      | Some body -> body (get owner) (side owner) (spawn owner)
      | None -> bottom
    in
    set_value owner direct;
    drain_owner owner

  let[@inline always] solve_all
      _fuel initialize pop_owner solve_owner roots =
    initialize roots;
    let rec loop () =
      match pop_owner () with
      | None -> true
      | Some owner ->
        solve_owner owner;
        loop ()
    in
    loop ()

  type seeded_result =
    | SeedsSolved of int
    | SeedsOutOfFuel

  let rec drain_with_fuel fuel pop_owner solve_owner =
    match pop_owner () with
    | None -> SeedsSolved fuel
    | Some owner ->
      if fuel = 0 then SeedsOutOfFuel
      else begin
        solve_owner owner;
        drain_with_fuel (fuel - 1) pop_owner solve_owner
      end

  let rec solve_seeds_with_fuel
      fuel is_started pop_owner solve_owner roots =
    match roots with
    | [] -> SeedsSolved fuel
    | root :: remaining_roots ->
      if is_started root then
        solve_seeds_with_fuel
          fuel is_started pop_owner solve_owner remaining_roots
      else if fuel = 0 then
        SeedsOutOfFuel
      else begin
        solve_owner root;
        match drain_with_fuel (fuel - 1) pop_owner solve_owner with
        | SeedsOutOfFuel -> SeedsOutOfFuel
        | SeedsSolved after_drain ->
          solve_seeds_with_fuel
            after_drain is_started pop_owner solve_owner remaining_roots
      end
end

module type CONTROL = sig
  type seeded_result = SeedsSolved of int | SeedsOutOfFuel

  val solve_one :
    'd ->
    ('v -> unit) ->
    ('v -> 'v -> 'd) ->
    ('v -> 'v -> 'd -> unit) ->
    ('v -> 'v -> unit) ->
    ('v -> 'd -> unit) ->
    ('v -> unit) ->
    ('v ->
     (('v -> 'd) ->
      ('v -> 'd -> unit) ->
      ('v -> unit) ->
      'd) option) ->
    'v ->
    unit

  val solve_all :
    int ->
    ('v list -> unit) ->
    (unit -> 'v option) ->
    ('v -> unit) ->
    'v list ->
    bool

  val solve_seeds_with_fuel :
    int ->
    ('v -> bool) ->
    (unit -> 'v option) ->
    ('v -> unit) ->
    'v list ->
    seeded_result
end

module Make
    (Control : CONTROL)
    (Update : UPDATE) : DemandEqIncrSolver =
  functor (Arg : IncrSolverArg)
    (S : DemandEqConstrSys)
    (VH : Hashtbl.S with type key = S.v) ->
  struct
    module U = Update (S)

    type marshal = unit

    let copy_marshal () = ()
    let relift_marshal () = ()

    let bottom = S.Dom.bot ()

    let trace_supported = true
    let trace_algorithm = "Simple"
    let trace_control = "inlined"

    type trace_output = {
      channel : out_channel;
      mutable sequence : int;
      evaluation_counts : (int, int) Stdlib.Hashtbl.t;
      mutable evaluations : int;
      mutable gets : int;
      mutable sides : int;
    }

    type trace =
      | TraceDisabled
      | TraceEnabled of trace_output

    let trace_write channel json =
      Yojson.Safe.to_channel channel json;
      Stdlib.output_char channel '\n'

    let create_trace () =
      if not trace_supported then TraceDisabled
      else
        match Stdlib.Sys.getenv_opt "QSOLVER_TRACE" with
        | None | Some "" -> TraceDisabled
        | Some path ->
          let channel = Stdlib.open_out path in
          trace_write channel
            (`Assoc
               [ "type", `String "header";
                 "format", `String "qsolver-trace";
                 "version", `Int 3;
                 "algorithm", `String trace_algorithm;
                 "control", `String trace_control ]);
          TraceEnabled
            {
              channel;
              sequence = 0;
              evaluation_counts = Stdlib.Hashtbl.create 128;
              evaluations = 0;
              gets = 0;
              sides = 0;
            }

    let[@inline always] trace_emit trace event fields =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled output ->
        let sequence = output.sequence in
        output.sequence <- sequence + 1;
        trace_write output.channel
          (`Assoc
             (("type", `String event) ::
              ("seq", `Int sequence) ::
              fields));
        if sequence land 255 = 255 then Stdlib.flush output.channel

    let[@inline always] trace_node trace rank variable =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled output ->
        Stdlib.Hashtbl.add output.evaluation_counts rank 0;
        let label =
          try GobPretty.sprintf "%a" S.Var.pretty_trace variable
          with _ -> S.Var.var_id variable
        in
        trace_emit trace "node"
          [ "rank", `Int rank;
            "rhs", `Bool (Option.is_some (S.system variable));
            "label", `String label ]

    let[@inline always] trace_queue trace action rank =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled _ ->
        trace_emit trace "queue"
          [ "action", `String action;
            "rank", `Int rank ]

    let[@inline always] trace_evaluation trace rank =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled output ->
        let count =
          match Stdlib.Hashtbl.find_opt output.evaluation_counts rank with
          | Some previous -> previous + 1
          | None -> 1
        in
        Stdlib.Hashtbl.replace output.evaluation_counts rank count;
        output.evaluations <- output.evaluations + 1;
        trace_emit trace "eval"
          [ "rank", `Int rank;
            "count", `Int count ]

    let[@inline always] trace_value trace rank changed =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled _ ->
        trace_emit trace "value"
          [ "rank", `Int rank;
            "changed", `Bool changed ]

    let[@inline always] trace_dependency
        trace event source target selected_rank new_w =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled output ->
        if event = "get"
        then output.gets <- output.gets + 1
        else output.sides <- output.sides + 1;
        trace_emit trace event
          [ "source", `Int source;
            "target", `Int target;
            "selects_w", `Bool (Option.is_some selected_rank);
            "w_rank", (match selected_rank with
                | Some rank -> `Int rank
                | None -> `Null);
            "new_w", `Bool new_w ]

    let trace_finish trace nodes w =
      match trace with
      | TraceDisabled -> ()
      | TraceEnabled output ->
        trace_emit trace "finish"
          [ "nodes", `Int nodes;
            "w", `Int w;
            "evals", `Int output.evaluations;
            "gets", `Int output.gets;
            "sides", `Int output.sides ];
        Stdlib.flush output.channel;
        Stdlib.close_out output.channel

    type set = unit VH.t
    type local_side = S.d VH.t

    type side_cell = {
      mutable phase : U.phase;
      mutable contribution : S.d;
    }

    type side_row = side_cell VH.t

    type rank_info = {
      mutable queued : bool;
      mutable started : bool;
      mutable variable : S.v;
      mutable controlled : bool;
      mutable normal_phase : U.phase option;
    }

    let[@inline always] make_rank_info variable =
      {
        queued = false;
        started = false;
        variable;
        controlled = false;
        normal_phase = None;
      }

    (** Default online order for callback-based solvers. Variables receive
        dense, increasing ranks when first encountered. Existing ranks never
        change, and the work queue always selects the greatest queued rank. *)
    module RankQueue = Pqueue.MakeMax (struct
        type t = int

        let compare = Int.compare
      end)

    type queue = {
      heap : RankQueue.t;
      trace : trace;
      infos : rank_info Dynarray.t;
      ranks : int VH.t;
      mutable next_rank : int;
    }

    let[@inline always] create_queue () =
      {
        heap = RankQueue.create ();
        trace = create_trace ();
        infos = Dynarray.create ();
        ranks = VH.create 32;
        next_rank = 0;
      }

    let[@inline always] rank queue variable =
      try VH.find queue.ranks variable with Not_found ->
        let result = queue.next_rank in
        queue.next_rank <- result + 1;
        VH.replace queue.ranks variable result;
        Dynarray.add_last queue.infos (make_rank_info variable);
        trace_node queue.trace result variable;
        result

    let[@inline always] queue_max_rank queue =
      RankQueue.max_elt queue.heap

    let[@inline always] queue_insert queue variable =
      let variable_rank = rank queue variable in
      let info = Dynarray.get queue.infos variable_rank in
      if not info.queued then begin
        info.variable <- variable;
        RankQueue.add queue.heap variable_rank;
        info.queued <- true;
        trace_queue queue.trace "push" variable_rank
      end

    let[@inline always] queue_pop queue =
      match RankQueue.pop_max queue.heap with
      | None -> None
      | Some result_rank ->
        let info = Dynarray.get queue.infos result_rank in
        info.queued <- false;
        trace_queue queue.trace "pop" result_rank;
        Some info.variable

    let[@inline always] trace_get queue source_rank target_rank =
      match queue.trace with
      | TraceDisabled -> ()
      | TraceEnabled _ ->
        let selected = target_rank <= source_rank in
        let new_w =
          selected
          && not (Dynarray.get queue.infos target_rank).controlled
        in
        trace_dependency queue.trace "get" source_rank target_rank
          (if selected then Some target_rank else None) new_w

    let[@inline always] trace_side queue source_rank target_rank =
      match queue.trace with
      | TraceDisabled -> ()
      | TraceEnabled _ ->
        let selected = source_rank <= target_rank in
        let new_w =
          selected
          && not (Dynarray.get queue.infos source_rank).controlled
        in
        trace_dependency queue.trace "side" source_rank target_rank
          (if selected then Some source_rank else None) new_w

    let[@inline always] seed_started queue variable =
      match VH.find_option queue.ranks variable with
      | None -> false
      | Some variable_rank ->
        (Dynarray.get queue.infos variable_rank).started

    (** Roots are discovered one at a time. Work reached from one seed is
        drained before the next seed can receive a rank. Starting-state
        variables which were not reached from a root are seeded afterward. *)
    let solve_initial queue solve_episode roots starts =
      let seeds = roots @ List.map fst starts in
      match
        Control.solve_seeds_with_fuel Stdlib.max_int
          (seed_started queue)
          (fun () -> queue_pop queue)
          solve_episode seeds
      with
      | Control.SeedsSolved _ -> ()
      | Control.SeedsOutOfFuel ->
        failwith "QSolver: exhausted max_int seed fuel"

    let finish_trace queue =
      match queue.trace with
      | TraceDisabled -> ()
      | TraceEnabled _ ->
        let w = ref 0 in
        Dynarray.iter
          (fun info -> if info.controlled then incr w)
          queue.infos;
        trace_finish queue.trace (Dynarray.length queue.infos) !w

    type state = {
      sigma : S.d VH.t;
      starts : S.d VH.t;
      readers : set VH.t;
      side_aggregates : S.d VH.t;
      side_cells : side_row VH.t;
      queue : queue;
    }

    type episode = {
      owner : S.v;
      owner_rank : int;
      mutable owner_phase : U.phase;
      mutable local_side : local_side option;
    }

    let[@inline always] create_state () =
      {
        sigma = VH.create 32;
        starts = VH.create 16;
        readers = VH.create 32;
        side_aggregates = VH.create 32;
        side_cells = VH.create 32;
        queue = create_queue ();
      }

    let[@inline always] get_or_create table key capacity =
      try VH.find table key with Not_found ->
        let row = VH.create capacity in
        VH.replace table key row;
        row

    let[@inline always] start_value state variable =
      VH.find_default state.starts variable bottom

    let[@inline always] side_aggregate state target =
      VH.find_default state.side_aggregates target bottom

    let[@inline always] materialize state variable =
      if not (VH.mem state.sigma variable) then
        VH.replace state.sigma variable bottom

    let[@inline always] lattice_equal left right =
      S.Dom.equal left right

    let[@inline always] recompute_aggregate cells =
      VH.fold
        (fun _ cell result -> S.Dom.join cell.contribution result)
        cells bottom

    let[@inline always] enqueue state variable =
      queue_insert state.queue variable

    let[@inline always] mark_controlled
        state dependent_rank dependency_rank =
      if dependency_rank >= dependent_rank then
        (Dynarray.get state.queue.infos dependent_rank).controlled <- true

    let[@inline always] write_changed_value state owner_rank owner value =
      VH.replace state.sigma owner value;
      trace_value state.queue.trace owner_rank true;
      begin match VH.find_option state.readers owner with
        | None -> ()
        | Some readers ->
          VH.iter (fun reader () -> enqueue state reader) readers;
          VH.clear readers
      end

    let[@inline always] set_value state frame direct =
      let owner = frame.owner in
      let candidate =
        S.Dom.join direct
          (S.Dom.join (start_value state owner) (side_aggregate state owner))
      in
      let old = VH.find state.sigma owner in
      let () =
        if not (lattice_equal candidate old) then
          if (Dynarray.get state.queue.infos frame.owner_rank).controlled then begin
            let next_phase, next =
              U.update false owner frame.owner_phase old candidate
            in
            frame.owner_phase <- next_phase;
            if not (lattice_equal next old) then
              write_changed_value state frame.owner_rank owner next
          end else
            write_changed_value state frame.owner_rank owner candidate
      in
      match state.queue.trace with
      | TraceDisabled -> ()
      | TraceEnabled _ ->
        let current = VH.find state.sigma owner in
        if lattice_equal current old then
          trace_value state.queue.trace frame.owner_rank false

    let solve xs roots _old_data =
      let state = create_state () in

      let rec solve_episode owner =
        let owner_rank = rank state.queue owner in
        (Dynarray.get state.queue.infos owner_rank).started <- true;
        let owner_system = S.system owner in
        let owner_system_lookup _ = owner_system in
        let owner_phase =
          match (Dynarray.get state.queue.infos owner_rank).normal_phase with
          | Some phase -> phase
          | None -> U.initial_phase false owner
        in
        let frame =
          {
            owner;
            owner_rank;
            owner_phase;
            local_side = None;
          }
        in
        materialize state owner;

        let start_evaluation _ =
          begin match frame.local_side with
            | Some local_side -> VH.clear local_side
            | None -> ()
          end;
          match owner_system with
          | Some _ ->
            trace_evaluation state.queue.trace owner_rank;
            incr SolverStats.evals
          | None -> ()
        in

        let rec evaluate_owner () =
          Control.solve_one bottom start_evaluation
            (fun _ -> get frame)
            (fun _ -> side frame)
            (fun _ -> spawn frame)
            (fun _ direct -> set_value state frame direct)
            (fun _ -> drain_episode ())
            owner_system_lookup owner

        and drain_episode () =
          match queue_max_rank state.queue with
          | Some next_rank when owner_rank <= next_rank ->
            begin match queue_pop state.queue with
              | None -> assert false
              | Some next ->
                if next_rank = owner_rank then
                  evaluate_owner ()
                else begin
                  solve_episode next;
                  drain_episode ()
                end
            end
          | Some _ | None -> ()
        in
        evaluate_owner ();
        let owner_info = Dynarray.get state.queue.infos owner_rank in
        if owner_info.controlled then
          owner_info.normal_phase <- Some frame.owner_phase

      and get frame target =
        let owner = frame.owner in
        let owner_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        trace_get state.queue owner_rank target_rank;
        let value =
          match VH.find_option state.sigma target with
          | Some value -> value
          | None ->
            if owner_rank < target_rank then begin
              solve_episode target;
              VH.find state.sigma target
            end else begin
              enqueue state target;
              VH.replace state.sigma target bottom;
              bottom
            end
        in
        mark_controlled state target_rank owner_rank;
        let target_readers = get_or_create state.readers target 4 in
        VH.replace target_readers owner ();
        value

      and side frame target raw =
        let local_side =
          match frame.local_side with
          | Some local_side -> local_side
          | None ->
            let local_side = VH.create 4 in
            frame.local_side <- Some local_side;
            local_side
        in
        let input =
          match VH.find_option local_side target with
          | Some previous -> S.Dom.join previous raw
          | None -> raw
        in
        VH.replace local_side target input;
        write_side_contribution frame target input

      and write_side_contribution frame target input =
        let source = frame.owner in
        let source_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        trace_side state.queue source_rank target_rank;
        mark_controlled state source_rank target_rank;
        let old_aggregate = side_aggregate state target in
        let cells = get_or_create state.side_cells target 4 in
        let cell =
          try VH.find cells source with Not_found ->
            let fresh =
              {
                phase = U.initial_phase true target;
                contribution = bottom;
              }
            in
            VH.replace cells source fresh;
            fresh
        in
        let old_contribution = cell.contribution in
        let next_phase, next_contribution =
          U.update true target cell.phase old_contribution input
        in
        cell.phase <- next_phase;
        cell.contribution <- next_contribution;
        let next_aggregate, aggregate_changed =
          if S.Dom.leq old_contribution next_contribution then
            if S.Dom.leq next_contribution old_aggregate
            then old_aggregate, false
            else S.Dom.join old_aggregate next_contribution, true
          else begin
            let next_aggregate = recompute_aggregate cells in
            next_aggregate,
            not (lattice_equal next_aggregate old_aggregate)
          end
        in
        if aggregate_changed then begin
          VH.replace state.side_aggregates target next_aggregate;
          if source_rank < target_rank
          && not (VH.mem state.sigma target)
          then
            solve_episode target
          else
            enqueue state target
        end

      and spawn frame target =
        let owner_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        if not (VH.mem state.sigma target) then
          if owner_rank < target_rank then
            solve_episode target
          else begin
            enqueue state target;
            materialize state target
          end
      in

      List.iter
        (fun (variable, input) ->
           let start = S.Dom.join (start_value state variable) input in
           VH.replace state.starts variable start;
           VH.replace state.sigma variable start)
        xs;
      solve_initial state.queue solve_episode roots xs;
      finish_trace state.queue;
      SolverStats.vars := !SolverStats.vars + VH.length state.sigma;
      state.sigma, ()
  end

module QSolver = Make (Control)
