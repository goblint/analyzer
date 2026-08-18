(* GENERATED FILE. DO NOT EDIT.
   Control: extraction/goblint_control_inlined.ml
   Variant: NarrowingStable
   Configuration: first-discovery instance of the policy-parametric Narrowing-Stable algorithm
   Adapter: extraction/goblint_qsolver_narrowing_stable.ml.in
   Regenerate with: make goblint-solvers *)
(**
   Goblint callback adapter for the Narrowing-Stable QSolver.

   This is a mutable implementation of the algorithm in
   [narrowing_stable/QSOLVER_NARROWING_STABLE.md]. The control operations at
   [@@CONTROL@@] are
   supplied either by Rocq extraction or by the explicitly inlined benchmark
   variant.

   This adapter is needed because Goblint transfer functions use effectful
   callbacks rather than the pure [Rhs] interaction tree.

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
    let trace_algorithm = "NarrowingStable"
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
    type side_row = S.d VH.t

    type side_cell = {
      mutable phase : U.phase;
      mutable reset_at : S.v option;
    }

    type side_cell_row = side_cell VH.t
    type reset_sources = set VH.t

    type episode_mode =
      | Grow
      | Narrow

    (** Target-local state lives beside its dense rank so callbacks hash a
        variable only once, when obtaining that rank. *)
    type rank_info = {
      mutable queued : bool;
      mutable started : bool;
      mutable variable : S.v;
      mutable controlled : bool;
      mutable readers : set option;
      mutable side_aggregate : S.d;
      mutable contributions : side_row option;
    }

    let[@inline always] make_rank_info variable =
      {
        queued = false;
        started = false;
        variable;
        controlled = false;
        readers = None;
        side_aggregate = bottom;
        contributions = None;
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

    (** Queue ranks are nonnegative, so [-1] denotes an empty heap without
        allocating an [option] in the episode-draining hot path. *)
    let[@inline always] queue_max_rank queue =
      if RankQueue.is_empty queue.heap
      then -1
      else RankQueue.get_max_elt queue.heap

    let[@inline always] queue_insert queue variable =
      let variable_rank = rank queue variable in
      let info = Dynarray.get queue.infos variable_rank in
      if not info.queued then begin
        info.variable <- variable;
        RankQueue.add queue.heap variable_rank;
        info.queued <- true;
        trace_queue queue.trace "push" variable_rank
      end

    let[@inline always] finish_queue_pop queue result_rank =
      let info = Dynarray.get queue.infos result_rank in
      info.queued <- false;
      trace_queue queue.trace "pop" result_rank;
      info.variable

    let[@inline always] queue_remove_max queue result_rank =
      RankQueue.remove_max queue.heap;
      finish_queue_pop queue result_rank

    let[@inline always] queue_pop queue =
      match RankQueue.pop_max queue.heap with
      | None -> None
      | Some result_rank ->
        Some (finish_queue_pop queue result_rank)

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
      side_cells : side_cell_row VH.t;
      mutable reset_at_close : reset_sources VH.t option;
      mutable last_targets : set VH.t option;
      queue : queue;
    }

    type episode = {
      owner : S.v;
      owner_rank : int;
      owner_info : rank_info;
      mutable owner_phase : U.phase;
      mutable mode : episode_mode;
      mutable outgoing_cells : side_cell_row option;
      mutable prepared_targets : set option;
      mutable deferred_dead : set option;
      mutable touched_side : bool;
      mutable local_side : side_row option;
      mutable previous_targets : set option;
      mutable current_targets : set option;
      mutable min_scope : S.v * int;
      mutable max_read_rank : int option;
    }

    let[@inline always] create_state () =
      {
        sigma = VH.create 32;
        starts = VH.create 16;
        side_cells = VH.create 32;
        reset_at_close = None;
        last_targets = None;
        queue = create_queue ();
      }

    let[@inline always] get_or_create table key capacity =
      try VH.find table key with Not_found ->
        let row = VH.create capacity in
        VH.replace table key row;
        row

    let[@inline always] start_value state variable =
      VH.find_default state.starts variable bottom

    let[@inline always] materialize state variable =
      if not (VH.mem state.sigma variable) then
        VH.replace state.sigma variable bottom

    let[@inline always] lattice_equal left right =
      S.Dom.equal left right

    let[@inline always] recompute_aggregate contributions =
      VH.fold
        (fun _ contribution result -> S.Dom.join contribution result)
        contributions bottom

    let[@inline always] enqueue state variable =
      queue_insert state.queue variable

    let[@inline always] mark_controlled
        dependent_info dependent_rank dependency_rank =
      if dependency_rank >= dependent_rank then
        dependent_info.controlled <- true
    let[@inline always] remove_registration state scope source target =
      match state.reset_at_close with
      | None -> ()
      | Some reset_at_close ->
        begin match VH.find_option reset_at_close scope with
          | None -> ()
          | Some sources ->
            begin match VH.find_option sources source with
              | None -> ()
              | Some targets ->
                VH.remove targets target;
                if VH.length targets = 0 then VH.remove sources source
            end;
            if VH.length sources = 0 then VH.remove reset_at_close scope
        end

    let[@inline always] add_registration state scope source target =
      let reset_at_close =
        match state.reset_at_close with
        | Some reset_at_close -> reset_at_close
        | None ->
          let reset_at_close = VH.create 16 in
          state.reset_at_close <- Some reset_at_close;
          reset_at_close
      in
      let sources = get_or_create reset_at_close scope 2 in
      let targets = get_or_create sources source 2 in
      VH.replace targets target ()

    let[@inline always] register_reset
        state source target target_rank scope scope_rank cell phase =
      let requested =
        if scope_rank < target_rank then Some scope else None
      in
      let unchanged =
        match cell.reset_at, requested with
        | None, None -> true
        | Some old_scope, Some new_scope ->
          S.Var.equal old_scope new_scope
        | _ -> false
      in
      if not unchanged then begin
        begin match cell.reset_at with
          | Some previous ->
            remove_registration state previous source target
          | None -> ()
        end;
        begin match requested with
          | Some next -> add_registration state next source target
          | None -> ()
        end;
        cell.reset_at <- requested
      end;
      cell.phase <- phase

    let[@inline always] reset_registered_cells state scope =
      match state.reset_at_close with
      | None -> ()
      | Some reset_at_close ->
        match VH.find_option reset_at_close scope with
        | None -> ()
        | Some sources ->
          VH.iter
            (fun source targets ->
               match VH.find_option state.side_cells source with
               | None -> assert false
               | Some cells ->
                 VH.iter
                   (fun target () ->
                      match VH.find_option cells target with
                      | None -> assert false
                      | Some cell ->
                        cell.phase <- U.initial_phase true target;
                        cell.reset_at <- None)
                   targets)
            sources;
          VH.remove reset_at_close scope

    let[@inline always] write_changed_value
        state owner_rank owner_info owner value =
      VH.replace state.sigma owner value;
      trace_value state.queue.trace owner_rank true;
      begin match owner_info.readers with
        | None -> ()
        | Some readers ->
          VH.iter (fun reader () -> enqueue state reader) readers;
          VH.clear readers
      end

    let[@inline always] set_value state frame direct =
      let owner = frame.owner in
      let owner_info = frame.owner_info in
      let candidate =
        S.Dom.join direct
          (S.Dom.join (start_value state owner) owner_info.side_aggregate)
      in
      let old = VH.find state.sigma owner in
      let candidate_le_old = S.Dom.leq candidate old in
      let () =
        if not (candidate_le_old && S.Dom.leq old candidate) then begin
          let controlled =
            owner_info.controlled
          in
          if controlled
          && frame.mode = Grow
          && candidate_le_old
          then ()
          else if controlled then begin
            let next_phase, next =
              U.update false owner frame.owner_phase old candidate
            in
            frame.owner_phase <- next_phase;
            if not (lattice_equal next old) then
              write_changed_value state frame.owner_rank owner_info owner next
          end else
            write_changed_value
              state frame.owner_rank owner_info owner candidate
        end
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
        let owner_info = Dynarray.get state.queue.infos owner_rank in
        owner_info.started <- true;
        let owner_system = S.system owner in
        let owner_system_lookup _ = owner_system in
        let owner_phase = U.initial_phase false owner in
        let frame =
          {
            owner;
            owner_rank;
            owner_info;
            owner_phase;
            mode = Grow;
            outgoing_cells = None;
            prepared_targets = None;
            deferred_dead = None;
            touched_side = false;
            local_side = None;
            previous_targets = None;
            current_targets = None;
            min_scope = owner, owner_rank;
            max_read_rank = None;
          }
        in
        materialize state owner;

        let start_evaluation _ =
          let previous =
            match state.last_targets with
            | Some last_targets -> VH.find_option last_targets owner
            | None -> None
          in
          frame.previous_targets <-
            begin match frame.mode, frame.deferred_dead with
              | Narrow, Some deferred_dead ->
                let combined = VH.create 4 in
                begin match previous with
                  | Some targets ->
                    VH.iter (fun target () -> VH.replace combined target ()) targets
                  | None -> ()
                end;
                VH.iter
                  (fun target () -> VH.replace combined target ())
                  deferred_dead;
                VH.clear deferred_dead;
                frame.deferred_dead <- None;
                Some combined
              | Grow, _ | Narrow, None -> previous
            end;
          frame.current_targets <- None;
          begin match frame.local_side with
            | Some local_side -> VH.clear local_side
            | None -> ()
          end;
          frame.min_scope <- owner, owner_rank;
          frame.max_read_rank <- None;
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
            (fun _ direct ->
               finish_evaluation frame;
               set_value state frame direct)
            (fun _ -> drain_episode ())
            owner_system_lookup owner

        and drain_episode () =
          let next_rank = queue_max_rank state.queue in
          if owner_rank <= next_rank then
            let next = queue_remove_max state.queue next_rank in
            if next_rank = owner_rank then
              evaluate_owner ()
            else begin
              solve_episode next;
              drain_episode ()
            end
          else
          if frame.mode = Grow
          && (owner_info.controlled
              || frame.touched_side)
          then begin
            frame.mode <- Narrow;
            evaluate_owner ()
          end
        in
        evaluate_owner ();
        reset_registered_cells state owner

      and get frame target =
        let owner = frame.owner in
        let owner_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        let target_info = Dynarray.get state.queue.infos target_rank in
        trace_get state.queue owner_rank target_rank;
        let value =
          try VH.find state.sigma target with Not_found ->
            if owner_rank < target_rank then begin
              solve_episode target;
              VH.find state.sigma target
            end else begin
              enqueue state target;
              VH.replace state.sigma target bottom;
              bottom
            end
        in
        mark_controlled target_info target_rank owner_rank;
        let _, old_min_rank = frame.min_scope in
        if target_rank < old_min_rank then
          frame.min_scope <- target, target_rank;
        frame.max_read_rank <-
          begin match frame.max_read_rank with
            | None -> Some target_rank
            | Some previous -> Some (max previous target_rank)
          end;
        let target_readers =
          match target_info.readers with
          | Some readers -> readers
          | None ->
            let readers = VH.create 4 in
            target_info.readers <- Some readers;
            readers
        in
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
        let current_targets =
          match frame.current_targets with
          | Some current_targets -> current_targets
          | None ->
            let current_targets = VH.create 4 in
            frame.current_targets <- Some current_targets;
            current_targets
        in
        VH.replace current_targets target ();
        write_side_contribution frame target input false

      and write_side_contribution frame target input dead =
        let source = frame.owner in
        let source_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        let target_info = Dynarray.get state.queue.infos target_rank in
        trace_side state.queue source_rank target_rank;
        frame.touched_side <- true;
        mark_controlled frame.owner_info source_rank target_rank;
        let old_aggregate = target_info.side_aggregate in
        let contributions =
          match target_info.contributions with
          | Some contributions -> contributions
          | None ->
            let contributions = VH.create 4 in
            target_info.contributions <- Some contributions;
            contributions
        in
        let old_contribution =
          VH.find_default contributions source bottom
        in
        let outgoing_cells =
          match frame.outgoing_cells with
          | Some outgoing_cells -> outgoing_cells
          | None ->
            let outgoing_cells = get_or_create state.side_cells source 4 in
            frame.outgoing_cells <- Some outgoing_cells;
            outgoing_cells
        in
        let cell =
          try VH.find outgoing_cells target with Not_found ->
            let fresh =
              {
                phase = U.initial_phase true target;
                reset_at = None;
              }
            in
            VH.replace outgoing_cells target fresh;
            fresh
        in
        let prepared =
          match frame.prepared_targets with
          | Some prepared_targets -> VH.mem prepared_targets target
          | None -> false
        in
        if frame.mode = Grow
        && (Dynarray.get state.queue.infos target_rank).controlled
        && not prepared
        then begin
          let prepared_targets =
            match frame.prepared_targets with
            | Some prepared_targets -> prepared_targets
            | None ->
              let prepared_targets = VH.create 4 in
              frame.prepared_targets <- Some prepared_targets;
              prepared_targets
          in
          VH.replace prepared_targets target ();
          cell.phase <- U.initial_phase true target
        end;
        let input_le_old = S.Dom.leq input old_contribution in
        let old_le_input =
          input_le_old && S.Dom.leq old_contribution input
        in
        let deferred =
          frame.mode = Grow && input_le_old && not old_le_input
        in
        if dead && deferred then begin
          let deferred_dead =
            match frame.deferred_dead with
            | Some deferred_dead -> deferred_dead
            | None ->
              let deferred_dead = VH.create 4 in
              frame.deferred_dead <- Some deferred_dead;
              deferred_dead
          in
          VH.replace deferred_dead target ()
        end;
        let scope, scope_rank = frame.min_scope in
        let phase = cell.phase in
        let next_phase, next_contribution =
          if deferred || (input_le_old && old_le_input)
          then phase, old_contribution
          else
            U.update true target phase old_contribution input
        in
        VH.replace contributions source next_contribution;
        register_reset state source target target_rank
          scope scope_rank cell next_phase;
        let next_aggregate, aggregate_changed =
          if S.Dom.leq old_contribution next_contribution then
            if S.Dom.leq next_contribution old_aggregate
            then old_aggregate, false
            else S.Dom.join old_aggregate next_contribution, true
          else begin
            let next_aggregate = recompute_aggregate contributions in
            next_aggregate,
            not (lattice_equal next_aggregate old_aggregate)
          end
        in
        if aggregate_changed then begin
          target_info.side_aggregate <- next_aggregate;
          if source_rank < target_rank
          && not (VH.mem state.sigma target)
          then
            solve_episode target
          else
            enqueue state target
        end

      and finish_evaluation frame =
        begin match frame.previous_targets with
          | None -> ()
          | Some previous_targets ->
            VH.iter
              (fun target () ->
                 let current =
                   match frame.current_targets with
                   | Some current_targets -> VH.mem current_targets target
                   | None -> false
                 in
                 if not current then
                   write_side_contribution frame target bottom true)
              previous_targets
        end;
        begin match state.last_targets, frame.current_targets with
          | Some last_targets, Some current_targets ->
            VH.replace last_targets frame.owner current_targets
          | None, Some current_targets ->
            let last_targets = VH.create 32 in
            state.last_targets <- Some last_targets;
            VH.replace last_targets frame.owner current_targets
          | Some last_targets, None ->
            VH.remove last_targets frame.owner
          | None, None -> ()
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
