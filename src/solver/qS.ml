(* GENERATED FILE. DO NOT EDIT.
   Control: Rocq extraction/QSolverGoblintControl.v
   Features: fixed, all enabled
   Adapter: extraction/goblint_qsolver.ml.in
   Regenerate with: make goblint-solvers *)
(**
   Goblint callback adapter for QSolver.

   This is a mutable implementation of the algorithm in [QSOLVER.md].  The
   control operations at [@@CONTROL@@] are supplied either by Rocq extraction
   or by the explicitly inlined benchmark variant.

   The pure solver in [QSolver.v] remains the primary extraction and proof
   target.  This adapter is needed because Goblint transfer functions use
   effectful callbacks rather than the pure [Rhs] interaction tree.

   Current incremental-solver limitation: each call starts from fresh state
   and ignores [old_data].
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes

open QS_update

module Control = struct

  type 'a m = 'a

  (** val ret : 'a1 -> 'a1 m **)

  let ret value =
    value

  (** val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m **)

  let bind computation next =
    next computation

  type ('v, 'd) callback =
    'v -> (('v -> 'd m) -> ('v -> 'd -> unit m) -> ('v -> unit m) -> 'd m)
      option

  (** val eval_rhs :
      ('a1 -> 'a2 m) -> ('a1 -> 'a2 -> unit m) -> ('a1 -> unit m) -> (('a1 ->
      'a2 m) -> ('a1 -> 'a2 -> unit m) -> ('a1 -> unit m) -> 'a2 m) -> 'a2 m **)

  let[@inline always] eval_rhs get side spawn body =
    body get side spawn

  (** val solve_one :
      'a2 -> ('a1 -> unit m) -> ('a1 -> 'a1 -> 'a2 m) -> ('a1 -> 'a1 -> 'a2 ->
      unit m) -> ('a1 -> 'a1 -> unit m) -> ('a1 -> 'a2 -> unit m) -> ('a1 ->
      unit m) -> ('a1, 'a2) callback -> 'a1 -> unit m **)

  let[@inline always] solve_one bottom start_evaluation get side spawn set_value drain_owner system owner =
    bind (start_evaluation owner) (fun _ ->
        bind
          (match system owner with
           | Some body -> eval_rhs (get owner) (side owner) (spawn owner) body
           | None -> ret bottom)
          (fun direct ->
             bind (set_value owner direct) (fun _ -> drain_owner owner)))

  (** val solve_loop_fuel :
      int -> (unit -> 'a1 option m) -> ('a1 -> unit m) -> bool m **)

  let rec solve_loop_fuel fuel pop_owner solve_owner =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> ret false)
      (fun fuel' ->
         bind (pop_owner ()) (fun picked ->
             match picked with
             | Some owner ->
               bind (solve_owner owner) (fun _ ->
                   solve_loop_fuel fuel' pop_owner solve_owner)
             | None -> ret true))
      fuel

  (** val solve_all :
      int -> ('a1 list -> unit m) -> (unit -> 'a1 option m) -> ('a1 -> unit m)
      -> 'a1 list -> bool m **)

  let[@inline always] solve_all fuel initialize pop_owner solve_owner roots =
    bind (initialize roots) (fun _ ->
        solve_loop_fuel fuel pop_owner solve_owner)
end

module type CONTROL = sig
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
end

module Features = struct
  (* Fixed to the behavior of the proved solver. *)
  let normal_phase_reset = true
  let side_effect_phase_reset = true
  let dead_side_elimination = true
end

module type FEATURES = sig
  val normal_phase_reset : bool
  val side_effect_phase_reset : bool
  val dead_side_elimination : bool
end

module Make
    (Control : CONTROL)
    (Features : FEATURES)
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

    type set = unit VH.t
    type side_row = S.d VH.t
    type side_cell = {
      mutable phase : U.phase;
      mutable reset_at : S.v option;
    }
    type side_cell_row = side_cell VH.t
    type reset_sources = set VH.t

    module RankedQueue = Pqueue.MakeMaxPoly (struct
        type 'a t = int * 'a

        let compare (left_rank, _) (right_rank, _) =
          Int.compare left_rank right_rank
      end)

    type queue = {
      heap : S.v RankedQueue.t;
      present : set;
      ranks : int VH.t;
      mutable next_rank : int;
    }

    type state = {
      sigma : S.d VH.t;
      starts : S.d VH.t;
      readers : set VH.t;
      contributions : side_row VH.t;
      side_aggregates : S.d VH.t;
      side_cells : side_cell_row VH.t;
      reset_at_close : reset_sources VH.t;
      last_targets : set VH.t;
      normal_phases : U.phase VH.t;
      controlled : set;
      queue : queue;
    }

    type episode = {
      owner : S.v;
      owner_rank : int;
      mutable owner_phase : U.phase;
      outgoing_cells : side_cell_row;
      local_side : side_row;
      mutable previous_targets : set;
      mutable current_targets : set;
      mutable min_scope : S.v * int;
      mutable max_read_rank : int option;
    }

    let[@inline always] create_queue () =
      {
        heap = RankedQueue.create ();
        present = VH.create 16;
        ranks = VH.create 32;
        next_rank = 0;
      }

    let[@inline always] create_state () =
      {
        sigma = VH.create 32;
        starts = VH.create 16;
        readers = VH.create 32;
        contributions = VH.create 32;
        side_aggregates = VH.create 32;
        side_cells = VH.create 32;
        reset_at_close = VH.create 16;
        last_targets = VH.create 32;
        normal_phases = VH.create 32;
        controlled = VH.create 32;
        queue = create_queue ();
      }

    (** The callback API has no static variable universe.  Ranks are therefore
        allocated once in first-seen order; larger ranks have higher priority. *)
    let[@inline always] rank queue variable =
      try VH.find queue.ranks variable with Not_found ->
        let result = queue.next_rank in
        queue.next_rank <- result + 1;
        VH.replace queue.ranks variable result;
        result

    let[@inline always] queue_max_rank queue =
      match RankedQueue.max_elt queue.heap with
      | Some (value_rank, _) -> Some value_rank
      | None -> None

    let[@inline always] queue_insert queue value =
      if not (VH.mem queue.present value) then begin
        let value_rank = rank queue value in
        RankedQueue.add queue.heap (value_rank, value);
        VH.replace queue.present value ()
      end

    let[@inline always] queue_pop queue =
      match RankedQueue.pop_max queue.heap with
      | None -> None
      | Some (_, result) ->
        VH.remove queue.present result;
        Some result

    let[@inline always] get_or_create table key capacity =
      try VH.find table key with Not_found ->
        let row = VH.create capacity in
        VH.replace table key row;
        row

    let[@inline always] sigma_value state variable =
      try VH.find state.sigma variable with Not_found -> bottom

    let[@inline always] start_value state variable =
      match VH.find_option state.starts variable with
      | Some value -> value
      | None -> bottom

    let[@inline always] side_aggregate state target =
      match VH.find_option state.side_aggregates target with
      | Some value -> value
      | None -> bottom

    let[@inline always] materialize state variable =
      if not (VH.mem state.sigma variable) then
        VH.replace state.sigma variable bottom

    let[@inline always] lattice_equal left right =
      S.Dom.leq left right && S.Dom.leq right left

    let[@inline always] recompute_aggregate contributions =
      VH.fold
        (fun _ contribution result -> S.Dom.join contribution result)
        contributions bottom

    let[@inline always] enqueue state variable =
      queue_insert state.queue variable

    let[@inline always] mark_controlled
        state dependent dependent_rank dependency_rank =
      if dependency_rank >= dependent_rank then
        VH.replace state.controlled dependent ()

    let[@inline always] remove_registration state scope source target =
      match VH.find_option state.reset_at_close scope with
      | None -> ()
      | Some sources ->
        begin match VH.find_option sources source with
          | None -> ()
          | Some targets ->
            VH.remove targets target;
            if VH.length targets = 0 then VH.remove sources source
        end;
        if VH.length sources = 0 then VH.remove state.reset_at_close scope

    let[@inline always] add_registration state scope source target =
      let sources = get_or_create state.reset_at_close scope 2 in
      let targets = get_or_create sources source 2 in
      VH.replace targets target ()

    let[@inline always] register_reset
        state source target target_rank scope scope_rank cell phase =
      if Features.side_effect_phase_reset then begin
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
        end
      end;
      cell.phase <- phase

    let[@inline always] reset_registered_cells state scope =
      if Features.side_effect_phase_reset then
        match VH.find_option state.reset_at_close scope with
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
          VH.remove state.reset_at_close scope

    let[@inline always] write_changed_value state owner value =
      VH.replace state.sigma owner value;
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
      let old = sigma_value state owner in
      if not (lattice_equal candidate old) then
        if VH.mem state.controlled owner then begin
          let next_phase, next =
            U.update false owner frame.owner_phase old candidate
          in
          frame.owner_phase <- next_phase;
          if not (lattice_equal next old) then
            write_changed_value state owner next
        end else
          write_changed_value state owner candidate

    let solve xs roots _old_data =
      let state = create_state () in

      let rec solve_episode owner =
        let owner_rank = rank state.queue owner in
        let owner_phase =
          if Features.normal_phase_reset then
            U.initial_phase false owner
          else
            match VH.find_option state.normal_phases owner with
            | Some phase -> phase
            | None -> U.initial_phase false owner
        in
        let frame =
          {
            owner;
            owner_rank;
            owner_phase;
            outgoing_cells = get_or_create state.side_cells owner 4;
            local_side = VH.create 4;
            previous_targets = VH.create 0;
            current_targets = VH.create 4;
            min_scope = owner, owner_rank;
            max_read_rank = None;
          }
        in
        materialize state owner;

        let start_evaluation _ =
          if Features.dead_side_elimination then begin
            frame.previous_targets <-
              begin match VH.find_option state.last_targets owner with
                | Some targets -> targets
                | None -> VH.create 0
              end;
            frame.current_targets <- VH.create 4
          end;
          VH.clear frame.local_side;
          frame.min_scope <- owner, owner_rank;
          frame.max_read_rank <- None;
          incr SolverStats.evals
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
            S.system owner

        and drain_episode () =
          match queue_max_rank state.queue with
          | Some next_rank when owner_rank <= next_rank ->
            begin match queue_pop state.queue with
              | None -> assert false
              | Some next ->
                if S.Var.equal next owner then
                  evaluate_owner ()
                else begin
                  solve_episode next;
                  drain_episode ()
                end
            end
          | Some _ | None -> ()
        in
        evaluate_owner ();
        if not Features.normal_phase_reset then
          VH.replace state.normal_phases owner frame.owner_phase;
        reset_registered_cells state owner

      and get frame target =
        let owner = frame.owner in
        let owner_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        if not (VH.mem state.sigma target) then begin
          if owner_rank < target_rank then
            solve_episode target
          else
            enqueue state target
        end;
        mark_controlled state owner owner_rank target_rank;
        let _, old_min_rank = frame.min_scope in
        if target_rank < old_min_rank then
          frame.min_scope <- target, target_rank;
        frame.max_read_rank <-
          begin match frame.max_read_rank with
            | None -> Some target_rank
            | Some previous -> Some (max previous target_rank)
          end;
        let target_readers = get_or_create state.readers target 4 in
        VH.replace target_readers owner ();
        materialize state target;
        sigma_value state target

      and side frame target raw =
        let input =
          match VH.find_option frame.local_side target with
          | Some previous -> S.Dom.join previous raw
          | None -> raw
        in
        VH.replace frame.local_side target input;
        if Features.dead_side_elimination then
          VH.replace frame.current_targets target ();
        write_side_contribution frame target input

      and write_side_contribution frame target input =
        let source = frame.owner in
        let source_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        begin match frame.max_read_rank with
          | Some read_rank ->
            mark_controlled state target target_rank read_rank
          | None -> ()
        end;
        let old_aggregate = side_aggregate state target in
        let contributions =
          get_or_create state.contributions target 4
        in
        let old_contribution =
          try VH.find contributions source with Not_found -> bottom
        in
        let cell =
          try VH.find frame.outgoing_cells target with Not_found ->
            let fresh =
              {
                phase = U.initial_phase true target;
                reset_at = None;
              }
            in
            VH.replace frame.outgoing_cells target fresh;
            fresh
        in
        let next_phase, next_contribution =
          if lattice_equal input old_contribution
          then cell.phase, old_contribution
          else U.update true target cell.phase old_contribution input
        in
        VH.replace contributions source next_contribution;
        let scope, scope_rank = frame.min_scope in
        register_reset state source target target_rank
          scope scope_rank cell next_phase;
        let next_aggregate =
          if S.Dom.leq old_contribution next_contribution then
            if S.Dom.leq next_contribution old_aggregate
            then old_aggregate
            else S.Dom.join old_aggregate next_contribution
          else
            recompute_aggregate contributions
        in
        if not (lattice_equal next_aggregate old_aggregate) then begin
          VH.replace state.side_aggregates target next_aggregate;
          if source_rank < target_rank
          && not (VH.mem state.sigma target)
          then
            solve_episode target
          else
            enqueue state target
        end

      and finish_evaluation frame =
        if Features.dead_side_elimination then begin
          VH.iter
            (fun target () ->
               if not (VH.mem frame.current_targets target) then
                 write_side_contribution frame target bottom)
            frame.previous_targets;
          VH.replace state.last_targets frame.owner frame.current_targets
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
           VH.replace state.sigma variable start;
           enqueue state variable)
        xs;
      let finished =
        Control.solve_all Stdlib.max_int
          (fun variables -> List.iter (enqueue state) variables)
          (fun () -> queue_pop state.queue)
          solve_episode roots
      in
      if not finished then
        failwith "QSolver: exhausted max_int queue fuel";
      VH.iter (fun _ _ -> incr SolverStats.vars) state.sigma;
      state.sigma, ()
  end

module QSolver = Make (Control) (Features)
