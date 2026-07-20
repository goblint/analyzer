(**
   Standalone Goblint adapter for QSolver.

   This file is intended to be copied verbatim to [src/solver/qS.ml].  It is
   the identity-monad extraction, normalized to ordinary imperative OCaml and
   specialized to Goblint's [DemandEqConstrSys] interface.

   First-benchmark limitations:
   - solving always starts from fresh state;
   - [old_data] and the post-solver flags are ignored;
   - [update] is instantiated by widening, without a narrowing pass.

   See [OCAML_CONTRACT.md] in the extraction directory for the external RHS,
   variable, and domain obligations.
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes

module QSolver : DemandEqIncrSolver =
  functor (Arg : IncrSolverArg)
    (S : DemandEqConstrSys)
    (VH : Hashtbl.S with type key = S.v) ->
  struct
    type marshal = unit

    let copy_marshal () = ()
    let relift_marshal () = ()

    type set = unit VH.t
    type side_row = S.d VH.t

    type queue = {
      mutable heap : S.v option array;
      mutable size : int;
      present : set;
    }

    type state = {
      sigma : S.d VH.t;
      starts : S.d VH.t;
      infl : set VH.t;
      sides : side_row VH.t;
      queue : queue;
    }

    let create_queue () =
      {heap = Array.make 16 None; size = 0; present = VH.create 16}

    let create_state () =
      {
        sigma = VH.create 32;
        starts = VH.create 16;
        infl = VH.create 32;
        sides = VH.create 32;
        queue = create_queue ();
      }

    let higher_priority left right = S.Var.compare left right > 0

    let queue_get queue index =
      match queue.heap.(index) with
      | Some value -> value
      | None -> invalid_arg "QSolver: corrupt priority queue"

    let queue_swap queue left right =
      let saved = queue.heap.(left) in
      queue.heap.(left) <- queue.heap.(right);
      queue.heap.(right) <- saved

    let queue_ensure_capacity queue =
      if queue.size = Array.length queue.heap then begin
        let grown = Array.make (max 1 (2 * queue.size)) None in
        Array.blit queue.heap 0 grown 0 queue.size;
        queue.heap <- grown
      end

    let rec queue_bubble_up queue index =
      if index > 0 then begin
        let parent = (index - 1) / 2 in
        if higher_priority (queue_get queue index) (queue_get queue parent) then begin
          queue_swap queue index parent;
          queue_bubble_up queue parent
        end
      end

    let rec queue_bubble_down queue index =
      let left = (2 * index) + 1 in
      if left < queue.size then begin
        let right = left + 1 in
        let best =
          if right < queue.size
          && higher_priority (queue_get queue right) (queue_get queue left)
          then right
          else left
        in
        if higher_priority (queue_get queue best) (queue_get queue index) then begin
          queue_swap queue best index;
          queue_bubble_down queue best
        end
      end

    let queue_insert queue value =
      if not (VH.mem queue.present value) then begin
        queue_ensure_capacity queue;
        queue.heap.(queue.size) <- Some value;
        VH.replace queue.present value ();
        queue.size <- queue.size + 1;
        queue_bubble_up queue (queue.size - 1)
      end

    let queue_peek queue =
      if queue.size = 0 then None else Some (queue_get queue 0)

    let queue_pop queue =
      if queue.size = 0 then None
      else begin
        let result = queue_get queue 0 in
        VH.remove queue.present result;
        queue.size <- queue.size - 1;
        if queue.size = 0 then
          queue.heap.(0) <- None
        else begin
          queue.heap.(0) <- queue.heap.(queue.size);
          queue.heap.(queue.size) <- None;
          queue_bubble_down queue 0
        end;
        Some result
      end

    let find_value table variable =
      match VH.find_option table variable with
      | Some value -> value
      | None -> S.Dom.bot ()

    let sigma_value state variable = find_value state.sigma variable
    let start_value state variable = find_value state.starts variable

    let materialize state variable =
      if not (VH.mem state.sigma variable) then
        VH.replace state.sigma variable (S.Dom.bot ())

    let lattice_equal left right =
      S.Dom.leq left right && S.Dom.leq right left

    (** The first benchmark uses a widening-only UPDATE.  Applying it to the
        join with [old] makes each variable and side cell monotone. *)
    let update old input = S.Dom.widen old (S.Dom.join old input)

    let get_or_create table key capacity =
      match VH.find_option table key with
      | Some row -> row
      | None ->
        let row = VH.create capacity in
        VH.replace table key row;
        row

    let join_sides state target =
      match VH.find_option state.sides target with
      | None -> S.Dom.bot ()
      | Some row ->
        VH.fold
          (fun _ contribution aggregate -> S.Dom.join contribution aggregate)
          row (S.Dom.bot ())

    let enqueue state variable = queue_insert state.queue variable

    let owner_may_still_be_queued state owner =
      match queue_peek state.queue with
      | None -> false
      | Some head ->
        S.Var.equal head owner || S.Var.compare owner head < 0

    let set_value state owner direct =
      let candidate =
        S.Dom.join direct
          (S.Dom.join (start_value state owner) (join_sides state owner))
      in
      let old = sigma_value state owner in
      if not (lattice_equal candidate old) then begin
        let next = update old candidate in
        if not (lattice_equal next old) then begin
          VH.replace state.sigma owner next;
          match VH.find_option state.infl owner with
          | None -> ()
          | Some readers ->
            VH.iter (fun reader () -> enqueue state reader) readers;
            VH.clear readers
        end
      end

    let solve xs vs _old_data =
      let state = create_state () in

      let rec solve_one owner =
        materialize state owner;
        let direct =
          match S.system owner with
          | None -> S.Dom.bot ()
          | Some rhs -> rhs (get owner) (side owner) (spawn owner)
        in
        set_value state owner direct;
        drain owner

      and get owner target =
        if not (VH.mem state.sigma target) then begin
          if S.Var.compare owner target < 0 then
            solve_one target
          else
            enqueue state target
        end;
        let readers = get_or_create state.infl target 4 in
        VH.replace readers owner ();
        materialize state target;
        sigma_value state target

      and side owner target input =
        let old_aggregate = join_sides state target in
        let contributions = get_or_create state.sides target 4 in
        let old_contribution =
          match VH.find_option contributions owner with
          | Some contribution -> contribution
          | None -> S.Dom.bot ()
        in
        let contribution = update old_contribution input in
        VH.replace contributions owner contribution;
        let new_aggregate = join_sides state target in
        if not (S.Dom.leq new_aggregate old_aggregate) then begin
          if S.Var.compare owner target < 0
          && not (VH.mem state.sigma target)
          then
            solve_one target
          else
            enqueue state target
        end

      and spawn owner target =
        if not (VH.mem state.sigma target) then begin
          if S.Var.compare owner target < 0 then
            solve_one target
          else begin
            enqueue state target;
            materialize state target
          end
        end

      and drain owner =
        if owner_may_still_be_queued state owner then begin
          match queue_pop state.queue with
          | None -> failwith "QSolver: queued owner disappeared"
          | Some next ->
            solve_one next;
            drain owner
        end
      in

      List.iter
        (fun (variable, input) ->
           let old_start = start_value state variable in
           let start = S.Dom.join old_start input in
           VH.replace state.starts variable start;
           VH.replace state.sigma variable start;
           enqueue state variable)
        xs;
      List.iter (enqueue state) vs;

      let rec solve_queue () =
        match queue_pop state.queue with
        | None -> ()
        | Some owner ->
          solve_one owner;
          solve_queue ()
      in
      solve_queue ();
      (state.sigma, ())
  end

let _ =
  Selector.add_solver ("qs_trivial", (module QSolver))