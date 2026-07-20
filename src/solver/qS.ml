(* GENERATED FILE. DO NOT EDIT.
   Sources: extraction/MonadErasureExperiment.v and
            extraction/goblint_qsolver.ml.in
   Regenerate with: make goblint-reset-extraction *)
(**
   Mutable Goblint adapter template for episode-resetting QSolver.

   The generated copy-ready file combines the extracted identity-monad
   solve-one and queue-loop control with this OCaml-specific mutable
   representation. Ordinary owner phases reset at fresh episode boundaries
   and survive dirty reruns. A side cell resets after the minimum-ranked
   variable read before that effect has published a changed, completed
   episode; read-free effects are stored raw.

   First-benchmark limitations:
   - solving always starts from fresh state;
   - the core ignores [old_data];
   - update strategy parameters are OCaml compile-time constants.

   See [OCAML_CONTRACT.md] for the external callback and domain contract.
*)

open Batteries
open Goblint_constraint.ConstrSys
open Goblint_constraint.SolverTypes

module Extracted = struct

  type 'a m = 'a

  (** val ret : 'a1 -> 'a1 m **)

  let ret value =
    value

  (** val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m **)

  let bind computation next =
    next computation

  type ('v, 'd) monadic_callback =
    'v -> (('v -> 'd m) -> ('v -> 'd -> unit m) -> ('v -> unit m) -> 'd m)
      option

  type ('v, 'd) erased_callback =
    'v -> (('v -> 'd) -> ('v -> 'd -> unit) -> ('v -> unit) -> 'd) option

  (** val erase_callback :
      ('a1, 'a2) monadic_callback -> ('a1, 'a2) erased_callback **)

  let erase_callback system =
    system

  (** val monadic_eval_rhs :
      ('a1 -> 'a2 m) -> ('a1 -> 'a2 -> unit m) -> ('a1 -> unit m) -> (('a1 ->
      'a2 m) -> ('a1 -> 'a2 -> unit m) -> ('a1 -> unit m) -> 'a2 m) -> 'a2 m **)

  let[@inline always] monadic_eval_rhs get side spawn body =
    body get side spawn

  (** val monadic_solve_one :
      'a2 -> ('a1 -> unit m) -> ('a1 -> 'a1 -> 'a2 m) -> ('a1 -> 'a1 -> 'a2 ->
      unit m) -> ('a1 -> 'a1 -> unit m) -> ('a1 -> 'a2 -> unit m) -> ('a1 ->
      unit m) -> ('a1, 'a2) monadic_callback -> 'a1 -> unit m **)

  let[@inline always] monadic_solve_one bot start_evaluation get side spawn set_value drain_owner tf owner =
    bind (start_evaluation owner) (fun _ ->
        bind
          (match tf owner with
           | Some body ->
             monadic_eval_rhs (get owner) (side owner) (spawn owner) body
           | None -> ret bot)
          (fun direct ->
             bind (set_value owner direct) (fun _ -> drain_owner owner)))

  (** val monadic_solve_loop_fuel :
      int -> (unit -> 'a1 option m) -> ('a1 -> unit m) -> bool m **)

  let rec monadic_solve_loop_fuel fuel pop_owner solve_owner =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> ret false)
      (fun fuel' ->
         bind (pop_owner ()) (fun picked ->
             match picked with
             | Some owner ->
               bind (solve_owner owner) (fun _ ->
                   monadic_solve_loop_fuel fuel' pop_owner solve_owner)
             | None -> ret true))
      fuel

  (** val monadic_solve_fuel :
      int -> ('a1 list -> unit m) -> (unit -> 'a1 option m) -> ('a1 -> unit m)
      -> 'a1 list -> bool m **)

  let[@inline always] monadic_solve_fuel fuel initialize pop_owner solve_owner initial =
    bind (initialize initial) (fun _ ->
        monadic_solve_loop_fuel fuel pop_owner solve_owner)

  (** val expose_callback :
      ('a1 -> (('a1 -> 'a2) -> ('a1 -> 'a2 -> unit) -> ('a1 -> unit) -> 'a2)
      option) -> 'a1 -> (('a1 -> 'a2) -> ('a1 -> 'a2 -> unit) -> ('a1 -> unit)
      -> 'a2) option **)

  let expose_callback system =
    system
end

module type UPDATE =
  functor (S : DemandEqConstrSys) -> sig
    type phase
    val initial_phase : phase
    val update : phase -> S.d -> S.d -> phase * S.d
  end

module WideningUpdate (S : DemandEqConstrSys) = struct
  type phase = unit

  let initial_phase = ()

  let[@inline always] update () old input =
    (), S.Dom.widen old (S.Dom.join old input)
end

module type BOX_UPDATE_CONFIG = sig
  val k : int
  val l : int
end

module BoxUpdate
    (Config : BOX_UPDATE_CONFIG)
    (S : DemandEqConstrSys) =
struct
  type phase =
    | Copy of int
    | BoxW of int
    | BoxN of int
    | Widen

  let initial_phase = Copy Config.k

  let[@inline always] update phase old input =
    match phase with
    | Copy remaining ->
      if remaining = 0 then BoxW Config.l, input
      else Copy (remaining - 1), input
    | BoxW remaining ->
      if S.Dom.leq input old then
        BoxN remaining, S.Dom.narrow old input
      else
        BoxW remaining, S.Dom.widen old input
    | BoxN remaining ->
      if S.Dom.leq input old then
        BoxN remaining, S.Dom.narrow old input
      else if remaining = 0 then
        Widen, S.Dom.widen old input
      else
        BoxW (remaining - 1), S.Dom.widen old input
    | Widen ->
      Widen, S.Dom.widen old input
end

(** Compile-time strategy selection.  [k = 0] performs one copy; [l = 0]
    permits the initial BoxW/BoxN pair and then commits to Widen on reopening. *)
module CopyUpdate = BoxUpdate (struct
    let k = 0
    let l = 0
  end)

module WNUpdate (S : DemandEqConstrSys) = 
struct
  module M = BoxUpdate (struct
      let k = 0
      let l = 0
    end) (S)

  type phase = M.phase
  let update = M.update
  let initial_phase = M.BoxW 0
end


module QSolver (Update : UPDATE) : DemandEqIncrSolver =
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
      mutable reset_scope : S.v option;
      mutable seen_closed_version : int;
    }
    type side_cell_row = side_cell VH.t

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
      infl : set VH.t;
      sides : side_row VH.t;
      side_aggregates : S.d VH.t;
      side_cells_by_source : side_cell_row VH.t;
      value_versions : int VH.t;
      closed_value_versions : int VH.t;
      widened : set;
      queue : queue;
    }

    type episode = {
      owner : S.v;
      owner_rank : int;
      mutable owner_phase : U.phase;
      outgoing_cells : side_cell_row;
      mutable min_read : (S.v * int) option;
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
        infl = VH.create 32;
        sides = VH.create 32;
        side_aggregates = VH.create 32;
        side_cells_by_source = VH.create 32;
        value_versions = VH.create 32;
        closed_value_versions = VH.create 32;
        widened = VH.create 32;
        queue = create_queue ();
      }

    (** Ranks are allocated once, in first-seen order.  A larger rank has
        higher queue priority, matching QSolver's greatest-first order. *)
    let[@inline always] rank queue variable =
      try VH.find queue.ranks variable with Not_found ->
        let value = queue.next_rank in
        queue.next_rank <- value + 1;
        VH.replace queue.ranks variable value;
        value

    let[@inline always] queue_max_rank queue =
      match RankedQueue.max_elt queue.heap with
      | Some (value_rank, _) -> Some value_rank
      | None -> None

    let[@inline always] queue_insert queue value =
      if not (VH.mem queue.present value) then begin
        let value_rank = rank queue value in
        RankedQueue.add queue.heap (value_rank, value);
        VH.replace queue.present value ();
      end

    let[@inline always] queue_pop queue =
      match RankedQueue.pop_max queue.heap with
      | None -> None
      | Some (_, result) ->
        VH.remove queue.present result;
        Some result

    let[@inline always] sigma_value state variable =
      try VH.find state.sigma variable with Not_found -> bottom

    let[@inline always] version table variable =
      try VH.find table variable with Not_found -> 0

    (** Starts and incoming side aggregates are usually absent.  Keep the
        option lookup on these paths instead of raising on every evaluation. *)
    let[@inline always] start_value state variable =
      match VH.find_option state.starts variable with
      | Some value -> value
      | None -> bottom

    let[@inline always] materialize state variable =
      if not (VH.mem state.sigma variable) then
        VH.replace state.sigma variable bottom

    let[@inline always] lattice_equal left right =
      S.Dom.leq left right && S.Dom.leq right left

    let[@inline always] get_or_create table key capacity =
      try VH.find table key with Not_found ->
        let row = VH.create capacity in
        VH.replace table key row;
        row

    let[@inline always] side_aggregate state target =
      match VH.find_option state.side_aggregates target with
      | Some aggregate -> aggregate
      | None -> bottom

    let[@inline always] recompute_side_aggregate contributions =
      VH.fold
        (fun _ contribution aggregate -> S.Dom.join contribution aggregate)
        contributions bottom

    let[@inline always] enqueue state variable =
      queue_insert state.queue variable

    let[@inline always] register_dependency
        state dependent dependent_rank dependency_rank =
      if dependency_rank >= dependent_rank then
        VH.replace state.widened dependent ()

    let[@inline always] write_changed_value state owner value =
      VH.replace state.sigma owner value;
      VH.replace state.value_versions owner
        (version state.value_versions owner + 1);
      try
        let readers = VH.find state.infl owner in
        VH.iter (fun reader () -> enqueue state reader) readers;
        VH.clear readers
      with Not_found -> ()

    let[@inline always] set_value state frame direct =
      let owner = frame.owner in
      let candidate =
        S.Dom.join direct
          (S.Dom.join (start_value state owner) (side_aggregate state owner))
      in
      let old = sigma_value state owner in
      if not (lattice_equal candidate old) then begin
        if VH.mem state.widened owner then begin
          let next_phase, next =
            U.update frame.owner_phase old candidate
          in
          frame.owner_phase <- next_phase;
          if not (lattice_equal next old) then
            write_changed_value state owner next
        end else
          write_changed_value state owner candidate
      end

    let solve xs vs _old_data =
      let state = create_state () in

      let rec solve_episode owner =
        let owner_rank = rank state.queue owner in
        let outgoing_cells =
          get_or_create state.side_cells_by_source owner 4
        in
        let frame =
          {
            owner;
            owner_rank;
            owner_phase = U.initial_phase;
            outgoing_cells;
            min_read = None;
            max_read_rank = None;
          }
        in
        materialize state owner;

        let start_evaluation _owner =
          frame.min_read <- None;
          frame.max_read_rank <- None
        in
        let rec evaluate_owner () =
          Extracted.monadic_solve_one bottom start_evaluation
            (fun _owner -> get frame)
            (fun _owner -> side frame)
            (fun _owner -> spawn frame)
            (fun _owner direct -> set_value state frame direct)
            (fun _owner -> drain_episode ())
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
        VH.replace state.closed_value_versions owner
          (version state.value_versions owner)

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
        register_dependency state owner owner_rank target_rank;
        frame.min_read <-
          begin match frame.min_read with
            | None -> Some (target, target_rank)
            | Some (_, old_rank) when target_rank < old_rank ->
              Some (target, target_rank)
            | old -> old
          end;
        frame.max_read_rank <-
          begin match frame.max_read_rank with
            | None -> Some target_rank
            | Some old_rank -> Some (max old_rank target_rank)
          end;
        let readers = get_or_create state.infl target 4 in
        VH.replace readers owner ();
        materialize state target;
        sigma_value state target

      and side frame target input =
        let owner = frame.owner in
        let owner_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        begin match frame.max_read_rank with
          | Some dependency_rank ->
            register_dependency state target target_rank dependency_rank
          | None -> ()
        end;
        let old_aggregate = side_aggregate state target in
        let contributions = get_or_create state.sides target 4 in
        let old_contribution =
          try VH.find contributions owner with Not_found -> bottom
        in
        let cell =
          try VH.find frame.outgoing_cells target with Not_found ->
            let fresh = {
              phase = U.initial_phase;
              reset_scope = None;
              seen_closed_version = 0;
            } in
            VH.replace frame.outgoing_cells target fresh;
            fresh
        in
        let contribution =
          match frame.min_read with
          | None ->
            cell.phase <- U.initial_phase;
            cell.reset_scope <- None;
            cell.seen_closed_version <- 0;
            input
          | Some (scope, _) ->
            let closed = version state.closed_value_versions scope in
            let phase =
              match cell.reset_scope with
              | Some old_scope
                when rank state.queue scope < target_rank
                  && S.Var.equal old_scope scope
                  && cell.seen_closed_version < closed ->
                U.initial_phase
              | _ -> cell.phase
            in
            let next_phase, updated =
              U.update phase old_contribution input
            in
            cell.phase <- next_phase;
            cell.reset_scope <- Some scope;
            cell.seen_closed_version <- closed;
            updated
        in
        VH.replace contributions owner contribution;
        let new_aggregate =
          if S.Dom.leq old_contribution contribution then
            if S.Dom.leq contribution old_aggregate then old_aggregate
            else S.Dom.join old_aggregate contribution
          else
            recompute_side_aggregate contributions
        in
        if new_aggregate != old_aggregate then
          VH.replace state.side_aggregates target new_aggregate;
        if not (S.Dom.leq new_aggregate old_aggregate) then begin
          if owner_rank < target_rank
          && not (VH.mem state.sigma target)
          then
            solve_episode target
          else
            enqueue state target
        end

      and spawn frame target =
        let owner_rank = frame.owner_rank in
        let target_rank = rank state.queue target in
        if not (VH.mem state.sigma target) then begin
          if owner_rank < target_rank then
            solve_episode target
          else begin
            enqueue state target;
            materialize state target
          end
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
      let finished =
        Extracted.monadic_solve_fuel Stdlib.max_int
          (fun roots -> List.iter (enqueue state) roots)
          (fun () -> queue_pop state.queue)
          solve_episode vs
      in
      if not finished then
        failwith "QSolver: exhausted max_int queue fuel";
      (state.sigma, ())
  end

module QSolverW    = QSolver (WideningUpdate)
module QSolverWNW  = QSolver (WNUpdate)
module QSolverCWNW = QSolver (CopyUpdate)

let _ =
  Selector.add_solver ("qs_w",    (module PostSolver.AddPost  (QSolverW)));
  Selector.add_solver ("qs_wnw",  (module PostSolver.AddPost  (QSolverWNW)));
  Selector.add_solver ("qs_cwnw", (module PostSolver.AddPost  (QSolverCWNW)))
