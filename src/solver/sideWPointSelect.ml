(** Strategies for widening leaf unknowns *)

open Batteries
open ConstrSys
open Messages

module type S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  sig
    type data

    (** Create data required by this widening point selection strategy.
        The parameters are not necessarily used by all strategies. 
        @param is_stable This callback should return whether an unknown is stable.
        @param add_infl  Allows the strategy to record additional influences.
                          This is mainly intended for strategies like unstable-self,
                          which records the influence of a side-effecting unknown x to the leaf y.
    *)
    val create_data: (S.v -> bool) -> (S.v -> S.v -> unit) -> data

    (** Notifies this strategy that a side-effect has occured.
        This allows the strategy to adapt its internal data structure.
        @param data The internal state of this strategy
        @param x    The optional source of the side-effect
        @param y    The leaf receiving the side-effect
    *)
    val notify_side: data -> S.v option -> S.v -> unit

    (** Whether the destabilization of the side-effected var should record the destabilization
        of called variables and start variables. This information should be passed to [should_mark_wpoint]
        by the solver.
    *)
    val record_destabilized_vs: bool

    (** This strategy can decide to prevent widening.
        Note that, if this strategy does not veto, this does not mean that widening
        will necessarily be performed. Nor does a call to this function imply that
        the value of the leaf has grown.
        @param data      The internal state of this strategy
        @param called    Set of called unknowns
        @param old_sides Prior side-effects to leaf y
        @param x         Optional source of the side-effect
        @param y         Side-effected leaf y
        @return [true]: widening will not be applied; [false]: widening might be applied
    *)
    val veto_widen: data -> unit HM.t -> VS.t -> S.v option -> S.v -> bool

    (** The value of the leaf has grown. Should it be marked a widening point?
        Widening points are widened when their value grows, unless vetoed.
        Even if this function is called, leaf y might already be a widening point
        from an earlier side-effect.
        @param data            The internal state of this strategy
        @param called          Set of called unknowns
        @param old_sides       Prior side-effects to leaf y
        @param x               Optional source of the side-effect
        @param y               Side-effected leaf y
        @param destabilized_vs Optional destabilization info, described in [record_destabilized_vs]
        @return [true]: mark as widening point; [false]: do not mark as widening point

    *)
    val should_mark_wpoint: data -> unit HM.t -> VS.t -> S.v option -> S.v -> bool option -> bool
  end

(** Any side-effect after the first one will be widened which will unnecessarily lose precision. *)
module Always : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit

    let create_data _ _ = ()
    let notify_side _ _ _ = ()
    let record_destabilized_vs = false
    let veto_widen _ _ _ _ _ = false
    let should_mark_wpoint _ _ _ _ _ _ = true
  end

(* On side-effect cycles, this should terminate via the outer `solver` loop. TODO check. *)
(** Never widen side-effects. *)
module Never : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit

    let create_data _ _ = ()
    let notify_side _ _ _ = ()
    let record_destabilized_vs = false
    let veto_widen _ _ _ _ _ = false
    let should_mark_wpoint _ _ _ _ _ _ = false
  end

(** Widening check happens by checking sides.
    Only widen if value increases and there has already been a side-effect from the same source *)
module SidesLocal : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit

    let create_data _ _ = ()
    let notify_side _ _ _ = ()
    let record_destabilized_vs = false
    let veto_widen state called old_sides x y =
      match x with
      | None -> false
      | Some x when VS.mem x old_sides -> false
      | _ -> true
    let should_mark_wpoint _ _ _ _ _ _ = true
  end

(** If there was already a `side x y d` from the same program point and now again, make y a widening point.
    Different from `Sides` in that it will not distinguish between side-effects from different contexts,
    only the program point matters. *)
module SidesPP : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit
    let create_data _ _ = ()
    let notify_side _ _ _ = ()
    let record_destabilized_vs = false
    let veto_widen state called old_sides x y = false
    let should_mark_wpoint state called old_sides x y _ = match x with
      | Some x ->
        let n = S.Var.node x in
        VS.exists (fun v -> Node.equal (S.Var.node v) n) old_sides
      | None -> false
      (* TODO: This is consistent with the previous implementation, but if multiple side-effects come in with x = None,
          the leaf will never be widened. This is different from SidesLocal *)
  end

(** If there already was a `side x y d` that changed rho[y] and now again, we make y a wpoint.
    x caused more than one update to y. >=3 partial context calls will be precise since sides come from different x. TODO this has 8 instead of 5 phases of `solver` for side_cycle.c *)
module Sides : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit

    let create_data _ _ = ()
    let notify_side _ _ _ = ()
    let record_destabilized_vs = false
    let veto_widen state called old_sides x y = false
    let should_mark_wpoint state called old_sides x y _ = match x with | Some(x) -> VS.mem x old_sides | None -> true
  end

(* TODO: The following two don't check if a vs got destabilized which may be a problem. *)

(* TODO test/remove. Check for which examples this is problematic! *)
(** Side to y destabilized itself via some infl-cycle. Records influences from unknowns to globals *)
module UnstableSelf : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = { is_stable: S.v -> bool; add_infl: S.v -> S.v -> unit }

    let create_data is_stable add_infl = { is_stable; add_infl }
    let notify_side data x y = (match x with None -> () | Some x -> data.add_infl x y)
    let record_destabilized_vs = false
    let veto_widen _ _ _ _ _ = false
    let should_mark_wpoint state called old_sides x y _ = not (state.is_stable y)
  end

(* TODO test/remove. *)
(** Widen if any called var (not just y) is no longer stable. Expensive! *)
module UnstableCalled : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = { is_stable: S.v -> bool }

    let create_data is_stable _ = { is_stable }
    let notify_side _ _ _ = ()
    let record_destabilized_vs = false
    let veto_widen state called old_sides y x = false
    let should_mark_wpoint state called old_sides y x _ = HM.exists (fun k _ -> not (state.is_stable k)) called (* this is very expensive since it iterates over called! see https://github.com/goblint/analyzer/issues/265#issuecomment-880748636 *)
  end

(** Destabilized a called or start var. Problem: two partial context calls will be precise, but third call will widen the state.
    If this side destabilized some of the initial unknowns vs, there may be a side-cycle between vs and we should make y a wpoint *)    
module Cycle : S =
  functor (S:EqConstrSys) ->
  functor (HM:Hashtbl.S with type key = S.v) ->
  functor (VS:Set.S with type elt = S.v) ->
  struct
    type data = unit

    let create_data _ _ = ()
    let notify_side _ _ _ = ()
    let record_destabilized_vs = true
    let veto_widen state called old_sides x y = false
    let should_mark_wpoint state called old_sides x y cycle =
      match cycle with
      | Some cycle ->
        if tracing && cycle then trace "side_widen" "cycle: should mark wpoint %a" S.Var.pretty_trace y;
        cycle
      | None ->
        failwith "destabilize_vs information not provided to side_widen cycle strategy";
  end

let choose_impl: unit -> (module S) = fun () ->
  let conf = GobConfig.get_string "solvers.td3.side_widen" in
  match conf with
  | "always" -> (module Always)
  | "never" -> (module Never)
  | "sides-local" -> (module SidesLocal)
  | "sides" -> (module Sides)
  | "sides-pp" -> (module SidesPP)
  | "unstable-self" -> (module UnstableSelf)
  | "unstable-called" -> (module UnstableCalled)
  | "cycle" -> (module Cycle)
  | _ -> failwith ("Unknown value '" ^ conf ^ "' for option solvers.td3.side_widen!")
