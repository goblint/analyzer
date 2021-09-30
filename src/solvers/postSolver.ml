open Prelude
open Analyses

module type Arg =
sig
  val should_prune: bool
  val should_verify: bool
  val should_warn: bool
end

module Post (Arg: Arg) (S: EqConstrSys) (VH: Hash.H with type key = S.v) =
struct
  open Arg

  let post xs vs vh =
    (* TODO: reachability/verify should do something with xs as well? *)
    ignore (Pretty.printf "Post solver\n");

    Goblintutil.in_verifying_stage := true;
    if should_verify then
      Goblintutil.verified := Some true;

    let old_should_warn = !Goblintutil.should_warn in
    Goblintutil.should_warn := should_warn;

    let complain_constraint x ~lhs ~rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a\n @[Solver computed:\n%a\nRight-Hand-Side:\n%a\nDifference: %a\n@]" S.Var.pretty_trace x S.Dom.pretty lhs S.Dom.pretty rhs S.Dom.pretty_diff (rhs, lhs))
    in
    let complain_side x y ~lhs ~rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a\nOrigin: %a\n @[Solver computed:\n%a\nSide-effect:\n%a\nDifference: %a\n@]" S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty lhs S.Dom.pretty rhs S.Dom.pretty_diff (rhs, lhs))
    in

    let reachability xs =
      let reachable = VH.create (VH.length vh) in
      let rec one_var x =
        if not (VH.mem reachable x) then (
          VH.replace reachable x ();
          Option.may (one_constraint x) (S.system x)
        )
      and one_constraint x f =
        let get y =
          one_var y;
          try VH.find vh y with Not_found -> S.Dom.bot ()
        in
        let set y d =
          if should_verify then (
            let y_lhs = try VH.find vh y with Not_found -> S.Dom.bot () in
            if not (S.Dom.leq d y_lhs) then
              complain_side x y ~lhs:y_lhs ~rhs:d
          );
          (* check before recursing *)
          one_var y
        in
        let rhs = f get set in
        if should_verify then (
          let lhs = try VH.find vh x with Not_found -> S.Dom.bot () in
          if not (S.Dom.leq rhs lhs) then
            complain_constraint x ~lhs ~rhs
        )
      in
      List.iter one_var xs;

      if should_prune then (
        (* TODO: expose VH.filteri_inplace *)
        VH.iter (fun x _ ->
            if not (VH.mem reachable x) then
              VH.remove vh x
          ) vh
      )
    in
    reachability vs;

    Goblintutil.in_verifying_stage := false;

    Goblintutil.should_warn := old_should_warn

  let post xs vs vh =
    if should_prune || should_verify || should_warn then
      Stats.time "postsolver" (post xs vs) vh
end

module Lift (Arg: Arg) (Solver: GenericEqBoxSolver): GenericEqBoxSolver =
  functor (S: EqConstrSys) (VH: Hash.H with type key = S.v) ->
  struct
    module Solver = Solver (S) (VH)
    module Post = Post (Arg) (S) (VH)

    let solve box xs vs =
      let (vh, _) as ret = Solver.solve box xs vs in
      Post.post xs vs vh;
      ret
  end
