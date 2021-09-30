open Prelude
open Analyses
open GobConfig

module Post (S: EqConstrSys) (VH: Hash.H with type key = S.v) =
struct
  let post xs vs vh =
    (* TODO: reachability/verify should do something with xs as well? *)
    ignore (Pretty.printf "Post solver\n");

    let should_verify = get_bool "verify" in
    Goblintutil.in_verifying_stage := true;
    (if should_verify then Goblintutil.verified := Some true);

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
      (* TODO: expose VH.filteri_inplace *)

      (* TODO: should_remove option *)
      VH.iter (fun x _ ->
          if not (VH.mem reachable x) then
            VH.remove vh x
        ) vh
    in
    reachability vs;

    Goblintutil.in_verifying_stage := false

  let post xs vs vh =
    Stats.time "postsolver" (post xs vs) vh
end

module Lift (Solver: GenericEqBoxSolver): GenericEqBoxSolver =
  functor (S: EqConstrSys) (VH: Hash.H with type key = S.v) ->
  struct
    module Solver = Solver (S) (VH)
    module Post = Post (S) (VH)

    let solve box xs vs =
      let (vh, _) as ret = Solver.solve box xs vs in
      Post.post xs vs vh;
      ret
  end
