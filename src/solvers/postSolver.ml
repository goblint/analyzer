open Prelude
open Analyses

module Post (S: EqConstrSys) (VH: Hash.H with type key = S.v) =
struct
  let post xs vs vh =
    (* TODO: reachability/verify should do something with xs as well? *)
    ignore (Pretty.printf "Post solver\n");

    let reachability xs =
      let reachable = VH.create (VH.length vh) in
      let rec one_var x =
        if not (VH.mem reachable x) then (
          VH.replace reachable x ();
          Option.may one_constraint (S.system x)
        )
      and one_constraint f =
        let get x =
          one_var x;
          try VH.find vh x with Not_found -> S.Dom.bot ()
        in
        let set x _ =
          one_var x
        in
        ignore (f get set)
      in
      List.iter one_var xs;
      (* TODO: expose VH.filteri_inplace *)

      VH.iter (fun x _ ->
          if not (VH.mem reachable x) then
            VH.remove vh x
        ) vh
    in
    reachability vs;

  (* TODO: add (combined) timing *)
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
