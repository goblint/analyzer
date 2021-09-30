open Analyses

module Post (S: EqConstrSys) (VH: Hash.H with type key = S.v) =
struct
  let post xs vs vh =
    (* TODO: reachability/verify should do something with xs as well? *)
    ignore (Pretty.printf "Post solver\n");
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
