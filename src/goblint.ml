(* We do this since the evaluation order of top-level bindings is not defined, but we want `main` to run after all the other side-effects (e.g. registering analyses/solvers) have happened. *)
let () = at_exit Maingoblint.main
