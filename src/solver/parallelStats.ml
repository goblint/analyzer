open Batteries
open GobConfig
open Goblint_constraint.ConstrSys

module ParallelSolverStats = 
struct
  open Messages

  let cas_success = Atomic.make 0
  let cas_fail = Atomic.make 0
  let nr_iterations = Atomic.make 0

  let start_time = ref 0.
  let end_time = ref 0.

  let solver_start_event () =
    start_time := Unix.gettimeofday ();
    Atomic.set cas_success 0;
    Atomic.set cas_fail 0

  let solver_end_event () =
    end_time := Unix.gettimeofday ()

  let start_iterate_event job_id =
    Atomic.incr nr_iterations

  let cas_success_event () = Atomic.incr cas_success
  let cas_fail_event () = Atomic.incr cas_fail

  let print_stats () =
    if tracing then trace "sol_stats" "Cas success: %d" (Atomic.get cas_success);
    if tracing then trace "sol_stats" "Cas fail: %d" (Atomic.get cas_fail);

    let duration = !end_time -. !start_time in
    if tracing then trace "sol_stats" "Solver duration: %.2f" duration;

    if tracing then trace "sol_stats" "Iterations: %d" (Atomic.get nr_iterations);

end

