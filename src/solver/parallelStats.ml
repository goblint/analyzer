open Batteries

module ParallelSolverStats = 
struct
  open Messages

  let cas_success = Atomic.make 0
  let cas_fail = Atomic.make 0
  let nr_iterations = Atomic.make 0

  let start_time = ref 0.
  let end_time = ref None (* None while the solver is running *)

  let solver_start_event () =
    start_time := Unix.gettimeofday ();
    end_time := None;
    Atomic.set cas_success 0;
    Atomic.set cas_fail 0;
    Atomic.set nr_iterations 0

  let solver_end_event () =
    end_time := Some (Unix.gettimeofday ())

  let start_iterate_event job_id =
    Atomic.incr nr_iterations

  let cas_success_event () = Atomic.incr cas_success
  let cas_fail_event () = Atomic.incr cas_fail

  let print_stats () =
    if tracing then trace "sol_stats" "Cas success: %d" (Atomic.get cas_success);
    if tracing then trace "sol_stats" "Cas fail: %d" (Atomic.get cas_fail);

    (match !end_time with
     | Some end_time -> if tracing then trace "sol_stats" "Solver duration: %.2f" (end_time -. !start_time)
     | None -> if tracing then trace "sol_stats" "Solver running for (s): %.2f" (Unix.gettimeofday () -. !start_time));

    if tracing then trace "sol_stats" "Iterations: %d" (Atomic.get nr_iterations);

end

