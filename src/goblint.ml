open Goblint_lib
open GobConfig
open Maingoblint

(** the main function *)
let main () =
  try
    Maingoblint.parse_arguments ();
    Cilfacade.init ();

    (* Timing. *)
    Maingoblint.reset_stats ();
    if get_bool "dbg.timing.enabled" then (
      let tef_filename = get_string "dbg.timing.tef" in
      if tef_filename <> "" then
        Goblint_timing.setup_tef tef_filename;
      Timing.Default.start {
        cputime = true;
        walltime = true;
        allocated = true;
        count = true;
        tef = true;
      };
      Timing.Program.start {
        cputime = false;
        walltime = false;
        allocated = false;
        count = false;
        tef = true;
      }
    );

    handle_extraspecials ();
    GoblintDir.init ();

    Logs.debug "%s" (GobUnix.localtime ());
    Logs.debug "%s" GobSys.command_line;
    (* When analyzing a termination specification, activate the termination analysis before pre-processing. *)
    if get_string "ana.specification" <> "" then AutoSoundConfig.enableAnalysesForTerminationSpecification ();
    if AutoTune.isActivated "termination" then AutoTune.focusOnTermination ();
    let file = lazy (Fun.protect ~finally:GoblintDir.finalize preprocess_parse_merge) in
    if get_bool "server.enabled" then (
      let file =
        if get_bool "server.reparse" then
          None
        else
          Some (Lazy.force file)
      in
      Server.start file
    )
    else (
      let file = Lazy.force file in
      let changeInfo =
        if GobConfig.get_bool "incremental.load" || GobConfig.get_bool "incremental.save" then
          diff_and_rename file
        else
          None
      in
      (* This is run independant of the autotuner being enabled or not to be sound for programs with longjmp *)
      AutoSoundConfig.activateLongjmpAnalysesWhenRequired ();
      if get_string "ana.specification" <> "" then AutoSoundConfig.enableAnalysesForSpecification ();
      if get_bool "ana.autotune.enabled" then AutoTune.chooseConfig file;
      file |> do_analyze changeInfo;
      do_html_output ();
      do_gobview file;
      do_stats ();
      Goblint_timing.teardown_tef ();
      (* TODO: generalize exit codes for AnalysisState.unsound_both_branches_dead? *)
      if !AnalysisState.verified = Some false then exit 3 (* verifier failed! *)
    )
  with
  | Stdlib.Exit ->
    do_stats ();
    Goblint_timing.teardown_tef ();
    exit 1
  | Sys.Break -> (* raised on Ctrl-C if `Sys.catch_break true` *)
    do_stats ();
    Printexc.print_backtrace stderr;
    Logs.error "%s" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted by SIGINT (Ctrl-C)!"));
    Goblint_timing.teardown_tef ();
    exit 131 (* same exit code as without `Sys.catch_break true`, otherwise 0 *)
  | Timeout.Timeout ->
    do_stats ();
    Logs.error "%s" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted because it reached the set timeout of " ^ get_string "dbg.timeout" ^ " or was signalled SIGPROF!"));
    Goblint_timing.teardown_tef ();
    exit 124
  | Svcomp.Error msg ->
    do_stats ();
    Witness.print_svcomp_result ("ERROR (" ^ msg ^ ")");
    Goblint_timing.teardown_tef ();
    exit 1

(* We do this since the evaluation order of top-level bindings is not defined, but we want `main` to run after all the other side-effects (e.g. registering analyses/solvers) have happened. *)
let () = at_exit main
