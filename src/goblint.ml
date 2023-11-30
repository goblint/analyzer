open Goblint_lib
open GobConfig
open Maingoblint
open Printf
open SharedFunctions
open AffineEqualityAnalysis
open VarManagement
open Apron
(* open VarManagement *)
let var1 = Var.of_string "test1"
let var2 = Var.of_string "test2"
let env_test = add_vars (bot()) [var1; var2] 
let print_opt opt print endl = match opt with | None -> print_endline "None"
                                              | Some x -> print x; if endl then print_endline ""
let const_expr i = Texpr1.Cst (Scalar (Mpqf (Mpqf.of_int i)))
let env_with_information = D.assign_texpr env_test var1 (const_expr 3)
(** the main function *)
let main () = 
  try
    Cilfacade.init ();
    Maingoblint.parse_arguments ();

    (* test test 
       print_endline "Test 1";
       let env_test = add_vars (bot()) [(Var.of_string "test1")] in
       print_t env_test;
       print_endline "";  print_endline "";*)

    (* test get_coeff 
    print_endline "Test get_coeff1";
    print_opt (get_coeff env_test (Var var1)) Equality.print false;
    print_env env_test.env;
    print_endline "";
    print_endline "Test get_coeff2";
    print_opt (get_coeff env_test (const_expr 3)) Equality.print false;
    print_endline "";
    print_endline "Test get_coeff2";
    print_opt (get_coeff env_test (const_expr 3)) Equality.print false;
    print_endline "";
    print_t env_test;*)

    (* test assign_texpr
    *)
  print_t env_with_information;

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

    if get_bool "dbg.verbose" then (
      print_endline (GobUnix.localtime ());
      print_endline GobSys.command_line;
    );
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
      (* This is run independant of the autotuner being enabled or not be sound for programs with longjmp *)
      AutoTune.activateLongjmpAnalysesWhenRequired ();
      if get_bool "ana.autotune.enabled" then AutoTune.chooseConfig file;
      file |> do_analyze changeInfo;
      do_html_output ();
      do_gobview file;
      do_stats ();
      Goblint_timing.teardown_tef ();
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
    eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted by SIGINT (Ctrl-C)!"));
    Goblint_timing.teardown_tef ();
    exit 131 (* same exit code as without `Sys.catch_break true`, otherwise 0 *)
  | Timeout.Timeout ->
    do_stats ();
    eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted because it reached the set timeout of " ^ get_string "dbg.timeout" ^ " or was signalled SIGPROF!"));
    Goblint_timing.teardown_tef ();
    exit 124

(* We do this since the evaluation order of top-level bindings is not defined, but we want `main` to run after all the other side-effects (e.g. registering analyses/solvers) have happened. *)
let () = at_exit main
