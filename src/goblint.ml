open GobConfig
open Goblintutil
open Maingoblint
open Prelude
open Printf

(** the main function *)
let main () =
  try
    Maingoblint.reset_stats ();
    parse_arguments ();
    check_arguments ();
    AfterConfig.run ();

    Sys.set_signal (Goblintutil.signal_of_string (get_string "dbg.solver-signal")) Signal_ignore; (* Ignore solver-signal before solving (e.g. MyCFG), otherwise exceptions self-signal the default, which crashes instead of printing backtrace. *)

    Cilfacade.init ();

    handle_extraspecials ();
    GoblintDir.init ();
    handle_flags ();
    if get_bool "dbg.verbose" then (
      print_endline (localtime ());
      print_endline Goblintutil.command_line;
    );
    let file = Fun.protect ~finally:GoblintDir.finalize preprocess_and_merge in
    if get_bool "ana.autoselect" then AutoSelect.chooseConfig file;
    if get_bool "server.enabled" then Server.start file else (
      let changeInfo = if GobConfig.get_bool "incremental.load" || GobConfig.get_bool "incremental.save" then diff_and_rename file else Analyses.empty_increment_data () in
      file|> do_analyze changeInfo;
      do_stats ();
      do_html_output ();
      do_gobview ();
      if !verified = Some false then exit 3)  (* verifier failed! *)
  with
  | Exit ->
    do_stats ();
    exit 1
  | Sys.Break -> (* raised on Ctrl-C if `Sys.catch_break true` *)
    do_stats ();
    (* Printexc.print_backtrace BatInnerIO.stderr *)
    eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted by SIGINT (Ctrl-C)!"));
    exit 131 (* same exit code as without `Sys.catch_break true`, otherwise 0 *)
  | Timeout ->
    do_stats ();
    eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr ("{RED}Analysis was aborted because it reached the set timeout of " ^ get_string "dbg.timeout" ^ " or was signalled SIGPROF!"));
    exit 124

(* We do this since the evaluation order of top-level bindings is not defined, but we want `main` to run after all the other side-effects (e.g. registering analyses/solvers) have happened. *)
let () = at_exit main
