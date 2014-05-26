open Batteries
open Arinc.Spec
open GobConfig

let cs = ref "??"

let save_all ch =
  ArincUtil.unmarshal ch;
  ArincUtil.print_actions ();
  ArincUtil.save_dot_graph ();
  ArincUtil.save_promela_model ()

let _ =
  set_auto "mainfun" "['main']"; (* TODO get original (maybe also marshal config) *)
  set_int "ana.arinc.cs_len" 99; (* TODO same *)
  set_bool "dbg.debug" true;
  set_bool "colors" true;
  if Array.length Sys.argv <= 1 then (
    print_endline "No input file given!";
    let file = "result/arinc.cs99.out" in (* what about cs_len? read from file name? *)
    print_endline @@ "Looking for "^file;
    if Sys.file_exists file then (
      print_endline @@ "Will use "^file^"!";
      save_all (open_in_bin file)
    ) else (
      print_endline @@ "No input :(";
      exit 1
    )
  ) else ( (* there are arguments *)
    let arg = Sys.argv.(1) in
    if arg = "-" then (
      print_endline "Will read from stdin!";
      save_all stdin
    ) else (
      if Sys.file_exists arg then (
        print_endline @@ "Will use "^arg^"!";
        save_all (open_in_bin arg)
      ) else (
        print_endline "Given file does not exist!";
        exit 1
      )
    )
  )
