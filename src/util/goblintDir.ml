open GobConfig

(** Temp directory, set by create_temp_dir, but used by the OSEK analysis. *)
let tempDirName = ref "goblint_temp"

(* The temp directory for preprocessing the input files *)
let create_temp_dir () =
  if Sys.file_exists (get_string "tempDir") then
    tempDirName := get_string "tempDir"
  else
    (* TODO: generalize .goblint for everything *)
    ignore (Goblintutil.create_dir ".goblint");
    let preprocessed_dir = Goblintutil.create_dir (Filename.concat ".goblint" "preprocessed") in
    assert (Sys.file_exists preprocessed_dir);
    (* raise Exit; *)
    tempDirName := preprocessed_dir

let remove_temp_dir () =
  if not (get_bool "pre.keep") then
    ignore (Goblintutil.rm_rf !tempDirName);
  GobSys.rmdir_if_empty ".goblint"
