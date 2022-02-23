open GobConfig

let root () = get_string "goblint-dir"

let preprocessed () = Filename.concat (root ()) "preprocessed"

let init () =
  (* TODO: generalize .goblint for everything *)
  ignore (Goblintutil.create_dir (root ()));
  ignore (Goblintutil.create_dir (preprocessed ()))

let finalize () =
  if not (get_bool "pre.keep") then
    ignore (Goblintutil.rm_rf (preprocessed ()));
  GobSys.rmdir_if_empty (root ())
