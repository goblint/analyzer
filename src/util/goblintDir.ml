open GobConfig

let root () = GobFpath.of_string_exn (get_string "goblint-dir")

let preprocessed () = Fpath.(root () / "preprocessed")

let init () =
  (* TODO: generalize .goblint for everything *)
  GobSys.mkdir_or_exists (Fpath.to_string (root ()));
  GobSys.mkdir_or_exists (Fpath.to_string (preprocessed ()))

let finalize () =
  if not (get_bool "pre.keep") then
    ignore (Goblintutil.rm_rf (Fpath.to_string (preprocessed ())));
  GobSys.rmdir_if_empty (Fpath.to_string (root ()))
