(** Intermediate data directory. *)

open GobConfig

let root () = Fpath.v (get_string "goblint-dir")

let preprocessed () = Fpath.(root () / "preprocessed")

let init () =
  (* TODO: generalize .goblint for everything *)
  GobSys.mkdir_or_exists (root ());
  GobSys.mkdir_or_exists (preprocessed ())

let finalize () =
  if not (get_bool "pre.keep") then
    ignore (GobSys.rmdir_recursive (preprocessed ()));
  GobSys.rmdir_if_empty (root ())
