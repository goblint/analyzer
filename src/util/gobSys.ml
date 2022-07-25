open Prelude

let rec mkdir_parents filename =
  let dirname = Fpath.parent filename in
  let dirname_str = Fpath.to_string dirname in
  if not (Sys.file_exists dirname_str) then (
    mkdir_parents dirname;
    Unix.mkdir dirname_str 0o770; (* TODO: what permissions? *)
  )

let mkdir_or_exists dirname =
  let dirname_str = Fpath.to_string dirname in
  try
    Unix.mkdir dirname_str 0o770 (* TODO: what permissions? *)
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    assert (Sys.is_directory dirname_str) (* may exist, but as a file *)

let rmdir_if_empty dirname =
  try
    Unix.rmdir (Fpath.to_string dirname)
  with Unix.Unix_error (Unix.ENOTEMPTY, _, _) ->
    ()
