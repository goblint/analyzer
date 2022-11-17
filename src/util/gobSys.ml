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


(* Sys.time gives runtime in seconds as float *)
let split_time () = (* gives CPU time in h,m,s,ms *)
  let f = Sys.time () in
  let i = int_of_float f in
  let ms = int_of_float (Float.modulo f 1.0 *. 1000.) in
  i / 3600, i / 60 mod 60, i mod 60, ms

let string_of_time () = (* CPU time as hh:mm:ss.ms *)
  let h,m,s,ms = split_time () in
  Printf.sprintf "%02d:%02d:%02d.%03d" h m s ms
