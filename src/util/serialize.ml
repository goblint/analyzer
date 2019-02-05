open Prelude
open GobConfig
open Analyses

let base_dir () = get_string "incremental.basedir"

let goblint_dirname = ".gob"

let versionMapFilename = "version.data"

let cilFileName = "ast.data"


let marshall obj fileName  =
  let objString = Marshal.to_string obj [] in
  let file = File.open_out fileName in
  Printf.fprintf file "%s" objString;
  flush file;
  close_out file;;

let unmarshall fileName =
  let marshalled = input_file fileName in
  Marshal.from_string marshalled 0
