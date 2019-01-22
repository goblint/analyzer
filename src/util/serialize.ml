open Prelude
open GobConfig
open Analyses

let base_dir () = get_string "incremental.basedir"

let current_commit = Git.current_commit "." (* TODO: change to file path of analyzed src *)

let current_commit_dir () = match current_commit with 
    | Some commit ->
        let dir = Filename.concat (base_dir ()) commit in
        Some (Goblintutil.create_dir dir)
    | None -> None (* git-directory not clean *)

let versionMapFilename = "version.data"

let cilFileName = "ast.data"

let marshall obj fileName  =
  let objString = Marshal.to_string obj [] in
  let file = File.open_out fileName in
  BatInnerIO.write_string file objString;
  BatInnerIO.close_out file

let unmarshall fileName =
  let file = File.open_in fileName in
  let marshalled = BatInnerIO.read_string file in
  Marshal.from_string marshalled 0 

let saveCil (file: Cil.file) = match current_commit_dir () with
  |Some dir ->
    let cilFile = Filename.concat dir cilFileName in
    Cil.saveBinaryFile file cilFile
  | None -> ()

let create_commit_dir (dirName: string) = match current_commit_dir () with
  | Some dir -> Some (Goblintutil.create_dir dir)
  | None -> None