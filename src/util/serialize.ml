open Prelude
open GobConfig
open Analyses

let base_dir () = get_string "incremental.basedir"

let goblint_dirname = ".gob"

let src_direcotry src_files =  let firstFile = List.first src_files in
                               Filename.dirname firstFile

let current_commit src_files =
                        Git.current_commit (src_direcotry src_files) (* TODO: change to file path of analyzed src *)

let current_commit_dir src_files = match current_commit src_files with 
    | Some commit -> (
      try
        let src_dir = src_direcotry src_files in
        let gob_dir  = Filename.concat src_dir goblint_dirname in
        let _path  = Goblintutil.create_dir gob_dir in
        let dir = Filename.concat gob_dir commit in
        Some (Goblintutil.create_dir dir)
      with e -> let error_message = (Printexc.to_string e) in
                print_newline ();
                print_string "The following error occured while creating a directory: " ;
                print_endline error_message;
                None)
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

let saveCil (file: Cil.file) (fileList: string list)= match current_commit_dir fileList with
  |Some dir ->
    let cilFile = Filename.concat dir cilFileName in
    Cil.saveBinaryFile file cilFile
  | None -> ()

let loadCil (fileList: string list) = 
  (* TODO: Use the previous commit, or more specifally, the last analyzed commit *)
  match current_commit_dir fileList with
  |Some dir ->
    let cilFile = Filename.concat dir cilFileName in
    Some (Cil.loadBinaryFile cilFile)
  | None -> None

let create_commit_dir (dirName: string) (fileList: string list)= match current_commit_dir fileList with
  | Some dir -> Some (Goblintutil.create_dir dir)
  | None -> None