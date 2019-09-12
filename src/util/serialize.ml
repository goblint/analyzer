open Prelude
open GobConfig
open Analyses

let goblint_dirname = ".gob"

let version_map_filename = "version.data"

let cilFileName = "ast.data"

let src_direcotry = ref ""

let gob_directory () = let src_dir = !src_direcotry in
  Filename.concat src_dir goblint_dirname

let current_commit () =
  Git.current_commit (!src_direcotry)

let commit_dir src_files commit =
  let gob_dir = gob_directory src_files in
  Filename.concat gob_dir commit

let current_commit_dir () = match current_commit () with
  | Some commit -> (
      try
        let gob_dir = gob_directory () in
        let _path  = Goblintutil.create_dir gob_dir in
        let dir = Filename.concat gob_dir commit in
        Some (Goblintutil.create_dir dir)
      with e -> let error_message = (Printexc.to_string e) in
        print_newline ();
        print_string "The following error occured while creating a directory: ";
        print_endline error_message;
        None)
  | None -> None (* git-directory not clean *)

(** A list of commits previously analyzed for the given src directory *)
let get_analyzed_commits src_files =
  let src_dir = gob_directory src_files in
  Sys.readdir src_dir

let last_analyzed_commit () =
  try
    let src_dir = !src_direcotry in
    let commits = Git.git_log src_dir in
    let commitList = String.split_on_char '\n' commits in
    let analyzed = get_analyzed_commits () in
    let analyzed_set = Set.of_array analyzed in
    Some (List.hd @@ List.drop_while (fun el -> not @@ Set.mem el analyzed_set) commitList)
  with e -> None

let marshal obj fileName  =
  let objString = Marshal.to_string obj [] in
  let file = File.open_out fileName in
  Printf.fprintf file "%s" objString;
  flush file;
  close_out file;;

let unmarshal fileName =
  let marshalled = input_file fileName in
  if GobConfig.get_bool "dbg.verbose" then print_endline ("Unmarshalling " ^ fileName ^ "... If type of content changed, this will result in a segmentation fault!");
  Marshal.from_string marshalled 0 (* use Marshal.from_channel? *)

let results_exist () =
  last_analyzed_commit () <> None

let last_analyzed_commit_dir (src_files: string list) =
  match last_analyzed_commit () with
  | Some commit -> commit_dir () commit
  | None -> failwith "No previous analysis results"

let load_latest_cil (src_files: string list) =
  try
    let dir = last_analyzed_commit_dir src_files in
    let cil = Filename.concat dir cilFileName in
    Some (unmarshal cil)
  with e -> None

let save_cil (file: Cil.file) = match current_commit_dir () with
  | Some dir ->
    let cilFile = Filename.concat dir cilFileName in
    marshal file cilFile
  | None -> print_endline "Failed saving cil: working directory is dirty"
