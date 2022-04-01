open Prelude
open Unix

let buff_size = 1024

(* Suffix of files combined by CIL *)
let comb_suffix = "_comb.c"

let exec_command ?path (command: string) =
  let current_dir = Sys.getcwd () in
  (match path with
   | Some path ->
     let path_str = Fpath.to_string path in
     if Sys.file_exists path_str && Sys.is_directory path_str then Sys.chdir path_str
     else failwith ("Directory " ^ path_str ^ " does not exist!")
   | None -> ());
  if GobConfig.get_bool "dbg.verbose" then print_endline ("executing command `" ^ command ^ "` in " ^ Sys.getcwd ());
  let (std_out, std_in) = open_process command in
  let output = Buffer.create buff_size in
  try
    while true do
      let line = input_char std_out in
      Buffer.add_char output line
    done;
    assert false;
  with End_of_file ->
    let exit_code = close_process (std_out,std_in) in
    let output = Buffer.contents output in
    Sys.chdir current_dir;
    (exit_code, output)


(* BFS for a file with a given suffix in a directory or any subdirectoy *)
let find_file_by_suffix (dir: Fpath.t) (file_name_suffix: string) =
  let list_files d = List.map (Fpath.add_seg dir) @@ Array.to_list @@ Sys.readdir (Fpath.to_string d) in
  let dirs = Queue.create () in
  let rec search (dir: Fpath.t) (files: Fpath.t list) = match files with
    | (h::t) -> let f = Fpath.to_string h in
      if Sys.file_exists f && Sys.is_directory f
      then (Queue.add h dirs; search dir t)
      else if Batteries.String.ends_with (Fpath.filename h) file_name_suffix then h else search dir t
    | [] ->
      if Queue.is_empty dirs then failwith ("find_file_by_suffix found no files with suffix "^file_name_suffix^" in "^ Fpath.to_string dir)
      else let d = Queue.take dirs in search d (list_files d)
  in
  search dir (list_files dir)

(* Delete all *_comb.c files in the directory *)
let remove_comb_files path =
  try
    while true do
      let comb = Fpath.to_string (find_file_by_suffix path comb_suffix) in
      if GobConfig.get_bool "dbg.verbose" then print_endline ("deleting " ^ comb);
      Sys.remove comb;
    done
  with Failure e -> ()

let run_cilly (path: Fpath.t) ~all_cppflags =
  let path_str = Fpath.to_string path in
  if Sys.file_exists path_str && Sys.is_directory path_str then (
    (* We need to `make clean` if `make` was run manually, otherwise it would say there is nothing to do and cilly would not be run and no combined C file would be created. *)
    let _ = exec_command ~path "make clean" in
    remove_comb_files path;
    (* Combine source files with make using cilly as compiler *)
    let gcc_path = GobConfig.get_string "exp.gcc_path" in
    let cflags = if all_cppflags = [] then "" else " CFLAGS+=" ^ Filename.quote (String.join " " all_cppflags) in
    let (exit_code, output) = exec_command ~path ("make CC=\"cilly --gcc=" ^ gcc_path ^ " --merge --keepmerged\"" ^cflags ^ " " ^
                                                  "LD=\"cilly --gcc=" ^ gcc_path ^ " --merge --keepmerged\"") in
    print_string output;
    (* fail if make failed *)
    if exit_code <> WEXITED 0 then
      failwith ("Failed combining files. Make was " ^ (GobUnix.string_of_process_status exit_code) ^ ".")
  )

let generate_and_combine makefile ~all_cppflags =
  let path = Fpath.parent makefile in
  let makefile_str = Fpath.to_string makefile in
  (* make sure the Makefile exists or try to generate it *)
  if not (Sys.file_exists makefile_str) then (
    print_endline ("Given " ^ makefile_str ^ " does not exist! Try to generate it.");
    let configure = ("configure", "./configure", Fpath.(path / "configure")) in
    let autogen = ("autogen", "sh autogen.sh && ./configure", Fpath.(path / "autogen.sh")) in
    let exception MakefileNotGenerated in
    let generate_makefile_with (name, command, file) = if Sys.file_exists (Fpath.to_string file) then (
        print_endline ("Trying to run " ^ name ^ " to generate Makefile");
        let exit_code, output = exec_command ~path command in
        print_endline (command ^ " " ^ GobUnix.string_of_process_status exit_code ^ ". Output: " ^ output);
        if not (Sys.file_exists makefile_str) then raise MakefileNotGenerated
      ) else raise MakefileNotGenerated in
    try generate_makefile_with configure
    with MakefileNotGenerated ->
    try generate_makefile_with autogen
    with MakefileNotGenerated -> failwith ("Could neither find given " ^ makefile_str ^ " nor generate it - abort!");
  );
  run_cilly path ~all_cppflags;
  find_file_by_suffix path comb_suffix
