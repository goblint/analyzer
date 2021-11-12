(* module for serializing the command objects of a compilation database *)
open MakefileUtil

let preprocess (path: string) : string list =
  if Sys.file_exists path && Sys.is_directory path then (
    print_endline "Preprocessing files with commands from compilation database.";
    remove_comb_files path;
    let comp_file = Filename.concat path CompilationDatabase.basename in
    let command_objs = CompilationDatabase.parse_file comp_file in
    let exec_command_obj obj =
      let outfile = (Filename.basename obj.CompilationDatabase.file) ^ ".i" in
      let regex_out = Str.regexp "-o \\([a-zA-Z0-9_/.-]+\\) " in
      let regex_gcc = Str.regexp "^\\(\\([a-zA-Z0-9_/.-]+\\) \\)" in
      (* TODO use given compiler or gcc version specified in config file? *)
      let preprocess_command = Str.replace_first regex_gcc "\\1 -E " (Option.get obj.CompilationDatabase.command) |> Str.replace_first regex_out ("-o " ^ outfile ^ " ") in
      let (exit_code, output) = MakefileUtil.exec_command ~path preprocess_command in
      print_string output;
      (* fail if command execution failed *)
      if exit_code <> WEXITED 0 then
        failwith ("Failed preprocessing file " ^ obj.file ^ ": " ^ (string_of_process_status exit_code) ^ ".");
      print_endline ("Preprocessed file: " ^ outfile);
      outfile in
    List.fold_right (fun o acc -> (exec_command_obj o) :: acc) command_objs []
  ) else failwith "Failed while preprocessing: Path does not exist."
