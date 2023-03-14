open Prelude

let basename = "compile_commands.json"

type command_object = {
  directory: GobFpath.t;
  file: GobFpath.t;
  command: string option [@default None];
  arguments: string list option [@default None];
  output: GobFpath.t option [@default None];
} [@@deriving yojson, show]

type t = command_object list [@@deriving yojson, show]

let parse_file filename =
  Result.get_ok (of_yojson (Yojson.Safe.from_file (Fpath.to_string filename)))

(* TODO: allow quoted .c file name? *)
let command_c_regexp = Str.regexp " +\\([^ ]+\\.c\\)\\b"
let argument_c_regexp = Str.regexp ".*\\.c$"

let split (obj: command_object): command_object list =
  match obj.command, obj.arguments with
  | Some command, None ->
    let rec map_command start =
      match Str.search_forward command_c_regexp command start with
      | match_begin ->
        let match_end = Str.match_end () in
        let init = Str.string_before command match_begin in
        let tail = Str.string_after command match_end in
        let matched_string = Str.matched_string command in (* must do before global_replace! *)
        let file' = Str.matched_group 1 command in (* must do before global_replace! *)
        let init' = Str.global_replace command_c_regexp "" init in
        let tail' = Str.global_replace command_c_regexp "" tail in
        let command' = init' ^ matched_string ^ tail' in
        {obj with command = Some command'; file = Fpath.v file'} :: map_command match_end
      | exception Not_found ->
        []
    in
    map_command 0
  | None, Some arguments ->
    let is_c s = Str.string_match argument_c_regexp s 0 in
    List.filteri_map (fun i argument ->
        if is_c argument then (
          let (init, tail) = List.split_at i arguments in
          let init' = List.filter (Fun.negate is_c) init in
          let tail' = List.filter (Fun.negate is_c) tail in
          let arguments' = init' @ argument :: tail' in
          Some {obj with arguments = Some arguments'; file = Fpath.v argument}
        )
        else
          None
      ) arguments
  | Some _, Some _ ->
    failwith ("CompilationDatabase.split: both command and arguments specified for " ^ Fpath.to_string obj.file)
  | None, None ->
    failwith ("CompilationDatabase.split: neither command nor arguments specified for " ^ Fpath.to_string obj.file)

let command_o_regexp = Str.regexp "-o +[^ ]+"
let command_program_regexp = Str.regexp "^ *\\([^ ]+\\)"

let load_and_preprocess ~all_cppflags filename =
  let database_dir = Fpath.parent @@ Fpath.normalize @@ GobFpath.cwd_append filename in
  let (reroot_string, reroot_path) =
    let original_path = GobConfig.get_string "pre.compdb.original-path" in
    if original_path <> "" then (
      let original_path = Fpath.normalize @@ GobFpath.cwd_append @@ Fpath.v original_path in
      let original_database_dir = Fpath.parent original_path in
      let old_root = GobFpath.rem_find_prefix database_dir original_database_dir in
      let new_root = GobFpath.rem_find_prefix original_database_dir database_dir in
      if GobConfig.get_bool "dbg.verbose" then
        Logs.Format.debug "Rerooting compilation database\n  from %a\n  to %a\n" Fpath.pp old_root Fpath.pp new_root;
      let reroot_path p =
        Fpath.append new_root (Option.get (Fpath.relativize ~root:old_root p))
      in
      let reroot_string =
        Str.global_replace (Str.regexp_string (Fpath.to_string old_root)) (Fpath.to_string new_root)
      in
      (reroot_string, reroot_path)
    )
    else
      (Fun.id, Fun.id)
  in
  let preprocessed_dir = Fpath.normalize @@ GobFpath.cwd_append @@ GoblintDir.preprocessed () in (* absolute due to cwd changes *)
  let preprocess obj =
    let file = Fpath.normalize @@ GobFpath.cwd_append @@ Fpath.append obj.directory obj.file in
    let extension = Fpath.get_ext file in
    if extension = ".s" || extension = ".S" then
      None
    else
      let preprocessed_file = Fpath.append preprocessed_dir (Fpath.set_ext ".i" (GobFpath.rem_find_prefix database_dir file)) in
      GobSys.mkdir_parents preprocessed_file;
      let preprocess_command = match obj.command, obj.arguments with
        | Some command, None ->
          (* TODO: extract o_file *)
          let command = reroot_string command in
          let preprocess_command = Str.replace_first command_program_regexp ("\\1 " ^ String.join " " (List.map Filename.quote all_cppflags) ^ " -E") command in
          if preprocess_command = command then (* easier way to check if match was found (and replaced) *)
            failwith ("CompilationDatabase.preprocess: no program found for " ^ Fpath.to_string file)
          else
            let preprocess_command_o = Str.replace_first command_o_regexp ("-o " ^ Fpath.to_string preprocessed_file) preprocess_command in
            if preprocess_command_o = preprocess_command then (* easier way to check if match was found (and replaced) *)
              preprocess_command ^ " -o " ^ Fpath.to_string preprocessed_file
            else
              preprocess_command_o
        | None, Some arguments ->
          let arguments_program, arguments =
            match List.map reroot_string arguments with
            | arguments_program :: arguments -> arguments_program, arguments
            | _ -> failwith ("CompilationDatabase.preprocess: no program found for " ^ Fpath.to_string file)
          in
          let preprocess_arguments =
            match List.findi (fun i e -> e = "-o") arguments with
            | (o_i, _) ->
              begin match List.split_at o_i arguments with
                | (arguments_init, _ :: o_file :: arguments_tl) -> arguments_init @ "-o" :: Fpath.to_string preprocessed_file :: arguments_tl
                | _ -> failwith ("CompilationDatabase.preprocess: no argument found for -o option for " ^ Fpath.to_string file)
              end
            | exception Not_found -> arguments @ "-o" :: [Fpath.to_string preprocessed_file]
          in
          let preprocess_arguments = all_cppflags @ "-E" :: preprocess_arguments in
          Filename.quote_command arguments_program preprocess_arguments
        | Some _, Some _ ->
          failwith ("CompilationDatabase.preprocess: both command and arguments specified for " ^ Fpath.to_string file)
        | None, None ->
          failwith ("CompilationDatabase.preprocess: neither command nor arguments specified for " ^ Fpath.to_string file)
      in
      let cwd = reroot_path obj.directory in
      if GobConfig.get_bool "dbg.verbose" then
        Logs.Format.debug "Preprocessing %a\n  to %a\n  using %s\n  in %a\n" Fpath.pp file Fpath.pp preprocessed_file preprocess_command Fpath.pp cwd;
      let preprocess_task = {ProcessPool.command = preprocess_command; cwd = Some cwd} in (* command/arguments might have paths relative to directory *)
      Some (preprocessed_file, Some preprocess_task)
  in
  parse_file filename
  |> (
    if GobConfig.get_bool "pre.compdb.split" then
      List.concat_map split
    else
      Fun.id
  )
  |> BatList.filter_map preprocess
