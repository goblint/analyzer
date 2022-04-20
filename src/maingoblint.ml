(** This is the main program! *)

open Prelude
open GobConfig
open Printf
open Goblintutil

let writeconffile = ref None

(** Print version and bail. *)
let print_version ch =
  printf "Goblint version: %s\n" Version.goblint;
  printf "Cil version:     %s\n" Cil.cilVersion;
  printf "Profile:         %s\n" ConfigProfile.profile;
  exit 0

(** Print helpful messages. *)
let print_help ch =
  fprintf ch "Usage: goblint [options] source-files\nOptions\n";
  fprintf ch "    -v                        Prints more status information.                 \n";
  fprintf ch "    -o <file>                 Prints the output to file.                      \n";
  fprintf ch "    -I <dir>                  Add include directory.                          \n";
  fprintf ch "    -IK <dir>                 Add kernel include directory.                   \n\n";
  fprintf ch "    --help                    Prints this text                                \n";
  fprintf ch "    --version                 Print out current version information.          \n\n";
  fprintf ch "    --conf <file>             Merge the configuration from the <file>.        \n";
  fprintf ch "    --writeconf <file>        Write the effective configuration to <file>     \n";
  fprintf ch "    --set <jpath> <jvalue>    Set a configuration variable <jpath> to the specified <jvalue>.\n";
  fprintf ch "    --sets <jpath> <string>   Set a configuration variable <jpath> to the string.\n";
  fprintf ch "    --enable  <jpath>         Set a configuration variable <jpath> to true.   \n";
  fprintf ch "    --disable <jpath>         Set a configuration variable <jpath> to false.  \n\n";
  fprintf ch "    --print_options           Print out commonly used configuration variables.\n";
  fprintf ch "    --print_all_options       Print out all configuration variables.          \n";
  fprintf ch "\n";
  fprintf ch "A <jvalue> is a string from the JSON language where single-quotes (')";
  fprintf ch " are used instead of double-quotes (\").\n\n";
  fprintf ch "A <jpath> is a path in a json structure. E.g. 'field.another_field[42]';\n";
  fprintf ch "in addition to the normal syntax you can use 'field[+]' append to an array.\n\n";
  exit 0

(** [Arg] option specification *)
let rec option_spec_list: Arg_complete.speclist Lazy.t = lazy (
  let add_string l = let f str = l := str :: !l in Arg_complete.String (f, Arg_complete.empty) in
  let add_int    l = let f str = l := str :: !l in Arg_complete.Int (f, Arg_complete.empty) in
  let set_trace sys =
    if Messages.tracing then Tracing.addsystem sys
    else (prerr_endline "Goblint has been compiled without tracing, recompile in trace profile (./scripts/trace_on.sh)"; raise Exit)
  in
  let oil file =
    set_string "ana.osek.oil" file;
    set_auto "ana.activated" "['base','threadid','threadflag','escape','OSEK','OSEK2','stack_trace_set','fmode','flag','mallocWrapper']";
    set_auto "mainfun" "[]"
  in
  let configure_html () =
    if (get_string "outfile" = "") then
      set_string "outfile" "result";
    if get_string "exp.g2html_path" = "" then
      set_string "exp.g2html_path" (Fpath.to_string exe_dir);
    set_bool "dbg.print_dead_code" true;
    set_bool "exp.cfgdot" true;
    set_bool "g2html" true;
    set_string "result" "fast_xml"
  in
  let configure_sarif () =
    if (get_string "outfile" = "") then
      set_string "outfile" "goblint.sarif";
    set_bool "dbg.print_dead_code" true;
    set_string "result" "sarif"
  in
  let complete_option_value option s =
    let completions = List.assoc option Options.completions in
    Arg_complete.strings completions s
  in
  let defaults_spec_list = List.map (fun path ->
      (* allow "--option value" as shorthand for "--set option value" *)
      ("--" ^ path, Arg_complete.String (set_auto path, complete_option_value path), "")
    ) Options.paths
  in
  let tmp_arg = ref "" in
  let last_complete_option = ref "" in
  let complete_option s =
    last_complete_option := s;
    Arg_complete.strings Options.paths s
  in
  let complete_bool_option s =
    let cs = complete_option s in
    let is_bool c =
      match GobConfig.get_json c with
      | `Bool _ -> true
      | _ -> false
    in
    List.filter is_bool cs
  in
  let complete_last_option_value s =
    complete_option_value !last_complete_option s
  in
  [ "-o"                   , Arg_complete.String (set_string "outfile", Arg_complete.empty), ""
  ; "-v"                   , Arg_complete.Unit (fun () -> set_bool "dbg.verbose" true; set_bool "printstats" true), ""
  ; "-j"                   , Arg_complete.Int (set_int "jobs", Arg_complete.empty), ""
  ; "-I"                   , Arg_complete.String (set_string "pre.includes[+]", Arg_complete.empty), ""
  ; "-IK"                  , Arg_complete.String (set_string "pre.kernel_includes[+]", Arg_complete.empty), ""
  ; "--set"                , Arg_complete.Tuple [Arg_complete.Set_string (tmp_arg, complete_option); Arg_complete.String ((fun x -> set_auto !tmp_arg x), complete_last_option_value)], ""
  ; "--sets"               , Arg_complete.Tuple [Arg_complete.Set_string (tmp_arg, complete_option); Arg_complete.String ((fun x -> prerr_endline "--sets is deprecated, use --set instead."; set_string !tmp_arg x), complete_last_option_value)], ""
  ; "--enable"             , Arg_complete.String ((fun x -> set_bool x true), complete_bool_option), ""
  ; "--disable"            , Arg_complete.String ((fun x -> set_bool x false), complete_bool_option), ""
  ; "--conf"               , Arg_complete.String ((fun fn -> merge_file (Fpath.v fn)), Arg_complete.empty), ""
  ; "--writeconf"          , Arg_complete.String ((fun fn -> writeconffile := Some (Fpath.v fn)), Arg_complete.empty), ""
  ; "--version"            , Arg_complete.Unit print_version, ""
  ; "--print_options"      , Arg_complete.Unit (fun () -> Options.print_options (); exit 0), ""
  ; "--print_all_options"  , Arg_complete.Unit (fun () -> Options.print_all_options (); exit 0), ""
  ; "--trace"              , Arg_complete.String (set_trace, Arg_complete.empty), ""
  ; "--tracevars"          , add_string Tracing.tracevars, ""
  ; "--tracelocs"          , add_int Tracing.tracelocs, ""
  ; "--help"               , Arg_complete.Unit (fun _ -> print_help stdout),""
  ; "--html"               , Arg_complete.Unit (fun _ -> configure_html ()),""
  ; "--sarif"               , Arg_complete.Unit (fun _ -> configure_sarif ()),""
  ; "--compare_runs"       , Arg_complete.Tuple [Arg_complete.Set_string (tmp_arg, Arg_complete.empty); Arg_complete.String ((fun x -> set_auto "compare_runs" (sprintf "['%s','%s']" !tmp_arg x)), Arg_complete.empty)], ""
  ; "--oil"                , Arg_complete.String (oil, Arg_complete.empty), ""
  (*     ; "--tramp"              , Arg_complete.String (set_string "ana.osek.tramp"), ""  *)
  ; "--osekdefaults"       , Arg_complete.Unit (fun () -> set_bool "ana.osek.defaults" false), ""
  ; "--osektaskprefix"     , Arg_complete.String (set_string "ana.osek.taskprefix", Arg_complete.empty), ""
  ; "--osekisrprefix"      , Arg_complete.String (set_string "ana.osek.isrprefix", Arg_complete.empty), ""
  ; "--osektasksuffix"     , Arg_complete.String (set_string "ana.osek.tasksuffix", Arg_complete.empty), ""
  ; "--osekisrsuffix"      , Arg_complete.String (set_string "ana.osek.isrsuffix", Arg_complete.empty), ""
  ; "--osekcheck"          , Arg_complete.Unit (fun () -> set_bool "ana.osek.check" true), ""
  ; "--oseknames"          , Arg_complete.Set_string (OilUtil.osek_renames, Arg_complete.empty), ""
  ; "--osekids"            , Arg_complete.Set_string (OilUtil.osek_ids, Arg_complete.empty), ""
  ; "--complete"           , Arg_complete.Rest_all_compat.spec (Lazy.force rest_all_complete), ""
  ] @ defaults_spec_list (* lowest priority *)
)
and rest_all_complete = lazy (Arg_complete.Rest_all_compat.create complete Arg_complete.empty_all)
and complete args =
  Arg_complete.complete_argv args (Lazy.force option_spec_list) Arg_complete.empty
  |> List.iter print_endline;
  raise Exit

(** Parse arguments. Print help if needed. *)
let parse_arguments () =
  let anon_arg = set_string "files[+]" in
  let arg_speclist = Arg_complete.arg_speclist (Lazy.force option_spec_list) in
  Arg.parse arg_speclist anon_arg "Look up options using 'goblint --help'.";
  Arg_complete.Rest_all_compat.finish (Lazy.force rest_all_complete);
  begin match !writeconffile with
    | Some writeconffile ->
      GobConfig.write_file writeconffile;
      raise Exit
    | None -> ()
  end;
  if get_string_list "files" = [] then (
    prerr_endline "No files for Goblint?";
    prerr_endline "Try `goblint --help' for more information.";
    raise Exit
  )

(** Initialize some globals in other modules. *)
let handle_flags () =
  let has_oil = get_string "ana.osek.oil" <> "" in
  if has_oil then Osek.Spec.parse_oil ();

  if get_bool "dbg.verbose" then (
    Printexc.record_backtrace true;
    Errormsg.debugFlag := true;
    Errormsg.verboseFlag := true
  );

  if get_bool "dbg.debug" then
    set_bool "warn.debug" true;

  match get_string "dbg.dump" with
  | "" -> ()
  | path ->
    Messages.formatter := Format.formatter_of_out_channel (Legacy.open_out (Legacy.Filename.concat path "warnings.out"));
    set_string "outfile" ""

let basic_preprocess_counts = Preprocessor.FpathH.create 3

(** Use gcc to preprocess a file. Returns the path to the preprocessed file. *)
let basic_preprocess ~all_cppflags fname =
  (* The actual filename of the preprocessed sourcefile *)
  let basename = Fpath.rem_ext (Fpath.base fname) in
  (* generate unique preprocessed filename in case multiple basic files have same basename (from different directories), happens in ddverify *)
  let count = Preprocessor.FpathH.find_default basic_preprocess_counts basename 0 in
  let unique_name =
    if count = 0 then
      basename
    else
      Fpath.add_ext (string_of_int count) basename
  in
  Preprocessor.FpathH.replace basic_preprocess_counts basename (count + 1);
  let nname = Fpath.append (GoblintDir.preprocessed ()) (Fpath.add_ext ".i" unique_name) in
  (* Preprocess using cpp. *)
  (* ?? what is __BLOCKS__? is it ok to just undef? this? http://en.wikipedia.org/wiki/Blocks_(C_language_extension) *)
  let arguments = "--undef" :: "__BLOCKS__" :: all_cppflags @ Fpath.to_string fname :: "-o" :: Fpath.to_string nname :: [] in
  let command = Filename.quote_command (Preprocessor.get_cpp ()) arguments in
  if get_bool "dbg.verbose" then print_endline command;
  (nname, Some {ProcessPool.command; cwd = None})

(** Preprocess all files. Return list of preprocessed files and the temp directory name. *)
let preprocess_files () =
  Preprocessor.FpathH.clear basic_preprocess_counts;
  Preprocessor.FpathH.clear Preprocessor.dependencies; (* clear for server mode *)

  (* Preprocessor flags *)
  let cppflags = ref (get_string_list "pre.cppflags") in

  (* the base include directory *)
  let custom_include_dirs =
    List.map Fpath.v (get_string_list "pre.custom_includes") @
    Fpath.(exe_dir / "includes") ::
    List.map Fpath.v Goblint_sites.includes
  in
  if get_bool "dbg.verbose" then (
    print_endline "Custom include dirs:";
    List.iteri (fun i custom_include_dir ->
        Format.printf "  %d. %a (exists=%B)\n" (i + 1) Fpath.pp custom_include_dir (Sys.file_exists (Fpath.to_string custom_include_dir))
      ) custom_include_dirs
  );
  let custom_include_dirs = List.filter (Sys.file_exists % Fpath.to_string) custom_include_dirs in
  if custom_include_dirs = [] then
    print_endline "Warning, cannot find goblint's custom include files.";

  let find_custom_include subpath =
    List.find_map (fun custom_include_dir ->
        let path = Fpath.append custom_include_dir subpath in
        if Sys.file_exists (Fpath.to_string path) then
          Some path
        else
          None
      ) custom_include_dirs
  in

  (* include flags*)
  let include_dirs = ref [] in
  let include_files = ref [] in

  (* fill include flags *)
  let one_include_f f x = include_dirs := f x :: !include_dirs in
  if get_string "ana.osek.oil" <> "" then include_files := Fpath.(GoblintDir.preprocessed () / OilUtil.header) :: !include_files;
  (* if get_string "ana.osek.tramp" <> "" then include_files := get_string "ana.osek.tramp" :: !include_files; *)
  get_string_list "pre.includes" |> List.map Fpath.v |> List.iter (one_include_f identity);

  include_dirs := custom_include_dirs @ !include_dirs;

  (* If we analyze a kernel module, some special includes are needed. *)
  if get_bool "kernel" then (
    let kernel_root = get_string "pre.kernel-root" in
    let kernel_roots =
      begin if kernel_root <> "" then (* cannot parse empty *)
          [Fpath.v kernel_root]
        else
          []
      end @ [
        Fpath.(exe_dir / "linux-headers");
        (* linux-headers not installed with goblint package *)
      ]
    in
    let kernel_root =
      try
        List.find (Sys.file_exists % Fpath.to_string) kernel_roots
      with Not_found ->
        prerr_endline "Root directory for kernel include files not found!";
        raise Exit
    in

    let kernel_dir = Fpath.(kernel_root / "include") in
    let arch_dir = Fpath.(kernel_root / "arch" / "x86" / "include") in (* TODO add arm64: https://github.com/goblint/analyzer/issues/312 *)

    get_string_list "pre.kernel_includes" |> List.map Fpath.v |> List.iter (Fpath.append kernel_root |> one_include_f);

    let preconf = find_custom_include Fpath.(v "linux" / "goblint_preconf.h") in
    let autoconf = Fpath.(kernel_dir / "linux" / "kconfig.h") in
    cppflags := "-D__KERNEL__" :: "-U__i386__" :: "-D__x86_64__" :: !cppflags;
    include_files := preconf :: autoconf :: !include_files;
    (* These are not just random permutations of directories, but based on USERINCLUDE from the
     * Linux kernel Makefile (in the root directory of the kernel distribution). *)
    include_dirs := !include_dirs @ [
        kernel_dir; Fpath.(kernel_dir / "uapi"); Fpath.(kernel_dir / "include" / "generated" / "uapi"); (* TODO: duplicate include with kernel_dir is bug? *)
        arch_dir; Fpath.(arch_dir / "generated"); Fpath.(arch_dir / "uapi"); Fpath.(arch_dir / "generated" / "uapi");
      ]
  );

  let include_args =
    List.concat_map (fun include_dir -> ["-I"; Fpath.to_string include_dir]) !include_dirs @
    List.concat_map (fun include_file -> ["-include"; Fpath.to_string include_file]) !include_files
  in

  let all_cppflags = !cppflags @ include_args in

  (* preprocess all the files *)
  if get_bool "dbg.verbose" then print_endline "Preprocessing files.";

  let rec preprocess_arg_file = function
    | filename when Fpath.filename filename = "Makefile" ->
      let comb_file = MakefileUtil.generate_and_combine filename ~all_cppflags in
      [basic_preprocess ~all_cppflags comb_file]

    | filename when Fpath.filename filename = CompilationDatabase.basename ->
      CompilationDatabase.load_and_preprocess ~all_cppflags filename

    | filename when Sys.is_directory (Fpath.to_string filename) ->
      let dir_files = Sys.readdir (Fpath.to_string filename) in
      if Array.mem CompilationDatabase.basename dir_files then (* prefer compilation database to Makefile in case both exist, because compilation database is more robust *)
        preprocess_arg_file (Fpath.add_seg filename CompilationDatabase.basename)
      else if Array.mem "Makefile" dir_files then
        preprocess_arg_file (Fpath.add_seg filename "Makefile")
      else
        [] (* don't recurse for anything else *)

    | filename when Fpath.get_ext filename = ".json" ->
      Format.eprintf "Unexpected JSON file argument (possibly missing --conf): %a\n" Fpath.pp filename;
      raise Exit

    | filename ->
      [basic_preprocess ~all_cppflags filename]
  in

  let extra_files = ref [] in

  extra_files := find_custom_include (Fpath.v "stdlib.c") :: find_custom_include (Fpath.v "pthread.c") :: !extra_files;

  if get_bool "ana.sv-comp.functions" then
    extra_files := find_custom_include (Fpath.v "sv-comp.c") :: !extra_files;

  let preprocessed = List.concat_map preprocess_arg_file (!extra_files @ List.map Fpath.v (get_string_list "files")) in
  if not (get_bool "pre.exist") then (
    let preprocess_tasks = List.filter_map snd preprocessed in
    let terminated task = function
      | Unix.WEXITED 0 -> ()
      | process_status -> failwith (GobUnix.string_of_process_status process_status)
    in
    ProcessPool.run ~jobs:(Goblintutil.jobs ()) ~terminated preprocess_tasks
  );
  preprocessed

(** Possibly merge all postprocessed files *)
let merge_preprocessed preprocessed =
  (* get the AST *)
  if get_bool "dbg.verbose" then print_endline "Parsing files.";

  let goblint_cwd = GobFpath.cwd () in
  let get_ast_and_record_deps (preprocessed_file, task_opt) =
    let transform_file (path_str, system_header) = match path_str with
      | "<built-in>" | "<command-line>" ->
        (path_str, system_header) (* ignore special "paths" *)
      | _ ->
        let path = Fpath.v path_str in
        let dir = (Option.get task_opt).ProcessPool.cwd |? goblint_cwd in (* relative to compilation database directory or goblint's cwd *)
        let path' = Fpath.normalize @@ Fpath.append dir path in
        let path' = Fpath.rem_prefix goblint_cwd path' |? path' in (* remove goblint cwd prefix (if has one) for readability *)
        Preprocessor.FpathH.modify_def Fpath.Map.empty preprocessed_file (Fpath.Map.add path' system_header) Preprocessor.dependencies; (* record dependency *)
        (Fpath.to_string path', system_header)
    in
    let transformLocation ~file ~line =
      let file' = Option.map transform_file file in
      Some (file', line)
    in
    Errormsg.transformLocation := transformLocation;

    Cilfacade.getAST preprocessed_file
  in
  let files_AST = List.map (get_ast_and_record_deps) preprocessed in

  let cilout =
    if get_string "dbg.cilout" = "" then Legacy.stderr else Legacy.open_out (get_string "dbg.cilout")
  in

  Errormsg.logChannel := Messages.get_out "cil" cilout;

  (* we use CIL to merge all inputs to ONE file *)
  let merged_AST =
    match files_AST with
    | [one] -> Cilfacade.callConstructors one
    | [] ->
      prerr_endline "No files to analyze!";
      raise Exit
    | xs -> Cilfacade.getMergedAST xs |> Cilfacade.callConstructors
  in

  Cilfacade.rmTemps merged_AST;

  (* create the Control Flow Graph from CIL's AST *)
  Cilfacade.createCFG merged_AST;
  Cilfacade.current_file := merged_AST;
  merged_AST

let preprocess_and_merge () = preprocess_files () |> merge_preprocessed

let do_stats () =
  if get_bool "printstats" then (
    print_newline ();
    ignore (Pretty.printf "vars = %d    evals = %d    narrow_reuses = %d    aborts = %d\n" !Goblintutil.vars !Goblintutil.evals !Goblintutil.narrow_reuses !Goblintutil.aborts);
    print_newline ();
    Stats.print (Messages.get_out "timing" Legacy.stderr) "Timings:\n";
    flush_all ()
  )

(** Perform the analysis over the merged AST.  *)
let do_analyze change_info merged_AST =
  (* direct the output to file if requested  *)
  if not (get_bool "g2html" || get_string "outfile" = "") then (
    if !Goblintutil.out <> Legacy.stdout then
      Legacy.close_out !Goblintutil.out;
    Goblintutil.out := Legacy.open_out (get_string "outfile"));

  let module L = Printable.Liszt (CilType.Fundec) in
  if get_bool "justcil" then
    (* if we only want to print the output created by CIL: *)
    Cilfacade.print merged_AST
  else (
    (* we first find the functions to analyze: *)
    if get_bool "dbg.verbose" then print_endline "And now...  the Goblin!";
    let (stf,exf,otf as funs) = Cilfacade.getFuns merged_AST in
    if stf@exf@otf = [] then failwith "No suitable function to start from.";
    if get_bool "dbg.verbose" then ignore (Pretty.printf "Startfuns: %a\nExitfuns: %a\nOtherfuns: %a\n"
                                             L.pretty stf L.pretty exf L.pretty otf);
    (* and here we run the analysis! *)

    let do_all_phases ast funs =
      let do_one_phase ast p =
        phase := p;
        if get_bool "dbg.verbose" then (
          let aa = String.concat ", " @@ get_string_list "ana.activated" in
          let at = String.concat ", " @@ get_string_list "trans.activated" in
          print_endline @@ "Activated analyses for phase " ^ string_of_int p ^ ": " ^ aa;
          print_endline @@ "Activated transformations for phase " ^ string_of_int p ^ ": " ^ at
        );
        try Control.analyze change_info ast funs
        with e ->
          let backtrace = Printexc.get_raw_backtrace () in (* capture backtrace immediately, otherwise the following loses it (internal exception usage without raise_notrace?) *)
          Messages.error "About to crash!"; (* TODO: move severity coloring to Messages *)
          (* trigger Generic.SolverStats...print_stats *)
          Goblintutil.(self_signal (signal_of_string (get_string "dbg.solver-signal")));
          do_stats ();
          print_newline ();
          Printexc.raise_with_backtrace e backtrace (* re-raise with captured inner backtrace *)
          (* Cilfacade.current_file := ast'; *)
      in
      (* new style is phases[i].ana.activated = [ana_1, ...]
         phases[i].ana.x overwrites setting ana.x *)
      let num_phases =
        let np,na,nt = Tuple3.mapn (List.length % get_list) ("phases", "ana.activated", "trans.activated") in
        (* TODO what about wrong usage like { phases = [...], ana.activated = [...] }? should child-lists add to parent-lists? *)
        if get_bool "dbg.verbose" then print_endline @@ "Using new format for phases!";
        if np = 0 && na = 0 && nt = 0 then failwith "No phases and no activated analyses or transformations!";
        max np 1
      in
      ignore @@ Enum.iter (do_one_phase ast) (0 -- (num_phases - 1))
    in

    (* Analyze with the new experimental framework. *)
    Stats.time "analysis" (do_all_phases merged_AST) funs
  )

let do_html_output () =
  (* TODO: Fpath *)
  let jar = Filename.concat (get_string "exp.g2html_path") "g2html.jar" in
  if get_bool "g2html" then (
    if Sys.file_exists jar then (
      let command = "java -jar "^ jar ^" --result-dir "^ (get_string "outfile")^" "^ !Messages.xml_file_name in
      try match Unix.system command with
        | Unix.WEXITED 0 -> ()
        | _ -> eprintf "HTML generation failed! Command: %s\n" command
      with Unix.Unix_error (e, f, a) ->
        eprintf "%s at syscall %s with argument \"%s\".\n" (Unix.error_message e) f a
    ) else
      eprintf "Warning: jar file %s not found.\n" jar
  )

let do_gobview () =
  (* TODO: Fpath *)
  let create_symlink target link =
    if not (Sys.file_exists link) then Unix.symlink target link
  in
  let gobview = GobConfig.get_bool "gobview" in
  let goblint_root =
    Filename.concat (Unix.getcwd ()) (Filename.dirname Sys.argv.(0))
  in
  let dist_dir = Filename.concat goblint_root "_build/default/gobview/dist" in
  let js_file = Filename.concat dist_dir "main.js" in
  if gobview then (
    if Sys.file_exists js_file then (
      let save_run = GobConfig.get_string "save_run" in
      let run_dir = if save_run <> "" then save_run else "run" in
      let dist_files =
        Sys.files_of dist_dir
        |> Enum.filter (fun n -> n <> "dune")
        |> List.of_enum
      in
      List.iter (fun n ->
          create_symlink
            (Filename.concat dist_dir n)
            (Filename.concat run_dir n)
        ) dist_files
    )
    else
      eprintf "Warning: Cannot locate Gobview.\n"
  )

let eprint_color m = eprintf "%s\n" (MessageUtil.colorize ~fd:Unix.stderr m)

let check_arguments () =
  (* let fail m = let m = "Option failure: " ^ m in eprint_color ("{red}"^m); failwith m in *) (* unused now, but might be useful for future checks here *)
  let warn m = eprint_color ("{yellow}Option warning: "^m) in
  if get_bool "allfuns" && not (get_bool "exp.earlyglobs") then (set_bool "exp.earlyglobs" true; warn "allfuns enables exp.earlyglobs.\n");
  if not @@ List.mem "escape" @@ get_string_list "ana.activated" then warn "Without thread escape analysis, every local variable whose address is taken is considered escaped, i.e., global!";
  if get_string "ana.osek.oil" <> "" && not (get_string "ana.base.privatization" = "protection-vesal" || get_string "ana.base.privatization" = "protection-old") then (set_string "ana.base.privatization" "protection-vesal"; warn "oil requires protection-old/protection-vesal privatization, setting ana.base.privatization to protection-vesal");
  if get_bool "ana.base.context.int" && not (get_bool "ana.base.context.non-ptr") then (set_bool "ana.base.context.int" false; warn "ana.base.context.int implicitly disabled by ana.base.context.non-ptr");
  (* order matters: non-ptr=false, int=true -> int=false cascades to interval=false with warning *)
  if get_bool "ana.base.context.interval" && not (get_bool "ana.base.context.int") then (set_bool "ana.base.context.interval" false; warn "ana.base.context.interval implicitly disabled by ana.base.context.int");
  if get_bool "incremental.only-rename" then (set_bool "incremental.load" true; warn "incremental.only-rename implicitly activates incremental.load. Previous AST is loaded for diff and rename, but analyis results are not reused.");
  if get_bool "incremental.restart.sided.enabled" && get_string_list "incremental.restart.list" <> [] then warn "Passing a non-empty list to incremental.restart.list (manual restarting) while incremental.restart.sided.enabled (automatic restarting) is activated."

let handle_extraspecials () =
  let funs = get_string_list "exp.extraspecials" in
  LibraryFunctions.add_lib_funs funs

(* Detects changes and renames vids and sids. *)
let diff_and_rename current_file =
  (* Create change info, either from old results, or from scratch if there are no previous results. *)
  let change_info: Analyses.increment_data =
    let warn m = eprint_color ("{yellow}Warning: "^m) in
    if GobConfig.get_bool "incremental.load" && not (Serialize.results_exist ()) then begin
      warn "incremental.load is activated but no data exists that can be loaded."
    end;
    let (changes, restarting, old_file, max_ids) =
      if Serialize.results_exist () && GobConfig.get_bool "incremental.load" then begin
        let old_file = Serialize.load_data Serialize.CilFile in
        let changes = CompareCIL.compareCilFiles old_file current_file in
        let max_ids = MaxIdUtil.load_max_ids () in
        let max_ids = UpdateCil.update_ids old_file max_ids current_file changes in

        let restarting = GobConfig.get_string_list "incremental.restart.list" in
        let restarting, not_found = VarQuery.varqueries_from_names current_file restarting in
        if not (List.is_empty not_found) then begin
          List.iter
            (fun s ->
              warn @@ "Should restart " ^ s ^ " but no such global could not be found in the CIL-file.")
            not_found;
          flush stderr
        end;
        (changes, restarting, Some old_file, max_ids)
      end else begin
        let max_ids = MaxIdUtil.get_file_max_ids current_file in
        (CompareCIL.empty_change_info (), [], None, max_ids)
      end
    in
    let solver_data = if Serialize.results_exist () && GobConfig.get_bool "incremental.load" && not (GobConfig.get_bool "incremental.only-rename")
      then Some (Serialize.load_data Serialize.SolverData)
      else None
    in
    if GobConfig.get_bool "incremental.save" then begin
      Serialize.store_data current_file Serialize.CilFile;
      Serialize.store_data max_ids Serialize.VersionData
    end;
    let old_data = match old_file, solver_data with
      | Some cil_file, Some solver_data -> Some ({cil_file; solver_data}: Analyses.analyzed_data)
      | _, _ -> None
    in
    {Analyses.changes = changes; restarting; old_data; new_file = current_file}
  in change_info

let () = (* signal for printing backtrace; other signals in Generic.SolverStats and Timeout *)
  let open Sys in
  (* whether interactive interrupt (ctrl-C) terminates the program or raises the Break exception which we use below to print a backtrace. https://ocaml.org/api/Sys.html#VALcatch_break *)
  catch_break true;
  set_signal (Goblintutil.signal_of_string (get_string "dbg.backtrace-signal")) (Signal_handle (fun _ -> Printexc.get_callstack 999 |> Printexc.print_raw_backtrace Stdlib.stderr; print_endline "\n...\n")) (* e.g. `pkill -SIGUSR2 goblint`, or `kill`, `htop` *)
