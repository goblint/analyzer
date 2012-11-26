(** This is the main program! *)

open Batteries_uni
open GobConfig
open Defaults
open Printf
open Json

(** Use this instead of [exit n]. *)
exception BailFromMain

(** Print version and bail. *)
let print_version ch = 
  let open Version in let open Config in
  let f ch b = if b then fprintf ch "enabled" else fprintf ch "disabled" in
  printf "Goblint version: %s\n" goblint;
  printf "Cil version:     %s (%s)\n" Cil.cilVersion cil;
  printf "Configuration:   tracing %a, tracking %a (n=%d)\n" f tracing f tracking track_n ;
  raise BailFromMain

(* Print helpful messages. *)
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
  fprintf ch "    --sets <jpath> <string>   Set a configuration variable <jpath> to the string.\n\n"; 
  fprintf ch "    --print_options           Print out commonly used configuration variables.\n";    
  fprintf ch "    --print_all_options       Print out all configuration variables.          \n";
  fprintf ch "\n";
  fprintf ch "A <jvalue> is a string from the JSON language where single-quotes (')";
  fprintf ch " are used instead of double-quotes (\").\n\n";
  fprintf ch "A <jpath> is a path in a json structure. E.g. 'field.another_field[42]';\n";
  fprintf ch "in addition to the normal syntax you can use 'field[+]' append to an array.\n\n"  
  
(** [Arg] option specification *)
let option_spec_list = 
  let set_trace sys = 
    let msg = "Goblin has been compiled without tracing, run ./scripts/trace_on.sh to recompile." in
    if Config.tracing then Tracing.addsystem sys
    else (prerr_endline msg; raise BailFromMain)
  in
  let tmp_arg = ref "" in
    [ "-o"                   , Arg.String (set_string "outfile"), ""
    ; "-v"                   , Arg.Unit (fun () -> set_bool "dbg.verbose" true), ""
    ; "-I"                   , Arg.String (set_string "includes[+]"), ""
    ; "-IK"                  , Arg.String (set_string "kernel_includes[+]"), ""
    ; "--set"                , Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (fun x -> set_auto !tmp_arg x)], ""
    ; "--sets"               , Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (fun x -> set_string !tmp_arg x)], ""
    ; "--conf"               , Arg.String merge_file, ""
    ; "--writeconf"          , Arg.String (fun fn -> File.with_file_out fn print; raise BailFromMain), ""
    ; "--version"            , Arg.Unit print_version, ""
    ; "--print_options"      , Arg.Unit (fun _ -> printCategory stdout Std; raise BailFromMain), ""
    ; "--print_all_options"  , Arg.Unit (fun _ -> printAllCategories stdout; raise BailFromMain), ""
    ; "--trace"              , Arg.String set_trace, ""
    ; "--help"               , Arg.Unit (fun _ -> print_help stdout),""
    ; "--halp"               , Arg.Unit (fun _ -> print_help stdout),""
    ; "-help"                , Arg.Unit (fun _ -> print_help stdout),""
    ; "--oil"                , Arg.String (set_string "ana.osek.oil"), ""
    ; "--tramp"              , Arg.String (set_string "ana.osek.tramp"), ""
    ; "--osektaskprefix"     , Arg.String (set_string "ana.osek.taskprefix"), ""
    ; "--osekisrprefix"      , Arg.String (set_string "ana.osek.isrprefix"), ""
    ; "--osektasksuffix"     , Arg.String (set_string "ana.osek.tasksuffix"), ""
    ; "--osekisrsuffix"      , Arg.String (set_string "ana.osek.isrsuffix"), ""
    ; "--osekcheck"          , Arg.Unit (fun () -> set_bool "ana.osek.check" true), ""
    ; "--oseknames"          , Arg.Set_string OilUtil.osek_renames, ""
    ]

(** List of C files to consider. *)
let cFileNames = ref []

(** Parse arguments and fill [cFileNames]. Print help if needed. *)
let parse_arguments () =
  let jsonRegex = Str.regexp ".+\\.json$" in
  let recordFile fname = 
    if Str.string_match jsonRegex fname 0
    then Goblintutil.jsonFiles := fname :: !Goblintutil.jsonFiles 
    else cFileNames := fname :: !cFileNames
  in
  Arg.parse option_spec_list recordFile "Look up options using 'goblint --help'."
  
(** Initialize some globals in other modules. *)
let handle_flags () =
  if get_bool "allfuns" || get_bool "nonstatic" 
      || get_string "ana.osek.oil" <> "''" then Goblintutil.multi_threaded := true;
  
  if get_bool "dbg.debug" then Messages.warnings := true;

  if get_bool "dbg.verbose" then begin
    Printexc.record_backtrace true;
    Errormsg.debugFlag := true;
    Errormsg.verboseFlag := true
  end;

  match get_string "dbg.dump" with
    | "" -> ()
    | path -> begin
        Messages.warn_out := Legacy.open_out (Legacy.Filename.concat path "warnings.out");
        set_string "outfile" ""
      end
  
(** Use gcc to preprocess a file. Returns the path to the preprocessed file. *)
let preprocess_one_file cppflags includes dirName fname =
  (* The actual filename of the preprocessed sourcefile *)
  let nname =  Filename.concat dirName (Filename.basename fname) in 
  
  (* Preprocess using gcc -E *)
  let command = "gcc --undef __BLOCKS__ -E " ^ cppflags ^ " " ^ includes ^ " " ^ fname ^ " -o " ^ nname in
  if get_bool "dbg.verbose" then print_endline command;
  
  (* if something goes wrong, we need to clean up and exit *)
  let rm_and_exit () =
    if not (get_bool "keepcpp") then ignore (Goblintutil.rm_rf dirName); raise BailFromMain
  in    
    try match Unix.system command with
          | Unix.WEXITED 0 -> nname
          | _ -> eprintf "Goblint: Preprocessing failed."; rm_and_exit ()
    with Unix.Unix_error (e, f, a) -> 
      eprintf "%s at syscall %s with argument \"%s\".\n" (Unix.error_message e) f a; rm_and_exit ()

(** Preprocess all files. Return list of preprocessed files and the temp directory name. *)
let preprocess_files () =
  (* Handy (almost) constants. *)
  let myname = Filename.dirname Sys.executable_name in
  let kernel_root = Filename.concat myname "../bench/linux-headers" in
  let kernel_dir = kernel_root ^ "/include" in
  let asm_dir = kernel_root ^ "/arch/x86/include" in

  (* Preprocessor flags *)
  let cppflags = ref (get_string "cppflags") in
  
  (* the base include directory *)
  let include_dir = 
    let incl1 = Filename.concat myname "includes" in 
    let incl2 = "/usr/share/goblint/includes" in
      if get_string "custom_incl" <> "" then (get_string "custom_incl") 
      else if Sys.file_exists incl1 then incl1 
      else if Sys.file_exists incl2 then incl2 
      else "/usr/local/share/goblint/includes" 
  in
  
  (* include flags*)
  let includes = ref "" in
  
  (* fill include flags *)
  let one_include_f f x = includes := "-I " ^ f (string x) ^ " " ^ !includes in
  if get_string "ana.osek.tramp" <> "" then includes := "-include " ^ get_string "ana.osek.tramp" ^" "^ !includes;
  get_list "includes" |> List.iter (one_include_f identity);
  get_list "kernel_includes" |> List.iter (Filename.concat kernel_root |> one_include_f);
  
  if Sys.file_exists include_dir 
  then includes := "-I" ^ include_dir ^ " " ^ !includes
  else print_endline "Warning, cannot find goblint's custom include files.";

  (* reverse the files again *)
  cFileNames := List.rev !cFileNames;
  
  (* possibly add our lib.c to the files *)
  if get_bool "custom_libc" then 
      cFileNames := (Filename.concat include_dir "lib.c") :: !cFileNames;
  
  (* If we analyze a kernel module, some special includes are needed. *)
  if get_bool "kernel" then begin
    let preconf = Filename.concat include_dir "linux/goblint_preconf.h" in 
    let autoconf = Filename.concat kernel_dir "generated/autoconf.h" in 
    cppflags := "-D__KERNEL__ -U__i386__ -include " ^ preconf ^ " -include " ^ autoconf ^ " " ^ !cppflags;
    includes := !includes ^ " -I" ^ kernel_dir ^ " -I" ^ asm_dir ^ " -I" ^ asm_dir ^ "/asm/mach-default"
  end;
  
  (* The temp directory for preprocessing the input files *)
  let dirName = Goblintutil.create_dir "goblint_temp" in
  
  (* preprocess all the files *)
  if get_bool "dbg.verbose" then print_endline "Preprocessing files.";
    List.rev_map (preprocess_one_file !cppflags !includes dirName) !cFileNames, dirName 


(** Possibly merge all postprocessed files *)
let merge_preprocessed (cpp_file_names, dirName) =
  (* get the AST *)
  if get_bool "dbg.verbose" then print_endline "Parsing files.";
  let files_AST = List.rev_map Cilfacade.getAST cpp_file_names in

  (* remove the files *)
  if not (get_bool "keepcpp") then ignore (Goblintutil.rm_rf dirName);
  
  let cilout = 
    if get_string "dbg.cilout" = "" then Legacy.stderr else Legacy.open_out (get_string "dbg.cilout")
  in
  
  (* direct the output to file if requested  *)
  if not (get_string "outfile" = "") then Goblintutil.out := Legacy.open_out (get_string "outfile");
  Errormsg.logChannel := Messages.get_out "cil" cilout;
  
  (* we use CIL to merge all inputs to ONE file *)
  let merged_AST = 
    match files_AST with
      | [one] -> Cilfacade.callConstructors one
      | [] -> prerr_endline "No arguments for Goblint?"; 
              prerr_endline "Try `goblint --help' for more information."; 
              raise BailFromMain
      | xs -> Cilfacade.getMergedAST xs |> Cilfacade.callConstructors  
  in
  
  (* using CIL's partial evaluation and constant folding! *)
  if get_bool "dopartial" then Cilfacade.partial merged_AST;
  Cilfacade.rmTemps merged_AST;
  
  (* creat the Control Flow Graph from CIL's AST *)
  Cilfacade.createCFG merged_AST;
  Cilfacade.ugglyImperativeHack := merged_AST;
  merged_AST

(** Perform the analysis over the merged AST.  *)
let do_analyze merged_AST =
  let module L = Printable.Liszt (Basetype.CilFundec) in  
  (* we let the "--eclipse" flag override result style: *)
  if get_bool "exp.eclipse" then set_string "result_style" "compact";

  if get_bool "justcil" then 
    (* if we only want to print the output created by CIL: *)
    Cilfacade.print merged_AST
  else begin
    (* we first find the functions to analyze: *)
    if get_string "ana.osek.oil" <> "" then Osek.Spec.parse_oil ();
    if get_bool "dbg.verbose" then print_endline "And now...  the Goblin!";
    let (stf,exf,otf as funs) = Cilfacade.getFuns merged_AST in
      if stf@exf@otf = [] then failwith "No suitable function to start from.";
      if get_bool "dbg.verbose" then ignore (Pretty.printf "Startfuns: %a\nExitfuns: %a\nOtherfuns: %a\n"
                                                 L.pretty stf L.pretty exf L.pretty otf);
      (* and here we run the analysis! *)
      if get_string "result" = "html" then Report.prepare_html_report ();
      
      (* Analyze with the new experimental framework or with the usual framework *)
      if get_bool "exp.new_fwk" 
      then Stats.time "analysis" (Constraints.analyze merged_AST) funs
      else Stats.time "analysis" (MCP.Analysis.analyze merged_AST) funs
  end
  
(* the main function *)
let main () = 
  try
    Stats.reset Stats.SoftwareTimer;  
    Cilfacade.init ();
    parse_arguments ();
    handle_flags ();
    preprocess_files () |> merge_preprocessed |> do_analyze;
    Report.do_stats !cFileNames
  with BailFromMain -> () 
  
  
  (*
let main () =
  let usage_str = "Usage: goblint [options] source-files" in
  let fileNames : string list ref = ref [] in
  (* default settings for the command line arguments: *)
  let myname = Filename.dirname Sys.executable_name in
  let include_dir = 
    let incl = Filename.concat myname "includes" in 
    if Sys.file_exists incl then ref incl else 
      let incl = "/usr/share/goblint/includes" in
      if Sys.file_exists incl then ref incl else 
        ref "/usr/local/share/goblint/includes" in
(*  let kernel_root = "/lib/modules/`uname -r`/build" in*)
  let kernel_root = Filename.concat myname "../bench/linux-headers" in
  let kernel_dir = kernel_root ^ "/include" in
  let asm_dir = kernel_root ^ "/arch/x86/include" in
  let other_includes = ref "" in
  let add_include x = other_includes := "-I " ^ x ^ " " ^ !other_includes in
  let add_include_file x = other_includes := "-include " ^ x ^ " " ^ !other_includes in
  let add_include_kernel x = other_includes := "-I " ^ Filename.concat kernel_root x ^ " " ^ !other_includes in
  let add_string l = let f str = l := str :: !l in Arg.String f in
  let add_int    l = let f str = l := str :: !l in Arg.Int f in
  let use_libc = ref false in
  let justCil = ref false in
  let dopartial = ref false in
  let keep_cpp = ref false in
  let cppflags = ref "" in
  let outFile = ref "" in 
  let cilout = ref stderr in
  (* Function for setting the style: *)
  let setstyle = function
      | "none" -> GU.result_style := GU.NoOutput
      | "indented" -> GU.result_style := GU.Indented
      | "compact" -> GU.result_style := GU.Compact
      | "pretty" -> GU.result_style := GU.Pretty
      | "html" -> GU.result_style := GU.Html
      | "newhtml" -> GU.result_style := GU.NewHtml
      | "glob" -> GU.dump_global_inv := true
      | _ -> raise (Arg.Bad "invalid result style") 
  in
  let phase x = 
    let rec appendTimes x y = function 
      | n when n < 0 -> x
      | 0 -> x
      | n -> appendTimes (x@y) y (n-1)
    in
    let n = int_of_string x in
    let cfs = Json.array !(Json.field GU.conf "analyses") in
    GU.phase := n;
    cfs := appendTimes !cfs [ref (Json.Build.array [])] (n-(List.length !cfs)+1)       
  in
  let setdump path = GU.dump_path := Some (GU.create_dir path) in
  let setcil path = cilout := open_out path in
  let analyzer str = (*legacy: use .json, --with and --no instead *)
      match str with
	| "containment" -> Contain.Analysis.analyze
 	| _ -> MCP.Analysis.analyze   
  in
  let analyze = ref (analyzer (JB.string !(JB.field GU.conf "analysis"))) in
  let oil file = (*GU.allfuns := true;*) GU.oil := true; GU.mainfuns := []; GU.conf_osek (); OilUtil.oilFile := file in
  let tramp file = OilUtil.resourceheaders := file; add_include_file file in
  let osek_names file = OilUtil.osek_renames := file in
  let osekisrprefix prefix = GU.isrprefix := prefix in
  let osektaskprefix prefix = GU.taskprefix := prefix in
  let osekisrsuffix suffix = GU.isrsuffix := suffix in
  let osektasksuffix suffix = GU.tasksuffix := suffix in
  let setanalysis str = 
    begin match str with
            | "containment" -> GU.conf_containment ()
            | "uninit" -> GU.conf_uninit ()
            | "malloc_null" -> GU.conf_malloc ()
            | "osek" -> GU.conf_osek ()
            | "base" -> GU.conf_base ()
            | "mutex" -> ()
            | _ -> raise (Arg.Bad ("Don't know what to do for '--analysis "^str^"'"))
    end;
    analyze := analyzer str 
  in
  let tmp_arg = ref "" in
  let set_prop b x = GU.modify_prop !tmp_arg x b in
  let set_feature b x =
    match x with
      | x when List.exists (fun y -> y.MCP.featurename = x) !MCP.analysesList
          -> GU.modify_ana x b
      | _ -> raise (Arg.Bad ("no such feature: "^x))
  in
  let set_context b x =
    match x with
      | x when List.exists (fun y -> y.MCP.featurename = x) !MCP.analysesList
          -> GU.modify_context x b
      | _ -> raise (Arg.Bad ("no such feature: "^x))
  in
  let setsolver str = 
    GU.solver := str
  in
  let set_trace sys = 
    if M.tracing then M.addsystem sys
    else (prerr_endline "Goblin has been compiled without tracing, run ./scripts/trace_on.sh to recompile."; exit 2)
  in
  let featurelist = List.map (fun x -> x.MCP.featurename) !MCP.analysesList in
  let speclist = 
    let tmp_arg = ref "" in
      [ "-o"                   , Arg.String (set_string "outfile"), ""
      ; "-v"                   , Arg.Unit (fun () -> set_bool "dbg.verbose" true), ""
      ; "-I"                   , Arg.String (set_string "includes[+]"), ""
      ; "-IK"                  , Arg.String (set_string "kernel_includes[+]"), ""
      ; "--set"                , Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (set_auto !tmp_arg)], ""
      ; "--conf"               , Arg.String merge_file, ""
      ; "--writeconf"          , Arg.String (fun fn -> BatFile.with_file_out fn GobConfig.print), ""
      ; "--version"            , Arg.Unit print_version, ""
      ; "--print_options"      , Arg.Unit (fun _ -> printCategory BatPervasives.stdout Std), ""
      ; "--print_all_options"  , Arg.Unit (fun _ -> printAllCategories BatPervasives.stdout), ""
      ]
  in
  (*let speclist = [
                 ("-o", Arg.Set_string outFile, "<file>  Prints the output to file.");
                 ("-v", Arg.Set GU.verbose, " Prints some status information.");
                 ("-I", Arg.String add_include,  " Add include directory.");
                 ("-IK", Arg.String add_include_kernel,  " Add kernel include directory.");
                 ("--version", Arg.Unit print_version, "Prints version information.");
                 ("--filter", Arg.Set_string GU.result_filter, "regexp filtering output file.");
                 ("--includes", Arg.Set_string include_dir, " Uses custom include files.");
                 ("--libc", Arg.Set use_libc, " Merge with a custom implementation of standard libs.");
                 ("--justcil", Arg.Set justCil, " Just print the transformed CIL output.");
                 ("--dopartial", Arg.Set dopartial, " Apply CIL's constant folding and partial evaluation.");
                 ("--cfg", Arg.Set GU.cfg_print, " prints the cfg into cfg.dot.");
                 ("--debug", Arg.Set GU.debug, " Debug mode: for testing the analyzer itself.");
                 ("--warnings", Arg.Set M.warnings, " Print soundness warnings.");
                 ("--trace", Arg.String set_trace, "<sys>  subsystem to show debug printfs for, such as con, sol.");
                 ("--tracevars", add_string M.tracevars, "<id> identifier name of interest for tracing.");
                 ("--tracelocs", add_int M.tracelocs, "<id> line number of interest for tracing.");
                 ("--stats", Arg.Set Cilutil.printStats, " Outputs timing information.");
                 ("--eclipse", Arg.Set GU.eclipse, " Flag for Goblin's Eclipse Plugin.");
                 ("--gccwarn", Arg.Set GU.gccwarn, " Output warnings in GCC format.");
                 ("--localclass", Arg.Set GU.local_class, " Analyzes classes defined in main Class.");
                 ("--allfuns", Arg.Set GU.allfuns, " Analyzes all the functions (not just beginning from main).");
                 ("--noverify", Arg.Clear GU.verify, " Skip the verification phase.");
                 ("--class", Arg.Set_string GU.mainclass, " Analyzes all the member functions of the class (CXX.json file required).");
                 ("--nonstatic", Arg.Set GU.nonstatic, " Analyzes all non-static functions.");
                 ("--mainfun", add_string GU.mainfuns, " Sets the name of the main functions.");
                 ("--exitfun", add_string GU.exitfuns, " Sets the name of the cleanup functions.");
                 ("--otherfun", add_string GU.otherfuns, " Sets the name of other functions.");
                 ("--allglobs", Arg.Set GU.allglobs, " Prints access information about all globals, not just races.");
                 ("--check", add_string Mutex.vips, "<variable/type name>  Check whether there is a race involving this variable.");
                 ("--earlyglobs", Arg.Set GU.earlyglobs, " Side-effecting of globals right after initialization.");
                 ("--write-races", Arg.Set Mutex.no_read, " Ignores read accesses altogether in reporting races.");
                 ("--failing-locks", Arg.Set LibraryFunctions.failing_locks, " Takes the possible failing of locking operations into account.");
                 ("--field-insensitive", Arg.Set Mutex.field_insensitive, " Turns off field-sensitivity.");
                 ("--region-offsets", Arg.Set GU.region_offsets, " Considers offsets for region accesses.");
                 ("--unmerged-fields", Arg.Set Mutex.unmerged_fields, " Does not merge accesses to possibly same fields, unsound.");
                 ("--die-on-collapse", Arg.Set GU.die_on_collapse, " Raise an exception as soon as an array collapses.");
                 ("--keepcpp", Arg.Set keep_cpp, " Keep the intermediate output of running the C preprocessor.");
                 ("--merge-conflicts", Arg.Clear Mergecil.ignore_merge_conflicts, " Abort on merging conflicts.");
                 ("--cppflags", Arg.Set_string cppflags, "<flags>  Pre-processing parameters.");
                 ("--kernel", Arg.Set GU.kernel, "For analyzing Linux Device Drivers.");
                 ("--showtemps", Arg.Set CF.showtemps, " Shows CIL's temporary variables when printing the state.");
                 ("--uncalled", Arg.Set GU.print_uncalled, " Display uncalled functions.");
                 ("--result", Arg.String setstyle, "<style>  Result style: none, glob, indented, compact, pretty or html.");
                 ("--analysis", Arg.String setanalysis, "<name>  Deprecated: Picks the analysis: mcp.");
                 ("--phase", Arg.String phase, "<nr>  Selects a phase. (<nr> >= 0) ");
                 ("--with", Arg.Symbol (featurelist, set_feature true), " Enables features in current phase.");
                 ("--no", Arg.Symbol (featurelist, set_feature false), " Disables features in current phase.");
                 ("--context", Arg.String (set_context true), "<name>  Enables context sensitivity on a feature.");
                 ("--no-context", Arg.String (set_context false), "<name>  Disables context sensitivity on a feature.");
                 ("--propset", Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (set_prop true)], "<prop> <name> Enables a propery, e.g., --propset int_domain interval.");
                 ("--propdel", Arg.Tuple [Arg.Set_string tmp_arg; Arg.String (set_prop false)], "<prop> <name> Disables a propery, e.g., --propdel int_domain interval.");
                 ("--type-inv", Arg.Bool ((:=) GU.use_type_invariants), "<bool>  Should we use type invariants?");
                 ("--list-type", Arg.Bool ((:=) GU.use_list_type), "<bool>  Should we use list types?");
                 ("--solver", Arg.Symbol (["effectWCon"; "effectWNCon"; "solverConSideRR"; "solverConSideWNRR"; "interactive"; "new"; "TD";"fwtn";"cmp";"s1";"s2";"s3";"n1";"n2";"n3";"hbox";"widen"], setsolver), " Picks the solver.");
                 ("--unique", add_string GU.singles, "<type name>  For types that have only one value.");
                 ("--dump", Arg.String setdump, "<path>  Dumps the results to the given path");
                 ("--cilout", Arg.String setcil, "<path>  Where to dump cil output");
		 ("--oil", Arg.String oil, "<file>  Oil file for the analysed program");
		 ("--tramp", Arg.String tramp, "<file>  Resource-ID-headers for the analysed program");
		 ("--oseknames", Arg.String osek_names, "<file>  OSEK API function (re)names for the analysed program");
		 ("--osekisrprefix", Arg.String osekisrprefix, "Prefix added by the ISR macro");
		 ("--osektaskprefix", Arg.String osektaskprefix, "Prefix added by the TASK macro");
		 ("--osekisrsuffix", Arg.String osekisrsuffix, "Suffix added by the ISR macro");
		 ("--osektasksuffix", Arg.String osektasksuffix, "Suffix added by the TASK macro");
		 ("--osekcheck", Arg.Set OilUtil.check, "Check if (assumed) OSEK conventions are fulfilled");
                 ("--intrpts", Arg.Set GU.intrpts, "Enable constraints for interrupts.");
                 ("--timeout", Arg.Set_float GU.anayzer_timeout, " Maximal time for analysis. (0 -- no timeout)");
                 ("--solver-progress", Arg.Bool ((:=) GU.solver_progress), " <bool> Used for debugging. Prints out a symbol on solving a rhs.");
                 ("--sharirpnueli", Arg.Set GU.sharir_pnueli, " Solve using the Sharir-Pnueli algorithm.");
                 ("--forward", Arg.Set GU.forward, " Use implicit forward propagation instead of the demand driven approatch.");
                 ("--full-context", Arg.Set GU.full_context, " Do not side-effect function entries.");
                 ("--addr-context", Arg.Set GU.addr_contexts, " Ignore non-address values in function contexts.");
                 ("--no-int-context", Arg.Set GU.no_int_contexts, " Ignore integer values in function contexts.");
                 ("--debug-sockets", Arg.Tuple [Arg.Set_int GU.command_port;Arg.Int GU.open_sockets], "<port> <port> Eclipse debuger plugin support.");
                 ("--new_fwk", Arg.Set GU.new_fwk, " Use the new framework.") ;
                 ("--print_dead_code", Arg.Set GU.print_dead_code, " Print information about dead code")
    ] in*)
  let jsonRegex = Str.regexp ".+\\.json$" in
  let recordFile fname = 
    if Str.string_match jsonRegex fname 0
    then GU.jsonFiles := fname :: !GU.jsonFiles 
    else fileNames := fname :: !fileNames
  in
  Stats.reset Stats.SoftwareTimer;  
  CF.init();
  Arg.parse speclist recordFile usage_str;
  
  
  if (get_bool "allfuns") || (get_bool "nonstatic") || !GU.oil then GU.multi_threaded := true;
  if (get_bool "dbg.debug") then M.warnings := true;
  if !GU.verbose then begin
    Printexc.record_backtrace true;
    Errormsg.debugFlag := true;
    Errormsg.verboseFlag := true;
  end;
  (* GU.regions := true; *)
  let _ = match !GU.dump_path with
    | Some path -> begin
        M	.warn_out := open_out (Filename.concat path "warnings.out");
        outFile := "" (*Filename.concat path "analysis.out";*)
        (* --dump overwrites the -o flag*)
      end
    | _ -> ()
  in
  (* The include files, libc stuff  *)
  let warn_includes () = print_endline "Warning, cannot find goblint's custom include files." in
  let includes = ref (if Sys.file_exists(!include_dir) then "-I" ^ !include_dir else (warn_includes () ; "")) in
  let _ = includes := !includes ^ " " ^ !other_includes in
  let libc = Filename.concat !include_dir "lib.c" in 
  fileNames := List.rev !fileNames;
  if !use_libc then fileNames := libc :: !fileNames;
  if !GU.kernel then begin
    let preconf = Filename.concat !include_dir "linux/goblint_preconf.h" in 
    let autoconf = Filename.concat kernel_dir "generated/autoconf.h" in 
    cppflags := "-D__KERNEL__ -U__i386__ -include " ^ preconf ^ " -include " ^ autoconf ^ " " ^ !cppflags;
    includes := !includes ^ " -I" ^ kernel_dir ^ " -I" ^ asm_dir ^ " -I" ^ asm_dir ^ "/asm/mach-default"
  end;
  if !GU.verbose then print_endline ("JSON file: " ^ GU.conf_file);
  (* The temp directory for preprocessing the input files *)
  let dirName = GU.create_dir "goblint_temp" in
  (* preprocess all the files *)
  let preproFile fname =
    (* The actual filename of the preprocessed sourcefile *)
    let nname =  Filename.concat dirName (Filename.basename fname) in 
    (* Preprocess using gcc -E *)
    let command = "gcc --undef __BLOCKS__ -E " ^ !cppflags ^ " " ^ !includes ^ " " ^ fname ^ " -o " ^ nname in
      if !GU.verbose then print_endline command;
      let status = try Unix.system command with 
          | Unix.Unix_error (e, f, a) -> 
              Printf.eprintf "%s at syscall %s with argument \"%s\".\n" (Unix.error_message e) f a; 
              if !keep_cpp then () else ignore (GU.rm_rf dirName); exit 2
      in
        match status with
          | Unix.WEXITED 0 -> nname
          | _ -> 
              prerr_endline "Goblint: Preprocessing failed."; 
              if !keep_cpp then () else ignore (GU.rm_rf dirName); exit 2
  in
  let cpp_file_names = 
    if !GU.verbose then print_endline "Preprocessing files.";
    List.rev_map preproFile !fileNames in
  (* and get their AST *)
  let files_AST = 
    if !GU.verbose then print_endline "Parsing files.";
    List.rev_map CF.getAST cpp_file_names in
  let _ = if !keep_cpp then () else ignore (GU.rm_rf dirName) in
  (* direct the output to file if requested  *)
  let _ = if not (!outFile = "") then GU.out :=  open_out !outFile in
  let _ = Errormsg.logChannel := M.get_out "cil" !cilout in
  (* we use CIL to merge all inputs to ONE file *)
  let merged_AST = 
    match files_AST with
      | [one] -> CF.callConstructors one
      | [] -> 
          prerr_endline "No arguments for Goblint?"; 
          prerr_endline usage_str; 	  
          prerr_endline "Try `goblint --help' for more information."; 
          exit 2
      | xs -> CF.callConstructors (CF.getMergedAST xs) 
  in
    (* using CIL's partial evaluation and constant folding! *)
    if !dopartial then CF.partial merged_AST;
    CF.rmTemps merged_AST;
    (* creat the Control Flow Graph from CIL's AST *)
    CF.createCFG merged_AST;
    CF.ugglyImperativeHack := merged_AST;
    (* we let the "--eclipse" flag override result style: *)
    if !GU.eclipse then GU.result_style := GU.Compact;
    if !justCil then 
      (* if we only want to print the output created by CIL: *)
      CF.print merged_AST
    else begin
      (* we first find the functions to analyze: *)
      if !GU.oil then Osek.Spec.parse_oil ();
      if !GU.verbose then print_endline "And now...  the Goblin!";
      let (stf,exf,otf as funs) = CF.getFuns merged_AST in
        if stf@exf@otf = [] then failwith "No suitable function to start from.";
        if !GU.verbose then ignore (Pretty.printf "Startfuns: %a\nExitfuns: %a\nOtherfuns: %a\n"
                                                   L.pretty stf L.pretty exf L.pretty otf);
        (* and here we run the analysis! *)
        if !GU.result_style = GU.Html then Report.prepare_html_report ();
        if (get_bool "exp.new_fwk") then analyze := Constraints.analyze ;
        Stats.time "analysis" (!analyze merged_AST) funs;
        Report.do_stats !fileNames
    end
*)
let _ = 
  main ()
