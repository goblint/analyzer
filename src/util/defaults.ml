
(** Main categories of configuration variables. *)
type category = Std           (** Parsing input, includes, standard stuff, etc. *)
              | Analyses      (** Analyses                                      *)
              | Experimental  (** Experimental features of analyses             *)
              | Debugging     (** Debugging, tracing, etc.                      *)
  
(** A place to store registered varibles *)
let registrar = ref []

(** A function to register a variable *)
let reg (c:category) (n:string) (def:string) (desc:string) =
  registrar := (c,(n,(desc,def))) :: !registrar;
  GobConfig.set_auto n def

(** {4 category [Std]} *)

let _ =
  reg Std "outfile"         ""      "File to print output to.";
  reg Std "include"         "null"  "List of include directories.";
  reg Std "kernel_includes" "null"  "List of kernel include directories.";
  reg Std "custom_includes" "null"  "List of custom include directories.";
  reg Std "custom_libc"     "false" "Use goblints custom libc.";
  reg Std "justcil"         "false" "Just parse and output the CIL.";
  reg Std "justcfg"         "false" "Only output the CFG in cfg.dot .";
  reg Std "dopartial"       "false" "Use Cil's partial evaluation & constant folding."

(** {4 category [Analyses]} *)


let _ =
  reg Analyses "ana.activated" "[['base','escape','mutex']]"  "Lists of activated analyses, split into phases.";
  
  reg Analyses "ana.ctx_sens.mutex" "true"  "Context sensitivity for the mutex analysis";
  reg Analyses "ana.ctx_sens.base"  "true"  "Context sensitivity for the base analysis";
  reg Analyses "ana.ctx_sens.osek"  "true"  "Context sensitivity for the osek analysis";
  
  reg Analyses "ana.pth_sens.mutex" "true"  "Path sensitivity for the mutex analysis"; 
  reg Analyses "ana.pth_sens.base"  "false" "Path sensitivity for the base analysis";
  reg Analyses "ana.pth_sens.osek"  "false" "Path sensitivity for the osek analysis"
  
(** {4 category [Experimental]} *)

let _ =
  reg Experimental "exp.field_insensitive" "false" "Control the field sensitivity of the Base analysis.";
  
(** {4 category [Debugging]} *)

(*
            ("-o", Arg.Set_string outFile, "<file>  Prints the output to file.");
            ("-v", Arg.Set GU.verbose, " Prints some status information.");
            ("--version", Arg.Unit print_version, "Prints version information.");
            ("--filter", Arg.Set_string GU.result_filter, "regexp filtering output file.");
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
            ("--result", Arg.String setstyle, "<style>  Result style: none, glob, indented, compact, or pretty.");
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
            ("--solver", Arg.Symbol (["effectWCon"; "effectWNCon"; "solverConSideRR"; "solverConSideWNRR"; "interactive"; "new"; "TD";"fwtn";"cmp"], setsolver), " Picks the solver.");
            ("--unique", add_string GU.singles, "<type name>  For types that have only one value.");
            ("--dump", Arg.String setdump, "<path>  Dumps the results to the given path");
            ("--cilout", Arg.String setcil, "<path>  Where to dump cil output");
("--oil", Arg.String oil, "<file>  Oil file for the analysed program");
("--tramp", Arg.String tramp, "<file>  Resource-ID-headers for the analysed program");
("--osekisrprefix", Arg.String osekisrprefix, "Prefix added by the ISR macro");
("--osektaskprefix", Arg.String osektaskprefix, "Prefix added by the TASK macro");
("--osekisrsuffix", Arg.String osekisrsuffix, "Suffix added by the ISR macro");
("--osektasksuffix", Arg.String osektasksuffix, "Suffix added by the TASK macro");
            ("--intrpts", Arg.Set GU.intrpts, " Enable constraints for interrupts.");
            ("--timeout", Arg.Set_float GU.anayzer_timeout, " Maximal time for analysis. (0 -- no timeout)");
            ("--solver-progress", Arg.Bool ((:=) GU.solver_progress), " <bool> Used for debugging. Prints out a symbol on solving a rhs.");
            ("--sharirpnueli", Arg.Set GU.sharir_pnueli, " Solve using the Sharir-Pnueli algorithm.");
            ("--forward", Arg.Set GU.forward, " Use implicit forward propagation instead of the demand driven approatch.");
            ("--full-context", Arg.Set GU.full_context, " Do not side-effect function entries.");
            ("--addr-context", Arg.Set GU.addr_contexts, " Ignore non-address values in function contexts.");
            ("--debug-sockets", Arg.Tuple [Arg.Set_int GU.command_port;Arg.Int GU.open_sockets], "<port> <port> Eclipse debuger plugin support.");
            ("--new_fwk", Arg.Set GU.new_fwk, " Use the new framework.") ;
            ("--print_dead_code", Arg.Set GU.print_dead_code, " Print information about dead code")
            
            *)
            