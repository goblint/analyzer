(** Default values for [GobConfig]-style configuration. *)

open Batteries_uni
open Printf
open List

(* TODO: add consistency checking *)

(** Main categories of configuration variables. *)
type category = Std           (** Parsing input, includes, standard stuff, etc. *)
              | Analyses      (** Analyses                                      *)
              | Experimental  (** Experimental features of analyses             *)
              | Debugging     (** Debugging, tracing, etc.                      *)
  
(** Description strings for categories. *)
let catDescription = function
  | Std          -> "Standard options for configuring input/output"
  | Analyses     -> "Options for analyses"
  | Experimental -> "Experimental features"
  | Debugging    -> "Debugging options"
  
(** A place to store registered varibles *)
let registrar = ref []

(** A function to register a variable *)
let reg (c:category) (n:string) (def:string) (desc:string) =
  registrar := (c,(n,(desc,def))) :: !registrar;
  GobConfig.set_auto n def

(** find all associations in the list *)
let rec assoc_all k = function
  | [] -> []
  | (x1,x2)::xs when k=x1 -> x2 :: assoc_all k xs
  | _::xs -> assoc_all k xs
  
(** Prints out all registered options with descriptions and defaults for one category. *)
let printCategory ch k =
  let print_one (n,(desc,def)) =
    fprintf ch "%-4s%-30s%s\n%-34sDefault value: \"%s\"\n\n" "" n desc "" def
  in
  catDescription k |> fprintf ch "%s:\n";
  assoc_all k !registrar |> rev |> iter print_one

(** Prints out all registered options. *)
let printAllCategories ch =
  iter (printCategory ch) [Std;Analyses;Experimental;Debugging]

(* {4 category [Std]} *)

let _ =
  reg Std "outfile"         ""             "File to print output to.";
  reg Std "includes"        "[]"           "List of directories to include.";
  reg Std "kernel_includes" "[]"           "List of kernel directories to include.";
  reg Std "custom_includes" "[]"           "List of custom directories to include.";
  reg Std "custom_incl"     "''"           "Use custom includes";
  reg Std "custom_libc"     "false"        "Use goblints custom libc.";
  reg Std "justcil"         "false"        "Just parse and output the CIL.";
  reg Std "justcfg"         "false"        "Only output the CFG in cfg.dot .";
  reg Std "dopartial"       "false"        "Use Cil's partial evaluation & constant folding.";
  reg Std "printstats"      "false"        "Outputs timing information.";
  reg Std "gccwarn"         "false"        "Output warnings in GCC format.";
  reg Std "noverify"        "false"        "Skip the verification phase.";
  reg Std "mainfun"         "['main']"     "Sets the name of the main functions.";
  reg Std "exitfun"         "[]"           "Sets the name of the cleanup functions.";
  reg Std "otherfun"        "[]"           "Sets the name of other functions.";
  reg Std "allglobs"        "false"        "Prints access information about all globals, not just races.";
  reg Std "keepcpp"         "false"        "Keep the intermediate output of running the C preprocessor.";
  reg Std "merge-conflicts" "true"         "Abort on merging conflicts.";
  reg Std "cppflags"        "''"           "Pre-processing parameters.";
  reg Std "kernel"          "false"        "For analyzing Linux Device Drivers.";
  reg Std "dump_globs"      "false"        "Print out the global invariant.";
  reg Std "result"          "'none'"       "Result style: none, indented, compact, or pretty.";
  reg Std "solver"          "'effectWCon'" "Picks the solver.";
  reg Std "allfuns"         "false"        "Analyzes all the functions (not just beginning from main).";
  reg Std "nonstatic"       "false"        "Analyzes all non-static functions."
                                               
(* {4 category [Analyses]} *)

let _ =
  reg Analyses "ana.activated" "[['base','escape','mutex']]"  "Lists of activated analyses, split into phases.";

  reg Analyses "ana.path_sens"  "['OESK','OESK2','mutex','malloc_null','uninit','shape']"  "List of path-sensitive analyses";
  reg Analyses "ana.ctx_insens" "['OESK2','OESK3','stack_trace_set']"                      "List of context-insensitive analyses";
  
  reg Analyses "ana.warnings"        "false" "Print soundness warnings.";
  reg Analyses "ana.cont.localclass" "false" "Analyzes classes defined in main Class.";
  reg Analyses "ana.cont.class"      "''"    "Analyzes all the member functions of the class (CXX.json file required).";
  reg Analyses "ana.osek.oil"        "''"    "Oil file for the analyzed program";
  reg Analyses "ana.osek.tramp"      "''"    "Resource-ID-headers for the analyzed program";
  reg Analyses "ana.osek.isrprefix"  "''"    "Prefix added by the ISR macro";
  reg Analyses "ana.osek.taskprefix" "''"    "Prefix added by the TASK macro";
  reg Analyses "ana.osek.isrsuffix"  "''"    "Suffix added by the ISR macro";
  reg Analyses "ana.osek.tasksuffix" "''"    "Suffix added by the TASK macro";
  reg Analyses "ana.osek.intrpts"    "false" "Enable constraints for interrupts.";
  reg Analyses "ana.osek.check"      "false" "Check if (assumed) OSEK conventions are fullfilled.";
  reg Analyses "ana.osek.names"      "[]"    "OSEK API function (re)names for the analysed program";
  reg Analyses "ana.int.trier"       "true"  "Exclusion set based integer domain.";
  reg Analyses "ana.int.interval"    "false" "Interval based integer domain."
  
(* {4 category [Experimental]} *)

let _ =
  reg Experimental "exp.field_insensitive" "false" "Control the field sensitivity of the Base analysis.";
  reg Experimental "exp.eclipse"           "false" "Flag for Goblin's Eclipse Plugin.";
  reg Experimental "exp.check"             "[]"    "Check whether there is a race involving this variable/type.";
  reg Experimental "exp.earlyglobs"        "false" "Side-effecting of globals right after initialization.";
  reg Experimental "exp.write-races"       "false" "Ignores read accesses altogether in reporting races.";
  reg Experimental "exp.failing-locks"     "false" "Takes the possible failing of locking operations into account.";
  reg Experimental "exp.field-insensitive" "false" "Turns off field-sensitivity.";
  reg Experimental "exp.region-offsets"    "false" "Considers offsets for region accesses.";
(*  reg Experimental "exp.unmerged-fields"   "false" "Does not merge accesses to possibly same fields, unsound.";
  reg Experimental "exp.die-on-collapse"   "false" "Raise an exception as soon as an array collapses.";*)
  reg Experimental "exp.type-inv"          "false" "Should we use type invariants?";
  reg Experimental "exp.unique"            "[]"    "For types that have only one value.";
  reg Experimental "exp.sharir-pnueli"     "false" "Use the Sharir/Pnueli algorithm for solving.";
  reg Experimental "exp.forward"           "false" "Use implicit forward propagation instead of the demand driven approach.";
  reg Experimental "exp.full-context"      "false" "Do not side-effect function entries.";
  reg Experimental "exp.addr-context"      "false" "Ignore non-address values in function contexts.";
  reg Experimental "exp.no-int-context"    "false" "Ignore integer values in function contexts.";
  reg Experimental "exp.new_fwk"           "false" "Use the new framework.";
  reg Experimental "exp.malloc-fail"       "false" "Consider the case where malloc fails.";
  reg Experimental "exp.volatiles_are_top" "true"  "volatile and extern keywords set variables permanently to top";
  reg Experimental "exp.back_loop_sep"     "false" "Only widen on nodes with back edges."
  
(* {4 category [Debugging]} *)

let _ =
  reg Debugging "dbg.debug"           "false" "Debug mode: for testing the analyzer itself.";
  reg Debugging "dbg.verbose"         "false" "Prints some status information.";
  reg Debugging "dbg.filter"          ""      "Regexp filtering output file.";
  (*reg Debugging "dbg.trace.sys"       ""      "Subsystem to show debug printfs for, such as con, sol.";
  reg Debugging "dbg.trace.vars"      "[]"    "Identifier name of interest for tracing.";
  reg Debugging "dbg.trace.locs"      "[]"    "Line number of interest for tracing.";*)
  reg Debugging "dbg.showtemps"       "false" "Shows CIL's temporary variables when printing the state.";
  reg Debugging "dbg.uncalled"        "false" "Display uncalled functions.";
  reg Debugging "dbg.dump"            ""      "Dumps the results to the given path";
  reg Debugging "dbg.cilout"          ""      "Where to dump cil output";
  reg Debugging "dbg.timeout"         "0"     "Maximal time for analysis. (0 -- no timeout)";
  reg Debugging "dbg.solver-progress" "false" "Used for debugging. Prints out a symbol on solving a rhs.";
  reg Debugging "dbg.debug-sockets"   "null"  "Eclipse debugger plugin support.";
  reg Debugging "dbg.print_dead_code" "false" "Print information about dead code"
  
