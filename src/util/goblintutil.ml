(** Globally accessible flags and utility functions. *)

open Cil

open Json_type
open Json_type.Browse

(* generate a default configuration *)
let default_conf () =
  let def_int = Build.objekt ["trier"      , Build.bool true
                             ;"interval"   , Build.bool false] in
  let def_ana = Build.objekt ["base"       , Build.bool true
                             ;"OSEK"       , Build.bool false
                             ;"OSEK2"      , Build.bool false
                             ;"access"     , Build.bool true
                             ;"thread"     , Build.bool false
                             ;"escape"     , Build.bool true
                             ;"mutex"      , Build.bool true
                             ;"symb_locks" , Build.bool false
                             ;"uninit"     , Build.bool false
                             ;"malloc_null", Build.bool false
                             ;"region"     , Build.bool false
                             ;"var_eq"     , Build.bool false] in
  let def_path = Build.objekt ["base"       , Build.bool false
                              ;"OSEK"       , Build.bool true
                              ;"OSEK2"      , Build.bool true
                              ;"access"     , Build.bool false
                              ;"thread"     , Build.bool false
                              ;"escape"     , Build.bool false
                              ;"mutex"      , Build.bool true
                              ;"symb_locks" , Build.bool false
                              ;"uninit"     , Build.bool true
                              ;"malloc_null", Build.bool true
                              ;"region"     , Build.bool false
                              ;"var_eq"     , Build.bool false] in
  let def_ctx = Build.objekt ["base"       , Build.bool true
                             ;"OSEK"       , Build.bool true
                              ;"OSEK2"     , Build.bool false
                             ;"access"     , Build.bool true
                             ;"thread"     , Build.bool true
                             ;"escape"     , Build.bool true
                             ;"mutex"      , Build.bool true
                             ;"symb_locks" , Build.bool true
                             ;"uninit"     , Build.bool true
                             ;"malloc_null", Build.bool true
                             ;"region"     , Build.bool true
                             ;"var_eq"     , Build.bool true] in
  Build.objekt ["int_domain" , def_int
               ;"analyses"   , def_ana
               ;"sensitive"  , def_path
               ;"context"    , def_ctx
               ;"analysis"   , Build.string "mcp"
               ;"solver"     , Build.string "effectWCon" ]

(* configuration structure -- get it from a file or generate a new one *)
let conf : (string, Json_type.t) Hashtbl.t ref = 
  let fn = Filename.concat (Filename.dirname (Sys.argv.(0))) "goblint.json" in
  try
    ref (make_table (objekt (Json_io.load_json ~allow_comments:true fn)))
  with (Sys_error x) -> 
    let c = default_conf () in
    Json_io.save_json fn c;
    ref (make_table (objekt c))

let modify_ana x b = 
  let old_ana = make_table (objekt (field !conf "analyses")) in
  let anas = ["base";"OSEK";"OSEK2";"access";"thread";"escape";"mutex";"symb_locks";"uninit";"malloc_null";"region";"var_eq"] in
  let set_ana_pair fe = 
    if fe = x 
    then fe, Build.bool b
    else  fe, field old_ana fe 
  in
  let modif = 
    Build.objekt ["int_domain" , field !conf "int_domain"
                 ;"analyses"   , Build.objekt (List.map set_ana_pair anas)
                 ;"sensitive"  , field !conf "sensitive"
                 ;"analysis"   , field !conf "analysis"
                 ;"context"    , field !conf "context"
                 ;"solver"     , field !conf "solver"] in
  conf := make_table (objekt modif)
  
let conf_uninit () = 
  modify_ana "thread" false;
  modify_ana "mutex" false;
  modify_ana "symb_locks" false;
  modify_ana "uninit" true;
  modify_ana "malloc_null" false;
  modify_ana "region" false

let conf_malloc () = 
  modify_ana "thread" false;
  modify_ana "mutex" false;
  modify_ana "symb_locks" false;
  modify_ana "uninit" false;
  modify_ana "malloc_null" true;
  modify_ana "region" false

let conf_osek () = 
  modify_ana "mutex" false;
  modify_ana "OSEK" true;
  modify_ana "OSEK2" true

(** when goblin is in debug mode *)
let debug = ref false 

(** whether to verify result *)
let verify = ref true 

(** Outputs information about what the goblin is doing *)
let verbose = ref false

(** prints the CFG on [getCFG] *)
let cfg_print = ref false 

(** analyze all the functions in the program, rather than just main *)
let allfuns = ref false
let nonstatic = ref false
(** analyze all functions corresponding to a osek task *)
let oil = ref false
let taskprefix = "function_of_"

(** name of the main / init function *)
let mainfun = ref "main"

(** name of the exit function, just additionally spawned ... *)
let exitfun = ref ([]: string list)

(** Automatically detect init and exit functions of kernel modules. *)
let harness = ref false

(** Whether a main function has been found. *)
let has_main = ref false

(** print information about all globals, not just races *)
let allglobs = ref false

(** an optional path to dump all output *)
let dump_path = ref (None : string option)

(** has any threads have been spawned *)
let multi_threaded = ref false

(** should globals be side-effected early *)
let earlyglobs = ref false

(** only report write races *)
let no_read = ref false

(** Truns off field-sensitivity. *)
let field_insensitive = ref false

(** Constraints for interrupts. *)
let intrpts = ref false

(** Avoids the merging of fields, not really sound *)
let unmerged_fields = ref false

(** Will terminate on a collapsed array --- for debugging. *)
let die_on_collapse = ref false

(** Adds support to failing mallocs. *)
let malloc_may_fail = ref false 

(** Tells the spec that result may still get smaller (on narrowing). 
   If this is false we can output messages and collect accesses. *)
let may_narrow = ref true

(** hack to use a special integer to denote synchronized array-based locking *)
let inthack = Int64.of_int (-19012009)

(** number of times that globals change *)
let globals_changed = ref 0

(** use the old accesses vs. the newer path based heap abstraction *)
let old_accesses = ref true

(** The file where everything is output *)
let out = ref stdout

type result_style =
  | None (** Do not print any output except warnings *)
  | State (** Only output the state of main function *)
  | Indented (** Output indented XML *)
  | Compact (** Output compact XML, for Eclipse plugin *)
  | Pretty (** Pretty-printed text outpu *)

(** The specified result style *)
let result_style = ref None

(** Is the goblin Eclipse Plugin calling the analyzer? *)
let eclipse = ref false

(** Analyzing Device Drivers? *)
let kernel = ref false

(** Length of summary description in XML output *)
let summary_length = 80

(** Do we need to print CIL's temporary variables? *)
let show_temps = ref true

(** If we want to display functions that are not called *)
let print_uncalled = ref false

(** A very nice imperative hack to get the current location. This can be
  * referenced from within any transfer function. *)
let current_loc = ref locUnknown

let solver = ref (string (field !conf "solver"))

let escape (x:string):string =
  let esc_1 = Str.global_replace (Str.regexp "&") "&amp;" x in
  let esc_2 = Str.global_replace (Str.regexp "<") "&lt;" esc_1 in
  let esc_3 = Str.global_replace (Str.regexp ">") "&gt;" esc_2 in
  let esc_4 = Str.global_replace (Str.regexp "\"") "&quot;" esc_3 in
    esc_4

let trim (x:string): string = 
  let len = String.length x in
    if x.[len-1] = ' ' then String.sub x 0 (len-1) else x


(** Creates a directory and returns the absolute path **)
let create_dir name = 
  let dirName = if Filename.is_relative name then Filename.concat (Unix.getcwd ()) name else name in
  (* The directory should be writable to group and user *)
  let dirPerm = 0o770 in
  let _ = 
    try
      Unix.mkdir dirName dirPerm
    with Unix.Unix_error(err, ctx1, ctx2) as ex -> 
      (* We can discared the EEXIST, we are happy to use the existing directory *)
      if err != Unix.EEXIST then begin
        (* Hopefully will be friendly enough :) *)
        print_endline ("Error, " ^ (Unix.error_message err));
        raise ex
      end
  in
    dirName

(** Remove directory and its content, as "rm -rf" would do. *)
let rm_rf path = 
  let rec f path = 
    if Sys.is_directory path then begin
      let files = Array.map (Filename.concat path) (Sys.readdir path) in
        Array.iter f files; 
        Unix.rmdir path
    end else 
      Sys.remove path
  in
    f path
