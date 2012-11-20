(** Globally accessible flags and utility functions. *)

open Cil
open Pretty

open Json
open JsonParser
open JsonLexer

(* MCP adds analysis here ... *)
let anas : string list ref = ref []
(* Phase of the analysis *)
let phase = ref 0

(* generate a default configuration *)
let default_conf () =
  let def_int = Build.objekt ["trier"      , Build.bool true
                             ;"interval"   , Build.bool false] in
  let def_ana = Build.array [Build.array [Build.string "base"
                                         ;Build.string "escape"
                                         ;Build.string "file"
                                         ;Build.string "mutex"]] in
(*  let def_ana = Build.objekt ["base"       , Build.bool true
                             ;"OSEK"       , Build.bool false
                             ;"OSEK2"      , Build.bool false
                             ;"OSEK3"      , Build.bool false
                             ;"access"     , Build.bool false
                             ;"thread"     , Build.bool false
                             ;"escape"     , Build.bool true
                             ;"mutex"      , Build.bool true
                             ;"symb_locks" , Build.bool false
                             ;"uninit"     , Build.bool false
                             ;"malloc_null", Build.bool false
                             ;"region"     , Build.bool false
                             ;"containment", Build.bool false
                             ;"shape"      , Build.bool false
                             ;"var_eq"     , Build.bool false] in  *)
  let def_path = Build.objekt ["base"       , Build.bool false
                              ;"OSEK"       , Build.bool true
                              ;"OSEK2"      , Build.bool true
                              ;"OSEK3"      , Build.bool false
                              ;"access"     , Build.bool false
                              ;"thread"     , Build.bool false
                              ;"escape"     , Build.bool false
                              ;"file"       , Build.bool false
                              ;"mutex"      , Build.bool true
                              ;"symb_locks" , Build.bool false
                              ;"uninit"     , Build.bool true
                              ;"malloc_null", Build.bool true
                              ;"region"     , Build.bool false
                              ;"containment", Build.bool false
                              ;"stack_trace", Build.bool false
                              ;"stack_trace_set", Build.bool false
                              ;"shape"      , Build.bool true
                              ;"var_eq"     , Build.bool false
                              ;"mtflag"     , Build.bool false
                              ;"lval_need"  , Build.bool false] in
  let def_ctx = Build.objekt ["base"       , Build.bool true
                             ;"OSEK"       , Build.bool true
                             ;"OSEK2"      , Build.bool false
                             ;"OSEK3"      , Build.bool false
                             ;"access"     , Build.bool true
                             ;"thread"     , Build.bool true
                             ;"escape"     , Build.bool true
                             ;"file"       , Build.bool true
                             ;"mutex"      , Build.bool true
                             ;"symb_locks" , Build.bool true
                             ;"uninit"     , Build.bool true
                             ;"malloc_null", Build.bool true
                             ;"region"     , Build.bool true
                             ;"stack_trace", Build.bool true
                             ;"stack_trace_set", Build.bool false
                             ;"containment", Build.bool true
                             ;"shape"      , Build.bool true
                             ;"var_eq"     , Build.bool true
                             ;"mtflag"     , Build.bool true
                             ;"lval_need"  , Build.bool true] in
  Build.objekt ["int_domain" , def_int
               ;"analyses"   , def_ana
               ;"sensitive"  , def_path
               ;"context"    , def_ctx
               ;"analysis"   , Build.string "mcp"
               ;"solver"     , Build.string "effectWCon" ]

let conf_file = Filename.concat (Sys.getcwd ()) "goblint.json"

(* configuration structure -- get it from a file or generate a new one *)
let conf : jvalue ref Object.t ref = 
  try
    match value token (Lexing.from_channel (open_in conf_file)) with
      | Object o -> o
      | _ -> raise (Sys_error "Bad json file: must be an object.")
  with (Sys_error x) -> 
    let c = default_conf () in
    let unwrap = function
      | Object o -> o
      | _ -> raise (Sys_error "Bad default conf: fix, recompile, etc.") in
    save_json conf_file c;
    (unwrap c)
    
let modify_ana x b = 
  let rec dropNth = function
    | []    -> fun x -> []
    | x::xs -> function 
            | 0 -> xs 
            | n -> x :: dropNth xs (n-1) 
  in
  let an = array !(field conf "analyses") in
  let ph = array !(List.nth !an !phase) in
  let rem_x = List.filter (fun y -> not (string !y = x)) !ph in
  if b || List.length rem_x > 0 then
    (if b then ph := ref (Build.string x) :: rem_x else ph := rem_x)
  else
    an := dropNth !an !phase

let modify_prop prop name b = 
  let d  = Object.find name !(objekt !(field conf prop)) in
    d := Build.bool b

let modify_context x b = modify_prop "context" x b

let conf_containment () = 
  modify_ana "containment" true;
  modify_ana "thread" false;
  modify_ana "mutex" false;
  modify_ana "symb_locks" false;
  modify_ana "uninit" false;
  modify_ana "malloc_null" false;
  modify_ana "region" false
  
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

let conf_base () = 
  modify_ana "base" true;
  modify_ana "containment" false;
  modify_ana "thread" false;
  modify_ana "mutex" false;
  modify_ana "symb_locks" false;
  modify_ana "uninit" false;
  modify_ana "malloc_null" false;
  modify_ana "region" false
  
let conf_osek () = 
  modify_ana "mutex" false;
  modify_ana "OSEK" true;
  modify_ana "OSEK2" true;
  modify_ana "OSEK3" true;
  modify_ana "stack_trace_set" true

(** command port for eclipse debuger support *)
let command_port = ref (-1)
(** event port for eclipse debuger support *)
let event_port = ref (-1)
let command_socket = Unix.socket (Unix.PF_INET) (Unix.SOCK_STREAM) 0
let event_socket   = Unix.socket (Unix.PF_INET) (Unix.SOCK_STREAM) 0
let command_in  = ref stdin
let command_out = ref stdout
let event_out   = ref stdout

let open_sockets i =
  event_port := i;
  ignore (Printf.printf "connecting...");
  Unix.setsockopt command_socket Unix.SO_REUSEADDR true;
  Unix.bind command_socket (Unix.ADDR_INET (Unix.inet_addr_loopback, !command_port));
  Unix.listen command_socket 1;
  let (client,_) = Unix.accept command_socket in 
  command_in  := Unix.in_channel_of_descr client;
  command_out := Unix.out_channel_of_descr client;
  set_binary_mode_in !command_in false;
  set_binary_mode_out !command_out false;
  Unix.setsockopt event_socket Unix.SO_REUSEADDR true;
  Unix.bind event_socket (Unix.ADDR_INET (Unix.inet_addr_loopback, i));
  Unix.listen event_socket 1;
  let (client,_) = Unix.accept event_socket in 
  event_out  := Unix.out_channel_of_descr client;
  set_binary_mode_out !event_out false;
  ignore (Printf.printf "done.\n")

(** the number of seconds the analyzer is aborted after; 0.0 means no timeout *)
let anayzer_timeout = ref 0.0

(** Do we side-effect function entries? If we use full contexts then there is no need. *)
let full_context = ref false

(** use the Sharir-Pnueli algorithm *)
let sharir_pnueli = ref false

(** forward propagation *)
let forward = ref false

(** Address contexts *)
let addr_contexts = ref false

(** No integer contexts *)
let no_int_contexts = ref false

(** singleton types *)
let singles = ref []

(** when goblin is in debug mode *)
let debug = ref false 

(** whether to verify result *)
let verify = ref true 

(** Outputs information about what the goblin is doing *)
let verbose = ref false

(** prints the CFG on [getCFG] *)
let cfg_print = ref false 

(** filter result xml *)
let result_filter = ref ".*"

let result_regexp = ref (Str.regexp "")

(** analyze all the functions in the program, rather than just main *)
let allfuns = ref false
let nonstatic = ref false
(** analyze all functions corresponding to a osek task *)
let oil = ref false
let taskprefix = ref ""
let isrprefix = ref ""
let tasksuffix = ref ""
let isrsuffix = ref ""

(** Name of the main / init function. 
  * FIXME: Any function named main will be considered a main function even
  * if user specifies otherwise.  *)
let mainfuns = ref ["main"]

(** Name of the exit function, same ID as main function, but runs in
  * multithreaded mode. *)
let exitfuns = ref ([]: string list)

(** Name of other functions, just additionally spawned ... *)
let otherfuns = ref ([]: string list)

(** Automatically detect init and exit functions of kernel modules. *)
let harness = ref false

(** print information about all globals, not just races *)
let allglobs = ref false

(** an optional path to dump all output *)
let dump_path = ref (None : string option)

(** Json files that are given as arguments *)
let jsonFiles : string list ref = ref [] 

(** Name of the class to analyse (containment) *)
let mainclass : string ref = ref ""

(** Dive into locally defined classes (containment) *)
let local_class = ref false


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

(** use the old accesses vs. the new pairwise accesses *)
let old_accesses = ref true

(** The file where everything is output *)
let out = ref stdout

(** use the new framework *)
let new_fwk = ref false

(* Print out dead code *)
let print_dead_code = ref false

(* Type invariant variables. *)
let type_inv_tbl = Hashtbl.create 13 
let type_inv (c:compinfo) : varinfo =
  try Hashtbl.find type_inv_tbl c.ckey
  with Not_found ->
      let i = makeGlobalVar ("{struct "^c.cname^"}") (TComp (c,[])) in
      Hashtbl.add type_inv_tbl c.ckey i;
      i

let is_blessed (t:typ): varinfo option =
  let me_gusta x = List.mem x !singles in 
  match unrollType t with
    | TComp (ci,_) when me_gusta ci.cname -> Some (type_inv ci)
    | _ -> (None : Cil.varinfo option)


type result_style =
  | NoOutput (** Do not print any output except warnings *)
  | Indented (** Output indented XML *)
  | Compact (** Output compact XML, for Eclipse plugin *)
  | Pretty (** Pretty-printed text outpu *)
  | Html (** HTML output *)
  | NewHtml

(** The specified result style *)
let result_style = ref NoOutput

(** Whether to pretty print the global invariant. *)
let dump_global_inv = ref false

(** Is the goblin Eclipse Plugin calling the analyzer? *)
let eclipse = ref false

(** output warnings in GCC form *)
let gccwarn = ref false

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

let global_initialization = ref false 
(** A hack to see if we are currently doing global inits *)

let use_type_invariants = ref false 
(** Use type invariants. *)

let use_list_type = ref false 
(** Use abstract list type instead of kernel struct list_head! *)

let solver_progress = ref false 
(** display a char for each processed rhs and each constraint *)

let region_offsets = ref false
(** Field-sensitive regions? *)

let in_verifying_stage = ref false
(** true if in verifying stage *)

let solver = ref (string !(field conf "solver"))

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
    
type name = 
   | Cons     
   | Dest     
   | Name     of string
   | Unknown  of string
   | Template of name 
   | Nested   of name * name
   | PtrTo    of name
   | TypeFun  of string * name
   
let rec name_to_string_hlp = function
  | Cons -> "constructor"
  | Dest -> "destructor"
  | Name x -> x
  | Unknown x -> "?"^x^"?"
  | Template a -> "("^name_to_string_hlp a^")"
  | Nested (Template x,y) -> "("^name_to_string_hlp x^ ")::" ^ name_to_string_hlp y
  | Nested (x,Cons) -> let c = name_to_string_hlp x in c ^ "::" ^ c
  | Nested (x,Dest) -> let c = name_to_string_hlp x in c ^ "::~" ^ c
  | Nested (x,Name "") -> name_to_string_hlp x
  | Nested (x,y) -> name_to_string_hlp x ^ "::" ^ name_to_string_hlp y
  | PtrTo x -> name_to_string_hlp x ^ "*"
  | TypeFun (f,x) -> f ^ "(" ^ name_to_string_hlp x ^ ")"

let prefix = Str.regexp "^::.*"

let name_to_string x =
	name_to_string_hlp x

let rec show = function
  | Cons -> "Cons"
  | Dest -> "Dest"
  | Name x -> "Name \""^x^"\""
  | Unknown x -> "Unknown \""^x^"\""
  | Template (a) -> "Template ("^show a^")"
  | Nested (x,y) -> "Nested ("^show x^","^show y^")"
  | PtrTo x -> "PtrTo ("^show x^")"
  | TypeFun (f,x) -> "TypeFun ("^f^","^ name_to_string x ^ ")"

 
let special    = Str.regexp "nw\\|na\\|dl\\|da\\|ps\\|ng\\|ad\\|de\\|co\\|pl\\|mi\\|ml\\|dv\\|rm\\|an\\|or\\|eo\\|aS\\|pL\\|mI\\|mL\\|dV\\|rM\\|aN \\|oR\\|eO\\|ls\\|rs\\|lS\\|rS\\|eq\\|ne\\|lt\\|gt\\|le\\|ge\\|nt\\|aa\\|oo\\|pp\\|mm\\|cm\\|pm\\|pt\\|cl\\|ix\\|qu\\|st\\|sz"
let dem_prefix = Str.regexp "^_Z\\(.+\\)"
let num_prefix = Str.regexp "^\\([0-9]+\\)\\(.+\\)"
let ti_prefix  = Str.regexp "^TI\\(.+\\)"
let tv_prefix  = Str.regexp "^TV\\(.+\\)"
let ts_prefix  = Str.regexp "^TS\\(.+\\)"
let tt_prefix  = Str.regexp "^TT\\(.+\\)"
let tt_prefix  = Str.regexp "^TT\\(.+\\)"
let nested     = Str.regexp "^N\\(.+\\)E"
let nested_std_t  = Str.regexp "^NSt\\(.+\\)E"
let nested_std_a  = Str.regexp "^NSa\\(.+\\)E"
let nested_std_s  = Str.regexp "^NSs\\(.+\\)E"
let strlift    = Str.regexp "^_OC_str\\([0-9]*\\)$"
let templ      = Str.regexp "^I\\(.+\\)E"
let const      = Str.regexp "^K"
let ptr_to     = Str.regexp "^P\\(.+\\)"
let constructor= Str.regexp "^C[1-3]"
let destructor = Str.regexp "^D[0-2]"
let varlift    = Str.regexp "^llvm_cbe_\\(.+\\)$"
let take n x = String.sub x 0 n 
let drop n x = String.sub x n (String.length x - n) 
let appp f (x,y) = f x, y

let op_name x = 
  let op_name = function
    | "nw"  -> "new" (* new   *)
    | "na"  -> "new[]" (* new[] *)
    | "dl"  -> "delete" (* delete         *)
    | "da"  -> "delete[]" (* delete[]       *)
    | "ps"  -> "+" (* + (unary) *)
    | "ng"  -> "-" (* - (unary)      *)
    | "ad"  -> "&" (* & (unary)      *)
    | "de"  -> "*" (* * (unary)      *)
    | "co"  -> "~" (* ~              *)
    | "pl"  -> "+" (* +              *)
    | "mi"  -> "-" (* -              *)
    | "ml"  -> "*" (* *              *)
    | "dv"  -> "/" (* /              *)
    | "rm"  -> "%" (* %              *)
    | "an"  -> "&" (* &              *)
    | "or"  -> "|" (* |              *)
    | "eo"  -> "^" (* ^              *)
    | "aS"  -> "=" (* =              *)
    | "pL"  -> "+=" (* +=             *)
    | "mI"  -> "-=" (* -=             *)
    | "mL"  -> "*=" (* *=             *)
    | "dV"  -> "/=" (* /=             *)
    | "rM"  -> "%=" (* %=             *)
    | "aN"  -> "&=" (* &=             *)
    | "oR"  -> "|=" (* |=             *)
    | "eO"  -> "^=" (* ^=             *)
    | "ls"  -> "<<" (* <<             *)
    | "rs"  -> ">>" (* >>             *)
    | "lS"  -> "<<=" (* <<=            *)
    | "rS"  -> ">>=" (* >>=            *)
    | "eq"  -> "==" (* ==             *)
    | "ne"  -> "!=" (* !=             *)
    | "lt"  -> "<" (* <              *)
    | "gt"  -> ">" (* >              *)
    | "le"  -> "<=" (* <=             *)
    | "ge"  -> ">=" (* >=             *)
    | "nt"  -> "!" (* !              *)
    | "aa"  -> "&&" (* &&             *)
    | "oo"  -> "||" (* ||             *)
    | "pp"  -> "++" (* ++             *)
    | "mm"  -> "--" (* --             *)
    | "cm"  -> "," (* ,              *)
    | "pm"  -> "->*" (* ->*            *)
    | "pt"  -> "->" (* ->             *)
    | "cl"  -> "()" (* ()             *)
    | "ix"  -> "[]" (* []             *)
    | "qu"  -> "?" (* ?              *)
    | "st"  -> "sizeof" (* sizeof (a type) *)
    | "sz"  -> "sizeof" (* sizeof (an expression) *)
    | "at"  -> "alignof" (* alignof (a type) *)
    | "az"  -> "alignof" (* alignof (an expression) *)
    | x -> x 
  (*   | "cv" <type> -> "(cast)" (* (cast)         *)
    | "v" <digit> <source-name> -> "vendor" (* vendor extended operator *)
  *)
  in 
  let on = op_name x in
  if on = x then x else "operator" ^ on

let rec num_p x : name list * string =
  if Str.string_match num_prefix x 0
  then let n = int_of_string (Str.matched_group 1 x) in
       let t = Str.matched_group 2 x in
       let r = drop n t in
       let xs, r = num_p r in
       (Name (take n t) :: xs), r
  else if Str.string_match templ x 0
  then let nn = Str.string_after x (Str.match_end ()) in
       let t,_ = conv (Str.matched_group 1 x) in
       let xs, r = num_p nn in
       (Template t::xs), r
  else if Str.string_match const x 0
  then num_p (Str.string_after x (Str.match_end ())) 
  else if Str.string_match constructor x 0
  then [Cons],Str.string_after x (Str.match_end ()) 
  else if Str.string_match destructor x 0
  then [Dest],Str.string_after x (Str.match_end ()) 
  else if Str.string_match special x 0
  then [Name (op_name x)],Str.string_after x (Str.match_end ()) 
  else ([],x)
  
and conv x : name * string =
  if Str.string_match num_prefix x 0
  then let n = int_of_string (Str.matched_group 1 x) in
        Name (take n (Str.matched_group 2 x)), drop n (Str.matched_group 2 x)
  else if Str.string_match ti_prefix x 0
  then appp (fun x -> TypeFun ("typeinfo", x)) (conv (Str.matched_group 1 x))
  else if Str.string_match tv_prefix x 0
  then appp (fun x -> TypeFun ("v_table", x)) (conv (Str.matched_group 1 x))
  else if Str.string_match ts_prefix x 0
  then appp (fun x -> TypeFun ("typeinfo_name", x)) (conv (Str.matched_group 1 x))
  else if Str.string_match tt_prefix x 0
  then appp (fun x -> TypeFun ("VTT", x)) (conv (Str.matched_group 1 x))
  else if Str.string_match templ x 0
  then let x,y = conv (Str.matched_group 1 x) in 
        appp (fun z -> Nested (Template x, z)) (conv (drop 1 y))
				
  else if Str.string_match nested_std_a x 0 then
		let ps, r = num_p ("9allocator"^(Str.matched_group 1 x)) in
        match List.rev ps with
          | p::ps -> let r = List.fold_left (fun xs x -> Nested (x,xs)) p ps, r in
                       Nested (Name "std",fst r),snd r        
          | _ -> Unknown x, r
  else if Str.string_match nested_std_s x 0
  then let ps, r = num_p ("6string"^(Str.matched_group 1 x)) in
        match List.rev ps with
          | p::ps -> let r = List.fold_left (fun xs x -> Nested (x,xs)) p ps, r in
                       Nested (Name "std",fst r),snd r        
          | _ -> Unknown x, r
  else if Str.string_match nested_std_t x 0
  then let ps, r = num_p (Str.matched_group 1 x) in
        match List.rev ps with
          | p::ps -> let r = List.fold_left (fun xs x -> Nested (x,xs)) p ps, r in
					   Nested (Name "std",fst r),snd r        
          | _ -> Unknown x, r
 
  else if Str.string_match nested x 0
  then let ps, r = num_p (Str.matched_group 1 x) in
        match List.rev ps with
          | p::ps -> List.fold_left (fun xs x -> Nested (x,xs)) p ps, r
          | _ -> Unknown x, r 
  else if Str.string_match ptr_to x 0
  then appp (fun x -> PtrTo x) (conv (Str.matched_group 1 x))
  else if Str.string_match constructor x 0
  then Cons,""
  else if Str.string_match destructor x 0
  then Dest,""
  else if Str.string_match special x 0
  then Name x,""
  else Unknown x, ""    
  

let to_name x = 
  if Str.string_match dem_prefix x 0
  then fst (conv (Str.matched_group 1 x))
  else if Str.string_match strlift x 0
  then Name ("str" ^ Str.matched_group 1 x)
  else if Str.string_match varlift x 0
  then Name (Str.matched_group 1 x)
  else Name x

let get_class x : string option = 
  let rec git tf : name -> string option = function 
    | Cons | Dest | Unknown _ | PtrTo _ | Template _ -> None
		| Name x when tf -> Some x 
		| Name _ -> None
    | TypeFun (x,y) -> git true y (*vtables don't have a function name*)
    | Nested (x,y) -> 
      begin match git tf y with 
        | None ->
					if not tf then begin match x with Name x -> Some x | _ -> None  end
					else begin match y with | Name s -> Some s | _ -> None end
        | x -> x 
      end
  in
  git false (to_name x)

let get_class_and_name x : (string * string) option = 
  let rec git = function 
    | Cons | Dest | Name _ | Unknown _ | PtrTo _ | TypeFun _ | Template _ -> None
    | Nested (Name x,Cons) -> Some (x,x) 
    | Nested (Name x,Dest) -> Some (x,"~"^x)
    | Nested (x,y) -> 
      begin match git y with 
        | None -> begin match x, y with Name x, Name y -> Some (x,y) | _ -> None  end
        | x -> x 
      end
  in
  git (to_name x)
  
let demangle x = 
  let y = to_name x in
(*   Printf.printf "%s -> %s -> %s\n" x (show y) (name_to_string y);   *)
  let res=name_to_string y in
  if res="??" then x else res

let set_timer tsecs =
  ignore (Unix.setitimer Unix.ITIMER_REAL
                         { Unix.it_interval = 0.0; Unix.it_value = tsecs })

exception Timeout

let handle_sigalrm signo = raise Timeout

let timeout f arg tsecs timeout_fn =
  let oldsig = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> timeout_fn ())) in
  set_timer tsecs;
  let res = f arg in
  set_timer 0.0;
  Sys.set_signal Sys.sigalrm oldsig;
  res

