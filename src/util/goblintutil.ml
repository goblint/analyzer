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
                             ;"access"     , Build.bool false
                             ;"thread"     , Build.bool false
                             ;"escape"     , Build.bool true
                             ;"mutex"      , Build.bool true
                             ;"symb_locks" , Build.bool false
                             ;"uninit"     , Build.bool false
                             ;"malloc_null", Build.bool false
                             ;"region"     , Build.bool false
                             ;"containment", Build.bool false
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
                              ;"containment", Build.bool false
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
                             ;"containment", Build.bool true
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
  let anas = ["base";"OSEK";"OSEK2";"access";"thread";"escape";"mutex";"symb_locks";"uninit";"malloc_null";"region";"containment";"var_eq"] in
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

(** Json files that are given as arguments *)
let jsonFiles : string list ref = ref [] 

(** Name of the class to analyse (containment) *)
let mainclass : string ref = ref "" 

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
  | NoOutput (** Do not print any output except warnings *)
  | State (** Only output the state of main function *)
  | Indented (** Output indented XML *)
  | Compact (** Output compact XML, for Eclipse plugin *)
  | Pretty (** Pretty-printed text outpu *)

(** The specified result style *)
let result_style = ref NoOutput

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
    | Cons | Dest | Name _ | Unknown _ | PtrTo _ | Template _ -> None 
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
