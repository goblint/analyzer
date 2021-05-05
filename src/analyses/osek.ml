(** Data race analysis for OSEK programs. *)

open Prelude.Ana
open Analyses
open GobConfig
open Messages

open OilParser
open OilLexer
open OilUtil

module Mutex = MutexAnalysis

module LockingPattern = Exp.LockingPattern


module CF = Cilfacade
module GU = Goblintutil

(* Some helper functions to avoid flagging race warnings on atomic types, and
 * other irrelevant stuff, such as mutexes and functions. *)

let is_atomic_type (t: typ): bool =
  (*  ignore (printf "Type %a\n" (printType plainCilPrinter) t);*)
  match t with
  | TNamed (info, attr) -> info.tname = "atomic_t"
  | TComp (info, attr) -> info.cname = "lock_class_key"
  | _ -> false

let is_atomic lval =
  let (lval, _) = removeOffsetLval lval in
  let typ = typeOfLval lval in
  is_atomic_type typ

let is_ignorable lval =
  (*  ignore (printf "Var %a\n" d_lval lval);*)
  try ValueDomain.Compound.is_immediate_type (Cilfacade.typeOfLval lval) || is_atomic lval
  with Not_found -> false


module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
end

let get_flag (state: (string * Obj.t) list) : Flag.t =
  (Obj.obj (List.assoc "threadflag" state), Obj.obj (List.assoc "threadid" state))


module Spec =
struct

  include Analyses.DefaultSpec

  (*lockset -> priority helper*)
  let names = function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully1) never happens!"
  let resourceset_to_priority = List.fold_left (fun y x -> if (pry x) > y then pry x else y) (min_int)

  let listrem x l = List.filter (fun y -> not( x=y)) l
  let proj3_1 (a,_,_) = a
  let proj3_2 (_,a,_) = a
  let proj3_3 (_,_,a) = a
  let proj2_1 = fst
  let proj2_2 = snd


  (* parsing*)
  let parse_oil () =
    if tracing then trace "osek" "Parsing OIL-file\n";
    let oil_file = get_string "ana.osek.oil" in
    if not (Sys.file_exists oil_file) then failwith "Cannot find oil file!";
    Hashtbl.add resources "RES_SCHEDULER" ("RES_SCHEDULER",-1, make_lock "RES_SCHEDULER");
    Hashtbl.add resources "DisableAllInterrupts" ("DisableAllInterrupts",-1, make_lock "DisableAllInterrupts");
    Hashtbl.add resources "SuspendAllInterrupts" ("SuspendAllInterrupts",-1, make_lock "SuspendAllInterrupts");
    Hashtbl.add resources "SuspendOSInterrupts" ("SuspendOSInterrupts",-1, make_lock "SuspendOSInterrupts");
    match file token (Lexing.from_channel (open_in oil_file)) with
    | [] -> failwith ( "No OIL-Objects found!")
    | objs -> List.iter add_to_table (List.sort compare_objs objs);
      if tracing then trace "osek" "Done parsing OIL-file\n";
      if tracing then trace "osek" "Computing ceiling priorities...\n";
      Hashtbl.iter compute_ceiling_priority resources;
      if tracing then trace "osek" "Generating goblint.h...\n";
      generate_header ();
      if tracing then trace "osek" "Activating autostart alarms...\n";
      finish_alarm_handling ();
      if (get_bool "ana.osek.check") then begin
        if tracing then trace "osek" "Checking conventions...\n";
        check_osek ()
      end;
      if tracing then trace "osek" "Done processing OIL-file\n"

  (*  let parse_tramp tramp =
      if tracing then trace "osek" "Parsing trampolineish header...\n";
      let input = open_in tramp in
      let comment = Str.regexp "//.* \\|/\\*.*" in
      let re = Str.regexp ".*resource_id_of_\\([a-zA-Z][a-zA-Z0-9_]*\\) +\\([0-9]+\\) *" in
      let ev = Str.regexp ".*event_id_of_\\([a-zA-Z][a-zA-Z0-9_]*\\) +\\([0-9]+\\) *" in
      let rec read_info () = try
        let line = input_line input in
      (* 	if tracing then trace "osek" "Line: %s\n" line; *)
      	if (Str.string_match comment line 0) then begin
      	  if tracing then trace "osek" "Trampolineish: Skipping (JUST 1!) line: %s\n" line;
      	end else begin
      	  if Str.string_match re line 0 then begin
      	    let name = (Str.matched_group 1 line) in
      	    let id = Str.matched_group 2 line in
      	    if tracing then trace "osek" "Adding id (%s) for resource %s\n" id name;
      	    let _ = try
      	      let (_,p,l) = Hashtbl.find resources name in
      	      Hashtbl.replace resources name (id,p,l)
      	    with
      	      | Not_found -> print_endline ("Error: Resource " ^ name ^ " not found. ID not added.")
      	      | e -> raise e
      	    in ()
      	  end;
      	  if Str.string_match ev line 0 then begin
      	  let name = (Str.matched_group 1 line) in
      	    let id = Str.matched_group 2 line in
      	    if tracing then trace "osek" "Adding id (%s) for event %s\n" id name;
      	    let _ = try
      	      let (_,b) = Hashtbl.find events name in
      	      Hashtbl.replace events name (id,b);
      	    with
      	      | Not_found -> print_endline ("Error: Event " ^ name ^ " not found. ID not added.")
      	      | e -> raise e
      	    in ()
      	  end;
      	end;
      	read_info ();
        with
      	| End_of_file -> ()
      	| e -> raise e
      in read_info ();
      if (get_bool "ana.osek.check") then check_tramp ();
      if tracing then trace "osek" "Done parsing trampolineish header\n";
      close_in input*)

  let parse_names names =
    if tracing then trace "osek" "Parsing API (re)names...\n";
    let input = open_in names in
    let comment = Str.regexp "//.* \\|/\\*.*" in
    let newname = Str.regexp " *\\(#define \\| *\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\) +\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
    let rec read_info () = try
        let line = input_line input in
        if tracing then trace "osek" "Line: %s\n" line;
        if (Str.string_match comment line 0) then begin
          if tracing then trace "osek" "API names: Skipping (JUST 1!) line: %s\n" line;
        end else begin
          if Str.string_match newname line 0 then begin
            let newname = (Str.matched_group 3 line) in
            let oldname = (Str.matched_group 2 line) in
            if tracing then trace "osek" "Adding newname (%s) for function %s\n" newname oldname;
            Hashtbl.add osek_names newname oldname;
            LibraryFunctions.add_lib_funs [newname]
          end;
        end;
        read_info ();
      with
      | End_of_file -> ()
      | e -> raise e
    in read_info ();
    LibraryFunctions.osek_renames := true;
    if tracing then trace "osek" "Done parsing API (re)names\n";
    close_in input

  let parse_ids ids =
    if tracing then trace "osek" "Parsing IDs...\n";
    let input = open_in ids in
    let comment = Str.regexp "//.* \\|/\\*.*" in
    let idregex = Str.regexp " *#define +\\([a-zA-Z_][a-zA-Z0-9_]*\\) +[^1-9]*\\([1-9][0-9]*\\|0\\)\\()\\| \\)*" in
    let rec read_info () = try
        let line = input_line input in
        if tracing then trace "osek" "Line: %s\n" line;
        if (Str.string_match comment line 0) then begin
          if tracing then trace "osek" ": Skipping (JUST 1!) line: %s\n" line;
        end else begin
          if Str.string_match idregex line 0 then begin
            let objectname = (Str.matched_group 1 line) in
            let id = (Str.matched_group 2 line) in
            match objectname with
            | x when Hashtbl.mem tasks (make_task objectname) -> begin
                if tracing then trace "osek" "Adding ID (%s) for task %s\n" id objectname;
                let intid = int_of_string id in
                Hashtbl.add taskids (Cil.integer(intid)) (make_task objectname)
              end
            | x when Hashtbl.mem isrs (make_isr objectname) -> begin
                if tracing then trace "osek" "Adding ID (%s) for isr %s\n" id objectname;
                let intid = int_of_string id in
                Hashtbl.add isrids (Cil.integer(intid)) (make_isr objectname)
              end
            | x when Hashtbl.mem resources objectname -> begin
                if tracing then trace "osek" "Adding ID (%s) for resource %s\n" id objectname;
                let intid = int_of_string id in
                Hashtbl.add resourceids (Cil.integer(intid)) objectname
              end
            | x when Hashtbl.mem spinlocks objectname -> begin
                if tracing then trace "osek" "Adding ID (%s) for spinlock %s\n" id objectname;
                let intid = int_of_string id in
                Hashtbl.add spinlockids (Cil.integer(intid)) objectname
              end
            | x when Hashtbl.mem events objectname -> begin
                if tracing then trace "osek" "Adding ID (%s) for event %s\n" id objectname;
                let intid = int_of_string id in
                Hashtbl.add eventids (Cil.integer(intid)) objectname
              end
            | x when Hashtbl.mem alarms objectname -> begin
                if tracing then trace "osek" "Adding ID (%s) for alarm %s\n" id objectname;
                let intid = int_of_string id in
                Hashtbl.add alarmids (Cil.integer(intid)) objectname
              end
            | _ -> if tracing then trace "osek" "No matching object found for %s\n" objectname
          end;
        end;
        read_info ();
      with
      | End_of_file -> ()
      | e -> raise e
    in read_info ();
    if tracing then trace "osek" "Done parsing IDs\n";
    close_in input




  module MyParam =
  struct
    module G = LockDomain.Priorities
    let effect_fun ?write:(w=false) (ls: LockDomain.Lockset.t) =
      let locks = LockDomain.Lockset.ReverseAddrSet.elements ls in
      let prys = List.map names locks in
      let staticprys = List.filter is_task_res prys in
      let pry = resourceset_to_priority staticprys in
      if pry = min_int then `Bot else `Lifted (Int64.of_int pry)
    let check_fun = effect_fun
  end

  module M = Mutex.MakeSpec (MyParam)
  module Offs = ValueDomain.Offs
  module Lockset = LockDomain.Lockset

  module Flags = FlagModes.Spec.D
  module Acc = Hashtbl.Make (Basetype.Variables)
  module AccKeySet = Set.Make (Basetype.Variables)
  module AccLoc = Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (Flag) (IntDomain.Booleans)) (Lockset) (Offs)
  module AccValSet = Set.Make (Printable.Prod (AccLoc) (Flags))
  let acc     : AccValSet.t Acc.t = Acc.create 100
  let accKeys : AccKeySet.t ref   = ref AccKeySet.empty

  module D = M.D
  module C = M.C
  module G = M.G

  let offensivepriorities : (string,int) Hashtbl.t= Hashtbl.create 16
  let off_pry_with_flag : (string,(Flags.t*int) list) Hashtbl.t = Hashtbl.create 16

  (* task resource handling *)
  let dummy_release f = Goblintutil.create_var (makeLocalVar f ?insert:(Some false) "ReleaseResource" voidType)
  let dummy_get f = Goblintutil.create_var (makeLocalVar f ?insert:(Some false) "GetResource" voidType)
  let is_task_res' lock = is_task_res (names lock)
  let partition = D.ReverseAddrSet.partition is_task_res'
  let lockset_to_task lockset =
    match D.ReverseAddrSet.elements lockset with
    | [x] -> names x
    | _ -> "???"
  let mem name lockset =
    let vinfo = match name with
      |AddrOf(Var v,_) -> v
      | _ ->  failwith "Impossible resource!"
    in
    let res_addr = LockDomain.Addr.from_var vinfo in
    D.ReverseAddrSet.mem (res_addr,true) lockset
  (*/task resource handling *)

  (* flag stuff*)
  let strip_tags y = match y with `Lifted x -> x | _ -> failwith "top/bot flags not properly filtered osek234"

  let get_val flag acc =
    (* let _ = print_endline (flag.vname ^"A" ^gl.vname) in     *)
    proj3_3 (strip_tags (Flags.find flag (proj2_2 acc)))
  let get_eq flag acc : bool =
    (* let _ = print_endline (flag.vname ^"B"^gl.vname) in     *)
    proj3_2 (strip_tags (Flags.find flag (proj2_2 acc)))
  let get_method flag acc : bool =
    (* let _ = print_endline (flag.vname ^ "C"^gl.vname) in     *)
    proj3_1 (strip_tags (Flags.find flag (proj2_2 acc)))
  let flag_unknown flag accs = (not (Flags.mem flag (proj2_2 accs)))
  let flag_top flag accs =  (Flags.find flag (proj2_2 accs) = `Top)
  (*   let flag_exists flag accs = List.filter (fun x -> (Flags.mem flag (proj2_2 x))) accs *)
  let rec flag_list_to_string l = match l with
    | []  -> ""
    | [x] -> x.vname
    | x::xs -> x.vname ^ " and " ^ (flag_list_to_string xs)
  let filter_bot flag accs =
    let doit flag acc =  not (( Flags.find flag (proj2_2 acc)) = `Bot) in
    List.filter (doit flag) accs
  (*  let check_top flag accs = (*true if top exist in list*)
      let doit f init acc =
          let res = match Flags.find f (proj2_2 acc) with
            | `Top -> true
            | _ -> false
          in
        init || res
      in
      List.fold_left (doit flag) false accs*)
  let split_accs_top flag accs =
    List.partition (flag_top flag) accs
  let split_accs_method flag accs =
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let ts,fs = List.partition (get_method flag) accs' in
    (ts@unknowns, fs@unknowns)
  let split_accs flag accs =
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let ts,fs = List.partition (get_eq flag) accs' in
    (ts@unknowns, fs@unknowns)
  let split_equals flag accs =
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let rec doit flag lists elem = match lists with
      | [] -> [[elem]]
      | x::xs -> if (get_val flag elem) = (get_val flag (List.hd x)) then ([elem]@x)::xs else x::(doit flag xs elem)
    in
    let vals = List.fold_left (doit flag) [] accs'
    in List.map (fun x -> x@unknowns) vals

  let split_may_eq flag value accs =
    let unknowns,accs' = List.partition (flag_unknown flag) accs in
    let doit value acc =
      let acc_value = get_val flag acc in
      let acc_eq = get_eq flag acc in
      (*let f (Const (CInt64 (x,_,_) )) = Int64.to_string x in
        let _ = print_endline ("may equal " ^ (f value)) in
        let _ = print_endline (string_of_bool acc_eq) in
        let _ = print_endline (f acc_value) in
        let res =*)
      ( ( (acc_value = value) && acc_eq ) || ( (not acc_eq) && not (acc_value = value) ) )
      (*in
        let _ = print_endline (string_of_bool res) in res*)
    in
    (List.filter (doit value) accs')@unknowns

  let strip_flags acc_list = List.map proj2_1 acc_list

  let get_flags state: Flags.t =
    Obj.obj (List.assoc "fmode" state)

  (*/flagstuff*)
  (*prioritystuff*)
  (*   let one_pry acc = resourceset_to_priority (List.filter is_task ((fun (_, dom_elem,_) -> (Lockset.ReverseAddrSet.elements dom_elem) ) acc)) *)

  let just_locks acc_list = List.map (fun (_, dom_elem,_) -> (Lockset.ReverseAddrSet.elements dom_elem) ) acc_list
  let prys acc_list = List.map (List.map names) (just_locks acc_list)
  let staticprys acc_list = List.map (List.filter is_task_res) acc_list
  let offprys acc_list = List.map resourceset_to_priority (staticprys (prys acc_list))
  let accprys acc_list = List.map resourceset_to_priority (prys acc_list)
  let maxpry acc_list = List.fold_left (fun y x -> if x > y then x else y) (min_int) (accprys acc_list)
  let minpry acc_list = List.fold_left (fun y x -> if x < y then x else y) (max_int) (accprys acc_list)
  let offpry acc_list = List.fold_left (fun y x -> if x > y then x else y) (min_int) (offprys acc_list)
  let minoffpry acc_list = List.fold_left (fun y x -> if x < y then x else y) (max_int) (offprys acc_list)
  (*/prioritystuff*)

  let offpry_flags (flagstate : Flags.t) (varinfo : Cil.varinfo) : int =
    let var = varinfo.vname in
    if tracing then trace "osek" "Computing flag priority for %s\n" var;

    let helper flag flag_value current =
      if tracing then trace "osek" "Using flag %s\n" flag.vname;
      let equal,value = match flag_value with
        | `Lifted (_,equal,value) -> equal,value
        | _  -> failwith "this (hopefully) never happens osek318"
      in

      let doit (acc_flagstate,pry) =
        let acc_equal,acc_value =
          if Flags.mem flag acc_flagstate then
            match Flags.find flag acc_flagstate with
            | `Lifted (_,equal,value) -> equal,value
            | _  -> failwith "this (hopefully) never happens osek325"
          else
            (false,0L)
        in
        if (((acc_value = value) && (not acc_equal = equal) ) || ( (acc_equal = equal) && not (acc_value = value) ) ) then
          pry
        else
          min_int
      in

      let acc_info : (Flags.t*int) list = if Hashtbl.mem off_pry_with_flag var then
          Hashtbl.find off_pry_with_flag var
        else begin
          if tracing then trace "osek" "Empty access information when computing flag offensive priority for variable %s\n" var;
          []
        end
      in

      let flag_prys = List.map doit acc_info in
      List.fold_left (fun y x -> if x > y then x else y) (current) flag_prys
    in
    Flags.fold helper flagstate min_int

  (* from thread*)
  let query_lv (ask: Queries.ask) exp =
    match ask.f (Queries.MayPointTo exp) with
    | `LvalSet l when not (Queries.LS.is_top l) ->
      Queries.LS.elements l
    | _ -> []

  let eval_fv ask exp =
    match query_lv ask exp with
    | [(v,_)] -> Some v (* This currently assumes that there is a single possible l-value. *)
    | _ -> None

  let eval_arg ctx (arg:exp) =
    match arg with
    | Lval (Var vinfo,_) -> vinfo
    | _ -> (match eval_fv ctx.ask arg with
        | Some v -> v
        | None   -> failwith "cannot extract arg")

  (* from mutex *)

  (* Just adds accesses. It says concrete, but we use it to add verified
     non-concrete accesses too.*)
  let add_concrete_access ctx fl loc ust (flagstate : Flags.t) (v, o, rv: Cil.varinfo * Offs.t * bool) =
    let ign_flag_filter acc tuple = not (AccLoc.equal acc (proj2_1 tuple)) in
    let remove_acc acc set = AccValSet.filter (ign_flag_filter acc) set in
    if (BaseUtil.is_global ctx.ask v) then begin
      if not (is_task v.vname) || flagstate = Flags.top() then begin
        if !GU.should_warn then begin
          let new_acc = ((loc,fl,rv),ust,o) in
          let curr : AccValSet.t = try Acc.find acc v with _ -> AccValSet.empty in
          let neww : AccValSet.t = AccValSet.add (new_acc,flagstate) (remove_acc new_acc curr) in
          Acc.replace acc v neww;
          accKeys := AccKeySet.add v !accKeys;
          let curr = try Hashtbl.find off_pry_with_flag v.vname with _ -> [] in
          let pry = offpry [new_acc] in
          Hashtbl.replace off_pry_with_flag v.vname ((flagstate,pry)::curr)
        end ;
        let ls = if rv then Lockset.filter proj2_2 ust else ust in
        let el = MyParam.effect_fun ls in
        (*       (if LockDomain.Mutexes.is_empty el then Messages.waitWhat ("Race on "^v.vname)); *)
        (*      let _ = printf "Access to %s with offense priority %a\n" v.vname P.Glob.Val.pretty el in*)
        ctx.sideg v el
      end else begin
        if tracing then trace "osek" "Ignoring access to task/isr variable %s\n" v.vname;
      end
    end

  let add_per_element_access a b c d =
    (* print_endline "Per element access not supported.";  *)
    false

  (* Type invariant variables. *)
  let type_inv_tbl = Hashtbl.create 13
  let type_inv (c:compinfo) : Lval.CilLval.t list =
    try [Hashtbl.find type_inv_tbl c.ckey,`NoOffset]
    with Not_found ->
      let i = Goblintutil.create_var (makeGlobalVar ("(struct "^c.cname^")") (TComp (c,[]))) in
      Hashtbl.add type_inv_tbl c.ckey i;
      [i, `NoOffset]

  (* Try to find a suitable type invariant --- and by that we mean a struct. *)
  let best_type_inv exs : (varinfo * Offs.t) option =
    let add_el es e : LockingPattern.ee list list =
      try LockingPattern.toEl e :: es
      with LockingPattern.NotSimpleEnough -> es
    in
    let full_els = List.fold_left add_el [] exs in
    let el_os = List.map LockingPattern.strip_fields full_els in
    (*     let dummy = integer 42 in *)
    let add_struct xs (e,fs) =
      match fs with
      | LockingPattern.EField f :: _ -> (e,f.fcomp,fs) :: xs
      | _ -> xs
      (*      match unrollType (typeOf (LockingPattern.fromEl e dummy)) with
              | TComp (c,_) -> (e,c,fs) :: xs
              | _ -> xs*)
    in
    try
      let es, c, fs = List.hd (List.fold_left add_struct [] el_os) in
      let e_inv = type_inv c in
      Some (fst (List.hd e_inv), Offs.from_offset (LockingPattern.ees_to_offs fs))
    with
    | LockingPattern.NotSimpleEnough -> None
    | Failure _ -> None

  let unknown_access () =
    (*M.report "unknown access 'with lockset:'";*)
    Messages.warn_all "Access to unknown address could be global"

  (* All else must have failed --- making a last ditch effort to generate type
      invariant if that fails then give up and become unsound. *)
  let add_type_access ctx fl loc ust flagstate (e,rw:exp * bool) =
    let eqset =
      match ctx.ask.f (Queries.EqualSet e) with
      | `ExprSet es
        when not (Queries.ES.is_bot es)
        -> Queries.ES.elements es
      | _ -> [e]
    in
    match best_type_inv eqset with
    | Some (v,o) -> add_concrete_access ctx fl loc ust flagstate (v,o,rw)
    | _ -> unknown_access ()

  type access = Concrete of (exp option * varinfo * Offs.t * bool)
              | Region   of (exp option * varinfo * Offs.t * bool)
              | Unknown  of (exp * bool)
  type accesses = access list

  let struct_type_inv (v:varinfo) (o:Offs.t) : (varinfo * Offs.t) option =
    let rec append os = function
      | `NoOffset    -> os
      | `Field (f,o) -> `Field (f,append o os)
      | `Index (i,o) -> `Index (i,append o os)
    in
    let replace_struct t (v,o) =
      begin match t with
        | TComp (c,_) when c.cstruct ->
          begin match type_inv c with
            | [(v,_)] -> (v,`NoOffset)
            | _   -> (v,o)
          end
        | _ -> (v,o)
      end
    in
    let rec get_lv t (v,u) = function
      | `NoOffset    -> (v,u)
      | `Field (f,o) -> get_lv f.ftype (replace_struct f.ftype (v, append (`Field (f,`NoOffset)) u)) o
      | `Index (i,o) ->
        begin match unrollType t with
          | TPtr (t,_)     -> get_lv t (replace_struct t (v, append (`Index (i,`NoOffset)) u)) o
          | TArray (t,_,_) -> get_lv t (replace_struct t (v, append (`Index (i,`NoOffset)) u)) o
          | _ -> raise Not_found
        end
    in
    match Offs.to_offset o with
    | [o] ->
      begin try
          let a,b = (get_lv (v.vtype) (replace_struct v.vtype (v,`NoOffset)) o) in
          Some (a, Offs.from_offset b)
        with Not_found -> None end
    | _ -> None

  let add_accesses ctx (accessed: accesses) (flagstate: Flags.t) (ust:D.t) =
    let fl = get_flag ctx.presub in
    if Flag.is_multi fl then
      let loc = !Tracing.current_loc in
      let dispatch ax =
        match ax with
        | Concrete (me,v,o,rw) ->
          begin match me with
            | Some e ->
              if   not (add_per_element_access ctx loc ust (e,rw))
              then add_concrete_access ctx fl loc ust flagstate (v,o,rw)
            | None ->
              add_concrete_access ctx fl loc ust flagstate (v,o,rw)
          end
        | Region (Some e,v,o,rw) ->
          if   not (add_per_element_access ctx loc ust (e,rw))
          then add_concrete_access ctx fl loc ust flagstate (v,o,rw)
        | Region (None,v,o,rw) ->
          add_concrete_access ctx fl loc ust flagstate (v,o,rw)
        | Unknown a ->
          if   not (add_per_element_access ctx loc ust a)
          then add_type_access ctx fl loc ust flagstate a
      in
      List.iter dispatch accessed

  let query ctx = { Queries.f = fun (type a) (q: a Queries.t) ->
    match q with
    | Queries.Priority "" ->
      let pry = resourceset_to_priority (List.map names (Mutex.Lockset.ReverseAddrSet.elements ctx.local)) in
      `Int (Int64.of_int pry)
    | Queries.Priority vname -> begin try `Int (Int64.of_int (Hashtbl.find offensivepriorities vname) ) with _ -> Queries.Result.top() end
    | Queries.MayBePublic {global=v; _} ->
      let pry = resourceset_to_priority (List.map names (Mutex.Lockset.ReverseAddrSet.elements ctx.local)) in
      if pry = min_int then
        `MayBool false
      else
        let off =
          (*         if !FlagModes.Spec.flag_list = [] then begin *)
          match (ctx.global v: G.t) with
          | `Bot -> min_int
          | `Lifted i -> Int64.to_int i
          | `Top -> max_int
          (*           end else begin *)
          (*             let flagstate = get_flags ctx.presub in *)
          (*             offpry_flags flagstate v *)
          (*           end *)
        in `MayBool (off > pry)
    | Queries.CurrentLockset -> (* delegate for MinePriv *)
      (* TODO: delegate other queries? *)
      (M.query ctx).f q
    | _ -> Queries.Result.top ()
    }

  let rec conv_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt64 (i,ik,s)),o) -> `Index (IntDomain.of_const (i,ik,s), conv_offset o)
    | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  let rec conv_const_offset x =
    match x with
    | NoOffset    -> `NoOffset
    | Index (Const (CInt64 (i,ik,s)),o) -> `Index (IntDomain.of_const (i,ik,s), conv_const_offset o)
    | Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_const_offset o)
    | Field (f,o) -> `Field (f, conv_const_offset o)

  let rec replace_elem (v,o) q ex =
    match ex with
    | AddrOf  (Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
    | StartOf (Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
    | Lval    (Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
    | CastE (_,e)           -> replace_elem (v,o) q e
    | _ -> v, Offs.from_offset (conv_offset o)


  let access_address (ask: Queries.ask) regs write lv : accesses =
    if is_ignorable lv then [] else
      let add_reg (v,o) =
        (*       Messages.report ("Region: "^(sprint 80 (d_lval () lv))^" = "^v.vname^(Offs.short 80 (Offs.from_offset (conv_offset o)))); *)
        Region (Some (Lval lv), v, Offs.from_offset (conv_offset o), write)
      in
      match ask.f (Queries.MayPointTo (mkAddrOf lv)) with
      | `LvalSet a when not (Queries.LS.is_top a) ->
        let to_accs (v,o) xs =
          Concrete (Some (Lval lv), v, Offs.from_offset (conv_offset o), write) :: xs
        in
        if List.length regs = 0 then begin
          if Queries.LS.mem (dummyFunDec.svar,`NoOffset) a
          then [Unknown (Lval lv,write)]
               @ Queries.LS.fold to_accs (Queries.LS.remove (dummyFunDec.svar,`NoOffset) a) []
          else Queries.LS.fold to_accs a []
        end else List.map add_reg regs
      | _ ->
        if List.length regs = 0
        then [Unknown (Lval lv,write)]
        else List.map add_reg regs

  let rec access_one_byval a rw (exp:exp): accesses  =
    let accs regs =
      match exp with
      (* Integer literals *)
      | Const _ -> []
      (* Variables and address expressions *)
      | Lval lval ->
        let a1 = access_address a regs rw lval in
        let a2 = access_lv_byval a lval in
        a1 @  a2
      (* Binary operators *)
      | BinOp (op,arg1,arg2,typ) ->
        let a1 = access_one_byval a rw arg1 in
        let a2 = access_one_byval a rw arg2 in
        a1 @ a2
      (* Unary operators *)
      | UnOp (op,arg1,typ) -> access_one_byval a rw arg1
      (* The address operators, we just check the accesses under them *)
      | AddrOf lval -> access_lv_byval a lval
      | StartOf lval -> access_lv_byval a lval
      (* Most casts are currently just ignored, that's probably not a good idea! *)
      | CastE  (t, exp) -> access_one_byval a rw exp
      | _ -> []
    in
    (*    let is_unknown x = match x with Unknown _ -> true | _ -> false in*)
    match a.f (Queries.Regions exp) with
    | `Bot ->
      (*          Messages.report ((sprint 80 (d_exp () exp))^" is thread local"); *)
      [] (*List.filter is_unknown (accs [])*)
    | `LvalSet regs ->
      (*           Messages.report ((sprint 80 (d_exp () exp))^" is in regions "^Queries.LS.short 800 regs); *)
      accs (Queries.LS.elements regs)
    | _ -> accs []
  (* Accesses during the evaluation of an lval, not the lval itself! *)
  and access_lv_byval a (lval:lval): accesses =
    let rec access_offset (ofs: offset): accesses =
      match ofs with
      | NoOffset -> []
      | Field (fld, ofs) -> access_offset ofs
      | Index (exp, ofs) ->
        let a1 = access_one_byval a false exp in
        let a2 = access_offset ofs in
        a1 @ a2
    in
    match lval with
    | Var x, ofs -> access_offset ofs
    | Mem n, ofs ->
      let a1 = access_one_byval a false n in
      let a2 = access_offset ofs in
      a1 @ a2

  let access_one_top = access_one_byval

  let access_byval a (rw: bool) (exps: exp list): accesses =
    List.concat (List.map (access_one_top a rw) exps)

  let access_reachable ask (exps: exp list) =
    (* Find the addresses reachable from some expression, and assume that these
     * can all be written to. *)
    let do_exp e =
      match ask (Queries.ReachableFrom e) with
      | `LvalSet a when not (Queries.LS.is_top a)
                     && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
        let to_extra (v,o) xs =
          if is_ignorable (Var v, Lval.CilLval.to_ciloffs o) then xs else
            Concrete (None, v, Base.Offs.from_offset (conv_offset o), true) :: xs  in
        Queries.LS.fold to_extra a []
      | `Bot -> []
      (* Ignore soundness warnings, as invalidation proper will raise them. *)
      | _ -> [Unknown (e,true)]
    in
    List.concat (List.map do_exp exps)

  let startstate v = D.top ()
  let exitstate  v = D.top ()

  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local

  let activate_task ctx (task_name : string) : unit =
    let task = Cilfacade.getFun task_name in
    ctx.spawn None task.svar []

  (* transfer functions *)
  let intrpt ctx : D.t =
    ctx.local (*currently ignored*)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    (*     if tracing then trace "osek" "ASSIGN\n"; *)
    if !GU.global_initialization then
      ctx.local
    else
      let b1 = access_one_byval ctx.ask true (Lval lval) in
      let b2 = access_one_byval ctx.ask false rval in
      add_accesses ctx (b1@b2) (get_flags ctx.presub) ctx.local;
      ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    let accessed = access_one_top ctx.ask false exp in
    add_accesses ctx accessed (get_flags ctx.presub) ctx.local;
    ctx.local

  let body ctx (f:fundec) : D.t =
    if tracing then trace "osek" "Analyzing function %s\n" f.svar.vname;
    let m_st = ctx.local in
    if (is_task f.svar.vname) then begin
      (* print_endline ( (string_of_int !Goblintutil.current_loc.line)  ^ " in " ^ !Goblintutil.current_loc.file); *)
      (* print_endline ( "Looking for " ^ f.svar.vname); *)
      M.special (swap_st ctx m_st) None (dummy_get f) [get_lock (trim f.svar.vname)]
    end else
      m_st

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let m_st = match exp with
      | Some exp -> begin
          let accessed = access_one_top ctx.ask false exp in
          add_accesses ctx accessed (get_flags ctx.presub) ctx.local;
          ctx.local
        end
      | None -> ctx.local
    in
    let fname = f.svar.vname in
    if (is_task fname) then begin
      (* let _ = print_endline ( "Leaving task " ^ f.svar.vname) in *)
      let x = M.special (swap_st ctx m_st) None (dummy_release f) [get_lock (trim fname)] in
      if (get_bool "ana.osek.check") && not(List.mem fname !warned) && not(D.is_empty x) then begin
        warned := fname :: !warned;
        let typ = if (Hashtbl.mem isrs fname) then "Interrupt " else "Task " in
        let res = List.fold_left (fun rs r -> (names r) ^ ", " ^ rs) "" (D.ReverseAddrSet.elements x) in
        print_endline( typ ^ fname ^ " terminated while holding resource(s) " ^ res)
      end;
      x
    end else
      m_st

  let eval_funvar ctx exp =
    let read = access_one_top ctx.ask false exp in
    add_accesses ctx read ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (M.enter ctx (lval: lval option) (f:varinfo) (args:exp list))

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    M.combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let fvname = get_api_names f.vname in
    if tracing then trace "osek" "special '%s'\n" fvname;
    match fvname with (* suppress all fails *)
    | "GetResource" | "ReleaseResource" -> if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      M.special ctx lval f (match arglist with
          | [Lval (Var info,_)] -> [get_lock info.vname]
          | [CastE (_, Const c ) | Const c] ->
            if tracing then trace "osek" "Looking up Resource-ID %a\n" d_const c;
            let name = Hashtbl.find resourceids (Const c) in
            [get_lock name]
          | x -> x)
    | "GetSpinlock" | "ReleaseSpinlock" ->
      M.special ctx lval f (match arglist with
          | [Lval (Var info,_)] -> [get_spinlock info.vname]
          | [CastE (_, Const c ) | Const c] ->
            if tracing then trace "osek" "Looking up Spinlock-ID %a\n" d_const c;
            let name = Hashtbl.find spinlockids (Const c) in
            [get_spinlock name]
          | x -> x)
    | "DisableAllInterrupts" -> let res = get_lock "DisableAllInterrupts" in
      if get_bool "ana.osek.check" && mem res ctx.local then print_endline "Nested calls of DisableAllInterrupts are not allowed!";
      M.special ctx lval (dummy_get (emptyFunction fvname)) [res]
    | "EnableAllInterrupts" -> M.special ctx lval (dummy_release (emptyFunction fvname)) [get_lock "DisableAllInterrupts"]
    | "SuspendAllInterrupts" -> M.special ctx lval (dummy_get (emptyFunction fvname)) [get_lock "SuspendAllInterrupts"]
    | "ResumeAllInterrupts" -> M.special ctx lval (dummy_release (emptyFunction fvname)) [get_lock "SuspendAllInterrupts"]
    | "SuspendOSInterrupts" -> M.special ctx lval (dummy_get (emptyFunction fvname)) [get_lock "SuspendOSInterrupts"]
    | "ResumeOSInterrupts" -> M.special ctx lval (dummy_release (emptyFunction fvname)) [get_lock "SuspendOSInterrupts"]
    | "ActivateTask" -> if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      let _ = match arglist with
        | [arg] ->
          let task_name' = match arg with
            | CastE (_, Const c ) | Const c ->
              if tracing then trace "osek" "Looking up Task-ID %a\n" d_const c;
              Hashtbl.find taskids (Const c)
            | _ -> let vinfo = eval_arg ctx arg in vinfo.vname
          in
          let task_name = make_task task_name' in
          if (is_task task_name) then (
            if tracing then tracel "osek" "Activating task %s\n" task_name';
            activate_task ctx task_name;
            [ctx.local, integer 1, true]
          ) else failwith (task_name' ^ " is not a task!")
        | _  -> failwith "ActivateTask arguments are strange"
      in
      M.special ctx lval f arglist
    | "ChainTask" ->
      print_endline "Found ChainTask!";
      if (get_bool "ana.osek.check") then check_api_use 2 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      M.special ctx lval f arglist
    | "WaitEvent" ->
      if (get_bool "ana.osek.check") then (
        check_api_use 2 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
        let static, regular = partition ctx.local in
        let task = lockset_to_task static in
        if not (D.is_empty regular) then (
          let res = List.fold_left (fun rs r -> (names r) ^ ", " ^ rs) "" (D.ReverseAddrSet.elements regular) in
          let ev = match arglist with
            | [CastE (_, Const (CInt64 (c,_,_))) | Const (CInt64 (c,_,_))] ->
              fst @@ Hashtbl.find events (Int64.to_string c)
            | _ -> print_endline "No event found for argument of WaitEvent"; "_not_found_"
          in
          print_endline (task ^ " waited for event "^ ev ^ " while holding resource(s) " ^ res)
        );
      );
      M.special ctx lval f arglist
    | "SetRelAlarm"
    | "SetAbsAlarm" -> if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      (match arglist with
       | [x;_;_] -> (
           let alarm = match x with
             | CastE (_, Const c ) | Const c ->
               if tracing then trace "osek" "Looking up Alarm-ID %a\n" d_const c;
               Hashtbl.find alarmids (Const c)
             | _ -> let vinfo = eval_arg ctx x in vinfo.vname
           in
           if (Hashtbl.mem alarms alarm) then (
             let (x,task_list) = Hashtbl.find alarms alarm in
             List.iter (activate_task ctx) task_list
           ) else
             failwith (alarm ^ "is not an alarm!")
         )
       | _  -> failwith "SetAlarm arguments are strange"
      );
      M.special ctx lval f arglist
    | "SetEvent"
    | "GetTaskID"
    | "GetTaskState"
    | "GetEvent"
    | "GetAlarmBase"
    | "GetAlarm"
    | "CancelAlarm"
    | "GetActiveApplicationMode"
    | "ShutdownOS" -> if (get_bool "ana.osek.check") then check_api_use 1 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      M.special ctx lval f arglist
    | "ClearEvent"
    | "TerminateTask"
    | "Schedule" -> if (get_bool "ana.osek.check") then check_api_use 2 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      M.special ctx lval f arglist
    | "StartOS" -> if (get_bool "ana.osek.check") then check_api_use 0 fvname (lockset_to_task (proj2_1 (partition ctx.local)));
      M.special ctx lval f arglist
    | _ -> M.special ctx lval f arglist
  (* with | _ -> M.special ctx lval f arglist (* suppress all fails  *) *)

  let name () = "OSEK"
  let es_to_string f _ = f.svar.vname

  let should_join x y = D.equal x y

  (** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true
  (*   let bad_flags = ref [] *)

  type access_status =
    | Race
    | Guarded of  Mutex.Lockset.t
    | Priority of int
    | Defence of int*int
    | Flag of string
    | ReadOnly
    | ThreadLocal
    | HighRead
    | HighWrite
    | LowRead
    | LowWrite
    | BadFlag
    | GoodFlag
    | SomeFlag of int

  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsMap = Map.Make (Offs)
  module OffsSet = Set.Make (Offs)

  let get_acc_map gl =
    let create_map (accesses_map) =
      let f ((((_, _, rw), _, offs),_) as accs) (map,set) =
        if OffsMap.mem offs map
        then (OffsMap.add offs ([accs] @ (OffsMap.find offs map)) map,
              OffsSet.add offs set)
        else (OffsMap.add offs [accs] map,
              OffsSet.add offs set)
      in
      AccValSet.fold f accesses_map (OffsMap.empty, OffsSet.empty)
    in
    let acc = Acc.find acc gl in
    let acc_info = create_map acc in
    proj2_1 acc_info

  let suppressed = ref 0
  let filtered = ref 0

  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc gl =
    let is_no_glob (gl:varinfo) = (match gl.vtype with TFun _ -> true | _ -> false )in
    if is_no_glob gl then () else
      (* create mapping from offset to access list; set of offsets  *)
      let get_common_locks acc_list =
        let f locks ((_,_,writing), lock, _) =
          let lock =
            if writing then
              (* when writing: ignore reader locks *)
              Lockset.filter proj2_2 lock
            else
              (* when reading: bump reader locks to exclusive as they protect reads *)
              Lockset.map (fun (x,_) -> (x,true)) lock
          in
          Lockset.join locks lock
        in
        List.fold_left f (Lockset.bot ()) acc_list
      in
      let rw ((_,_,x),_,_) = x in
      let non_main ((_,x,_),_,_) = Flag.is_bad x in
      let is_race_no_flags acc_list =
        let offpry = offpry acc_list in
        let minpry = minpry acc_list in
        let maxpry = maxpry acc_list in
        let var_str = gl.vname in
        (* let _ = print_endline ("Var: " ^var_str) in *)
        (* let _ = print_endline ("Offpry " ^ (string_of_int offpry)) in *)
        (* let _ = print_endline ("minpry " ^ (string_of_int minpry)) in *)
        (* let _ = print_endline ("maxpry " ^ (string_of_int maxpry)) in*)
        let _ = Hashtbl.add offensivepriorities var_str offpry in
        let locks = get_common_locks acc_list in
        if not (Lockset.is_empty locks || Lockset.is_top locks) then
          Guarded locks
        else if (maxpry=minpry) then
          Priority maxpry
        else if (minpry >= offpry) then
          Defence (minpry,offpry)
        else if not (List.exists rw acc_list) then
          ReadOnly
        else if not (List.exists non_main acc_list) then
          ThreadLocal
        else
          Race
      in (*/is_race_no_flags*)
      let is_race acc_list_some_glob =
        let check_valset flag guards acc valset = (*do accesses with flag value valset race with guards?*)
          let value = get_val flag (List.hd valset) in
          let may_eq_guards = split_may_eq flag value guards in
          let accs = valset@may_eq_guards in
          acc && (not((is_race_no_flags (strip_flags accs)) = Race))
        in (*/check_valset*)
        let check_one_flag acc_list_some_glob' flag = (*does flag prevent the race?*)
          (* are flag values concrete enough?*)
          let acc_list_some_glob'' = filter_bot flag acc_list_some_glob' in
          (*     if check_top flag acc_list_some_glob then [] else begin (*too strong. check if top accesses are safe by priority*) *)
          let tops,acc_list_some_glob = split_accs_top flag acc_list_some_glob'' in
          let guards,assigns = split_accs_method flag acc_list_some_glob in
          (* let _ = print_endline ((string_of_int (List.length guards))^" guards and "^string_of_int (List.length assigns)^" assigns"  ) in        *)
          let eq_asgn,weird_asgn = split_accs flag assigns in
          if (weird_asgn != [] ) then failwith "this never happens! osek 750";
          if (is_race_no_flags (strip_flags (eq_asgn@tops)) = Race) then begin
            [] (* setting does not protect. setters race *)
          end else begin
            (* check each set value against all guard access, which are not ineq value *)
            let valsets' = split_equals flag eq_asgn in
            let valsets = List.map (fun x -> (x@tops)) valsets' in
            if List.fold_left (check_valset flag (guards@tops)) true valsets then
              [flag]
            else
              []
              (*     end     *)
          end
        in (*/check_one_flag*)
        let get_keys_from_flag_map flag_map = Flags.fold (fun key value acc -> (key::acc)) flag_map [] in
        let get_flags acc_list_some_glob : (varinfo list) =
          let flag_list = List.map proj2_2 acc_list_some_glob in
          let doit acc flag_map =
            if Flags.is_bot(flag_map) then
              acc
            else
              let rec join l1 l2 = match l2 with
                | [] -> l1
                | l::ls -> let res = (join l1 ls) in
                  if List.mem l l1 then res else (l :: res)
              in
              let flags = get_keys_from_flag_map flag_map in
              join acc flags
          in
          List.fold_left doit [] flag_list
        in (*/get_flags*)
        let valid_flag (flag :varinfo) : int=
          (*        let add_flag flag res =
                    	  match res with
                    	    | BadFlag -> begin
                    	      bad_flags := flag::!bad_flags;
                    	      if tracing then trace "osek" "Flag %s is invalid\n" flag.vname
                    	      end
                    	    | SomeFlag _ | GoodFlag -> ()
                    	    | _ -> failwith "This never happens! osekml687"
                    in (*/add_flag*)*)
          (*        if List.mem flag !bad_flags then false else*)
          let status_list = ref [] in
          let check_one_list offset acc_list_some_glob = (* here we know of_pry of f *)
            let acc_list = List.map proj2_1 acc_list_some_glob in
            let writes,reads = List.partition rw acc_list in
            let res : access_status =
              if writes = [] || [] = reads then
                BadFlag
              else if ((is_race_no_flags writes) = Race) then
                BadFlag
              else
                let write_off_pry = offpry writes in
                let read_off_pry = offpry reads in
                if write_off_pry > read_off_pry then
                  BadFlag
                else
                  (* let _ = print_endline ("flagpry: " ^ (string_of_int write_off_pry)) in             *)
                  SomeFlag write_off_pry
            in
            (*           let _ = add_flag flag res in *)
            status_list := res::!status_list
          in (*/check_one_list*)
          let _ = OffsMap.iter check_one_list  (get_acc_map flag) in
          let fold_fun acc status = match status with SomeFlag p -> max acc p | BadFlag -> max_int | _ -> acc in
          List.fold_left (fold_fun) min_int !status_list
        in (*/valid_flag*)
        let check_flags acc_list' =
          let acc_pry = minpry (strip_flags acc_list') in
          (* let _ = print_endline ("accpry: " ^ (string_of_int acc_pry)) in *)
          let flag_list = List.filter (fun f -> (valid_flag f) <= acc_pry) (get_flags acc_list') in
          (* let _ = print_endline ("flaglist: " ^ (List.fold_left (fun x y -> (y.vname ^", " ^x) ) "" flag_list)) in *)
          List.flatten (List.map (check_one_flag acc_list') flag_list)
        in (*/check_flags*)
        let check_high_acc acc_list = (*check high_accs (mark if higher only reads/writes*)
          let filter_pry p acc =  not ((offpry [acc]) = p) in
          let is_write acc = (fun ((_,_,x),_,_) -> x) acc in
          let is_read acc = (fun ((_,_,x),_,_) -> not x) acc in
          let low = minoffpry acc_list in
          let high = offpry acc_list in
          let highs = List.filter (filter_pry low) acc_list in
          let lows = List.filter (filter_pry high) acc_list in
          let high_writes = List.filter is_write highs in
          let high_reads = List.filter is_read highs in
          let low_writes = List.filter is_write lows in
          let low_reads = List.filter is_read lows in
          if (high_writes = []) then HighRead else
          if (high_reads = []) then HighWrite else
          if (low_writes = []) then LowRead else
          if (low_reads = []) then LowWrite else
            Race
        in (*/check high_accs*)
        let res = is_race_no_flags (strip_flags acc_list_some_glob) in
        if res = Race then begin
          if not(List.mem gl.vname !FlagModes.Spec.flag_list) then begin
            (* let _ = print_endline "check flag protection" in *)
            let flags = check_flags acc_list_some_glob in
            if not(flags = []) then
              (*   let _ = print_endline "flag!!" in *)
              Flag (flag_list_to_string flags)
            else
              check_high_acc (strip_flags acc_list_some_glob)
          end else begin
            (*handle Flag vars*)
            (*            if List.mem gl !bad_flags then
                          BadFlag
                          else *)
            if (valid_flag gl < max_int) then
              GoodFlag
            else
              BadFlag
          end
        end else begin
          match res with GoodFlag | BadFlag | SomeFlag(_)  ->
            failwith "This never happens! osekml812"
                       | _ -> res
        end
      in


      let report_race offset acc_list =
        let f  (((loc, fl, write), dom_elem,o),flagstate) =
          let lock_str = Lockset.short 80 dom_elem in
          let my_locks = List.map (function (LockDomain.Addr.Addr (x,_) ,_) -> x.vname | _ -> failwith "This (hopefully2) never happens!" ) (Lockset.ReverseAddrSet.elements dom_elem) in
          let pry = List.fold_left (fun y x -> if pry x > y then pry x else y) (min_int) my_locks in
          let flag_str = if !Errormsg.verboseFlag then " and flag state: " ^ (Pretty.sprint 80 (Flags.pretty () flagstate)) else "" in
          let action = if write then "write" else "read" in
          let thread = "\"" ^ Flag.short 80 fl ^ "\"" in
          let warn = action ^ " in " ^ thread ^ " with priority: " ^ (string_of_int pry) ^ ", lockset: " ^ lock_str ^ flag_str in
          (warn,loc)
        in (*/f*)
        let warnings =  List.map f acc_list in
        let var_str = gl.vname ^ ValueDomain.Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
        let handle_race def_warn = begin
          if (List.mem gl.vname  (List.map Json.string @@ get_list "ana.osek.safe_vars")) then begin
            suppressed := !suppressed+1;
            if (get_bool "allglobs") then
              print_group (safe_str "safe variable") warnings
            else
              ignore (printf "Suppressed warning: %s is not guarded\n" var_str)
          end else begin
            (*filter out safe task access. redo is_race report accordingly List.not List.mem gl.vname  *)
            let safe_tasks = List.map make_task (List.map Json.string @@ get_list "ana.osek.safe_task") in
            let safe_irpts = List.map make_isr (List.map Json.string @@ get_list "ana.osek.safe_isr") in
            let safe_funs = safe_irpts @ safe_tasks in
            if safe_funs = [] then begin
              race_free := false;
              let warn = def_warn ^ " at " ^ var_str in
              print_group warn warnings
            end else begin
              filtered := !filtered +1;
              (*((_, dom_elem,_),_) -> let lock_names_list = names (Lockset.ReverseAddrSet.elements dom_elem) in
                any in there also in safe_tasks ... %TODO *)
              let filter_fun ((_, dom_elem,_),_) =
                List.fold_left (fun old name -> not (old || List.mem name safe_funs)) false (List.map names (Lockset.ReverseAddrSet.elements dom_elem))
              in
              let acc_list' = List.filter filter_fun acc_list in
              match (is_race acc_list') with
              | Race -> begin
                  race_free := false;
                  let warn = "Datarace at " ^ var_str in
                  print_group warn warnings
                end
              | Guarded locks ->
                let lock_str = Mutex.Lockset.short 80 locks in
                if (get_bool "allglobs") then
                  print_group (safe_str "common mutex after filtering") warnings
                else
                  ignore (printf "Found correlation: %s is guarded by lockset %s after filtering\n" var_str lock_str)
              | Priority pry ->
                if (get_bool "allglobs") then
                  print_group (safe_str "same priority after filtering") warnings
                else
                  ignore (printf "Found correlation: %s is guarded by priority %s after filtering\n" var_str (string_of_int pry))
              | Defence (defpry,offpry) ->
                if (get_bool "allglobs") then
                  print_group (safe_str "defensive priority exceeds offensive priority after filtering") warnings
                else
                  ignore (printf "Found correlation: %s is guarded by defensive priority %s against offensive priority %s after filtering\n" var_str (string_of_int defpry) (string_of_int offpry))
              | Flag (flagvar) ->
                if (get_bool "allglobs") then
                  print_group (safe_str ("variable "^flagvar ^" used to prevent concurrent accesses after filtering") ) warnings
                else
                  ignore (printf "Found correlation: %s is guarded by flag %s after filtering\n" var_str flagvar)
              | ReadOnly ->
                if (get_bool "allglobs") then
                  print_group (safe_str "only read after filtering") warnings
              | ThreadLocal ->
                if (get_bool "allglobs") then
                  print_group (safe_str "thread local after filtering") warnings
              | BadFlag -> begin
                  race_free := false;
                  let warn = "Writerace at 'flag' " ^ var_str in
                  print_group warn warnings
                end
              | GoodFlag -> begin
                  if (get_bool "allglobs") then begin
                    print_group (safe_str "is a flag after filtering") warnings
                  end else  begin
                    ignore (printf "Found flag behaviour after filtering: %s\n" var_str)
                  end
                end
              | SomeFlag _ | HighRead | HighWrite | LowRead | LowWrite -> failwith "This never happens! osekml984"
            end
          end
        end in
        let res = match is_race acc_list with
          | Race -> handle_race "Datarace"
          | HighRead -> handle_race "High read datarace"
          | HighWrite -> handle_race "High write datarace"
          | LowRead -> handle_race "Low read datarace"
          | LowWrite -> handle_race "Low write datarace"
          | Guarded locks ->
            let lock_str = Mutex.Lockset.short 80 locks in
            if (get_bool "allglobs") then
              print_group (safe_str "common mutex") warnings
            else
              ignore (printf "Found correlation: %s is guarded by lockset %s\n" var_str lock_str)
          | Priority pry ->
            if (get_bool "allglobs") then
              print_group (safe_str "same priority") warnings
            else
              ignore (printf "Found correlation: %s is guarded by priority %s\n" var_str (string_of_int pry))
          | Defence (defpry,offpry) ->
            if (get_bool "allglobs") then
              print_group (safe_str "defensive priority exceeds offensive priority") warnings
            else
              ignore (printf "Found correlation: %s is guarded by defensive priority %s against offensive priority %s\n" var_str (string_of_int defpry) (string_of_int offpry))
          | Flag (flagvar) ->
            if (get_bool "allglobs") then
              print_group (safe_str ("variable "^flagvar ^" used to prevent concurrent accesses") ) warnings
            else
              ignore (printf "Found correlation: %s is guarded by flag %s\n" var_str flagvar)
          | ReadOnly ->
            if (get_bool "allglobs") then
              print_group (safe_str "only read") warnings
          | ThreadLocal ->
            if (get_bool "allglobs") then
              print_group (safe_str "thread local") warnings
          | BadFlag -> begin
              race_free := false;
              let warn = "Writerace at 'flag' " ^ var_str in
              print_group warn warnings
            end
          | GoodFlag -> begin
              if (get_bool "allglobs") then begin
                print_group (safe_str "is a flag") warnings
              end else  begin
                ignore (printf "Found flag behaviour: %s\n" var_str)
              end
            end
          | SomeFlag _ -> failwith "This never happens! osekml1027"
        in
        res
      in (*/report_race*)
      OffsMap.iter report_race (get_acc_map gl)

  (** postprocess and print races and other output *)
  let finalize () =
    AccKeySet.iter postprocess_acc !accKeys;
    if !race_free then
      print_endline "Goblint did not find any Data Races in this program!";
    if !suppressed = 1 then
      print_endline ("However 1 warning has been suppressed.");
    if !suppressed > 1 then
      print_endline ("However " ^ (string_of_int !suppressed) ^ " warnings have been suppressed.");
    if !filtered > 0 then
      print_endline ("Filtering of safe tasks/irpts was used " ^  (string_of_int !filtered) ^ " time(s).")

  let init () = (*
    let tramp = get_string "ana.osek.tramp" in
    if Sys.file_exists(tramp) then begin
      parse_tramp tramp;
    end else begin
      prerr_endline "Trampoline headers not found." ;
      exit 2;
    end;*)
    if get_bool "ana.osek.warnfiles" then init_warn_files();
    LibraryFunctions.add_lib_funs osek_API_funs;
    let names = !osek_renames in
    if Sys.file_exists(names) then begin
      parse_names names;
    end;
    let names = !osek_ids in
    if Sys.file_exists(names) then begin
      parse_ids names;
    end;
end

let () = MCP.register_analysis ~dep:["base";"threadid";"threadflag";"fmode"] (module Spec : MCPSpec)
