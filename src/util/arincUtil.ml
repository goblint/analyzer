open Prelude
open Cil
(* we don't want to use M.debug_each because everything here should be done after the analysis, so the location would be some old value for all invocations *)
let debug_each msg = print_endline @@ Messages.colorize @@ "{blue}"^msg

(* ARINC types and Hashtables for collecting CFG *)
type resource = Process | Function | Semaphore | Event | Logbook | SamplingPort | QueuingPort | Buffer | Blackboard [@@deriving show { with_path = false }]
(* id is resource type and name, there is a 1:1 mapping to varinfo in the analysis used for assignments *)
type id = resource*string [@@deriving show]
let infinity = 4294967295L (* time value used for infinity *)
type time = int64 [@printer fun fmt t -> Format.(if t = infinity then fprintf fmt "∞" else fprintf fmt "%Ldns" t)] [@@deriving show]

(* map int values to enum names *)
type partition_mode = Idle | Cold_Start | Warm_Start | Normal [@@deriving show { with_path = false }, enum]
let show_partition_mode_opt = String.uppercase % Option.default "Unknown!" % Option.map show_partition_mode
let mode_is f i = match Option.bind (ArincDomain.Pmo.to_int i) (partition_mode_of_enum % Int64.to_int) with Some x -> f x | None -> false
let mode_is_init  = mode_is (function Cold_Start | Warm_Start -> true | _ -> false)
let mode_is_multi = mode_is (function Normal -> true | _ -> false)
type queuing_discipline = Fifo | Prio [@@deriving show { with_path = false }, enum]
let string_of_queuing_discipline = String.uppercase % Option.default "Unknown!" % Option.map show_queuing_discipline % queuing_discipline_of_enum % Int64.to_int
(* return code data type *)
type return_code = (* taken from ARINC_653_part1.pdf page 46 *)
  | NO_ERROR       (* request valid and operation performed *)
  | NO_ACTION      (* system’s operational status unaffected by request *)
  | NOT_AVAILABLE  (* the request cannot be performed immediately *)
  | INVALID_PARAM  (* parameter specified in request invalid *)
  | INVALID_CONFIG (* parameter specified in request incompatible with current configuration (e.g., as specified by system integrator) *)
  | INVALID_MODE   (* request incompatible with current mode of operation *)
  | TIMED_OUT      (* time-out associated with request has expired *)
[@@deriving show { with_path = false }, enum]

let pname_ErrorHandler = "ErrorHandler"

module Action = (* encapsulate types because some process field names are also used for D.t -> use local opening of modules (since OCaml 4.00) for output *)
struct
  type process = { pid: id; f: CilType.Varinfo.t; pri: int64; per: time; cap: time } [@@deriving show]
  type semaphore = { sid: id; cur: int64; max: int64; queuing: int64 } [@@deriving show]
end
type action =
  | Nop
  | Cond of string * string
  | Assign of string * string (* var_callee = var_caller *)
  | Call of string
  | LockPreemption | UnlockPreemption | SetPartitionMode of partition_mode option
  | CreateProcess of Action.process | CreateErrorHandler of id * CilType.Varinfo.t | Start of id | Stop of id | Suspend of id | SuspendSelf of id * time | Resume of id
  | CreateBlackboard of id | DisplayBlackboard of id | ReadBlackboard of id * time | ClearBlackboard of id
  | CreateSemaphore of Action.semaphore | WaitSemaphore of id * time | SignalSemaphore of id
  | CreateEvent of id | WaitEvent of id * time | SetEvent of id | ResetEvent of id
  | TimedWait of time | PeriodicWait [@@deriving show { with_path = false }]
type node = ArincDomain.Pred.Base.t
let string_of_node = ArincDomain.Pred.string_of_elt
type edge = node * action * string option * node
let action_of_edge (_, action, _, _) = action
type edges = (id, edge Set.t) Hashtbl.t
let edges = ref (Hashtbl.create 199 : edges)
let get_a (a,_,_,_) = a
let get_b (_,_,_,b) = b

let marshal ch = Marshal.to_channel ch !edges []
let unmarshal ch = edges := Marshal.from_channel ch

let get_edges (pid:id) : edge Set.t =
  Hashtbl.find_default !edges pid Set.empty
let add_edge (pid:id) edge =
  Hashtbl.modify_def Set.empty pid (Set.add edge) !edges

let filter_map_actions p =
  let all_edges = Hashtbl.values !edges |> List.of_enum |> List.map Set.elements |> List.concat in
  List.filter_map (p%action_of_edge) all_edges

let filter_actions p =
  (* filter_map_actions (Option.filter p % Option.some) *)
  filter_map_actions (fun x -> if p x then Some x else None)

let funs_for_process id : varinfo list =
  let get_funs = function
    | CreateProcess x when x.Action.pid=id -> Some x.Action.f
    | CreateErrorHandler (id', f) when id'=id -> Some f
    | _ -> None
  in
  filter_map_actions get_funs |> List.unique

module type GenSig = sig type k type v val getNew: v Enum.t -> v end
module type SymTblSig = sig type k type v val get: k -> v val to_list: unit -> (k*v) list end
module SymTbl (Gen: GenSig) : SymTblSig with type k = Gen.k and type v = Gen.v =
struct
  type k = Gen.k
  type v = Gen.v
  let h = (Hashtbl.create 123 : (k, v) Hashtbl.t)
  let get k =
    try Hashtbl.find h k
    with Not_found ->
      let v = Gen.getNew (Hashtbl.values h) in
      Hashtbl.replace h k v;
      v
  let to_list () = Hashtbl.enum h |> List.of_enum
end

(* this is just used to make sure that every var that is read has been written before *)
let return_vars = (Hashtbl.create 100 : (id * [`Read | `Write], string Set.t) Hashtbl.t)
let add_return_var pid kind var = Hashtbl.modify_def Set.empty (pid, kind) (Set.add var) return_vars
let get_return_vars pid kind =
  if fst pid <> Process then failwith "get_return_vars: tried to get var for Function, but vars are saved per Process!" else
    Hashtbl.find_default return_vars (pid, kind) Set.empty
let decl_return_vars xs = Set.elements xs |> List.map (fun vname -> "mtype " ^ vname ^ ";")
let is_global vname = startsWith "G" vname
let get_locals pid = Set.union (get_return_vars pid `Read) (get_return_vars pid `Write) |> Set.filter (neg is_global) |> decl_return_vars
let flatten_set xs = Set.fold Set.union xs Set.empty
let get_globals () = Hashtbl.values return_vars |> Set.of_enum |> flatten_set |> Set.filter is_global |> decl_return_vars

(* ARINC output *)
(* console and dot *)
let str_resource id =
  let str_funs fs = "["^(List.map CilType.Varinfo.show fs |> String.concat ", ")^"]" in
  match id with
  | Process, "mainfun" ->
    "mainfun/["^String.concat ", " (List.map Json.string (GobConfig.get_list "mainfun"))^"]"
  | Process, name ->
    name^"/"^str_funs @@ funs_for_process id
  | resource_type, name ->
    name
let str_resources ids = "["^(String.concat ", " @@ List.map str_resource ids)^"]"
let str_action pid = function
  | Cond (r, cond) -> "If "^cond
  | SetPartitionMode m -> "SetPartitionMode "^show_partition_mode_opt m
  | CreateProcess x ->
    Action.("CreateProcess "^str_resource x.pid^" (fun "^CilType.Varinfo.show x.f^", prio "^Int64.to_string x.pri^", period "^show_time x.per^", capacity "^show_time x.cap^")")
  | CreateErrorHandler (id, funs) -> "CreateErrorHandler "^str_resource id
  | CreateSemaphore x ->
    Action.("CreateSemaphore "^str_resource x.sid^" ("^Int64.to_string x.cur^"/"^Int64.to_string x.max^", "^string_of_queuing_discipline x.queuing^")")
  | a -> show_action a
let str_return_code = function Some r -> " : " ^ r | None -> ""
(* spin/promela *)
let pml_resources = Hashtbl.create 13
let _ = Hashtbl.add pml_resources (Process, "mainfun") 0L
let id_pml id = (* give id starting from 0 (get_pid_by_id for all resources) *)
  let resource, name as k = id in
  try Hashtbl.find pml_resources k
  with Not_found ->
    let ids = Hashtbl.filteri (fun (r,n) v -> r=resource) pml_resources |> Hashtbl.values in
    let id = if Enum.is_empty ids then 0L else Int64.succ (Enum.arg_max identity ids) in
    Hashtbl.replace pml_resources k id;
    id
let str_id_pml id = Int64.to_string @@ id_pml id
let str_pid_pml id = (if fst id = Process then "P" else "F") ^ str_id_pml id (* process or function *)
let str_ids_pml ids f = String.concat " " (List.map (f%str_id_pml) ids)
(* let ref_apply f r = r := f !r *)
let unset_ret_vars = ref Set.empty
let undef_funs = ref Set.empty
let action_may_fail = function
  | SuspendSelf _ | ReadBlackboard _ | WaitSemaphore _ | WaitEvent _ -> true
  | _ -> false
(* TODO also may_fail b/c they have timeout but missing above (b/c they don't affect the status of the modelled system (e.g. communication with outside)):
 * {Send,Receive}QueuingMessage
 * {Send,Receive}Buffer
*)
let str_action_pml pid r action =
  let action_str =
    match action with
    | Nop -> "tmp = 0;"
    | Cond (r, cond) ->
      (* if the return var that is branched on was never set, we warn about it at the end and just leave out the condition, which leads to non-det. branching, i.e. the same behaviour as if it was set to top. TODO: this is not precise for globals, since those are initialized with 0. *)
      if not @@ Set.mem r (get_return_vars pid `Write) then (
        unset_ret_vars := Set.add r !unset_ret_vars; ""
      ) else if cond = "true" then "" else cond ^ " -> "
    | Assign (lhs, rhs) -> (* for function parameters this is callee = caller *)
      (* if the lhs is never read, we don't need to do anything *)
      if not @@ Set.mem lhs (get_return_vars pid `Read) then ""
      else lhs^" = "^rhs^";"
    | Call fname ->
      (* we shouldn't have calls to functions without edges! *)
      if Hashtbl.mem !edges (Function, fname) then "goto Fun_"^fname^";" else (undef_funs := Set.add fname !undef_funs; "")
    | LockPreemption -> "LockPreemption();"
    | UnlockPreemption -> "UnlockPreemption();"
    | SetPartitionMode i -> "SetPartitionMode("^show_partition_mode_opt i^");"
    | CreateProcess x ->
      Action.("CreateProcess("^str_id_pml x.pid^", "^Int64.to_string x.pri^", "^Int64.to_string x.per^", "^Int64.to_string x.cap^"); /* "^str_resource x.pid^" (prio "^Int64.to_string x.pri^", period "^show_time x.per^", capacity "^show_time x.cap^") */")
    | CreateErrorHandler (id, f) -> "CreateErrorHandler("^str_id_pml id^");"
    | Start id -> "Start("^str_id_pml id^");"
    | Stop id -> "Stop("^str_id_pml id^");"
    | Suspend id -> "Suspend("^str_id_pml id^");"
    | SuspendSelf (id, timeout) -> "Suspend("^str_id_pml id^");"
    | Resume id -> "Resume("^str_id_pml id^");"
    | CreateBlackboard id -> "CreateBlackboard("^str_id_pml id^");"
    | DisplayBlackboard id -> "DisplayBlackboard("^str_id_pml id^");"
    | ReadBlackboard (id, timeout) -> "ReadBlackboard("^str_id_pml id^");"
    | ClearBlackboard id -> "ClearBlackboard("^str_id_pml id^");"
    | CreateSemaphore x ->
      Action.("CreateSemaphore("^str_id_pml x.sid^", "^Int64.to_string x.cur^", "^Int64.to_string x.max^", "^string_of_queuing_discipline x.queuing^");")
    | WaitSemaphore (id, timeout) -> "WaitSemaphore("^str_id_pml id^");"
    | SignalSemaphore id -> "SignalSemaphore("^str_id_pml id^");"
    | CreateEvent id -> "CreateEvent("^str_id_pml id^");"
    | WaitEvent (id, timeout) -> "WaitEvent("^str_id_pml id^");"
    | SetEvent id -> "SetEvent("^str_id_pml id^");"
    | ResetEvent id -> "ResetEvent("^str_id_pml id^");"
    | TimedWait t -> "TimedWait("^Int64.to_string t^");"
    | PeriodicWait -> "PeriodicWait();"
  in
  match r with
  | Some r when action_may_fail action (* when Set.mem r (get_return_vars pid `Read) *) -> "if :: "^action_str^" "^r^" = SUCCESS :: "^r^" = ERROR fi;"
  | Some r -> action_str^" "^r^" = SUCCESS;"
  | None -> action_str

(* helpers *)
let find_option p xs = try Some (List.find p xs) with Not_found -> None (* why is this in batteries for Hashtbl but not for List? *)

(* simplify graph here, i.e. merge functions which consist of the same edges and contract call chains *)
let simplify () =
  let dups = Hashtbl.enum !edges |> List.of_enum |> List.group (compareBy ~cmp:Set.compare snd) |> List.filter_map (function x::y::ys -> Some (x, y::ys) | _ -> None) in
  let replace_call oldname newname =
    (* debug_each @@ "Replacing function calls to "^oldname^" with "^newname; *)
    let f = function
      | a, Call x, r, b when x = oldname -> a, Call newname, r, b
      | x -> x
    in
    Hashtbl.map_inplace (fun _ v -> Set.map f v) !edges
  in
  let merge ((res,name),_) xs =
    if res = Process then failwith "There should be no processes with duplicate content!" else
      (* replace calls to the other functions with calls to name and remove them *)
      List.iter (fun (k,_) ->
          replace_call (snd k) name;
          Hashtbl.remove_all !edges k
        ) xs
  in
  List.iter (uncurry merge) dups;
  (* contract call chains: replace functions which only contain another call *)
  let rec contract_call_chains () =
    let single_calls = Hashtbl.filter_map (fun (res,_) v -> match Set.choose v with _, Call name, _, _ when Set.cardinal v = 1 && res = Function -> Some (Function, name) | _ -> None) !edges in
    let last_calls = Hashtbl.filteri (fun k v -> not @@ Hashtbl.mem single_calls v) single_calls in
    Hashtbl.iter (fun k v ->
        replace_call (snd k) (snd v);
        Hashtbl.remove_all !edges k
      ) last_calls;
    if Hashtbl.length single_calls > 0 then contract_call_chains ()
  in contract_call_chains ()

(* output warnings *)
let validate () =
  debug_each @@ "Validating arinc graph...";
  if neg Set.is_empty !undef_funs then (
    debug_each "The following functions are called, but have no edges:";
    Set.iter (fun fname -> debug_each @@ "call to undefined function " ^ fname) !undef_funs
  );
  (* search for multi-edges (might appear for call-edges with intermediate context hash) *)
  let warn_multi_edge id s =
    Set.to_list s |> List.group (compareBy (fun x -> get_a x, get_b x)) |> List.filter ((>) 1 % List.length)
    |> List.iter (fun xs -> let x = List.hd xs in debug_each @@ str_resource id ^ ": Found " ^ string_of_int (List.length xs) ^" multi-edges between " ^ string_of_node (get_a x) ^ " and " ^ string_of_node (get_b x) ^ ": [" ^ String.concat ", " (List.map (str_action id % action_of_edge) xs) ^ "]")
  in
  Hashtbl.iter (fun k v -> warn_multi_edge k v) !edges;
  if neg Set.is_empty !unset_ret_vars then (
    debug_each "The following return code variables have never been set by arinc functions or assignments (conditions for these variables are ignored):"; (* this probably shouldn't happen in the input program - at least for local variables... *)
    Set.iter (fun var -> debug_each @@ "branching on unset return var " ^ var) !unset_ret_vars
  )

(* print to stdout *)
let print_actions () =
  let print_process pid =
    let str_node = string_of_node in
    let str_edge (a, action, r, b) = str_node a ^ " -> " ^ str_action pid action ^ str_return_code r ^ " -> " ^ str_node b in
    let xs = Set.map str_edge (get_edges pid) in
    debug_each @@ str_resource pid^" ->\n\t"^String.concat "\n\t" (Set.elements xs)
  in
  Hashtbl.keys !edges |> Enum.iter print_process

(* helper for exporting results *)
let save_result desc ext content = (* output helper *)
  let dir = Goblintutil.create_dir "result" in (* returns abs. path *)
  let path = dir ^ "/arinc." ^ ext in
  output_file path content;
  print_endline @@ "saved " ^ desc ^ " as " ^ path

let save_dot_graph () =
  let dot_process pid =
    (* 1 -> w1 [label="fopen(_)"]; *)
    let str_node x = "\"" ^ str_pid_pml pid ^ "_" ^ string_of_node x ^ "\"" in (* quote node names for dot *)
    let str_edge (a, action, r, b) = str_node a ^ "\t->\t" ^ str_node b ^ "\t[label=\"" ^ str_action pid action ^ str_return_code r ^ "\"]" in
    let xs = Set.map str_edge (get_edges pid) |> Set.elements in
    ("subgraph \"cluster_"^str_resource pid^"\" {") :: xs @ ("label = \""^str_resource pid^"\";") :: ["}\n"]
  in
  let lines = Hashtbl.keys !edges |> List.of_enum |> List.map dot_process |> List.concat in
  let dot_graph = String.concat "\n  " ("digraph file {"::lines) ^ "\n}" in
  save_result "graph" "dot" dot_graph

module FunTbl = SymTbl (struct type k = string*string type v = int let getNew xs = List.length @@ List.of_enum xs end)
let save_promela_model () =
  let open Action in (* needed to distinguish the record field names from the ones of D.t *)
  let indent s = "\t"^s in
  let procs  = List.unique @@ filter_map_actions (function CreateProcess x -> Some x | _ -> None) in
  let has_error_handler = not @@ List.is_empty @@ filter_actions (function CreateErrorHandler _ -> true | _ -> false) in
  let bboards = List.unique @@ filter_map_actions (function CreateBlackboard id -> Some id | _ -> None) in
  let semas   = List.unique @@ filter_map_actions (function CreateSemaphore x -> Some x | _ -> None) in
  let events  = List.unique @@ filter_map_actions (function CreateEvent id -> Some id | _ -> None) in
  let nproc   = List.length procs + 1 + (if has_error_handler then 1 else 0) in (* +1 is init process *)
  let nbboard = List.length bboards in
  let nsema   = List.length semas in
  let nevent  = List.length events in
  let run_processes = List.map (fun x -> let name = snd x.pid in let id = id_pml x.pid in id, "run "^name^"("^Int64.to_string id^");") procs |> List.sort (compareBy fst) |> List.map snd in
  let init_body =
    "preInit;" ::
    "run mainfun(0);" :: (* keep mainfun as name for init process? *)
    "postInit();" ::
    "run monitor();" ::
    (if has_error_handler then "run ErrorHandler("^str_id_pml (Process, "ErrorHandler")^")" else "// no ErrorHandler") ::
    run_processes
  in
  let current_pname = ref "" in
  let called_funs_done = ref Set.empty in
  let rec process_def id =
    if fst id = Function && Set.mem (snd id) !called_funs_done then [] else (* if we already generated code for this function, we just return [] *)
      let iid = id_pml id in (* id is type*name, iid is int64 (starting from 0 for each type of resource) *)
      let spid = str_pid_pml id in (* string for id (either Function or Process) *)
      (* set the name of the current process (this function is also run for functions, which need a reference to the process for checking branching on return vars) *)
      if fst id = Process then current_pname := snd id;
      (* for a process we start with no called functions, for a function we add its name *)
      called_funs_done := if fst id = Process then Set.empty else Set.add (snd id) !called_funs_done;
      (* build adjacency matrix for all nodes of this process *)
      let module HashtblN = Hashtbl.Make (ArincDomain.Pred.Base) in
      let a2bs = HashtblN.create 97 in
      Set.iter (fun (a, _, _, b as edge) -> HashtblN.modify_def Set.empty a (Set.add edge) a2bs) (get_edges id);
      let nodes = HashtblN.keys a2bs |> List.of_enum in
      (* let out_edges node = HashtblN.find_default a2bs node Set.empty |> Set.elements in (* Set.empty leads to Out_of_memory!? *) *)
      let out_edges node = try HashtblN.find a2bs node |> Set.elements with Not_found -> [] in
      let in_edges node = HashtblN.filter (Set.mem node % Set.map get_b) a2bs |> HashtblN.values |> List.of_enum |> flat_map Set.elements in
      let is_end_node = List.is_empty % out_edges in
      let is_start_node = List.is_empty % in_edges in
      let start_node = List.find is_start_node nodes in (* node with no incoming edges is the start node *)
      (* let str_nodes xs = "{"^(List.map string_of_node xs |> String.concat ",")^"}" in *)
      let label n = spid ^ "_" ^ string_of_node n in
      let end_label = spid ^ "_end" in
      let goto node = "goto " ^ label node in
      let called_funs = ref [] in
      let str_edge (a, action, r, b) =
        let target_label = if is_end_node b then end_label else label b in
        let mark = match action with
          | Call fname ->
            called_funs := fname :: !called_funs;
            let pc = string_of_int @@ FunTbl.get (fname,target_label) in "mark("^pc^"); "
          | _ -> ""
        in
        (* for function calls the goto will never be reached since the function's return will already jump to that label; however it's nice to see where the program will continue at the site of the call. *)
        mark ^ str_action_pml (Process, !current_pname) r action ^ " goto " ^ target_label
      in
      let choice xs = List.map (fun x -> "::\t"^x ) xs in (* choices in if-statements are prefixed with :: *)
      let walk_edges (a, out_edges) =
        let edges = Set.elements out_edges |> List.map str_edge in
        (label a ^ ":") ::
        if List.length edges > 1 then
          "if" :: (choice edges) @ ["fi"]
        else
          edges
      in
      let locals = if not @@ GobConfig.get_bool "ana.arinc.assume_success" && fst id = Process then get_locals id else [] in
      let body = locals @ goto start_node :: (flat_map walk_edges (HashtblN.enum a2bs |> List.of_enum)) @ [end_label ^ ":" ^ if fst id = Process then " status[id] = DONE" else " ret_"^snd id^"()"] in
      let head = match id with
        | Process, name ->
          let proc = find_option (fun x -> x.pid=id) procs in (* None for mainfun *)
          let priority = match proc with Some proc -> " priority "^Int64.to_string proc.pri | _ -> "" in
          "proctype "^name^"(byte id)"^priority^" provided (canRun("^Int64.to_string iid^") PRIO"^Int64.to_string iid^") {\nint stack[20]; int sp = -1;"
        | Function, name ->
          "Fun_"^name^":"
        | _ -> failwith "Only Process and Function are allowed as keys for collecting ARINC actions"
      in
      let called_fun_ids = List.map (fun fname -> Function, fname) !called_funs in
      let funs = flat_map process_def called_fun_ids in
      "" :: head :: List.map indent body @ funs @ [if fst id = Process then "}" else ""]
  in
  (* used for macros oneIs, allAre, noneAre... *)
  let checkStatus = "(" ^ (String.concat " op2 " @@ List.of_enum @@ (0 --^ nproc) /@ (fun i -> "status["^string_of_int i^"] op1 v")) ^ ")" in
  let allTasks = "(" ^ (String.concat " && " @@ List.of_enum @@ (0 --^ nproc) /@ (fun i -> "prop("^string_of_int i^")")) ^ ")" in
  (* generate priority based running constraints for each process (only used ifdef PRIOS): process can only run if no higher prio process is ready *)
  let prios =
    let def proc =
      let id = str_id_pml proc.pid in
      let pri = proc.pri in
      let higher = List.filter (fun x -> x.pri > pri) procs in
      if List.is_empty higher
      then None
      else Some ("#undef PRIO" ^ id ^ "\n#define PRIO" ^ id ^ String.concat "" @@ List.map (fun x -> " && status[" ^ str_id_pml x.pid ^ "] != READY") higher)
    in
    List.filter_map def procs
  in
  (* sort definitions so that inline functions come before the processes *)
  let process_defs = Hashtbl.keys !edges |> List.of_enum |> List.filter (fun id -> fst id = Process) |> List.sort (compareBy str_pid_pml) |> flat_map process_def in
  let fun_mappings =
    let fun_map xs =
      if List.is_empty xs then [] else
        let (name,_),_ = List.hd xs in
        let entries = xs |> List.map (fun ((_,k),v) -> "\t:: (stack[sp] == " ^ string_of_int v ^ ") -> sp--; goto " ^ k ^" \\") in
        let debug_str = if GobConfig.get_bool "ana.pml.debug" then "\t:: else -> printf(\"wrong pc on stack!\"); assert(false) " else "" in
        ("#define ret_"^name^"() if \\") :: entries @ [debug_str ^ "fi"]
    in
    FunTbl.to_list () |> List.group (compareBy (fst%fst)) |> flat_map fun_map
  in
  let promela = String.concat "\n" @@
    ("#define nproc "^string_of_int nproc) ::
    ("#define nbboard "^string_of_int nbboard) ::
    ("#define nsema "^string_of_int nsema) ::
    ("#define nevent "^string_of_int nevent) :: "" ::
    ("#define checkStatus(op1, v, op2) "^checkStatus) :: "" ::
    ("#define allTasks(prop) "^allTasks) :: "" ::
    "#include \"arinc.base.pml\"" :: "" ::
    "init {" :: List.map indent init_body @ "}" :: "" ::
                                            (List.of_enum @@ (0 --^ nproc) /@ (fun i -> "#define PRIO" ^ string_of_int i)) @
    "#ifdef PRIOS" :: prios @ "#endif" ::
                              "" :: fun_mappings @
    "" :: get_globals () @
    process_defs
  in
  save_result "promela model" "pml" promela;
  print_endline ("Copy spin/arinc_base.pml to same folder and then do: spin -a arinc.pml && cc -o pan pan.c && ./pan")
