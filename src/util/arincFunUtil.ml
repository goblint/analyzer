open Batteries
open Cil
module M  = Messages

(* ARINC types and Hashtables for collecting CFG *)
type resource = Process | Function | Semaphore | Event | Logbook | SamplingPort | QueuingPort | Buffer | Blackboard
let str_resource_type = function
  | Process -> "Process"
  | Function -> "Function"
  | Semaphore -> "Semaphore"
  | Event -> "Event"
  | Logbook -> "Logbook"
  | SamplingPort -> "SamplingPort"
  | QueuingPort -> "QueuingPort"
  | Buffer -> "Buffer"
  | Blackboard -> "Blackboard"
(* id is resource type and name, there is a 1:1 mapping to varinfo in the analysis uses for assignments *)
type id = resource*string
type ids = id list
type time = int64 (* Maybe use Nativeint which is the same as C long. OCaml int is just 31 or 63 bits wide! *)
module Action = (* encapsulate types because some process field names are also used for D.t -> use local opening of modules (since OCaml 4.00) for output *)
struct
  type process = { pid: id; funs: varinfo list; pri: int64; per: time; cap: time }
  type semaphore = { sid: id; cur: int64; max: int64; queuing: int64 }
end
type action =
  | Nop
  | Cond of string * string
  | Param of string * string (* var_callee = var_caller *)
  | Call of string
  | LockPreemption | UnlockPreemption | SetPartitionMode of int64
  | CreateProcess of Action.process | CreateErrorHandler of id * varinfo list | Start of ids | Stop of ids | Suspend of ids | Resume of ids
  | CreateBlackboard of id | DisplayBlackboard of ids | ReadBlackboard of ids * time | ClearBlackboard of ids
  | CreateSemaphore of Action.semaphore | WaitSemaphore of ids | SignalSemaphore of ids
  | CreateEvent of id | WaitEvent of ids * time | SetEvent of ids | ResetEvent of ids
  | TimedWait of time | PeriodicWait
type node = MyCFG.node
let string_of_node = ArincFunDomain.Pred.string_of_elt
type edge = node * action * string option * node
let action_of_edge (_, action, _, _) = action
type edges = (id, edge Set.t) Hashtbl.t
let edges = ref (Hashtbl.create 199 : edges)

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
    | CreateProcess x when x.Action.pid=id -> Some x.Action.funs
    | CreateErrorHandler (id', funs) when id'=id -> Some funs
    | _ -> None
  in
  filter_map_actions get_funs |> List.concat |> List.unique

let return_vars = (Hashtbl.create 100 : (id * [`Branch | `Call], string Set.t) Hashtbl.t)
let add_return_var pid kind var = Hashtbl.modify_def Set.empty (pid, kind) (Set.add var) return_vars
let get_return_vars pid kind =
  Hashtbl.find_default return_vars (pid, kind) Set.empty

(* constants and helpers *)
let infinity = 4294967295L (* time value used for infinity *)
let string_of_partition_mode = function
  | 0L -> "IDLE"
  | 1L -> "COLD_START"
  | 2L -> "WARM_START"
  | 3L -> "NORMAL"
  | _  -> "UNKNOWN!"
let string_of_queuing_discipline = function
  | 0L -> "FIFO"
  | 1L -> "PRIO"
  | _  -> "UNKNOWN!"

(* ARINC output *)
(* common *)
let str_i64 id = string_of_int (i64_to_int id)
let str_time t = if t = infinity then "∞" else str_i64 t^"ns"
(* console and dot *)
let str_funs funs = "["^(List.map (fun v -> v.vname) funs |> String.concat ", ")^"]"
let str_resource id =
  match id with
  | Process, "mainfun" ->
      "mainfun/["^String.concat ", " (List.map Json.string (GobConfig.get_list "mainfun"))^"]"
  | Process, name ->
      name^"/"^str_funs @@ funs_for_process id
  | resource_type, name ->
      name
let str_resources ids = "["^(String.concat ", " @@ List.map str_resource ids)^"]"
let str_action pid = function
  | Nop -> "Nop"
  | Cond (r, cond) -> if Set.mem r (get_return_vars pid `Call) then "If "^cond else ""
  | Param (callee, caller) -> "Assign "^callee^" = "^caller
  | Call fname -> "Call "^fname
  | LockPreemption -> "LockPreemption"
  | UnlockPreemption -> "UnlockPreemption"
  | SetPartitionMode i -> "SetPartitionMode "^string_of_partition_mode i
  | CreateProcess x ->
      Action.("CreateProcess "^str_resource x.pid^" (funs "^str_funs x.funs^", prio "^str_i64 x.pri^", period "^str_time x.per^", capacity "^str_time x.cap^")")
  | CreateErrorHandler (id, funs) -> "CreateErrorHandler "^str_resource id
  | Start ids -> "Start "^str_resources ids
  | Stop ids when ids=[pid] -> "StopSelf"
  | Stop ids -> "Stop "^str_resources ids
  | Suspend ids when ids=[pid] -> "SuspendSelf"
  | Suspend ids -> "Suspend "^str_resources ids
  | Resume ids -> "Resume "^str_resources ids
  | CreateBlackboard id -> "CreateBlackboard "^str_resource id
  | DisplayBlackboard ids -> "DisplayBlackboard "^str_resources ids
  | ReadBlackboard (ids, timeout) -> "ReadBlackboard "^str_resources ids^" (timeout "^str_time timeout^")"
  | ClearBlackboard ids -> "ClearBlackboard "^str_resources ids
  | CreateSemaphore x ->
      Action.("CreateSemaphore "^str_resource x.sid^" ("^str_i64 x.cur^"/"^str_i64 x.max^", "^string_of_queuing_discipline x.queuing^")")
  | WaitSemaphore ids -> "WaitSemaphore "^str_resources ids
  | SignalSemaphore ids -> "SignalSemaphore "^str_resources ids
  | CreateEvent id -> "CreateEvent "^str_resource id
  | WaitEvent (ids, timeout) -> "WaitEvent "^str_resources ids^" (timeout "^str_time timeout^")"
  | SetEvent ids -> "SetEvent "^str_resources ids
  | ResetEvent ids -> "ResetEvent "^str_resources ids
  | TimedWait t -> "TimedWait "^str_time t
  | PeriodicWait -> "PeriodicWait"
let str_return_code = function Some r -> " : " ^ r | None -> ""
(* spin/promela *)
let pml_resources = Hashtbl.create 13
let _ = Hashtbl.add pml_resources (Process, "mainfun") 0L
let id_pml id = (* give ids starting from 0 (get_pid_by_id for all resources) *)
  let resource, name as k = id in
  try Hashtbl.find pml_resources k
  with Not_found ->
    let ids = Hashtbl.filteri (fun (r,n) v -> r=resource) pml_resources |> Hashtbl.values in
    let id = if Enum.is_empty ids then 0L else Int64.succ (Enum.arg_max identity ids) in
    Hashtbl.replace pml_resources k id;
    id
let str_id_pml id = str_i64 @@ id_pml id
let str_pid_pml id = (if fst id = Process then "P" else "F") ^ str_id_pml id (* process or function *)
let str_ids_pml ids f = String.concat " " (List.map (f%str_id_pml) ids)
let str_action_pml pid = function
  | Nop -> ""
  | Cond (r, cond) -> if Set.mem r (get_return_vars pid `Call) then cond ^ " -> " else ""
  | Param (callee, caller) -> callee^" = "^caller^";"
  | Call fname ->
      (* we shouldn't have calls to functions without edges! *)
      if Hashtbl.mem !edges (Function, fname) then "Fun_"^fname^"();" else failwith @@ "call to undefined function " ^ fname
  | LockPreemption -> "LockPreemption();"
  | UnlockPreemption -> "UnlockPreemption();"
  | SetPartitionMode i -> "SetPartitionMode("^string_of_partition_mode i^");"
  | CreateProcess x ->
      Action.("CreateProcess("^str_id_pml x.pid^", "^str_i64 x.pri^", "^str_i64 x.per^", "^str_i64 x.cap^"); /* "^str_resource x.pid^" (prio "^str_i64 x.pri^", period "^str_time x.per^", capacity "^str_time x.cap^") */")
  | CreateErrorHandler (id, funs) -> "CreateErrorHandler("^str_id_pml id^");"
  | Start ids -> str_ids_pml ids (fun id -> "Start("^id^");")
  | Stop ids -> str_ids_pml ids (fun id -> "Stop("^id^");")
  | Suspend ids -> str_ids_pml ids (fun id -> "Suspend("^id^");")
  | Resume ids -> str_ids_pml ids (fun id -> "Resume("^id^");")
  | CreateBlackboard id -> "CreateBlackboard("^str_id_pml id^");"
  | DisplayBlackboard ids -> str_ids_pml ids (fun id -> "DisplayBlackboard("^id^");")
  | ReadBlackboard (ids, timeout) -> str_ids_pml ids (fun id -> "ReadBlackboard("^id^");")
  | ClearBlackboard ids -> str_ids_pml ids (fun id -> "ClearBlackboard("^id^");")
  | CreateSemaphore x ->
      Action.("CreateSemaphore("^str_id_pml x.sid^", "^str_i64 x.cur^", "^str_i64 x.max^", "^string_of_queuing_discipline x.queuing^");")
  | WaitSemaphore ids -> str_ids_pml ids (fun id -> "WaitSemaphore("^id^");")
  | SignalSemaphore ids -> str_ids_pml ids (fun id -> "SignalSemaphore("^id^");")
  | CreateEvent id -> "CreateEvent("^str_id_pml id^");"
  | WaitEvent (ids, timeout) -> str_ids_pml ids (fun id -> "WaitEvent("^id^");")
  | SetEvent ids -> str_ids_pml ids (fun id -> "SetEvent("^id^");")
  | ResetEvent ids -> str_ids_pml ids (fun id -> "ResetEvent("^id^");")
  | TimedWait t -> "TimedWait("^str_i64 t^");"
  | PeriodicWait -> "PeriodicWait();"
let str_return_code_pml id action = function
  | Some r -> "if :: "^action^" "^r^" = SUCCESS :: "^r^" = ERROR fi;"
  | _ -> action

(* helpers *)
let comp2 f g a b = f (g a) (g b) (* why is this not in batteries? *)
let compareBy ?cmp:(cmp=compare) f = comp2 cmp f
let find_option p xs = try Some (List.find p xs) with Not_found -> None (* why is this in batteries for Hashtbl but not for List? *)
let flat_map f = List.flatten % List.map f (* and this? *)

(* simplify graph here, i.e. merge functions which consist of the same edges and contract call chains *)
let simplify () =
  let dups = Hashtbl.enum !edges |> List.of_enum |> List.group (compareBy ~cmp:Set.compare snd) |> List.filter_map (function x::y::ys -> Some (x, y::ys) | _ -> None) in
  let replace_call oldname newname =
    (* M.debug_each @@ "Replacing function calls to "^oldname^" with "^newname; *)
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

(* print to stdout *)
let print_actions () =
  let print_process pid =
    let str_node = string_of_node in
    let str_edge (a, action, r, b) = str_node a ^ " -> " ^ str_action pid action ^ str_return_code r ^ " -> " ^ str_node b in
    let xs = Set.map str_edge (get_edges pid) in
    M.debug @@ str_resource pid^" ->\n\t"^String.concat "\n\t" (Set.elements xs)
  in
  Hashtbl.keys !edges |> Enum.iter print_process

(* helper for exporting results *)
let save_result desc ext content = (* output helper *)
  let dir = Goblintutil.create_dir "result" in (* returns abs. path *)
  let path = dir ^ "/arinc.fun." ^ ext in
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
  let run_processes = List.map (fun x -> let name = snd x.pid in let id = id_pml x.pid in id, "run "^name^"("^str_i64 id^");") procs |> List.sort (compareBy fst) |> List.map snd in
  let init_body =
    "preInit;" ::
    "run mainfun(0);" :: (* keep mainfun as name for init process? *)
    "postInit();" ::
    "run monitor();" ::
    (if has_error_handler then "run ErrorHandler("^str_id_pml (Process, "ErrorHandler")^")" else "// no ErrorHandler") ::
    run_processes
  in
  let process_def id =
    let pid = id_pml id in (* id is type*name, pid is int64 *)
    let spid = str_pid_pml id in
    (* build adjacency matrix for all nodes of this process *)
    let module HashtblN = Hashtbl.Make (ArincFunDomain.Pred.Base) in
    let a2bs = HashtblN.create 97 in
    Set.iter (fun (a, _, _, b as edge) -> HashtblN.modify_def Set.empty a (Set.add edge) a2bs) (get_edges id);
    let nodes = HashtblN.keys a2bs |> List.of_enum in
    (* let get_a (a,_,_) = a in *)
    let get_b (_,_,_,b) = b in
    (* let out_edges node = HashtblN.find_default a2bs node Set.empty |> Set.elements in (* Set.empty leads to Out_of_memory!? *) *)
    let out_edges node = try HashtblN.find a2bs node |> Set.elements with Not_found -> [] in
    let in_edges node = HashtblN.filter (Set.mem node % Set.map get_b) a2bs |> HashtblN.values |> List.of_enum |> flat_map Set.elements in
    let start_node = List.find (List.is_empty % in_edges) nodes in (* node with no incoming edges is the start node *)
    (* let str_nodes xs = "{"^(List.map string_of_node xs |> String.concat ",")^"}" in *)
    let label n = spid ^ "_" ^ string_of_node n in
    let end_label = spid ^ "_end" in
    let goto node = "goto " ^ label node in
    let str_edge (a, action, r, b) = let target = if List.is_empty (out_edges b) then "goto "^end_label else goto b in str_return_code_pml id (str_action_pml id action) r ^ " " ^ target in
    let choice xs = List.map (fun x -> "::\t"^x ) xs in (* choices in if-statements are prefixed with :: *)
    let walk_edges (a, out_edges) =
      (* str_action_pml filters out calls to functions that have no definitions *)
      let edges = Set.elements out_edges |> List.map str_edge in
      (label a ^ ":") ::
      if List.length edges > 1 then
        "if" :: (choice edges) @ ["fi"]
      else
        edges
    in
    let locals = if not @@ GobConfig.get_bool "ana.arinc.assume_success" && fst id = Process then Set.elements (Set.union (get_return_vars id `Branch) (get_return_vars id `Call)) |> List.map (fun vname -> "byte " ^ vname ^ ";") else [] in
    let body = locals @ goto start_node :: (flat_map walk_edges (HashtblN.enum a2bs |> List.of_enum)) @ [end_label ^ ":" ^ if fst id = Process then " status[id] = DONE" else ""] in
    let head = match id with
      | Process, name ->
          let proc = find_option (fun x -> x.pid=id) procs in (* None for mainfun *)
          let priority = match proc with Some proc -> " priority "^str_i64 proc.pri | _ -> "" in
          "proctype "^name^"(byte id)"^priority^" provided (canRun("^str_i64 pid^") PRIO"^str_i64 pid^") {"
      | Function, name ->
          "inline Fun_"^name^"() {"
      | _ -> failwith "Only Process and Function are allowed as keys for collecting ARINC actions"
    in
    "" :: head :: List.map indent body @ ["}"]
  in
  (* used for macros oneIs, allAre, noneAre... *)
  let checkStatus = "(" ^ (String.concat " op2 " @@ List.of_enum @@ (0 --^ nproc) /@ (fun i -> "status["^string_of_int i^"] op1 v")) ^ ")" in
  (* generate priority based running constraints for each process (only used ifdef PRIOS): process can only run if no higher prio process is ready *)
  let prios =
    let def proc =
      let id = str_id_pml proc.pid in
      let pri = proc.pri in
      let higher = List.filter (fun x -> x.pri > pri) procs in
      if List.is_empty higher
      then None
      else Some ("#define PRIO" ^ id ^ String.concat "" @@ List.map (fun x -> " && status[" ^ str_id_pml x.pid ^ "] != READY") higher)
    in
    List.filter_map def procs
  in
  (* sort definitions so that inline functions come before the processes *)
  let process_defs = Hashtbl.keys !edges |> List.of_enum |> List.sort (compareBy str_pid_pml) |> List.map process_def |> List.concat in
  let promela = String.concat "\n" @@
    ("#define nproc "^string_of_int nproc) ::
    ("#define nbboard "^string_of_int nbboard) ::
    ("#define nsema "^string_of_int nsema) ::
    ("#define nevent "^string_of_int nevent) :: "" ::
    ("#define checkStatus(op1, v, op2) "^checkStatus) :: "" ::
    "#include \"arinc.base.pml\"" :: "" ::
    "init {" :: List.map indent init_body @ "}" :: "" ::
    (List.of_enum @@ (0 --^ nproc) /@ (fun i -> "#define PRIO" ^ string_of_int i)) @
    "#ifdef PRIOS" :: prios @ "#endif" ::
    process_defs
  in
  save_result "promela model" "pml" promela;
  print_endline ("Copy spin/arinc_base.pml to same folder and then do: spin -a arinc.pml && cc -o pan pan.c && ./pan")
