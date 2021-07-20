(** Extract osek function calls. *)

open Prelude.Ana
open Analyses
module OList = List (* TODO get rid of *)
open BatteriesExceptionless

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name () = "extract_osek"

  let init () =
    LibraryFunctions.add_lib_funs (Pml.special_funs ())

  (* domains *)
  (* Process ID *)
  module Pid = IntDomain.Flattened
  (* context hash for function calls *)
  module Ctx = IntDomain.Flattened
  (* set of predecessor nodes *)
  module Pred = struct
    include SetDomain.Make (Basetype.ProgLocation)
    let of_node = singleton % Node.location
    let of_current_node () = of_node @@ Option.get !MyCFG.current_node
    let string_of_elt = Basetype.ProgLocation.show
  end
  module D = Lattice.Prod3 (Pid) (Ctx) (Pred)
  module C = D
  module Tasks = SetDomain.Make (Lattice.Prod (Queries.LS) (D)) (* set of created tasks to spawn when going multithreaded *)
  module G = Tasks
  let tasks_var = Goblintutil.create_var (makeGlobalVar "__GOBLINT_OSEK_TASKS" voidPtrType)

  type pname = string (* process name *)
  type fname = string (* function name *)
  type id = pname * fname
  type node  = location
  type action = Call of fname | Sys of string
  type edge = node * action * node

  let extracted : (id, edge Set.t) Hashtbl.t = Hashtbl.create 123

  let add_edge pid edge =
    Hashtbl.modify_def Set.empty pid (Set.add edge) extracted
  let get_edges pid =
    Hashtbl.find_default extracted pid Set.empty

  (* code generation *)
  module SymTbl (G : sig type k end) = struct (* generate int id *)
    let h : (G.k, 'v) Hashtbl.t = Hashtbl.create 13
    let get k =
      Option.default_delayed (fun () ->
          let v = List.length @@ List.of_enum @@ Hashtbl.keys h in
          Hashtbl.replace h k v;
          v) (Hashtbl.find h k)
    let inv v = Hashtbl.enum h |> List.of_enum |> List.assoc_inv v
    let to_list () = Hashtbl.enum h |> List.of_enum
  end

  module Pids = SymTbl (struct type k = pname end) (* process name -> int id *) (* TODO remove and use Res instead? *)
  module FunTbl = SymTbl (struct type k = fname*string end) (* function call (fname, target_label) -> int id *)

  module H (G : sig type k type v end) = struct
    let h : (G.k, G.v) Hashtbl.t = Hashtbl.create 13
    let add k v = Hashtbl.replace h k v
    let get k = Hashtbl.find h k
    let inv v = Hashtbl.enum h |> List.of_enum |> List.assoc_inv v
  end

  module Prios = H (struct type k = pname type v = int64 end) (* process name -> prio *)
  module Pfuns = H (struct type k = pname type v = fname end) (* process name -> function name *)

  module Res = struct (* (resource_kind, resource_name) -> (varinfo_id, int_id) *)
    let resources = Hashtbl.create 13
    let get (resource,name as k) =
      Option.default_delayed (fun () ->
          let vname = resource^":"^name in
          let v = Goblintutil.create_var (makeGlobalVar vname voidPtrType) in
          let i = Hashtbl.keys resources |> List.of_enum |> List.filter (fun x -> fst x = resource) |> List.length in
          Hashtbl.replace resources k (v,i);
          v,i) (Hashtbl.find resources k)
    let inv_by f k =
      Hashtbl.filter (fun k' -> f k' = k) resources |> Hashtbl.keys |> Enum.get
    let inv_v = inv_by fst
    let inv_i = inv_by snd
    let i_by_v v = snd (get (Option.get_exn (inv_v v) (Invalid_argument ("No resource found for variable "^v.vname))))
  end

  (* code generation *)
  let indent x = "  "^x
  let get_globals () = [] (* TODO *)

  let codegen_proc (pname, fname as id) =
    let pid = string_of_int @@ Pids.get pname in
    let pfun = Option.get @@ Pfuns.get pname in
    print_endline @@ "### codegen_proc: "^pname^", "^fname^", "^pfun;
    let is_proc = fname = pfun in
    let spid = if is_proc then "P_"^pname else "F_"^fname in
    let head = if is_proc then (* process *)
        let prio = match Prios.get pname with Some x -> " priority "^Int64.to_string x | None -> "" in
        "proctype "^pname^"(byte tid)"^prio^" provided (can_run("^pid^") PRIO"^pid^") {\n  int stack[20]; int sp = -1;"
      else (* some function call in process *)
        "Fun_"^fname^":"
    in
    (* build adjacency matrix for all nodes of this process *)
    let module HashtblN = Hashtbl.Make (Basetype.ProgLocation) in
    let a2bs = HashtblN.create 97 in
    Set.iter (fun (a, _, b as edge) -> HashtblN.modify_def Set.empty a (Set.add edge) a2bs) (get_edges id);
    let nodes = HashtblN.keys a2bs |> List.of_enum in
    (* let out_edges node = HashtblN.find_default a2bs node Set.empty |> Set.elements in (* Set.empty leads to Out_of_memory!? *) *)
    let out_edges node = try HashtblN.find a2bs node |> Set.elements with Not_found -> [] in
    let in_edges node = HashtblN.filter (Set.mem node % Set.map Tuple3.third) a2bs |> HashtblN.values |> List.of_enum |> flat_map Set.elements in
    let is_end_node = List.is_empty % out_edges in
    let is_start_node = List.is_empty % in_edges in
    let start_node = OList.find is_start_node nodes in (* node with no incoming edges is the start node *)
    let label n = spid ^ "_" ^ Pred.string_of_elt n in
    let end_label = spid ^ "_end" in
    let goto label = "goto " ^ label ^ ";" in
    let codegen_edge (a, action, b) =
      let target_label = if is_end_node b then end_label else label b in
      let str_action = match action with
        | Call fname ->
          let pc = string_of_int @@ FunTbl.get (fname,target_label) in
          "mark("^pc^"); " ^ goto ("Fun_"^fname)
        | Sys x -> x
      in
      (* for function calls the goto will never be reached since the function's return will already jump to that label; however it's nice to see where the program will continue at the site of the call. *)
      str_action ^ " " ^ goto target_label
    in
    let choice xs = List.map (fun x -> "::\t"^x ) xs in (* choices in if-statements are prefixed with :: *)
    let walk_edges (a, out_edges) =
      let edges = Set.elements out_edges |> List.map codegen_edge in
      (label a ^ ":") ::
      if List.length edges > 1 then
        "if" :: (choice edges) @ ["fi"]
      else
        edges
    in
    let locals = [] in (* TODO *)
    let body = locals @ goto (label start_node) :: (flat_map walk_edges (HashtblN.enum a2bs |> List.of_enum)) @ [end_label ^ ":" ^ if is_proc then " status[tid] = DONE" else " ret_"^fname^"()"] in
    String.concat "\n" @@ head :: List.map indent body @ [if is_proc then "}\n" else ""]

  let codegen () =
    let procs = Pids.to_list () in
    let nproc = Hashtbl.length OilUtil.tasks in
    Hashtbl.keys extracted |> List.of_enum |> List.iter (fun (pname, fname) -> print_endline @@ pname ^ "_" ^ fname);
    let proc_defs = Hashtbl.keys extracted |> List.of_enum |> List.map codegen_proc in
    let num_actions s = Hashtbl.values extracted |> Set.of_enum |> flip (Set.fold Set.union) Set.empty |> Set.filter (fun (_,a,_) -> match a with Sys a -> startsWith s a | _ -> false) |> Set.cardinal in
    let has_error_handler = num_actions "CreateErrorHandler" > 0 in
    let run_processes = procs |> List.filter_map (fun (name, id) -> if name = "main" then None else Some (id, "run "^name^"("^string_of_int id^");")) |> List.sort (compareBy fst) |> List.map snd in
    let init_body =
      "status[0] = READY;" ::
      "run main(0);" ::
      "(partition_mode == NORMAL); // TODO assert that all resources were created, see postInit" ::
      (*"run monitor();" ::*)
      (if has_error_handler then "run ErrorHandler("^string_of_int (Pids.get "ErrorHandler")^")" else "// no ErrorHandler") ::
      run_processes
    in
    (* used for macros oneIs, allAre, noneAre... *)
    let checkStatus = "(" ^ (String.concat " op2 " @@ List.of_enum @@ (0 --^ nproc) /@ (fun i -> "status["^string_of_int i^"] op1 v")) ^ ")" in
    (* generate priority based running constraints for each process (only used ifdef PRIOS): process can only run if no higher prio process is ready *)
    let prios =
      let def (pname,id) =
        let pri = Prios.get pname in
        let higher = List.filter (fun (n,_) -> Prios.get n > pri) procs in
        if List.is_empty higher
        then None
        else Some ("#undef PRIO" ^ string_of_int id ^ "\n#define PRIO" ^ string_of_int id ^ String.concat "" @@ List.map (fun (_,i) -> " && status[" ^ string_of_int i ^ "] != READY") higher)
      in
      List.filter_map def procs
    in
    let fun_mappings =
      let fun_map xs =
        if List.is_empty xs then [] else
          let (name,_),_ = OList.hd xs in
          let entries = xs |> List.map (fun ((_,k),v) -> "\t:: (stack[sp] == " ^ string_of_int v ^ ") -> sp--; goto " ^ k ^" \\") in
          let debug_str = if GobConfig.get_bool "ana.pml.debug" then "\t:: else -> printf(\"wrong pc on stack!\"); assert(false) " else "" in
          ("#define ret_"^name^"() if \\") :: entries @ [debug_str ^ "fi"]
      in
      FunTbl.to_list () |> List.group (compareBy (fst%fst)) |> flat_map fun_map
    in
    String.concat "\n" @@
    ("#define checkStatus(op1, v, op2) "^checkStatus) :: "" ::
    "#include \"osek.os.pml\"" :: "" ::
    "init {" :: List.map indent init_body @ "}" :: "" ::
                                            (List.of_enum @@ (0 --^ nproc) /@ (fun i -> "#define PRIO" ^ string_of_int i)) @
    "#ifdef PRIOS" :: prios @ "#endif" ::
                              "" :: fun_mappings @
    "" :: get_globals () @
    proc_defs

  (* queries *)
  let query ctx (type a) (q: a Queries.t) =
    match q with
    | _ -> Queries.Result.top q

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    match List.assoc "base" ctx.presub with
    | Some base ->
      let pid, ctxh, pred = ctx.local in
      let module BaseMain = (val Base.get_main ()) in
      let base_context = BaseMain.context_cpa @@ Obj.obj base in
      let context_hash = Hashtbl.hash (base_context, pid) in
      pid, Ctx.of_int (Int64.of_int context_hash), pred
    | None -> ctx.local (* TODO when can this happen? *)

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let d_caller = ctx.local in
    let pid, ctxh, pred = ctx.local in
    let d_callee = if D.is_bot ctx.local then ctx.local else pid, Ctx.top (), Pred.of_node (MyCFG.Function f) in (* set predecessor set to start node of function *)
    [d_caller, d_callee]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    if D.is_bot ctx.local || D.is_bot au then ctx.local else
      let pid, ctxh, pred = ctx.local in (* caller *)
      let _ , _, pred' = au in (* callee *)
      (* check if the callee has some relevant edges, i.e. advanced from the entry point. if not, we generate no edge for the call and keep the predecessors from the caller *)
      if Pred.is_bot pred' then failwith "d_callee.pred is bot!"; (* set should never be empty *)
      if Pred.equal pred' (Pred.of_node (MyCFG.Function f)) then
        ctx.local
      else (
        (* set current node as new predecessor, since something interesting happend during the call *)
        pid, ctxh, Pred.of_node (Option.get !MyCFG.current_node )
      )

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    if not (String.starts_with f.vname "LAP_Se_") then ctx.local else
      let pid, ctx_hash, pred = ctx.local in
      if Pid.is_bot pid || Ctx.is_bot ctx_hash || Pred.is_bot pred then ctx.local else
        let pname = Pid.to_int pid |> Option.get |> Int64.to_int |> Pids.inv |> Option.get in
        let fname = str_remove "LAP_Se_" f.vname in
        let eval_int exp =
          match ctx.ask (Queries.EvalInt exp) with
          | x when Queries.ID.is_int x -> [IntOps.BigIntOps.to_string (Option.get @@ Queries.ID.to_int x)]
          | _ -> failwith @@ "Could not evaluate int-argument "^sprint d_plainexp exp
        in
        let eval_str exp =
          match ctx.ask (Queries.EvalStr exp) with
          | `Lifted x -> [x]
          | _ -> failwith @@ "Could not evaluate string-argument "^sprint d_plainexp exp
        in
        let eval_id exp =
          let module LS = Queries.LS in
          match ctx.ask (Queries.MayPointTo exp) with
          | x when not (LS.is_top x) ->
            let top_elt = dummyFunDec.svar, `NoOffset in
            if LS.mem top_elt x then M.debug_each "Query result for MayPointTo contains top!";
            let xs = LS.remove top_elt x |> LS.elements in
            List.map (fun (v,o) -> string_of_int (Res.i_by_v v)) xs
          | _ -> failwith @@ "Could not evaluate id-argument "^sprint d_plainexp exp
        in
        let assign_id exp id =
          if M.tracing then M.trace "extract_osek" "assign_id %a %s\n" d_exp exp id.vname;
          match exp with
          | AddrOf lval -> ctx.assign ~name:"base" lval (mkAddrOf @@ var id)
          | _ -> failwith @@ "Could not assign id. Expected &id. Found "^sprint d_exp exp
        in
        (* evaluates an argument and returns a list of possible values for that argument. *)
        let eval = function
          | Pml.EvalSkip -> const None
          | Pml.EvalInt -> fun e -> Some (try eval_int e with _ -> eval_id e)
          | Pml.EvalString -> fun e -> Some (List.map (fun x -> "\""^x^"\"") (eval_str e))
          | Pml.EvalEnum f -> fun e -> Some (List.map (fun x -> Option.get (f (int_of_string x))) (eval_int e))
          | Pml.AssignIdOfString (res, pos) -> fun e ->
            (* evaluate argument at i as string *)
            let name = OList.hd @@ eval_str (OList.at arglist pos) in
            (* generate variable from it *)
            let v,i = Res.get (res, name) in
            (* assign generated variable in base *)
            assign_id e v;
            Some [string_of_int i]
        in
        let node = Option.get !MyCFG.current_node in
        let fundec = Node.find_fundec node in
        let id = pname, fundec.svar.vname in
        let extract_fun ?(info_args=[]) args =
          let comment = if List.is_empty info_args then "" else " /* " ^ String.concat ", " info_args ^ " */" in (* append additional info as comment *)
          let action = fname^"("^String.concat ", " args^");"^comment in
          print_endline @@ "EXTRACT in "^pname^": "^action;
          Pred.iter (fun pred -> add_edge id (pred, Sys action, Node.location node)) pred;
          pid, ctx_hash, Pred.of_node node
        in
        match Pml.special_fun fname with
        | None -> M.debug_each ("extract_osek: unhandled function "^fname); ctx.local
        | Some eval_args ->
          if M.tracing then M.trace "extract_osek" "extract %s, args: %i code, %i pml\n" f.vname (List.length arglist) (List.length eval_args);
          let rec combine_opt f a b = match a, b with
            | [], [] -> []
            | x::xs, y::ys -> (x,y) :: combine_opt f xs ys
            | [], x::xs -> f None (Some x) :: combine_opt f [] xs
            | x::xs, [] -> f (Some x) None :: combine_opt f xs []
          in
          (* combine list of eval rules with list of arguments, fill with Skip *)
          let combine_skip a b = combine_opt (curry @@ function None, Some e -> Pml.EvalSkip, e | _, _ -> assert false) a b in
          print_endline @@ String.concat "; " @@ List.map (fun (e,a) -> Pml.show_eval e^", "^sprint d_exp a) (combine_skip eval_args arglist);
          let args_eval = List.filter_map (uncurry eval) @@ combine_skip eval_args arglist in
          List.iter (fun args -> assert (args <> [])) args_eval; (* arguments that are not skipped always need to evaluate to at least one value *)
          print_endline @@ "osek: FUN " ^ fname ^ " with args_eval " ^ String.concat "; " (List.map (String.concat ", ") args_eval);
          let args_product = List.n_cartesian_product @@ args_eval in
          print_endline @@ "osek: FUN " ^ fname ^ " with args_product " ^ String.concat "; " (List.map (String.concat ", ") args_product);
          List.fold_left (fun d args ->
              let str_args, args = List.partition (flip String.starts_with "\"") args in (* strings can't be arguments, but we want them as a comment *)
              extract_fun ~info_args:str_args args
            ) ctx.local args_product

  let startstate v = Pid.of_int 0L, Ctx.top (), Pred.of_node (MyCFG.Function (emptyFunction "main"))
  let threadenter ctx lval f args = [D.bot ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.bot ()

  let init () = (* registers which functions to extract and writes out their definitions *)
    Osek.Spec.parse_oil ();
    let mainfuns = List.map Json.string (GobConfig.get_list "mainfun") in
    ignore @@ List.map Pids.get mainfuns;
    ignore @@ List.map (fun name -> Res.get ("process", name)) mainfuns;
    assert (List.length mainfuns = 1); (* TODO? *)
    List.iter (fun fname -> Pfuns.add "main" fname) mainfuns;
    output_file (Goblintutil.create_dir "result/" ^ "osek.os.pml") (snd (Pml_osek.init ()))

  let finalize () = (* writes out collected cfg *)
    (* TODO call Pml_osek.init again with the right number of resources to find out of bounds accesses? *)
    output_file "result/osek.pml" (codegen ())
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
