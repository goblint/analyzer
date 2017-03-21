(** Extract function calls and variables. *)

open Prelude.Ana
open Analyses
module OList = List (* TODO get rid of *)
open BatteriesExceptionless

(* helpers *)
let comp2 f g a b = f (g a) (g b)
let compareBy ?cmp:(cmp=compare) f = comp2 cmp f
let find_option p xs = try Some (List.find p xs) with Not_found -> None
let flat_map f = List.flatten % List.map f

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "extract_arinc"

  let init () =
    LibraryFunctions.add_lib_funs (Pml.special_funs ())

  (* domains *)
  (* Process ID *)
  module Pid = IntDomain.Flattened
  (* context hash for function calls *)
  module Ctx = IntDomain.Flattened
  (* predecessor nodes *)
  module Pred =
  struct
    module Base =
    struct
      (* include Basetype.ProgLines *)
      (* copied and adjusted from Basetype.ProgLine... Problem was that it doesn't discern byte *)
      open Pretty
      type t = location
      let isSimple _  = true
      let equal = (=)
      let compare = compare
      let hash = Hashtbl.hash
      let toXML_f sf x = Xml.Element ("Loc", [("file", x.file); ("line", string_of_int x.line); ("byte", string_of_int x.byte); ("text", sf 80 x)], [])
      (* let short _ x = if x <> locUnknown then Filename.basename x.file ^ ":" ^ string_of_int x.line else "S" *)
      let show loc =
        let f i = (if i < 0 then "n" else "") ^ string_of_int (abs i) in
        f loc.line ^ "b" ^ f loc.byte
      let short w x = show x
      let pretty_f sf () x = text (sf max_int x)
      let toXML m = toXML_f short m
      let pretty () x = pretty_f short () x
      let name () = "proglines_byte"
      let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
      let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
    end
    include SetDomain.Make (Base)
    let of_node = singleton % MyCFG.getLoc
    let of_current_node () = of_node @@ Option.get !MyCFG.current_node
    let string_of_elt = Base.show
  end
  module D = Lattice.Prod3 (Pid) (Ctx) (Pred)
  module C = D
  module Tasks = SetDomain.Make (Lattice.Prod (Queries.LS) (D)) (* set of created tasks to spawn when going multithreaded *)
  module G = Tasks
  let tasks_var = makeGlobalVar "__GOBLINT_ARINC_TASKS" voidPtrType

  type pname = string (* process name *)
  type fname = string (* function name *)
  type id = pname * fname option
  type node  = location
  type action = string
  type edge = node * action * node
  let extracted : (id, edge Set.t) Hashtbl.t = Hashtbl.create 123

  let add_edge pid edge =
    Hashtbl.modify_def Set.empty pid (Set.add edge) extracted

  (* code generation *)
  module SymTbl () =
  struct
    let h = Hashtbl.create 123
    let get k =
      Option.default_delayed (fun () ->
        let v = List.length @@ List.of_enum @@ Hashtbl.keys h in
        Hashtbl.replace h k v;
        v) (Hashtbl.find h k)
    let inv v = Hashtbl.enum h |> List.of_enum |> List.assoc_inv
    let to_list () = Hashtbl.enum h |> List.of_enum
  end
  module ProcIds = SymTbl ()
  let id_of_proc name = ProcIds.get name
  let prio_of_proc name = None (* TODO *)
  let codegen_proc (pname, fname) edges =
    let id = string_of_int @@ id_of_proc name in
    let prio = match prio_of_proc name with Some x -> " priority "^Int64.to_string x | None -> "" in
    "proctype "^name^"(byte id)"^prio^" provided (canRun("^id^") PRIO"^id^") {\nint stack[20]; int sp = -1;"
  let codegen () =
    let procs = Hashtbl.enum extracted |> List.of_enum |> List.map (uncurry codegen_proc) |> String.concat "\n" in
    procs

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
    | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    (*let pid, ctxh, pred = ctx.local in
    let base_context = fst @@ Base.Main.context @@ Obj.obj @@ List.assoc "base" ctx.presub in
    let context_hash = Hashtbl.hash (base_context, pid) in
    pid, Ctx.of_int (Int64.of_int context_hash), pred*)
    ctx.local (* TODO above throws Not_found *)

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let d_caller = ctx.local in
    let pid, ctxh, pred = ctx.local in
    let d_callee = if D.is_bot ctx.local then ctx.local else pid, Ctx.top (), Pred.of_node (MyCFG.Function f) in (* set predecessor set to start node of function *)
    [d_caller, d_callee]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  (* generate id (varinfo, int) via (resource, name) *)
  let resources = Hashtbl.create 13
  let get_id (resource,name as k) =
    Option.default_delayed (fun () ->
      let vname = resource^":"^name in
      let v = makeGlobalVar vname voidPtrType in
      let i = Hashtbl.keys resources |> List.of_enum |> List.filter (fun x -> fst x = resource) |> List.length in
      Hashtbl.replace resources k (v,i);
      v,i) (Hashtbl.find resources k)
  let get_by_id id =
    Hashtbl.filter (fun (v,i) -> v = id) resources |> Hashtbl.keys |> Enum.get

  (* map process name to integer used in Pid domain *)
  let pnames = Hashtbl.create 13
  let _ = Hashtbl.add pnames "mainfun" 0L
  let get_by_pid pid =
    Hashtbl.filter ((=) pid) pnames |> Hashtbl.keys |> Enum.get
  let get_pid pname =
    Option.default_delayed (fun () ->
      let ids = Hashtbl.values pnames in
      let id = if Enum.is_empty ids then 1L else Int64.succ (Enum.arg_max identity ids) in
      Hashtbl.replace pnames pname id;
      id) (Hashtbl.find pnames pname)
  let get_pid_by_id id = get_by_id id |> Option.get |> snd |> get_pid

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    if not (String.starts_with f.vname "LAP_Se_") then ctx.local else
    let pid, ctx_hash, pred = ctx.local in
    if Pid.is_bot pid || Ctx.is_bot ctx_hash || Pred.is_bot pred then ctx.local else
    let pname = Option.get (get_by_pid (Option.get (Pid.to_int pid))) in
    let str_remove m s = String.nreplace ~str:s ~sub:m ~by:"" in
    let fname = str_remove "LAP_Se_" f.vname in
    let sprint f x = Pretty.sprint 80 (f () x) in
    let eval_int exp =
      match ctx.ask (Queries.EvalInt exp) with
      | `Int x -> [Int64.to_string x]
      | _ -> failwith @@ "Could not evaluate int-argument "^sprint d_plainexp exp
    in
    let eval_str exp =
      match ctx.ask (Queries.EvalStr exp) with
      | `Str x -> [x]
      | _ -> failwith @@ "Could not evaluate string-argument "^sprint d_plainexp exp
    in
    let eval_id exp =
      match ctx.ask (Queries.MayPointTo exp) with
      | `LvalSet x when not (Queries.LS.is_top x) ->
        let top_elt = dummyFunDec.svar, `NoOffset in
        if Queries.LS.mem top_elt x then M.debug_each "Query result for MayPointTo contains top!";
        let xs = Queries.LS.elements x in
        (* assert (List.length xs = 1); (* TODO return list list instead, outer list is number of arguments, inner list is possible values -> cross product for edges *) *)
        let id2i (id,o) =
          let v,i = Hashtbl.filter (fun (v,i) -> v = id) resources |> Hashtbl.values |> List.of_enum |> OList.hd in
          string_of_int i
        in
        List.map id2i xs
      | _ -> failwith @@ "Could not evaluate id-argument "^sprint d_plainexp exp
    in
    let assign_id exp id =
      if M.tracing then M.trace "extract_arinc" "assign_id %a %s\n" d_exp exp id.vname;
      match exp with
      | AddrOf lval -> ctx.assign ~name:"base" lval (mkAddrOf @@ var id)
      | _ -> failwith @@ "Could not assign id. Expected &id. Found "^sprint d_exp exp
    in
    (* evaluates an argument. returns a list because a struct argument could contain multiple arguments that are relevant, and some arguments are not relevant at all. the inner list is the possible values for that argument. *)
    let eval = function
      | Pml.EvalSkip -> const []
      | Pml.EvalInt -> fun e -> [try eval_int e with _ -> eval_id e]
      | Pml.EvalString -> fun e -> [List.map (fun x -> "\""^x^"\"") (eval_str e)]
      | Pml.EvalEnum f -> fun e -> [List.map (fun x -> Option.get (f (int_of_string x))) (eval_int e)]
      | Pml.AssignIdOfString (res, pos) -> fun e ->
          (* evaluate argument at i as string *)
          let name = OList.hd @@ eval_str (OList.at arglist pos) in
          (* generate variable from it *)
          let v,i = get_id (res, name) in
          (* assign generated variable in base *)
          assign_id e v;
          [[string_of_int i]]
    in
    let node = Option.get !MyCFG.current_node in
    let fundec = MyCFG.getFun node in
    let id = pname, Some fundec.svar.vname in
    let extract_fun ?(info_args=[]) args =
      let comment = if List.is_empty info_args then "" else " // " ^ String.concat ", " info_args in (* append additional info as comment *)
      let action = fname^"("^String.concat ", " args^");"^comment in
      (*print_endline @@ "EXTRACT in "^pid^": "^call;*)
      Pred.iter (fun pred -> add_edge id (pred, action, MyCFG.getLoc node)) pred;
      pid, ctx_hash, Pred.of_node node
    in
    match fname, arglist with (* first some special cases *)
    | "CreateProcess", [AddrOf attr; pid'; r] ->
      let cm = match unrollType (typeOfLval attr) with
        | TComp (c,_) -> c
        | _ -> failwith "type-error: first argument of LAP_Se_CreateProcess not a struct."
      in
      let struct_fail f x =
        f @@ "LAP_Se_CreateProcess: problem with first argument: " ^
              begin match x with
                | `Field ofs -> "cannot access field " ^ ofs
                | `Result (name, entry_point, pri, per, cap) ->
                  "struct PROCESS_ATTRIBUTE_TYPE needs all of the following fields (with result): NAME ("^name^"), ENTRY_POINT ("^entry_point^"), BASE_PRIORITY ("^pri^"), PERIOD ("^per^"), TIME_CAPACITY ("^cap^")"
              end ^ ". Running scrambled: "^string_of_bool Goblintutil.scrambled
      in
      let field ofs =
        try Lval (addOffsetLval (Field (getCompField cm ofs, NoOffset)) attr)
        with Not_found -> struct_fail failwith (`Field ofs)
      in
      let name = ctx.ask (Queries.EvalStr (field Goblintutil.arinc_name)) in
      let entry_point = ctx.ask (Queries.ReachableFrom (AddrOf attr)) in
      let pri  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_base_priority)) in
      let per  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_period)) in
      let cap  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_time_capacity)) in
      begin match name, entry_point, pri, per, cap with
        | `Str name, `LvalSet ls, `Int pri, `Int per, `Int cap when not (Queries.LS.is_top ls)
                                                                  && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
          let funs_ls = Queries.LS.filter (fun (v,o) -> let lval = Var v, Lval.CilLval.to_ciloffs o in isFunctionType (typeOfLval lval)) ls in (* do we need this? what happens if we spawn a variable that's not a function? shouldn't this check be in spawn? *)
          if M.tracing then M.tracel "extract_arinc" "starting a thread %a with priority '%Ld' \n" Queries.LS.pretty funs_ls pri;
          let funs = funs_ls |> Queries.LS.elements |> List.map fst |> List.unique in
          let f_d = Pid.of_int (get_pid name), Ctx.top (), Pred.of_node (MyCFG.Function f) in
          let tasks = Tasks.add (funs_ls, f_d) (ctx.global tasks_var) in
          ctx.sideg tasks_var tasks;
          let v,i = get_id ("process", name) in
          assign_id pid' v;
          List.fold_left (fun d f -> extract_fun ~info_args:[f.vname] [string_of_int i]) ctx.local funs
        | _ -> let f = Queries.Result.short 30 in struct_fail M.debug_each (`Result (f name, f entry_point, f pri, f per, f cap)); ctx.local
      end
    | _ -> match Pml.special_fun fname with
      | None -> M.debug_each ("extract_arinc: unhandled function"^fname); ctx.local
      | Some eval_args ->
        if M.tracing then M.trace "extract_arinc" "extract %s, args: %i code, %i pml\n" f.vname (List.length arglist) (List.length eval_args);
        let rec combine_opt f a b = match a, b with (* combine list of arguments with list of eval rules, fill with Skip *)
          | [], [] -> []
          | x::xs, y::ys -> (x,y) :: combine_opt f xs ys
          | [], x::xs -> f None (Some x) :: combine_opt f [] xs
          | x::xs, [] -> f (Some x) None :: combine_opt f xs []
        in
        let combine_skip a b = combine_opt (curry @@ function None, Some e -> Pml.EvalSkip, e | _, _ -> assert false) a b in
        let args_product = List.flatten @@ List.n_cartesian_product @@ List.map (uncurry eval) @@ combine_skip eval_args arglist in
        List.fold_left (fun d args ->
          (* some calls have side effects *)
          begin match fname, args with
            | "SetPartitionMode", "NORMAL"::_ ->
              let tasks = ctx.global tasks_var in
              ignore @@ printf "arinc: SetPartitionMode NORMAL: spawning %i processes!\n" (Tasks.cardinal tasks);
              Tasks.iter (fun (fs,f_d) -> Queries.LS.iter (fun f -> ctx.spawn (fst f) f_d) fs) tasks;
            | "SetPartitionMode", x::_ -> failwith @@ "SetPartitionMode: arg "^x
            | _ -> ()
          end;
          let str_args, args = List.partition (flip String.starts_with "\"") args in (* strings can't be arguments, but we want them as a comment *)
          extract_fun ~info_args:str_args args
        ) ctx.local args_product

  let startstate v = Pid.of_int 0L, Ctx.top (), Pred.of_node (MyCFG.Function (emptyFunction "main").svar)
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()

  let init () = (* registers which functions to extract and writes out their definitions *)
    output_file "result/arinc.os.pml" (snd (Pml_arinc.init ()))

  let finalize () = (* writes out collected cfg *)
    output_file "result/arinc.pml" (codegen ())
end

let _ =
  MCP.register_analysis (module Spec : Spec)
