(** Tracking of pthread lib code. Output to promela. *)

open Prelude.Ana
open Analyses
open Cil
open BatteriesExceptionless
open Option.Infix
open PthreadDomain

class uniqueVarPrinterClass =
  object (self)
    inherit defaultCilPrinterClass as super

    method! pVar (v : varinfo) =
      text v.vname ++ chr '_' ++ text (string_of_int v.vid)

    method! pExp () =
      function
      | Const (CInt (i, _, _)) ->
        (* Fix the constants with long/unsigned suffixes, e.g. 1LL *)
        text @@ string_of_int @@ Z.to_int i
      | x ->
        super#pExp () x
  end

let printer = new uniqueVarPrinterClass

(** [Function] module represents the supported pthread functions for the analysis *)
module Function = struct
  type t =
    | Exit
    | ThreadCreate
    | ThreadJoin
    | ThreadExit
    | MutexInit
    | MutexLock
    | MutexUnlock
    | CondVarInit
    | CondVarBroadcast
    | CondVarSignal
    | CondVarWait

  let funs =
    [ ("exit", Exit)
    ; ("pthread_create", ThreadCreate)
    ; ("pthread_join", ThreadJoin)
    ; ("pthread_exit", ThreadExit)
    ; ("pthread_mutex_init", MutexInit)
    ; ("pthread_mutex_lock", MutexLock)
    ; ("pthread_mutex_unlock", MutexUnlock)
    ; ("pthread_cond_init", CondVarInit)
    ; ("pthread_cond_broadcast", CondVarBroadcast)
    ; ("pthread_cond_signal", CondVarSignal)
    ; ("pthread_cond_wait", CondVarWait)
    ]


  let error_code = -1

  let supported = List.map fst funs

  let from_string s = Option.map snd @@ List.find (( = ) s % fst) funs
end

(* Typealiases for better readability *)

type thread_id = int

type thread_name = string

type mutex_id = int

type mutex_name = string

type cond_var_id = int

type cond_var_name = string

type fun_name = string

module Resource = struct
  type resource_type =
    | Thread
    | Function
  [@@deriving show]

  type resource_name = string

  type t = resource_type * resource_name

  let make t n : t = (t, n)

  let res_type = fst

  let res_name = snd

  let show t =
    let str_res_type = show_resource_type @@ res_type t in
    let str_res_name = res_name t in
    str_res_type ^ ":" ^ str_res_name
end

module Action = struct
  type thread =
    { tid : thread_id
    ; f : varinfo  (** a function being called *)
    }

  type cond_wait =
    { cond_var_id : cond_var_id
    ; mid : mutex_id
    }

  (** uniquely identifies the function call
   ** created/defined by `fun_ctx` function *)
  type fun_call_id = string

  (** ADT of all possible edge actions types *)
  type t =
    | Call of fun_call_id
    | Assign of string * string
    | Cond of string (* pred *)
    | ThreadCreate of thread
    | ThreadJoin of thread_id
    | ThreadExit
    | MutexInit of mutex_id
    | MutexLock of mutex_id
    | MutexUnlock of mutex_id
    | CondVarInit of cond_var_id
    | CondVarBroadcast of cond_var_id
    | CondVarSignal of cond_var_id
    | CondVarWait of cond_wait
    | Nop
end

(** type of a node in CFG *)
type node = PthreadDomain.Pred.Base.t

(** type of a single edge in CFG *)
type edge = node * Action.t * node

module Tbls = struct
  module type SymTblGen = sig
    type k

    type v

    val make_new_val : (k, v) Hashtbl.t -> k -> v
  end

  module type TblGen = sig
    type k

    type v
  end

  module SymTbl (G : SymTblGen) = struct
    let table = (Hashtbl.create 123 : (G.k, G.v) Hashtbl.t)

    let get k =
      (* in case there is no value for the key, populate the table with the next value (id) *)
      let new_value_thunk () =
        let new_val = G.make_new_val table k in
        Hashtbl.replace table k new_val ;
        new_val
      in
      Hashtbl.find table k |> Option.default_delayed new_value_thunk


    let get_key v =
      table |> Hashtbl.filter (( = ) v) |> Hashtbl.keys |> Enum.get


    let to_list () = table |> Hashtbl.enum |> List.of_enum
  end

  module Tbl (G : TblGen) = struct
    let table = (Hashtbl.create 123 : (G.k, G.v) Hashtbl.t)

    let add k v = Hashtbl.replace table k v

    let get k = Hashtbl.find table k

    let get_key v = table |> Hashtbl.enum |> List.of_enum |> List.assoc_inv v
  end

  let all_keys_count table =
    table |> Hashtbl.keys |> List.of_enum |> List.length


  module ThreadTidTbl = SymTbl (struct
      type k = thread_name

      type v = thread_id

      let make_new_val table k = all_keys_count table
    end)

  module FunNameToTids = struct
    include Tbl (struct
        type k = fun_name

        type v = thread_id Set.t
      end)

    let extend k v = Hashtbl.modify_def Set.empty k (Set.add v) table

    let get_fun_for_tid v =
      Hashtbl.keys table
      |> List.of_enum
      |> List.find (fun k ->
          Option.get @@ Hashtbl.find table k |> Set.exists (( = ) v))
  end

  module MutexMidTbl = SymTbl (struct
      type k = mutex_name

      type v = mutex_id

      let make_new_val table k = all_keys_count table
    end)

  module CondVarIdTbl = SymTbl (struct
      type k = cond_var_name

      type v = cond_var_id

      let make_new_val table k = all_keys_count table
    end)

  (* context hash to differentiate function calls *)
  module CtxTbl = SymTbl (struct
      type k = int

      type v = int

      let make_new_val table k = all_keys_count table
    end)

  module FunCallTbl = SymTbl (struct
      type k = fun_name * string (* fun and target label *)

      type v = int

      let make_new_val table k = all_keys_count table
    end)

  module NodeTbl = SymTbl (struct
      (* table from sum type to negative line number for new intermediate node (-1 to -4 have special meanings) *)
      type k = int (* stmt sid *)

      type v = MyCFG.node

      (* function for creating a new intermediate node (will generate a new sid every time!) *)
      let make_new_val table k =
        (* TODO: all same key occurences instead *)
        let line = -5 - all_keys_count table in
        let loc = { !Tracing.current_loc with line } in
        MyCFG.Statement
          { (mkStmtOneInstr @@ Set (var dummyFunDec.svar, zero, loc, loc)) with
            sid = new_sid ()
          }
    end)
end

let promela_main : fun_name = "mainfun"

(* assign tid: promela_main -> 0 *)
let _ = Tbls.ThreadTidTbl.get promela_main

let fun_ctx ctx f =
  let ctx_hash =
    match PthreadDomain.Ctx.to_int ctx with
    | Some i ->
      i |> i64_to_int |> Tbls.CtxTbl.get |> string_of_int
    | None ->
      "TOP"
  in
  f.vname ^ "_" ^ ctx_hash


module Tasks = SetDomain.Make (Lattice.Prod (Queries.LS) (PthreadDomain.D))
module rec Env : sig
  type t

  val get : (PthreadDomain.D.t, Tasks.t, PthreadDomain.D.t, _) ctx -> t

  val d : t -> PthreadDomain.D.t

  val node : t -> MyCFG.node

  val resource : t -> Resource.t
end = struct
  type t =
    { d : PthreadDomain.D.t
    ; node : MyCFG.node
    ; resource : Resource.t
    }

  let get ctx =
    let d : PthreadDomain.D.t = ctx.local in
    let node = Option.get !MyCFG.current_node in
    let fundec = Node.find_fundec node in
    let thread_name =
      let cur_tid =
        Int64.to_int @@ Option.get @@ PthreadDomain.Tid.to_int d.tid
      in
      Option.get @@ Tbls.ThreadTidTbl.get_key cur_tid
    in
    let resource =
      let is_main_fun =
        promela_main
        |> GobConfig.get_list
        |> List.map Yojson.Safe.to_string
        |> List.mem fundec.svar.vname
      in
      let is_thread_fun =
        let fun_of_thread = Edges.fun_for_thread thread_name in
        Some fundec.svar = fun_of_thread
      in
      let open Resource in
      if is_thread_fun || is_main_fun
      then Resource.make Thread thread_name
      else Resource.make Function (fun_ctx d.ctx fundec.svar)
    in
    { d; node; resource }


  let d env = env.d

  let node env = env.node

  let resource env = env.resource
end

and Edges : sig
  val table : (Resource.t, edge Set.t) Hashtbl.t

  val add : ?dst:Node.t -> ?d:PthreadDomain.D.t -> Env.t -> Action.t -> unit

  val get : Resource.t -> edge Set.t

  val filter_map_actions : (Action.t -> 'a option) -> 'a list

  val fun_for_thread : thread_name -> varinfo option
end = struct
  let table = Hashtbl.create 199

  (** [add] adds an edge for the current environment resource id (Thread or Function)
   ** [dst] destination node
   ** [d] domain
   ** [env] environment
   ** [action] edge action of type `Action.t` *)
  let add ?dst ?d env action =
    let open PthreadDomain in
    let preds =
      let env_d = Env.d env in
      (d |? env_d).pred
    in
    let add_edge_for_node node =
      let env_node = Env.node env in
      let env_res = Env.resource env in
      let action_edge = (node, action, Node.location (dst |? env_node)) in
      Hashtbl.modify_def Set.empty env_res (Set.add action_edge) table
    in
    Pred.iter add_edge_for_node preds


  let get res_id = Hashtbl.find_default table res_id Set.empty

  let filter_map_actions f =
    let action_of_edge (_, action, _) = action in
    let all_edges =
      table
      |> Hashtbl.values
      |> List.of_enum
      |> List.map Set.elements
      |> List.concat
    in
    List.filter_map (f % action_of_edge) all_edges


  let fun_for_thread thread_name =
    let open Action in
    let get_funs = function
      | ThreadCreate t when Tbls.ThreadTidTbl.get_key t.tid = Some thread_name
        ->
        Some t.f
      | _ ->
        None
    in
    List.hd @@ filter_map_actions get_funs
end

module Variable = struct
  type t = varinfo

  let is_integral v = match v.vtype with TInt _ -> true | _ -> false

  let is_global v = v.vglob

  let is_mem v = v.vaddrof

  let make_from_lhost = function
    | Var v when is_integral v && not (is_mem v) ->
      Some v
    | _ ->
      None


  let make_from_lval (lhost, _) = make_from_lhost lhost

  let show = sprint (fun () -> printer#pVar)

  let show_def v = "int " ^ show v ^ ";"
end

module Variables = struct
  type var_state =
    | Top of Variable.t
    | Var of Variable.t

  let table = ref (Hashtbl.create 123 : (thread_id, var_state Set.t) Hashtbl.t)

  let get_globals () =
    Hashtbl.values !table
    |> List.of_enum
    |> List.map Set.elements
    |> List.flatten
    |> List.filter_map (function
        | Var v when Variable.is_global v ->
          Some v
        | Top v when Variable.is_global v ->
          Some v
        | _ ->
          None)
    |> List.unique


  let get_locals tid =
    Hashtbl.find !table tid
    |> Option.default Set.empty
    |> Set.filter_map (function
        (* no globals *)
        | Var v when not (Variable.is_global v) ->
          Some v
        | Top v when not (Variable.is_global v) ->
          Some v
        | _ ->
          None)
    |> Set.enum
    |> List.of_enum


  let is_top tid var =
    if Variable.is_global var
    then
      !table
      |> Hashtbl.values
      |> List.of_enum
      |> List.exists (Set.exists (( = ) (Top var)))
    else
      Set.exists (( = ) (Top var)) @@ Hashtbl.find_default !table tid Set.empty


  let add tid var =
    if not (is_top tid var)
    then Hashtbl.modify_def Set.empty tid (Set.add (Var var)) !table


  let add_top tid var =
    Hashtbl.modify_def Set.empty tid (Set.remove (Var var)) !table ;
    Hashtbl.modify_def Set.empty tid (Set.add (Top var)) !table


  (* is a local var for thread tid or a global
   * var must not be set to top *)
  let valid_var tid var =
    if Variable.is_global var
    then
      !table
      |> Hashtbl.values
      |> List.of_enum
      |> List.exists (Set.exists (( = ) (Var var)))
    else
      Set.exists (( = ) (Var var)) @@ Hashtbl.find_default !table tid Set.empty


  (* all vars on rhs should be already registered, otherwise -> do not add this var *)
  let rec all_vars_are_valid ctx = function
    | Const _ ->
      true
    | Lval l ->
      let open PthreadDomain in
      let d = Env.d @@ Env.get ctx in
      let tid = Int64.to_int @@ Option.get @@ Tid.to_int d.tid in

      l
      |> Variable.make_from_lval
      |> Option.map @@ valid_var tid
      |> Option.default false
    | UnOp (_, e, _) ->
      all_vars_are_valid ctx e
    | BinOp (_, a, b, _) ->
      all_vars_are_valid ctx a && all_vars_are_valid ctx b
    | _ ->
      false
end

(** promela source code *)
type promela_src = string

module Codegen = struct
  (** [PmlResTbl] module maps resources to unique ids used as prefix for edge labeling  *)
  module PmlResTbl = struct
    module FunTbl = Tbls.SymTbl (struct
        type k = fun_name

        type v = int

        let make_new_val table k = Tbls.all_keys_count table
      end)

    let get res =
      let prefix =
        if Resource.res_type res = Resource.Thread then "T" else "F"
      in
      let id =
        match res with
        | Resource.Thread, thread_name ->
          Tbls.ThreadTidTbl.get thread_name
        | Resource.Function, fun_name ->
          FunTbl.get fun_name
      in
      prefix ^ string_of_int id
  end

  module AdjacencyMatrix = struct
    module HashtblN = Hashtbl.Make (PthreadDomain.Pred.Base)

    let make () = HashtblN.create 97

    (** build adjacency matrix for all nodes of this process *)
    let populate a2bs edges =
      Set.iter
        (fun ((a, _, _) as edge) ->
           HashtblN.modify_def Set.empty a (Set.add edge) a2bs)
        edges ;
      a2bs


    let nodes a2bs = HashtblN.keys a2bs |> List.of_enum

    let items a2bs = HashtblN.enum a2bs |> List.of_enum

    let in_edges a2bs node =
      let get_b (_, _, b) = b in
      HashtblN.filter (Set.mem node % Set.map get_b) a2bs
      |> HashtblN.values
      |> List.of_enum
      |> List.concat_map Set.elements


    let out_edges a2bs node =
      try HashtblN.find a2bs node |> Set.elements with Not_found -> []
  end

  module Action = struct
    include Action

    let extract_thread_create = function ThreadCreate x -> Some x | _ -> None

    let to_pml = function
      | Call fname ->
        "goto Fun_" ^ fname ^ ";"
      | Assign (a, b) ->
        a ^ " = " ^ b ^ ";"
      | Cond cond ->
        cond ^ " -> "
      | ThreadCreate t ->
        "ThreadCreate(" ^ string_of_int t.tid ^ ");"
      | ThreadJoin tid ->
        "ThreadWait(" ^ string_of_int tid ^ ");"
      | ThreadExit ->
        "ThreadExit();"
      | MutexInit mid ->
        "MutexInit(" ^ string_of_int mid ^ ");"
      | MutexLock mid ->
        "MutexLock(" ^ string_of_int mid ^ ");"
      | MutexUnlock mid ->
        "MutexUnlock(" ^ string_of_int mid ^ ");"
      | CondVarInit id ->
        "CondVarInit(" ^ string_of_int id ^ ");"
      | CondVarBroadcast id ->
        "CondVarBroadcast(" ^ string_of_int id ^ ");"
      | CondVarSignal id ->
        "CondVarSignal(" ^ string_of_int id ^ ");"
      | CondVarWait cond_var_wait ->
        "CondVarWait("
        ^ string_of_int cond_var_wait.cond_var_id
        ^ ", "
        ^ string_of_int cond_var_wait.mid
        ^ "); "
      | Nop ->
        ""
  end


  module Writer = struct
    let write desc ext content =
      let dir = Goblintutil.create_dir (Fpath.v "pml-result") in
      let path = Fpath.to_string @@ Fpath.append dir  (Fpath.v ("pthread." ^ ext)) in
      output_file ~filename:path ~text:content ;
      print_endline @@ "saved " ^ desc ^ " as " ^ path
  end

  let tabulate = ( ^ ) "\t"

  let define = ( ^ ) "#define "

  let goto_str = ( ^ ) "goto "

  let escape xs =
    let last = Option.get @@ List.last xs in
    let rest = List.take (List.length xs - 1) xs in
    List.map (fun s -> s ^ " \\") rest @ [ last ]


  let if_clause stmts =
    [ "if" ] @ List.map (( ^ ) "::" % tabulate) stmts @ [ "fi" ]


  let run ?arg f = "run " ^ f ^ "(" ^ Option.default "" arg ^ ");"

  let string_of_node = PthreadDomain.Pred.string_of_elt

  let save_promela_model () =
    let threads =
      List.unique @@ Edges.filter_map_actions Action.extract_thread_create
    in

    let thread_count = List.length threads + 1 in
    let mutex_count = List.length @@ Tbls.MutexMidTbl.to_list () in
    let cond_var_count = List.length @@ Tbls.CondVarIdTbl.to_list () in

    let current_thread_name = ref "" in
    let called_funs_done = ref Set.empty in

    let rec process_def res =
      print_endline @@ Resource.show res ;
      let res_type = Resource.res_type res in
      let res_name = Resource.res_name res in
      let is_thread = res_type = Resource.Thread in
      (* if we already generated code for this function, we just return [] *)
      if res_type = Resource.Function && Set.mem res_name !called_funs_done
      then []
      else
        let res_id = PmlResTbl.get res in
        (* set the name of the current thread
         * (this function is also run for functions, which need a reference to the thread for checking branching on return vars *)
        if is_thread
        then (
          current_thread_name := res_name ;
          called_funs_done := Set.empty )
        else called_funs_done := Set.add res_name !called_funs_done ;
        (* build adjacency matrix for all nodes of this process *)
        let a2bs =
          let edges = Edges.get res in
          AdjacencyMatrix.populate (AdjacencyMatrix.make ()) edges
        in
        let nodes = AdjacencyMatrix.nodes a2bs in
        let out_edges = AdjacencyMatrix.out_edges a2bs in
        let in_edges = AdjacencyMatrix.in_edges a2bs in

        let is_end_node = List.is_empty % out_edges in
        let is_start_node = List.is_empty % in_edges in
        let label n = res_id ^ "_" ^ string_of_node n in
        let end_label = res_id ^ "_end" in
        let goto = goto_str % label in
        let goto_start_node =
          match List.find is_start_node nodes with
          | Some node ->
            goto node
          | None ->
            ""
        in
        let called_funs = ref [] in
        let str_edge (a, action, b) =
          let target_label = if is_end_node b then end_label else label b in
          match action with
          | Action.Call fun_name when fun_name = "exit" ->
            "exit();"
          | Action.Call fun_name ->
            called_funs := fun_name :: !called_funs ;
            let pc =
              string_of_int @@ Tbls.FunCallTbl.get (fun_name, target_label)
            in
            "mark(" ^ pc ^ "); " ^ Action.to_pml action
          | _ ->
            Action.to_pml action ^ " " ^ goto_str target_label
        in
        let walk_edges (node, out_edges) =
          let edges = Set.map str_edge out_edges |> Set.elements in
          let body = match edges with
            | _::_::_ -> if_clause edges
            | _ -> edges
          in
          (label node ^ ":") :: body
        in
        let body =
          let return =
            let end_stmt =
              match res with
              | Thread, "mainfun" ->
                "exit()"
              | Thread, _ ->
                "ThreadExit()"
              | Function, f ->
                "ret_" ^ f ^ "()"
            in
            end_label ^ ": " ^ end_stmt
          in
          goto_start_node
          :: (List.concat_map walk_edges @@ AdjacencyMatrix.items a2bs)
          @ [ return ]
        in
        let head =
          match res with
          | Thread, name ->
            let tid = Tbls.ThreadTidTbl.get name in
            let defs =
              let local_defs =
                Variables.get_locals tid
                |> List.map Variable.show_def
                |> List.unique
              in
              let stack_def = [ "int stack[20];"; "int sp = -1;" ] in

              [ stack_def; local_defs ]
              |> List.flatten
              |> List.map tabulate
              |> String.concat "\n"
            in
            "proctype "
            ^ name
            ^ "(byte tid)"
            ^ " provided (canRun("
            ^ string_of_int tid
            ^ ")) {\n"
            ^ defs
            ^ "\n"
          | Function, name ->
            "Fun_" ^ name ^ ":"
        in
        let called_fun_ids =
          List.map (fun fname -> (Resource.Function, fname)) !called_funs
        in
        let funs = List.concat_map process_def called_fun_ids in
        ("" :: head :: List.map tabulate body)
        @ funs
        @ [ (if is_thread then "}" else "") ]
    in
    (* used for macros oneIs, allAre, noneAre... *)
    let checkStatus =
      "("
      ^ ( String.concat " op2 "
          @@ List.of_enum
          @@ (0 --^ thread_count)
             /@ fun i -> "status[" ^ string_of_int i ^ "] op1 v" )
      ^ ")"
    in
    let allTasks =
      "("
      ^ ( String.concat " && "
          @@ List.of_enum
          @@ ((0 --^ thread_count) /@ fun i -> "prop(" ^ string_of_int i ^ ")") )
      ^ ")"
    in

    (* sort definitions so that inline functions come before the threads *)
    let process_defs =
      Edges.table
      |> Hashtbl.keys
      |> List.of_enum
      |> List.filter (fun res -> Resource.res_type res = Resource.Thread)
      |> List.unique
      |> List.sort (compareBy PmlResTbl.get)
      |> List.concat_map process_def
    in
    let fun_ret_defs =
      let fun_map fun_calls =
        match List.hd fun_calls with
        | None ->
          []
        | Some ((name, _), _) ->
          let dec_sp ((_, target_label), id) =
            "(stack[sp] == "
            ^ string_of_int id
            ^ ") -> sp--; "
            ^ goto_str target_label
          in
          let if_branches = List.map dec_sp fun_calls in
          let body = (define "ret_" ^ name ^ "()") :: if_clause if_branches in
          escape body
      in
      Tbls.FunCallTbl.to_list ()
      |> List.group (compareBy (fst % fst))
      |> List.concat_map fun_map
    in
    let globals = List.map Variable.show_def @@ Variables.get_globals () in
    let promela =
      let empty_line = "" in
      let defs =
        [ define "thread_count " ^ string_of_int thread_count
        ; define "mutex_count " ^ string_of_int mutex_count
        ; define "cond_var_count " ^ string_of_int cond_var_count
        ; empty_line
        ; define "checkStatus(op1, v, op2) " ^ checkStatus
        ; empty_line
        ; define "allTasks(prop) " ^ allTasks
        ; empty_line
        ; "#include \"../spin/pthread.base.pml\""
        ; empty_line
        ]
      in
      let init =
        let run_threads =
          (* NOTE: assumes no args are passed to the thread func *)
          List.map
            (fun t ->
               let tid = Action.(t.tid) in
               let thread_name = Option.get @@ Tbls.ThreadTidTbl.get_key tid in
               let tid_str = string_of_int tid in
               run thread_name ~arg:tid_str)
            threads
        in
        let run_main = run promela_main ~arg:"0" in
        let init_body = (run_main :: run_threads) @ [ "setReady(0);" ] in
        [ "init {" ] @ List.map tabulate init_body @ [ "}" ]
      in
      let body =
        let separator = [ empty_line ] in
        List.flatten
          [ defs
          ; init
          ; separator
          ; fun_ret_defs
          ; separator
          ; globals
          ; process_defs
          ]
      in
      String.concat "\n" body
    in
    let dot_graph =
      let dot_thread tid =
        let show_edge (a, action, b) =
          let show_node x =
            "\"" ^ Resource.res_name tid ^ "_" ^ string_of_node x ^ "\""
          in
          show_node a
          ^ "\t->\t"
          ^ show_node b
          ^ "\t[label=\""
          ^ Action.to_pml action
          ^ "\"]"
        in
        let subgraph_head =
          "subgraph \"cluster_" ^ Resource.res_name tid ^ "\" {"
        in
        let subgraph_tail =
          "label = \"" ^ Resource.res_name tid ^ "\";\n  }\n"
        in
        let edges_decls = Set.elements @@ Set.map show_edge @@ Edges.get tid in
        (subgraph_head :: edges_decls) @ [ subgraph_tail ]
      in
      let lines =
        Hashtbl.keys Edges.table
        |> List.of_enum
        |> List.unique
        |> List.map dot_thread
        |> List.concat
      in
      String.concat "\n  " ("digraph file {" :: lines) ^ "}"
    in

    Writer.write "promela model" "pml" promela ;
    Writer.write "graph" "dot" dot_graph ;
    print_endline
      "Copy spin/pthread_base.pml to same folder and then do: spin -a \
       pthread.pml && cc -o pan pan.c && ./pan"
end


module Spec : Analyses.MCPSpec = struct
  (* Spec implementation *)
  include Analyses.DefaultSpec

  module M = Messages
  module List = BatList
  module V = VarinfoV

  (** Domains *)
  module D = PthreadDomain.D

  module C = D

  (** Set of created tasks to spawn when going multithreaded *)
  module Tasks = SetDomain.Make (Lattice.Prod (Queries.LS) (D))

  module G = Tasks

  let tasks_var =
    Goblintutil.create_var (makeGlobalVar "__GOBLINT_PTHREAD_TASKS" voidPtrType)


  module ExprEval = struct
    let eval_ptr ctx exp =
      let mayPointTo ctx exp =
        let a = ctx.ask (Queries.MayPointTo exp) in
        if (not (Queries.LS.is_top a)) && Queries.LS.cardinal a > 0 then
          let top_elt = (dummyFunDec.svar, `NoOffset) in
          let a' =
            if Queries.LS.mem top_elt a
            then (* UNSOUND *)
              Queries.LS.remove top_elt a
            else a
          in
          Queries.LS.elements a'
        else
          []
      in
      List.map fst @@ mayPointTo ctx exp


    let eval_var ctx exp =
      match exp with
      | Lval (Mem e, _) ->
        eval_ptr ctx e
      | Lval (Var v, _) ->
        [ v ]
      | _ ->
        eval_ptr ctx exp


    let eval_ptr_id ctx exp get =
      List.map (get % Variable.show) @@ eval_ptr ctx exp


    let eval_var_id ctx exp get =
      List.map (get % Variable.show) @@ eval_var ctx exp
  end

  let name () = "extract-pthread"

  let init () = LibraryFunctions.add_lib_funs Function.supported

  let assign ctx (lval : lval) (rval : exp) : D.t =
    let should_ignore_assigns = GobConfig.get_bool "ana.extract-pthread.ignore_assign" in
    if PthreadDomain.D.is_bot ctx.local || should_ignore_assigns
    then ctx.local
    else if Option.is_none !MyCFG.current_node
    then (
      (* it is global var assignment *)
      let var_opt = Variable.make_from_lval lval in
      if Variables.all_vars_are_valid ctx rval
      (* TODO: handle the assignment of the global *)
      then Option.may (Variables.add (-1)) var_opt
      else Option.may (Variables.add_top (-1)) var_opt ;
      ctx.local )
    else
      let env = Env.get ctx in
      let d = Env.d env in
      let tid = Int64.to_int @@ Option.get @@ Tid.to_int d.tid in

      let var_opt = Variable.make_from_lval lval in

      if Option.is_none var_opt || (not @@ Variables.all_vars_are_valid ctx rval)
      then (
        (* set lhs var to TOP *)
        Option.may (Variables.add_top tid) var_opt ;
        ctx.local )
      else
        let var = Option.get var_opt in

        let lhs_str = Variable.show var in
        let rhs_str = sprint printer#pExp rval in
        Edges.add env @@ Action.Assign (lhs_str, rhs_str) ;

        Variables.add tid var ;

        { d with pred = Pred.of_node @@ Env.node env }


  let branch ctx (exp : exp) (tv : bool) : D.t =
    if PthreadDomain.D.is_bot ctx.local
    then ctx.local
    else
      let env = Env.get ctx in
      let d = Env.d env in
      let tid = Int64.to_int @@ Option.get @@ Tid.to_int d.tid in
      let is_valid_var =
        Option.default false
        % Option.map (Variables.valid_var tid)
        % Variable.make_from_lhost
      in
      let var_str = Variable.show % Option.get % Variable.make_from_lhost in
      let pred_str op lhs rhs =
        let cond_str = lhs ^ " " ^ sprint d_binop op ^ " " ^ rhs in
        if tv then cond_str else "!(" ^ cond_str ^ ")"
      in

      let add_action pred_str =
        match Env.node env with
        | MyCFG.Statement { skind = If (e, bt, bf, loc, _); _ } ->
          let intermediate_node =
            let then_stmt =
              List.hd
              @@
              if List.is_empty bt.bstmts
              then
                let le = List.nth bf.bstmts (List.length bf.bstmts - 1) in
                le.succs
              else bt.bstmts
            in
            let else_stmt =
              List.hd
              @@
              if List.is_empty bf.bstmts
              then
                let le = List.nth bt.bstmts (List.length bt.bstmts - 1) in
                le.succs
              else bf.bstmts
            in
            Tbls.NodeTbl.get (if tv then then_stmt else else_stmt).sid
          in
          Edges.add ~dst:intermediate_node env (Action.Cond pred_str) ;
          { ctx.local with pred = Pred.of_node intermediate_node }
        | _ ->
          failwith "branch: current_node is not an If"
      in

      let handle_binop op lhs rhs =
        match (lhs, rhs) with
        | Lval (lhost, _), Const (CInt (i, _, _))
        | Const (CInt (i, _, _)), Lval (lhost, _)
          when is_valid_var lhost ->
          add_action @@ pred_str op (var_str lhost) (Z.to_string i)
        | Lval (lhostA, _), Lval (lhostB, _)
          when is_valid_var lhostA && is_valid_var lhostB ->
          add_action @@ pred_str op (var_str lhostA) (var_str lhostB)
        | _ ->
          ctx.local
      in
      let handle_unop x tv =
        match x with
        | Lval (lhost, _) when is_valid_var lhost ->
          let pred = (if tv then "" else "!") ^ var_str lhost in
          add_action pred
        | _ ->
          ctx.local
      in
      match exp with
      | BinOp (op, a, b, _) ->
        handle_binop op (stripCasts a) (stripCasts b)
      | UnOp (LNot, a, _) ->
        handle_unop a (not tv)
      | Const (CInt _) ->
        handle_unop exp tv
      | _ ->
        ctx.local


  let body ctx (f : fundec) : D.t =
    (* enter is not called for spawned threads -> initialize them here *)
    let context_hash = Int64.of_int (if not !Goblintutil.global_initialization then ControlSpecC.hash (ctx.control_context ()) else 37) in
    { ctx.local with ctx = Ctx.of_int context_hash }


  let return ctx (exp : exp option) (f : fundec) : D.t = ctx.local

  let enter ctx (lval : lval option) (f : fundec) (args : exp list) :
    (D.t * D.t) list =
    (* on function calls (also for main); not called for spawned threads *)
    let d_caller = ctx.local in
    let d_callee =
      if D.is_bot ctx.local
      then ctx.local
      else
        { ctx.local with
          pred = Pred.of_node (MyCFG.Function f)
        ; ctx = Ctx.top ()
        }
    in
    (* set predecessor set to start node of function *)
    [ (d_caller, d_callee) ]


  let combine
      ctx
      (lval : lval option)
      fexp
      (f : fundec)
      (args : exp list)
      fc
      (au : D.t) : D.t =
    if D.any_is_bot ctx.local || D.any_is_bot au
    then ctx.local
    else
      let d_caller = ctx.local in
      let d_callee = au in
      (* check if the callee has some relevant edges, i.e. advanced from the entry point
       * if not, we generate no edge for the call and keep the predecessors from the caller *)
      (* set should never be empty *)
      if Pred.is_bot d_callee.pred then failwith "d_callee.pred is bot!" ;
      if Pred.equal d_callee.pred @@ Pred.of_node @@ MyCFG.Function f
      then
        (* set current node as new predecessor, since something interesting happend during the call *)
        { d_callee with pred = d_caller.pred; ctx = d_caller.ctx }
      else
        let env = Env.get ctx in
        (* write out edges with call to f coming from all predecessor nodes of the caller *)
        ( if Ctx.is_int d_callee.ctx
          then
            let last_pred = d_caller.pred in
            let action = Action.Call (fun_ctx d_callee.ctx f.svar) in
            Edges.add ~d:{ d_caller with pred = last_pred } env action ) ;
        (* set current node as new predecessor, since something interesting happend during the call *)
        { d_callee with
          pred = Pred.of_node @@ Env.node env
        ; ctx = d_caller.ctx
        }


  let special ctx (lval : lval option) (f : varinfo) (arglist : exp list) : D.t
    =
    let fun_name = f.vname in
    let pthread_fun = Function.from_string fun_name in
    if D.any_is_bot ctx.local || Option.is_none pthread_fun
    then ctx.local
    else
      let env = Env.get ctx in
      let d = Env.d env in
      let tid = Int64.to_int @@ Option.get @@ Tid.to_int d.tid in

      let add_actions (actions : Action.t list) =
        let add_failed_assign_action () =
          lval
          >>= Variable.make_from_lval
          >>= fun var ->
          Variables.add tid var ;

          Option.some
          @@ Action.Assign (Variable.show var, string_of_int Function.error_code)
        in

        List.iter (Edges.add env) actions ;

        let should_assume_success =
          GobConfig.get_bool "ana.extract-pthread.assume_success"
        in
        if not should_assume_success
        then Option.may (Edges.add env) @@ add_failed_assign_action () ;

        if List.is_empty actions
        then d
        else { d with pred = Pred.of_node @@ Env.node env }
      in
      let add_action action = add_actions [ action ] in

      let arglist = List.map (stripCasts % constFold false) arglist in
      let open Function in
      match (Option.get pthread_fun, arglist) with
      | Exit, _ ->
        add_action (Action.Call "exit")
      | ThreadCreate, [ thread; thread_attr; func; fun_arg ] ->
        let funs_ls =
          let ls = ctx.ask (Queries.ReachableFrom func) in
          Queries.LS.filter
            (fun (v, o) ->
               let lval = (Var v, Lval.CilLval.to_ciloffs o) in
               isFunctionType (typeOfLval lval))
            ls
        in
        let thread_fun =
          funs_ls
          |> Queries.LS.elements
          |> List.map fst
          |> List.unique ~eq:(fun a b -> a.vid = b.vid)
          |> List.hd
        in

        let add_task tid =
          let tasks =
            let f_d =
              { tid = Tid.of_int @@ Int64.of_int tid
              ; pred = Pred.of_node (ctx.prev_node)
              ; ctx = Ctx.top ()
              }
            in
            Tasks.singleton (funs_ls, f_d)
          in
          ctx.sideg tasks_var tasks ;
        in
        let thread_create tid =
          let fun_name = Variable.show thread_fun in
          let add_visited_edges fun_tids =
            let existing_tid = List.hd @@ Set.elements fun_tids in
            let resource_from_tid tid =
              Resource.make Resource.Thread
              @@ Option.get
              @@ Tbls.ThreadTidTbl.get_key tid
            in
            let edges = Edges.get @@ resource_from_tid existing_tid in

            let launches_child_thread =
              let is_thread_create = function
                | _, Action.ThreadCreate _, _ ->
                  true
                | _ ->
                  false
              in
              Set.exists is_thread_create edges
            in

            if launches_child_thread
            then
              failwith
                "Unsupported use case! Thread is not allowed to launch a \
                 child thread" ;

            Hashtbl.add Edges.table (resource_from_tid tid) edges
          in

          add_task tid ;

          (* multiple threads may be launched with the same function entrypoint
           * but functions are visited only once
           * Want to have add the visited edges to the new thread that visits
           * the same function *)
          Option.may add_visited_edges @@ Tbls.FunNameToTids.get fun_name ;
          Tbls.FunNameToTids.extend fun_name tid ;

          Action.ThreadCreate { f = thread_fun; tid }
        in

        add_actions
        @@ List.map thread_create
        @@ ExprEval.eval_ptr_id ctx thread Tbls.ThreadTidTbl.get
      | ThreadJoin, [ thread; thread_ret ] ->
        add_actions
        @@ List.map (fun tid -> Action.ThreadJoin tid)
        @@ ExprEval.eval_var_id ctx thread Tbls.ThreadTidTbl.get
      | ThreadExit, [ status ] ->
        add_action Action.ThreadExit
      | MutexInit, [ mutex; mutex_attr ] ->
        (* TODO: reentrant mutex handling *)
        add_actions
        @@ List.map (fun mid -> Action.MutexInit mid)
        @@ ExprEval.eval_ptr_id ctx mutex Tbls.MutexMidTbl.get
      | MutexLock, [ mutex ] ->
        add_actions
        @@ List.map (fun mid -> Action.MutexLock mid)
        @@ ExprEval.eval_ptr_id ctx mutex Tbls.MutexMidTbl.get
      | MutexUnlock, [ mutex ] ->
        add_actions
        @@ List.map (fun mid -> Action.MutexUnlock mid)
        @@ ExprEval.eval_ptr_id ctx mutex Tbls.MutexMidTbl.get
      | CondVarInit, [ cond_var; cond_var_attr ] ->
        add_actions
        @@ List.map (fun id -> Action.CondVarInit id)
        @@ ExprEval.eval_ptr_id ctx cond_var Tbls.CondVarIdTbl.get
      | CondVarBroadcast, [ cond_var ] ->
        add_actions
        @@ List.map (fun id -> Action.CondVarBroadcast id)
        @@ ExprEval.eval_ptr_id ctx cond_var Tbls.CondVarIdTbl.get
      | CondVarSignal, [ cond_var ] ->
        add_actions
        @@ List.map (fun id -> Action.CondVarSignal id)
        @@ ExprEval.eval_ptr_id ctx cond_var Tbls.CondVarIdTbl.get
      | CondVarWait, [ cond_var; mutex ] ->
        let cond_vars = ExprEval.eval_ptr ctx cond_var in
        let mutex_vars = ExprEval.eval_ptr ctx mutex in
        let cond_var_action (v, m) =
          let open Action in
          CondVarWait
            { cond_var_id = Tbls.CondVarIdTbl.get @@ Variable.show v
            ; mid = Tbls.MutexMidTbl.get @@ Variable.show m
            }
        in
        add_actions
        @@ List.map cond_var_action
        @@ List.cartesian_product cond_vars mutex_vars
      | _ ->
        add_action Nop


  let startstate v =
    let open D in
    make
      (Tid.of_int 0L)
      (Pred.of_node (MyCFG.Function (emptyFunction "main")))
      (Ctx.top ())


  let threadenter ctx lval f args =
    let d : D.t = ctx.local in
    let tasks = ctx.global tasks_var in
    (* TODO: optimize finding *)
    let tasks_f =
      Tasks.filter
        (fun (fs, f_d) -> Queries.LS.exists (fun (ls_f, _) -> ls_f = f) fs)
        tasks
    in
    let f_d = snd (Tasks.choose tasks_f) in
    [ { f_d with pred = d.pred } ]


  let threadspawn ctx lval f args fctx = ctx.local

  let exitstate v = D.top ()

  let finalize = Codegen.save_promela_model

  let init _ = ()
end

let _ = MCP.register_analysis ~dep:[ "base" ] (module Spec : MCPSpec)
