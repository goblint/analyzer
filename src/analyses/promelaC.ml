open Prelude
open Cil
open Deriving.Cil
open PthreadAnalysis

(** promela source code *)
type promela_src = string

module type Ctx = sig
  val filter_map_actions : (Action.t -> 'a option) -> 'a list

  val get_edges : Resource.t -> edge Set.t

  val edges_map : (Resource.t, edge Set.t) Hashtbl.t
end

module Codegen (Ctx : Ctx) = struct
  let tabulate s = "\t" ^ s

  module PmlResTbl = struct
    let table = Hashtbl.create 13

    let init () = Hashtbl.add table (Resource.Thread, "mainfun") 0L

    let get_id res =
      let ((resource, name) as k) = res in
      try Hashtbl.find table k with
      | Not_found ->
          let ids =
            Hashtbl.filteri (fun (r, n) v -> r = resource) table
            |> Hashtbl.values
          in
          let res =
            if Enum.is_empty ids
            then 0L
            else Int64.succ (Enum.arg_max identity ids)
          in
          Hashtbl.replace table k res ;
          res


    let show_id_for_res = Int64.to_string % get_id

    let show_prefixed_id_for_res res =
      let prefix =
        (* thread or function *)
        if Resource.res_type res = Resource.Thread then "T" else "F"
      in
      prefix ^ show_id_for_res res
  end

  module Action = struct
    include PthreadAnalysis.Action

    let extract_thread_create = function ThreadCreate x -> Some x | _ -> None

    let extract_mutex_init = function MutexInit x -> Some x | _ -> None

    let to_pml res = function
      | ThreadCreate t ->
          ""
      | MutexInit m ->
          ""
      | _ ->
          ""
  end

  module ReturnVarsTbl = struct
    (* this is just used to make sure that every var that is read has been written before *)
    let return_vars =
      ( Hashtbl.create 100
        : (Resource.t * [ `Read | `Write ], string Set.t) Hashtbl.t )


    let add_return_var pid kind var =
      Hashtbl.modify_def Set.empty (pid, kind) (Set.add var) return_vars


    let get_return_vars pid kind =
      if fst pid <> Resource.Thread
      then
        failwith
          "get_return_vars: tried to get var for Function, but vars are saved \
           per Thread!"
      else Hashtbl.find_default return_vars (pid, kind) Set.empty


    let decl_return_vars xs =
      Set.elements xs |> List.map (fun vname -> "mtype " ^ vname ^ ";")


    let is_global = startsWith "G"

    let get_locals pid =
      Set.union (get_return_vars pid `Read) (get_return_vars pid `Write)
      |> Set.filter (neg is_global)
      |> decl_return_vars


    let get_globals () =
      let flatten_set xs = Set.fold Set.union xs Set.empty in
      return_vars
      |> Hashtbl.values
      |> Set.of_enum
      |> flatten_set
      |> Set.filter is_global
      |> decl_return_vars
  end

  module Writer = struct
    let write_result desc ext content =
      let dir = Goblintutil.create_dir "result" in
      let path = dir ^ "/pthread." ^ ext in
      output_file path content ;
      print_endline @@ "saved " ^ desc ^ " as " ^ path
  end

  let save_promela_model () =
    let threads =
      List.unique @@ Ctx.filter_map_actions Action.extract_thread_create
    in
    let mutexes =
      List.unique @@ Ctx.filter_map_actions Action.extract_mutex_init
    in

    let thread_count = List.length threads + 1 in
    let mutex_count = List.length mutexes in

    let run_threads =
      let open Action in
      let run_command x = "run " ^ x ^ "();" in
      List.map (fun t -> run_command t.f.vname) threads
    in

    let init_body =
      "preInit;"
      :: "run mainfun(0);"
      :: "postInit();"
      :: "run monitor();"
      :: run_threads
    in

    let current_thread_name = ref "" in
    let called_funs_done = ref Set.empty in

    let rec process_def res =
      let string_of_node = PthreadDomain.Pred.string_of_elt in

      let res_type = Resource.res_type res in
      let res_name = Resource.res_name res in
      let is_thread = res_type = Resource.Thread in
      (* if we already generated code for this function, we just return [] *)
      if res_type = Resource.Function && Set.mem res_name !called_funs_done
      then []
      else
        (* res is type*name, res_id is int64 (starting from 0 for each type of resource) *)
        let res_id = PmlResTbl.get_id res in
        let pref_res_id = PmlResTbl.show_prefixed_id_for_res res in

        (* set the name of the current thread
         * (this function is also run for functions, which need a reference to the thread for checking branching on return vars)
         * TODO: but why was it so? for a process we start with no called functions, for a function we add its name *)
        if is_thread
        then (
          current_thread_name := res_name ;
          called_funs_done := Set.empty )
        else called_funs_done := Set.add res_name !called_funs_done ;

        (* build adjacency matrix for all nodes of this process *)
        let module HashtblN = Hashtbl.Make (PthreadDomain.Pred.Base) in
        let a2bs = HashtblN.create 97 in
        let get_a (a, _, _, _) = a in
        let get_b (_, _, _, b) = b in
        let edges = Ctx.get_edges res in
        Set.iter
          (fun ((a, _, _, b) as edge) ->
            HashtblN.modify_def Set.empty a (Set.add edge) a2bs)
          edges ;

        let nodes = HashtblN.keys a2bs |> List.of_enum in
        let out_edges node =
          try HashtblN.find a2bs node |> Set.elements with Not_found -> []
        in
        let in_edges node =
          HashtblN.filter (Set.mem node % Set.map get_b) a2bs
          |> HashtblN.values
          |> List.of_enum
          |> flat_map Set.elements
        in
        let is_end_node = List.is_empty % out_edges in
        (* node with no incoming edges is the start node *)
        let is_start_node = List.is_empty % in_edges in

        let start_node = List.find is_start_node nodes in
        let label n = pref_res_id ^ "_" ^ string_of_node n in
        let end_label = pref_res_id ^ "_end" in
        let goto node = "goto " ^ label node in

        let called_funs = ref [] in

        let str_edge (a, action, r, b) =
          let target_label = if is_end_node b then end_label else label b in
          let mark =
            let open Action in
            match action with
            | Call fun_name ->
                called_funs := fun_name :: !called_funs ;
                let pc =
                  string_of_int @@ Tbls.FunTbl.get (fun_name, target_label)
                in
                "mark(" ^ pc ^ "); "
            | _ ->
                ""
          in
          (* for function calls the goto will never be reached since the function's return will already jump to that label; however it's nice to see where the program will continue at the site of the call. *)
          mark
          ^ Action.to_pml (Resource.Thread, !current_thread_name) action
          ^ " goto "
          ^ target_label
        in

        let walk_edges (a, out_edges) =
          let edges = Set.elements out_edges |> List.map str_edge in
          let body =
            if List.length edges > 1
            then
              (* choices in if-statements are prefixed with :: *)
              let prefix_branch x = "::" ^ tabulate x in
              let if_branches = List.map prefix_branch edges in
              [ "if" ] @ if_branches @ [ "fi" ]
            else edges
          in
          (label a ^ ":") :: body
        in

        let locals = if is_thread then ReturnVarsTbl.get_locals res else [] in

        let body =
          locals
          @ goto start_node
            :: flat_map walk_edges (HashtblN.enum a2bs |> List.of_enum)
          @ [ ( end_label
              ^ ":"
              ^
              if is_thread
              then " status[res] = DONE"
              else " ret_" ^ snd res ^ "()" )
            ]
        in

        let head =
          let open Action in
          match res with
          | Thread, name ->
              let thread =
                let find_option f xs =
                  try Some (List.find f xs) with Not_found -> None
                in
                find_option (fun t -> t.f.vname = name) threads
              in
              (* None for mainfun *)
              let priority =
                match thread with
                | Some thread ->
                    " priority " ^ "1"
                    (* TODO: add later Int64.to_string thread.pri *)
                | _ ->
                    ""
              in
              "proctype "
              ^ name
              ^ "(byte res)"
              ^ priority
              ^ " provided (canRun("
              ^ Int64.to_string res_id
              ^ ") PRIO"
              ^ Int64.to_string res_id
              ^ ") {\nint stack[20]; int sp = -1;"
          | Function, name ->
              "Fun_" ^ name ^ ":"
          | _ ->
              failwith
                "Only Thread and Function are allowed as keys for collecting \
                 Pthread actions"
        in

        let called_fun_ids =
          List.map (fun fname -> (Resource.Function, fname)) !called_funs
        in

        let funs = flat_map process_def called_fun_ids in
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
      Ctx.edges_map
      |> Hashtbl.keys
      |> List.of_enum
      |> List.filter (fun res -> Resource.res_type res = Resource.Thread)
      |> List.sort (compareBy PmlResTbl.show_prefixed_id_for_res)
      |> flat_map process_def
    in

    let promela =
      String.concat "\n"
      @@ ("#define thread_count " ^ string_of_int thread_count)
         :: ("#define mutex_count " ^ string_of_int mutex_count)
         :: ""
         :: ("#define checkStatus(op1, v, op2) " ^ checkStatus)
         :: ""
         :: ("#define allTasks(prop) " ^ allTasks)
         :: ""
         :: "#include \"pthread.base.pml\""
         :: ""
         :: "init {"
         :: List.map tabulate init_body
      @ "}"
        :: ""
        :: ( List.of_enum
           @@ ((0 --^ thread_count) /@ fun i -> "#define PRIO" ^ string_of_int i)
           )
      (* @ ("#ifdef PRIOS" :: prios) *)
      (* @ ("#endif" :: "" :: fun_mappings) *)
      @ ("" :: ReturnVarsTbl.get_globals ())
      @ process_defs
    in
    Writer.write_result "promela model" "pml" promela ;
    print_endline
      "Copy spin/pthread_base.pml to same folder and then do: spin -a \
       pthread.pml && cc -o pan pan.c && ./pan"

  (* generate priority based running constraints for each process (only used ifdef PRIOS):
     *  process can only run if no higher prio process is ready *)
  (*FIXME: DO IT *)
  (* let prios =
   *   let def thread =
   *     let res = show_id_for_res thread.pid in
   *     let pri = thread.pri in
   *     let higher = List.filter (fun x -> x.pri > pri) procs in
   *     if List.is_empty higher
   *     then None
   *     else
   *       Some
   *         ( "#undef PRIO"
   *         ^ res
   *         ^ "\n#define PRIO"
   *         ^ res
   *         ^ String.concat ""
   *         @@ List.map
   *              (fun x -> " && status[" ^ show_id_for_res x.pid ^ "] != READY")
   *              higher )
   *   in
   *   List.filter_map def procs
   * in *)

  (* FIXME: DO IT *)
  (* let fun_mappings =
   *   let fun_map xs =
   *     if List.is_empty xs
   *     then []
   *     else
   *       let (name, _), _ = List.hd xs in
   *       let entries =
   *         xs
   *         |> List.map (fun ((_, k), v) ->
   *                "\t:: (stack[sp] == "
   *                ^ string_of_int v
   *                ^ ") -> sp--; goto "
   *                ^ k
   *                ^ " \\")
   *       in
   *       (("#define ret_" ^ name ^ "() if \\") :: entries) @ [ "fi" ]
   *   in
   *   FunTbl.to_list ()
   *   |> List.group (compareBy (fst % fst))
   *   |> flat_map fun_map
   * in *)
end
