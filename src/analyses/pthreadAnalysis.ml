open Prelude.Ana
open Analyses
open Cil
open Deriving.Cil
open BatteriesExceptionless

(** [Function] module represents the supported pthread functions for the analysis *)
module Function = struct
  type t =
    | ThreadCreate
    | ThreadJoin
    | MutexInit
    | MutexLock
    | MutexUnlock
    | Other of string

  let prefix = "pthread_"

  let supported =
    List.map
      (( ^ ) prefix)
      [ "create"; "join"; "mutex_init"; "mutex_lock"; "mutex_unlock" ]


  let is_pthread_fun f = String.starts_with f prefix

  let from_string = function
    | "pthread_create" ->
        ThreadCreate
    | "pthread_join" ->
        ThreadJoin
    | "pthread_mutex_init" ->
        MutexInit
    | "pthread_mutex_lock" ->
        MutexLock
    | "pthread_mutex_unlock" ->
        MutexUnlock
    | s ->
        Other s
end

(** [Resource] module represts different resources extracted for the analysis *)
module Resource = struct
  (** Enumeration of all resources relevant for the analysis *)
  type resource_type =
    | Thread
    | Function
    | Mutex
  [@@deriving show]

  (** name of the resource in code *)
  type resource_name = string

  (** type [t] represents the resource *)
  type t = resource_type * resource_name

  let make t n : t = (t, n)

  let res_type = fst

  let res_name = snd

  let show t =
    let str_res_type = show_resource_type @@ res_type t in
    let str_res_name = res_name t in
    str_res_type ^ ":" ^ str_res_name
end

type thread_name = string

type fun_name = string

module Action = struct
  type thread =
    { t : varinfo (* a global var from Tbls.ResourceTbl *)
    ; f : varinfo
    ; pri : int (* TODO: extract this info from the attributes *)
    }

  type mutex = { m : varinfo }

  type thread_var = varinfo

  (** uniquely identifies the function call
     ** created/defined by `fun_ctx` function *)
  type fun_call_id = string

  (** ADT of all possible edge types actions *)
  type t =
    (* | Assign of string * string (\* var_callee = var_caller *\) *)
    | Call of fun_call_id
    | ThreadCreate of thread
    | ThreadJoin of thread_var
    | MutexInit of mutex
    | MutexLock (*TODO: add associated args for it*)
    | MutexUnlock (*TODO: add associated args for it*)
    | Nop
end

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
    (* the number of keys in the Hashtbl that pass the filter *)
    let key_count table f =
      table |> Hashtbl.keys |> List.of_enum |> List.filter f |> List.length
    in
    key_count table @@ const true


  module ResourceTbl = SymTbl (struct
    type k = Resource.t

    type v = varinfo

    let make_new_val table k =
      let var_name = Resource.show k in
      (* creates a global var for resource with the unique var name of type void* *)
      Goblintutil.create_var (makeGlobalVar var_name voidPtrType)
  end)

  module ThreadPrioTbl = Tbl (struct
    type k = thread_name

    type v = int64
  end)

  module ThreadFunTbl = Tbl (struct
    type k = thread_name

    type v = fun_name
  end)

  module ThreadTidTbl = SymTbl (struct
    type k = thread_name

    type v = int

    let make_new_val table k = all_keys_count table
  end)

  (* context hash to differentiate function calls *)
  module CtxTbl = SymTbl (struct
    type k = int

    type v = int

    let make_new_val table k = all_keys_count table
  end)

  module FunTbl = SymTbl (struct
    type k = string * string (* TODO: what do they represent *)

    type v = int

    let make_new_val table k = all_keys_count table
  end)
end

(** type of a node in CFG *)
type node = PthreadDomain.Pred.Base.t

(** type of a single edge in CFG *)
type edge = node * Action.t * string option * node

module Spec : Analyses.Spec = struct
  module M = Messages
  module List = BatList

  (* Spec implementation *)
  include Analyses.DefaultSpec

  (** Domains *)
  include PthreadDomain

  (* TODO: what is C, Context? *)
  module C = D

  (** Set of created tasks to spawn when going multithreaded *)
  module Tasks = SetDomain.Make (Lattice.Prod (Queries.LS) (D))

  (* TODO: what is G *)
  module G = Tasks

  let tasks_var =
    Goblintutil.create_var (makeGlobalVar "__GOBLINT_PTHREAD_TASKS" voidPtrType)


  let fun_ctx ctx f =
    let ctx_hash =
      match Ctx.to_int ctx with
      | Some i ->
          i |> i64_to_int |> Tbls.CtxTbl.get |> string_of_int
      | None ->
          "TOP"
    in
    f.vname ^ "_" ^ ctx_hash


  module rec Env : sig
    type t

    val get : (D.t, G.t, C.t) ctx -> t

    val d : t -> D.t

    val node : t -> MyCFG.node

    val id : t -> Resource.t
  end = struct
    type t =
      { d : D.t
      ; node : MyCFG.node
      ; fundec : fundec
      ; thread_name : thread_name
      ; id : Resource.t
      }

    let get ctx =
      let d : D.t = ctx.local in
      let node = Option.get !MyCFG.current_node in
      (* determine if we are at the root of a thread or in some called function *)
      let fundec = MyCFG.getFun node in
      let cur_tid = Int64.of_int 1 in
      let thread_name = Tbls.ThreadPrioTbl.get_key cur_tid |> Option.get in
      let id =
        let is_main_fun name =
          List.mem name @@ List.map Json.string @@ GobConfig.get_list "mainfun"
        in
        let funs_of_proc = Edges.funs_for_thread thread_name in
        let open Resource in
        if List.exists (( = ) fundec.svar) funs_of_proc
           || is_main_fun fundec.svar.vname
        then Resource.make Thread thread_name
        else Resource.make Function (fun_ctx d.ctx fundec.svar)
      in
      { d; node; fundec; thread_name; id }


    let d env = env.d

    let node env = env.node

    let id env = env.id
  end

  and Edges : sig
    val table : (Resource.t, edge Set.t) Hashtbl.t

    val add : ?r:string -> ?dst:Node.node -> ?d:D.t -> Env.t -> Action.t -> unit

    val get : Resource.t -> edge Set.t

    val funs_for_thread : thread_name -> varinfo list
  end = struct
    let table = Hashtbl.create 199

    (** [add] adds an edge for the current environment resource id (Thread or Function)
     ** [r] is return status code
     ** [dst] destination node
     ** [d] domain
     ** [env] environment
     ** [action] edge action of type `Action.t` *)
    let add ?r ?dst ?d env action =
      let preds =
        let env_d = Env.d env in
        (d |? env_d).pred
      in
      let add_edge_for_node node =
        let env_node = Env.node env in
        let env_id = Env.id env in
        let action_edge = (node, action, r, MyCFG.getLoc (dst |? env_node)) in
        Hashtbl.modify_def Set.empty env_id (Set.add action_edge) table
      in
      Pred.iter add_edge_for_node preds


    let get res_id = Hashtbl.find_default table res_id Set.empty

    let funs_for_thread proc_name =
      (* let open Action in *)
      let get_funs = function
        (* | ThreadCreate t when t.name = proc_name -> *)
        (* Some t.f *)
        | _ ->
            None
      in
      let filter_map_actions f =
        let action_of_edge (_, action, _, _) = action in
        let all_edges =
          table
          |> Hashtbl.values
          |> List.of_enum
          |> List.map Set.elements
          |> List.concat
        in
        List.filter_map (f % action_of_edge) all_edges
      in
      filter_map_actions get_funs |> List.unique
  end

  let name () = "pthread slicer and promela codegen"

  let init () = LibraryFunctions.add_lib_funs Function.supported

  (*TODO: implement non "assume_success" scenario *)
  let assign ctx (lval : lval) (rval : exp) : D.t = ctx.local

  (*TODO: implement non "assume_success" scenario *)
  let branch ctx (exp : exp) (tv : bool) : D.t = ctx.local

  let body ctx (f : fundec) : D.t =
    (* enter is not called for spawned threads -> initialize them here *)
    let context_hash =
      let base_context =
        Base.Main.context_cpa @@ Obj.obj @@ List.assoc "base" ctx.presub
      in
      Int64.of_int @@ Hashtbl.hash (base_context, ctx.local.tid)
    in
    { ctx.local with ctx = Ctx.of_int context_hash }


  let return ctx (exp : exp option) (f : fundec) : D.t = ctx.local

  let enter ctx (lval : lval option) (f : varinfo) (args : exp list) :
      (D.t * D.t) list =
    (* on function calls (also for main); not called for spawned processes *)
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
      (f : varinfo)
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
          let action = Action.Call (fun_ctx d_callee.ctx f) in
          Edges.add ~d:{ d_caller with pred = last_pred } env action ) ;

        (* set current node as new predecessor, since something interesting happend during the call *)
        { d_callee with
          pred = Pred.of_node @@ Env.node env
        ; ctx = d_caller.ctx
        }


  module ExprEval = struct
    let eval_int ctx exp =
      match ctx.ask (Queries.EvalInt exp) with
      | `Int x ->
          Int64.to_int x
      | _ ->
          failwith @@ "Could not evaluate int-argument " ^ sprint d_plainexp exp


    let eval_str ctx exp =
      match ctx.ask (Queries.EvalStr exp) with
      | `Str x ->
          x
      | _ ->
          failwith
          @@ "Could not evaluate string-argument "
          ^ sprint d_plainexp exp


    let eval_id ctx exp =
      let mayPointTo ctx exp =
        match ctx.ask (Queries.MayPointTo exp) with
        | `LvalSet a
          when (not (Queries.LS.is_top a)) && Queries.LS.cardinal a > 0 ->
            let top_elt = (dummyFunDec.svar, `NoOffset) in
            let a' =
              if Queries.LS.mem top_elt a
              then (
                M.debug_each
                @@ "mayPointTo: query result for "
                ^ sprint d_exp exp
                ^ " contains TOP!" ;
                (* UNSOUND *)
                Queries.LS.remove top_elt a )
              else a
            in
            Queries.LS.elements a'
        | `Bot ->
            []
        | v ->
            M.debug_each
            @@ "mayPointTo: query result for "
            ^ sprint d_exp exp
            ^ " is "
            ^ sprint Queries.Result.pretty v ;
            []
      in
      mayPointTo ctx exp
      |> List.map (Option.get % Tbls.ResourceTbl.get_key % fst)
  end

  module Assign = struct
    let id ctx exp id =
      match exp with
      | AddrOf lval ->
          ctx.assign ~name:"base" lval (mkAddrOf @@ var id)
      | _ ->
          failwith
          @@ "Could not assign id. Expected &id. Found "
          ^ sprint d_exp exp


    let id_by_name ctx resource_type name exp =
      id
        ctx
        exp
        (Tbls.ResourceTbl.get (resource_type, ExprEval.eval_str ctx name))
  end

  let special ctx (lval : lval option) (f : varinfo) (arglist : exp list) : D.t
      =
    let fun_name = f.vname in
    let not_pthread_fun = not @@ Function.is_pthread_fun fun_name in
    if D.any_is_bot ctx.local || not_pthread_fun
    then ctx.local
    else
      let arglist = List.map (stripCasts % constFold false) arglist in
      let add_actions (actions : Action.t list) =
        let env = Env.get ctx in
        let d = Env.d env in
        List.iter (Edges.add env) actions ;
        if List.is_empty actions
        then d
        else { d with pred = Pred.of_node @@ Env.node env }
      in
      let add_action action = add_actions [ action ] in
      let pthread_fun = Function.from_string fun_name in
      let open Function in
      match (pthread_fun, arglist) with
      | ( ThreadCreate
        , [ AddrOf thread; AddrOf thread_attr; AddrOf func; AddrOf fun_arg ] )
        ->
          (* TODO: take into consideration thread attributes, like prio *)
          let pri = 0 in
          let funs_ls =
            let ls =
              let start_routine =
                ctx.ask (Queries.ReachableFrom (AddrOf func))
              in
              match start_routine with `LvalSet ls -> ls | _ -> failwith ""
            in
            Queries.LS.filter
              (fun (v, o) ->
                let lval = (Var v, Lval.CilLval.to_ciloffs o) in
                isFunctionType (typeOfLval lval))
              ls
          in
          let funs =
            funs_ls |> Queries.LS.elements |> List.map fst |> List.unique
          in

          (* TODO: thread name is not unique! *)
          let thread_name = (List.hd funs).vname in
          let thread_goblint_var =
            let thread_res = Resource.make Resource.Thread thread_name in
            Tbls.ResourceTbl.get thread_res
          in

          (* TODO: what's the point of the whole assign thingy?
           *       assign, such that we can later eval it?
           *       then we need to assign the value from code and not goblint_global_var *)
          Assign.id ctx (AddrOf thread) thread_goblint_var ;

          (* create new task for the new thread created *)
          (* TODO: why? why do we need tasks for *)
          let tasks =
            let f_d =
              { tid =
                  Tid.of_int
                  @@ Int64.of_int
                  @@ Tbls.ThreadTidTbl.get thread_name
              ; pri = Pri.of_int @@ Int64.of_int pri
              ; pred = Pred.of_node (MyCFG.Function f)
              ; ctx = Ctx.top ()
              }
            in

            Tasks.add (funs_ls, f_d) (ctx.global tasks_var)
          in
          ctx.sideg tasks_var tasks ;

          let thread_create f =
            Action.ThreadCreate Action.{ t = thread_goblint_var; f; pri }
          in
          add_actions @@ List.map thread_create funs
      | ThreadJoin, [ thread; AddrOf thread_ret ] ->
          (* TODO: take into consideration the return value of thread join *)
          let potential_thread_resources = ExprEval.eval_id ctx thread in
          let thread_join_for_res res =
            Action.ThreadJoin (Tbls.ResourceTbl.get res)
          in
          add_actions @@ List.map thread_join_for_res potential_thread_resources
      | MutexInit, [ AddrOf mutex; AddrOf mutex_attr ] ->
          add_action Nop (* add_action (MutexInit mutex) *)
      | MutexLock, [ AddrOf mutex ] ->
          add_action MutexLock
      | MutexUnlock, [ AddrOf mutex ] ->
          add_action MutexUnlock
      | _ ->
          add_action Nop


  let startstate v =
    let open D in
    make
      (Tid.of_int 0L)
      (Pri.top ())
      (Pred.of_node (MyCFG.Function (emptyFunction "main").svar))
      (Ctx.top ())


  let otherstate v = D.bot ()

  let exitstate v = D.bot ()

  let finalize () =
    let module CodegenCtx : PromelaC.Ctx = struct
      let filter_map_actions f = []

      let get_edges = Edges.get

      let edges_map = Edges.table
    end in
    let module Codegen = PromelaC.Codegen (CodegenCtx) in
    Codegen.save_promela_model ()

  (* print_endline @@ "Model saved as " ^ path ;
   * print_endline "Run ./spin/check.sh to verify." *)
end
