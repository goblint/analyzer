open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Dom = ConcDomain.ThreadDomain
  module Glob = Global.Make (Lattice.Unit) (* no global state *)
  
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []
 
  let eval_fv ask exp = 
    match query_lv ask exp with
      | [(v,_)] -> Some v (* This currently assumes that there is a single possible l-value? *)
      | _ -> None

  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    ctx.local
  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au
    
  let call_unusable (fname:string) =
    Messages.bailwith (Printf.sprintf "%s has unusable arguments!" fname)
    
  let eval_arg ctx (arg:exp) =
    match eval_fv ctx.ask arg with
      | Some v -> v
      | None   -> Messages.bailwith "cannot extract arg"

  (** Retrieves the current thread id from the domain value. *)
  let current_thread (state : Dom.t) : Basetype.Variables.t =
    let thread_id_set = ConcDomain.ThreadIdSet.elements (Dom.current_thread state) in
    match thread_id_set with
      | [thread_id] -> thread_id
      | _           -> Messages.bailwith "Current thread id is unusable"

  (** Handles creation of the new thread. thread_id has been already resolved. *)
  let pthread_create_id ctx start_routine (thread_id : Basetype.Variables.t) =
    let current_thread_id = current_thread ctx.local in
    let new_state = Dom.create_thread ctx.local current_thread_id thread_id in
      ctx.spawn start_routine (Dom.make_entry new_state thread_id);
      [new_state, Cil.integer 1, true]
  
  (** Handles creation of the new thread. Resolves pthread_create arguments. *)    
  let pthread_create ctx (args:exp list) =
	  match args with
	    | [id; _; start; _] ->
        let start_routine = eval_arg ctx start in
        let thread_id = eval_arg ctx id in
          pthread_create_id ctx start_routine thread_id
	    | _ -> call_unusable "pthread_create"

  (** Handles join with another thread. thread_id has been already resolved. *)
  let pthread_join_id (state : Dom.t) (thread_id : Basetype.Variables.t) =
    let current_thread_id = current_thread state in
      [Dom.join_thread state current_thread_id thread_id, Cil.integer 1, true]

  (** Handles join with another thread. Resolves pthread_join arguments. *)
  let pthread_join ctx (args:exp list) =
    match args with
      | [Lval id; _] -> pthread_join_id ctx.local (eval_arg ctx (mkAddrOf id))
      | _            -> call_unusable "pthread_join"

  (** Handles unknown functions (functions without known definition). We
      catch both pthread_create and pthread_join here. *)
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    print_string "special_fn";
    print_string f.vname;
    match f.vname with
       | "pthread_create" -> pthread_create ctx arglist
       | "pthread_join"   -> pthread_join ctx arglist
       | _                -> [ctx.local,Cil.integer 1, true]
  
  let startstate () = (
    ConcDomain.ThreadIdSet.singleton (Cil.makeGlobalVar "main" Cil.voidType),
    ConcDomain.ThreadsVector.bot())
    
  let otherstate () = Dom.bot ()
  let exitstate  () = Dom.bot ()

  let name = "Thread analysis"
end

module ThreadMCP = (* MCP - master control program, see mCP.ml *)
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "thread"
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Thread x
                let extract_l x = match x with `Thread x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
