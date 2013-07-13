(** Thread-id analyses. *)

open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module D = ConcDomain.ThreadDomain
  module C = D
  module G = Lattice.Unit (* no global state *)

  let name = "thread_analysis"

  (* query_lv and eval_lv are used for resolving function pointers
     into function bodies. *)
  
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []
 
  let eval_fv ask exp = 
    match query_lv ask exp with
      | [(v,_)] -> Some v (* This currently assumes that there is a single possible l-value. *)
      | _ -> None

  (* Transfer functions for instructions that do not deal with threads,
      have no influence for this analysis. *)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    ctx.local
  
  let body ctx (f:fundec) : D.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    ctx.local
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au
    
  let call_unusable (fname:string) =
    Messages.bailwith (Printf.sprintf "%s has unusable arguments!" fname)
    
  let eval_arg ctx (arg:exp) =
    match eval_fv ctx.ask arg with
      | Some v -> v
      | None   -> Messages.bailwith "cannot extract arg"

  (** Retrieves the current thread id from the domain value. *)
  let current_thread (state : D.t) : Basetype.Variables.t =
    let thread_id_set = ConcDomain.ThreadIdSet.elements (D.current_thread state) in
    match thread_id_set with
      | [thread_id] -> thread_id
      | _           -> Messages.bailwith "Current thread id is unusable"

  (** Handles creation of the new thread. thread_id has been already resolved. *)
  let pthread_create_id ctx start_routine (thread_id : Basetype.Variables.t) =
    let current_thread_id = current_thread ctx.local in
    let new_state = D.create_thread ctx.local current_thread_id thread_id in
      ctx.spawn start_routine (D.make_entry new_state thread_id);
      [new_state, integer 1, true]
  
  (** Handles creation of the new thread. Resolves pthread_create arguments. *)    
  let pthread_create ctx (args:exp list) =
	  match args with
	    | [id; _; start; _] ->
        let start_routine = eval_arg ctx start in
        let thread_id = eval_arg ctx id in
          pthread_create_id ctx start_routine thread_id
	    | _ -> call_unusable "pthread_create"

  (** Handles join with another thread. thread_id has been already resolved. *)
  let pthread_join_id (state : D.t) (thread_id : Basetype.Variables.t) =
    let current_thread_id = current_thread state in
      [D.join_thread state current_thread_id thread_id, integer 1, true]

  (** Handles join with another thread. Resolves pthread_join arguments. *)
  let pthread_join ctx (args:exp list) =
    match args with
      | [Lval id; _] -> pthread_join_id ctx.local (eval_arg ctx (mkAddrOf id))
      | _            -> call_unusable "pthread_join"

  (** Handles unknown functions (functions without known definition). We
      catch both pthread_create and pthread_join here. *)
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (D.t * exp * bool) list =
    print_string "special";
    print_string f.vname;
    match f.vname with
       | "pthread_create" -> pthread_create ctx arglist
       | "pthread_join"   -> pthread_join ctx arglist
       | _                -> [ctx.local,integer 1, true]
  
  (* We denote the main thread by the global thread id variable named "main" *)
  let startstate v = (
    ConcDomain.ThreadIdSet.singleton (makeGlobalVar "main" voidType),
    ConcDomain.ThreadsVector.bot())
    
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end


(* really stupid thread-ids *)
module StartLocIDs =
struct
  include Analyses.DefaultSpec

  let name = "thread-id-location"
  module D = ConcDomain.ThreadStringSet
  module C = D
  module G = Lattice.Unit
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : D.t =  ctx.local
  let body ctx (f:fundec) : D.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : D.t = ctx.local
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = [ctx.local,ctx.local]
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t = ctx.local
    
  (* Helper function to convert query-offsets to valuedomain-offsets *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)
    
  (* Query the value (of the locking argument) to a list of locks. *)
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a)
                     && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) -> 
          Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []    
      | _ -> []
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    begin match LibraryFunctions.classify f.vname arglist with
      | `ThreadCreate (fn, x) -> 
          let fns = eval_exp_addr ctx.ask fn in
          let location x = let l = !Tracing.current_loc in l.file ^ ":" ^ string_of_int l.line ^ ":" ^ x.vname in
          let new_thread x = ctx.spawn x (D.singleton (location x)) in
          List.iter new_thread (List.concat (List.map ValueDomain.Addr.to_var_may fns))
      | _ -> ()
    end;
    ctx.local

  let main = D.singleton "main"
  let startstate v = main
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ = 
  MCP.register_analysis (module StartLocIDs : Spec)         
