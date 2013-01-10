open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Dom = ConcDomain.ThreadDomain
  module Glob = Glob.Make (Lattice.Unit) (* no global state *)
  
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
  
  (* We denote the main thread by the global thread id variable named "main" *)
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

(* really stupid thread-ids *)
module StartLocIDs =
struct
  include Analyses.DefaultSpec

  let name = "Unit analysis"
  module Dom  = ConcDomain.ThreadStringSet
  module Glob = Glob.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = ctx.local
  let branch ctx (exp:exp) (tv:bool) : Dom.t =  ctx.local
  let body ctx (f:fundec) : Dom.t =  ctx.local
  let return ctx (exp:exp option) (f:fundec) : Dom.t = ctx.local
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = [ctx.local,ctx.local]
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = ctx.local
    
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
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    begin match LibraryFunctions.classify f.vname arglist with
      | `ThreadCreate (fn, x) -> 
          let fns = eval_exp_addr ctx.ask fn in
          let location x = let l = !Tracing.current_loc in l.file ^ ":" ^ string_of_int l.line ^ ":" ^ x.vname in
          let new_thread x = ctx.spawn x (Dom.singleton (location x)) in
          List.iter new_thread (List.concat (List.map ValueDomain.Addr.to_var_may fns))
      | _ -> ()
    end;
    [ctx.local,Cil.integer 1, true]

  let main = Dom.singleton "main"
  let startstate () = main
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
end

module ThreadLocIDMCP =
  MCP.ConvertToMCPPart
        (StartLocIDs)
        (struct let name = "thread-id-location"
                let depends = []
                type lf = StartLocIDs.Dom.t
                let inject_l x = `ThreadLocSet x
                let extract_l x = match x with `ThreadLocSet x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = StartLocIDs.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
