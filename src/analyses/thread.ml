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

  (* transfer functions *)
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
    
  (* Handles the call of pthread_create. *)
  let pt_create ctx (args:exp list) =
	  match args with
	    | [id; _; start; _] ->
        let start_routine = eval_arg ctx start in
        let thread_id = eval_arg ctx id in
        let n = Dom.create_thread thread_id ctx.local in
          ctx.spawn start_routine n;
          [n, Cil.integer 1, true]
	    | _ -> call_unusable "pthread_create"

  (* Handles the call of pthread_join. *)
  let pt_join ctx (args:exp list) =
    match args with
      | [Lval id; _] ->
        let thread_id = eval_arg ctx (mkAddrOf id) in
          [Dom.join_thread thread_id ctx.local, Cil.integer 1, true]
      | _ -> call_unusable "pthread_join"

  (* special_fn is called, when given varinfo is not connected to a fundec -- no function definition is given
  in definition of varinfo. *)
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    match f.vname with (* special_fn - value after function call *)
       | "pthread_create" -> pt_create ctx arglist
       | "pthread_join"   -> pt_join ctx arglist
       | _                -> [ctx.local,Cil.integer 1, true]
        (* We actually want to spawn threads for some escaped function pointers,
           but lets ignore that for now. *)
      
  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot () (* should be top? *)

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
