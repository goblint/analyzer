open Cil
open Pretty
open Analyses
open OilUtil

module Spec =
struct
  include Analyses.DefaultSpec

  module Dom  = IntDomain.Flattened
  module Glob = Glob.Make (Lattice.Unit)

  let ask_it ctx = let q = ctx.ask (Queries.Priority "") in 
    match q with (`Int p) -> `Lifted p | _ -> failwith "This (hopefully3) never happens!"

  (* transfer functions *)

  let assign ctx (lval:lval) (rval:exp) : Dom.t = ask_it ctx
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = ask_it ctx
      
  let body ctx (f:fundec) : Dom.t = ask_it ctx

  let return ctx (exp:exp option) (f:fundec) : Dom.t = ask_it ctx
  
(*   let eval_funvar ctx (fv:exp) =  [(ctx.local,ctx.local)] *)
   
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = [(ctx.local,ctx.local)]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = ask_it ctx
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list = 
    let fvname = get_api_names f.vname in
    match fvname with
      | "GetResource" 
      | "ReleaseResource"  
      | "ActivateTask"
      | "ChainTask"
      | "DisableAllInterrupts"
      | "EnsableAllInterrupts"
      | "SuspendAllInterrupts"
      | "ResumeAllInterrupts"
      | "SuspendOSInterrupts"
      | "ResumeOSInterrupts"
      | "TerminateTask"
      | "WaitEvent" 
      | "SetEvent"
      | "ClearEvent"
      | "GetEvent"
      | "Schedule"
      | "GetTaskID"
      | "GetTaskState"
      | "GetAlarmBase" 
      | "GetAlarm" 
      | "SetRelAlarm" 
      | "SetAbsAlarm" 
      | "CancelAlarm" 
      | "GetActiveApplicationMode" 
      | "StartOS" 
      | "ShutdownOS" 
      | _ -> [ctx.local,Cil.integer 1, true]

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
  
  let name = "OSEK3"

  let should_join _ _ = true
 
  (** postprocess and print races and other output *)
  let finalize () =  ()

  let init () =  ()

end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK3" 
                let depends = ["OSEK"]
                type lf = Spec.Dom.t
                let inject_l x = `OSEK3 x
                let extract_l x = match x with `OSEK3 x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
         
(*module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)*)
