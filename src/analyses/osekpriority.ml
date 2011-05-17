open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Dom  = IntDomain.Flattened
  module Glob = Global.Make (Lattice.Unit)

  (* transfer functions *)
  let intrpt ctx : Dom.t = Dom.top()

  let assign ctx (lval:lval) (rval:exp) : Dom.t = Dom.top()
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = Dom.top()
      
  let body ctx (f:fundec) : Dom.t = Dom.top()

  let return ctx (exp:exp option) (f:fundec) : Dom.t = Dom.top()
  
  let eval_funvar ctx (fv:exp) =  []
   
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = []
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = Dom.top()
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list = 
    match f.vname with
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
      | _ -> []

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
  
  let name = "OSEK priorities"

  let should_join _ _ = true


(** Finalization and other result printing functions: *)
   
  (** postprocess and print races and other output *)
  let finalize () = Base.Main.finalize ()

  let init () =  ()

end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "OSEK priorities" 
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
