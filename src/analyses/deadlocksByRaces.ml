open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Deadlock Checking by Data Race Detection"
  module MSpec = Mutex.Spec
  module Dom  = MSpec.Dom
  module Glob = MSpec.Glob
  
  let extra_var = makeGlobalVar "__deadlock_variable" intType
  let gate_var = makeGlobalVar "__gatelock_variable" intType
  
  let init     () = MSpec.init ()
  let finalize () = MSpec.finalize ()
  
  let sync = MSpec.sync

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = MSpec.assign ctx lval rval
  let branch ctx (exp:exp) (tv:bool) : Dom.t = MSpec.branch ctx exp tv 
  let body ctx (f:fundec) : Dom.t = MSpec.body ctx f
  let return ctx (exp:exp option) (f:fundec) : Dom.t =  MSpec.return ctx exp f
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = MSpec.enter_func ctx lval f args
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = MSpec.leave_func ctx lval fexp f args au
  
  let fake_unlock = makeGlobalVar "pthread_mutex_unlock" intType
  
  let add_access may must tid = true
  let add_gatelock may must tid = true
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let thread, maylocks = 
      match ctx.presub with
        | [`ThreadLocSet t; `MayLocks l] -> t, l
        | _ -> failwith "Deadlock Checking by Data Race Detection Broken --- dependencies missing!"
    in
    match (LibraryFunctions.classify f.vname arglist, f.vname) with
      | `Lock (failing, rw), _ when add_access ctx.local maylocks thread
          ->  if add_gatelock ctx.local maylocks thread then
              begin match MSpec.special_fn ctx None f [AddrOf (Var gate_var, NoOffset)] with
               | [(nd,_,_)] ->
                   let nd = MSpec.assign (swap_st ctx nd) (Var extra_var, NoOffset) one in
                   begin match MSpec.special_fn (swap_st ctx nd) None fake_unlock [AddrOf (Var gate_var, NoOffset)] with
                     | [(nd,_,_)] ->          
                         MSpec.special_fn (swap_st ctx nd) lval f arglist 
                     | _ -> failwith "I refuse to believe that this ever happens."
                   end
               | _ -> failwith "I refuse to believe that this ever happens."
             end
             else
               let nd = MSpec.assign ctx (Var extra_var, NoOffset) one in
               [nd,Cil.one,true]
      | _ -> MSpec.special_fn ctx lval f arglist 
    

  let startstate () = MSpec.startstate ()
  let otherstate () = MSpec.otherstate ()
  let exitstate  () = MSpec.exitstate ()
end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "oslo" 
                let depends = ["thread-id-location";"maylocks"]
                type lf = Spec.Dom.t
                let inject_l x = `Oslo x
                let extract_l x = match x with `Oslo x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Oslo x
                let extract_g x = match x with `Oslo x -> x | _ -> raise MCP.SpecificationConversionError
         end)


module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "oslo" (module Spec2 : Spec2)