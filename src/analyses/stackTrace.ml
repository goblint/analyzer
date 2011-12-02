open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "stack trace"
  module Dom  = StackDomain.Dom
  module Glob = Global.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    if f.svar.vname = "goblin_initfun" then ctx.local else Dom.push f.svar ctx.local

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    ctx.local
  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    ctx.local
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    [ctx.local,Cil.integer 1, true]

  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
end

module UninitMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "stack_trace" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Stack x
                let extract_l x = match x with `Stack x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

