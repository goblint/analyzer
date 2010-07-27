open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Containment analysis"
  module Dom  = ContainDomain.Dom
  module Glob = Global.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

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
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    [ctx.local,Cil.integer 1, true]

  let fork ctx lv f args = 
    [] 

  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()
end

module ContainmentMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "containment" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Contain x
                let extract_l x = match x with `Contain x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

