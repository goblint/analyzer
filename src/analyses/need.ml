open Cil
open Pretty
open Analyses

module Spec 
  : Analyses.Spec 
  with type Dom.t = unit 
   and type Glob.Val.t = unit 
   and module Glob.Var = Basetype.Variables =
struct
  include Analyses.DefaultSpec

  let name = "Unit analysis"
  module Dom  = Lattice.Unit
  module Glob = Glob.Make (Lattice.Unit)
  
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

  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
end


module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "lval_need" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l () = `Need ()
                let extract_l = function `Need x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g () = `None  
                let extract_g = function `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "lval_need" (module Spec2 : Spec2)         