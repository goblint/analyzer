open Cil
open Pretty
open Analyses
module LF = LibraryFunctions

module Spec (D: StackDomain.S)=
struct
  include Analyses.DefaultSpec

  let name = "stack trace"
  module Dom  = D
  module Glob = Glob.Make (Lattice.Unit)
  
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
  let otherstate () = Dom.bot ()
  let exitstate  () = Dom.top ()
end

module SpecLoc =
struct
  include Analyses.DefaultSpec

  let name = "stack trace"
  module Dom  = StackDomain.Dom3
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
    [ctx.local, Dom.push !Tracing.current_loc ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    ctx.local
  
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []

  let fork ctx lv f args = 
    match LF.classify f.vname args with 
      | `ThreadCreate (start,ptc_arg) -> 
          let nst = Dom.push !Tracing.current_loc ctx.local in
            List.map (fun (v,_) -> (v,nst)) (query_lv ctx.ask start)
      | _ ->  []

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let forks = fork ctx lval f arglist in
    let spawn (x,y) = ctx.spawn x y in 
    let _ = List.iter spawn forks in 
      [ctx.local,Cil.integer 1, true]


  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()
  let exitstate  () = Dom.top ()
end

module UninitMCP = 
  MCP.ConvertToMCPPart
        (Spec (StackDomain.Dom))
        (struct let name = "stack_trace" 
                let depends = []
                type lf = Spec(StackDomain.Dom).Dom.t
                let inject_l x = `Stack x
                let extract_l x = match x with `Stack x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec(StackDomain.Dom).Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module UninitMCP2 = 
  MCP.ConvertToMCPPart
        (Spec (StackDomain.Dom2))
        (struct let name = "stack_trace_set" 
                let depends = []
                type lf = Spec(StackDomain.Dom2).Dom.t
                let inject_l x = `Stack2 x
                let extract_l x = match x with `Stack2 x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec(StackDomain.Dom2).Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module UninitMCP3 = 
  MCP.ConvertToMCPPart
        (SpecLoc)
        (struct let name = "stack_loc" 
                let depends = []
                type lf = SpecLoc.Dom.t
                let inject_l x = `Stack3 x
                let extract_l x = match x with `Stack3 x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = SpecLoc.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 = Constraints.Spec2OfSpec (Spec (StackDomain.Dom))
module Spec2' = Constraints.Spec2OfSpec (Spec (StackDomain.Dom2))
module Spec2'' = Constraints.Spec2OfSpec (SpecLoc)
let _ = 
  MCP.register_analysis "stack_loc" (module Spec2'' : Spec2);        
  MCP.register_analysis "stack_trace" (module Spec2 : Spec2);        
  MCP.register_analysis "stack_trace_set" (module Spec2' : Spec2)         