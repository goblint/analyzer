(** Stack-trace "analyses". *)

open Cil
open Pretty
open Analyses
module LF = LibraryFunctions

module Spec (D: StackDomain.S) (P: sig val name : string end)=
struct
  include Analyses.DefaultSpec

  let name = P.name
  module D = D
  module C = D
  module G = Lattice.Unit
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    ctx.local
  
  let body ctx (f:fundec) : D.t = 
    if f.svar.vname = "goblin_initfun" then ctx.local else D.push f.svar ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    ctx.local
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local,ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    ctx.local
  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.top ()
end

module SpecLoc =
struct
  include Analyses.DefaultSpec

  let name = "stack_loc"
  module D = StackDomain.Dom3
  module C = StackDomain.Dom3
  module G = Lattice.Unit
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    ctx.local
  
  let body ctx (f:fundec) : D.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t = 
    ctx.local
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, D.push !Tracing.current_loc ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    ctx.local
  
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []

  let fork ctx lv f args = 
    match LF.classify f.vname args with 
      | `ThreadCreate (start,ptc_arg) -> 
          let nst = D.push !Tracing.current_loc ctx.local in
            List.map (fun (v,_) -> (v,nst)) (query_lv ctx.ask start)
      | _ ->  []

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let forks = fork ctx lval f arglist in
    let spawn (x,y) = ctx.spawn x y in 
    let _ = List.iter spawn forks in 
      ctx.local


  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.top ()
end


module Spec1 = Spec (StackDomain.Dom1) (struct let name = "stack_trace" end)
module Spec2 = Spec (StackDomain.Dom2) (struct let name = "stack_trace_set" end)
let _ = 
  MCP.register_analysis (module SpecLoc : Spec);        
  MCP.register_analysis (module Spec1 : Spec);        
  MCP.register_analysis (module Spec2 : Spec)         
