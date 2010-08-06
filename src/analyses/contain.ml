open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Containment analysis"
  
  module Dom  = 
  struct
    include ContainDomain.Dom
    let short n (_,x:t) = Danger.short n x
    let toXML_f sf ((_,x):t) = 
      match Danger.toXML_f (fun _ x -> sf 800 (ContainDomain.FuncName.bot (),x)) x with
        | Xml.Element (node, (text, _)::xs, []) -> 
            Xml.Element (node, (text, "Containment Analysis (top)")::xs, [])              
        | Xml.Element (node, (text, _)::xs, elems) -> 
            Xml.Element (node, (text, "Containment Analysis")::xs, elems)     
        | x -> x
    let toXML x = toXML_f short x
  end
  
  module Glob = Global.Make (Lattice.Unit)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    Dom.warn_bad_reachables ctx.ask [AddrOf lval] false ctx.local;
    Dom.assign_argmap ctx.ask lval rval ctx.local
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  let body ctx (f:fundec) : Dom.t =
    Dom.set_funname f (Dom.add_formals f ctx.local)

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    let arglist = match exp with Some x -> [x] | _ -> [] in
    Dom.warn_bad_reachables ctx.ask arglist true ctx.local;
    Dom.remove_formals f ctx.local

  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    au
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    Dom.warn_bad_reachables ctx.ask arglist true ctx.local;
    [ctx.local,Cil.integer 1, true]

  let fork ctx lv f args = 
    [] 

  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()
end

module ContainmentMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "containment" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x:lf) = (`Contain x:MCP.local_state)
                let extract_l x = match x with `Contain x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

