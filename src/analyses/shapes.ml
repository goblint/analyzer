open Cil
open Pretty
open Analyses

open ShapeDomain

module GU = Goblintutil

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Shape Analysis for Cyclic Doubly Linked Lists"
  module Dom  = ShapeDomain.Dom
  module Glob = Global.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    let st = ctx.local in
    try 
    match eval_lp ctx.ask (Lval lval), eval_lp ctx.ask rval with
      | Some (l,`Next), Some (r,`NA) -> Dom.map (normal l `Next r) st
      | Some (l,`Prev), Some (r,`NA) -> Dom.map (normal l `Prev r) st
      | Some (l,`NA), Some (r,dir) -> 
          Dom.fold (fun d xs -> List.fold_right Dom.add (add_alias l (r,dir) d) xs) st (Dom.empty ()) 
      | _ -> st
    with x -> ignore (Pretty.printf "Exception %s \n" (Printexc.to_string x)); st
    
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
    let lift_st x = [x, Cil.integer 1, true] in
    match f.vname, arglist with
      | "kill", [ee] -> begin
          match eval_lp ctx.ask ee with
            | Some (lp, _) -> lift_st (Dom.map (kill lp) ctx.local)
            | _ -> lift_st ctx.local
        end
      | "collapse", [e1;e2] -> begin
          match eval_lp ctx.ask e1, eval_lp ctx.ask e2 with
            | Some (lp1, _), Some (lp2, _) -> lift_st (Dom.map (collapse_summary lp1 lp2) ctx.local)
            | _ -> lift_st ctx.local
        end
      | _ -> lift_st ctx.local
      
  let startstate () = Dom.singleton (SHMap.top ())
  let otherstate () = Dom.top ()
end

module ShapeMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "shape" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Shape x
                let extract_l x = match x with `Shape x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
