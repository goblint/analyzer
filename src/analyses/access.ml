open Cil
open Analyses

module Path = AccessDomain.Path

module Spec =
struct 
  module Glob = Global.Make (Lattice.Unit)
  module Dom = AccessDomain.Access
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()

  let get_diff _ = []
  let reset_diff x = x
  
  let name = "Access Analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true
  let finalize () = ()
  let init () = ()
  
  let query _ _ : Queries.Result.t = 
    Queries.Result.top ()

  (* todo:
     Everything that changes must be dropped from PathMaps left hand side 
     and inlined into right hand sides. Assign to vars and globals work, but escaped 
     and indirect changes do not. *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t = 
    Dom.assign lval rval ctx.local
  
  let branch ctx (exp:exp) (tv:bool) : Dom.t = ctx.local
  let body ctx (f:fundec) : Dom.t =  Dom.top ()
  let return ctx (exp:exp option) (f:fundec) : Dom.t =  Dom.top ()
  let eval_funvar ctx (fv:exp) : varinfo list = []
  let fork ctx lv f args = [] 
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = [Dom.top (), Dom.top ()]
  let leave_func ctx (lval:lval option) (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = ctx.local
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    match lval with 
       | None -> [ctx.local,Cil.integer 1, true]
       | Some (Var v,o) -> [Dom.kill (Dom.Lvals.from_var v) ctx.local,Cil.integer 1, true]
       | _ -> [Dom.top (),Cil.integer 1, true] (*i think this should not happen*)
end

module AccessMCP =
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "access"
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x:lf) = (`Access x:MCP.local_state)
                let extract_l x = match x with `Access x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

