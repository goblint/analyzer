open Cil

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
  
  let query ask _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    Queries.Result.top ()

  (* todo:
     Everything that changes must be dropped from PathMaps left hand side 
     and inlined into right hand sides. Assign to vars and globals work, but escaped 
     and indirect changes do not. *)
     
  let assign a (lval:lval) (rval:exp) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    Dom.assign lval rval st
  
  let branch a (exp:exp) (tv:bool) (gl:glob_fun) (st:Dom.t) : Dom.t = st
  let body a (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t =  Dom.top ()
  let return a (exp:exp option) (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t =  Dom.top ()
  let eval_funvar a (fv:exp) (gl:glob_fun) (st:Dom.t) : varinfo list = []
  let fork ask lv f args gs ls = [] 
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Dom.t) list = [Dom.top (), Dom.top ()]
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (bu:Dom.t) (au:Dom.t) : Dom.t = bu
  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    match lval with 
       | None -> [st,Cil.integer 1, true]
       | Some (Var v,o) -> [Dom.kill (Dom.Lvals.from_var v) st,Cil.integer 1, true]
       | _ -> [Dom.top (),Cil.integer 1, true] (*i think this should not happen*)
end

module AccessMCP =
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "access"
                type lf = Spec.Dom.t
                let inject_l (x:lf) = (`Access x:MCP.local_state)
                let extract_l x = match x with `Access x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

