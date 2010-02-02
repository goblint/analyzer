open Cil
open Pretty

module M = Messages

module Spec =
struct
  let name = "Escape analysis"
  module Dom  = SetDomain.HeadlessSet (Basetype.Variables) 
  module Glob = Global.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* queries *)
  let query ask g (st: Dom.t) (q:Queries.t) : Queries.Result.t = 
    match q with
      | Queries.MayEscape v -> `Bool (Dom.mem v st)
      | _ -> Queries.Result.top ()
 
  (* transfer functions *)
  let assign a (lval:lval) (rval:exp) (gl:glob_fun) (st:Dom.t) : Dom.t =
    st
   
  let branch a (exp:exp) (tv:bool) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st
  
  let body a (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st

  let return a (exp:exp option) (f:fundec) (gl:glob_fun) (st:Dom.t) : Dom.t = 
    st
  
  let eval_funvar a (fv:exp) (gl:glob_fun) (st:Dom.t) : varinfo list = 
    []
    
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Dom.t) list =
    [st,st]
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (bu:Dom.t) (au:Dom.t) : Dom.t =
    au

  let rec cut_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (_,o) -> `NoOffset
      | `Field (f,o) -> `Field (f, cut_offset o)
  
  let reachable ask e: Dom.t = 
    match ask (Queries.ReachableFrom e) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
           (* let to_extra (v,o) set = Dom.add (Addr.from_var_offset (v, cut_offset o)) set in *)
          let to_extra (v,o) set = Dom.add v set in
            Queries.LS.fold to_extra a (Dom.empty ())
      (* Ignore soundness warnings, as invalidation proper will raise them. *)
      | _ -> Dom.empty ()

  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    match f.vname with
      | "pthread_create" -> begin        
          match arglist with
            | [_; _; _; ptc_arg] -> begin
                [reachable a ptc_arg,Cil.integer 1, true]
              end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> [st,Cil.integer 1, true]

  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []
 
  let rec eval_fv ask (exp:Cil.exp): varinfo option = 
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

  let fork ask lv f args gs ls = 
    let finish_him () = Messages.bailwith "pthread_create arguments are strange!" in
    match f.vname with
      | "pthread_create" -> begin        
          match args with
            | [_; _; start; ptc_arg] -> begin
                match eval_fv ask start with
                  | Some v -> [v, reachable ask ptc_arg]
                  | None -> finish_him ()
              end
            | _ -> finish_him () 
        end
      | _ -> [] 

  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()

  let get_diff _ = []
  let reset_diff x = x
  
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true
  let finalize () = ()
  let init () = ()
end

module TransparentSignatureHack: Analyses.Spec = Spec

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "escape" 
                type lf = Spec.Dom.t
                let inject_l (x: lf): MCP.local_state = `Escape x
                let extract_l x = match x with `Escape x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
