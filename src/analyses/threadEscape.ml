(** Variables that escape threads using the last argument from pthread_create. *)

open Cil
open Pretty
open Analyses

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Escaped Variables"
  module Dom  = EscapeDomain.EscapedVars
  module Glob = Glob.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t = 
    match q with
      | Queries.MayEscape v -> `Bool (Dom.mem v ctx.local)
      | _ -> Queries.Result.top ()
 
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
 
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []

  let rec eval_fv ask (exp:Cil.exp): varinfo option = 
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

  let fork ctx lv f args = 
    match f.vname with
      | "pthread_create" -> begin        
          match args with
            | [_; _; start; ptc_arg] ->
				let r = reachable ctx.ask ptc_arg in
				  List.map (fun (v,_) -> (v,r)) (query_lv ctx.ask start)
            | _ -> Messages.bailwith "pthread_create arguments are strange!"
        end
      | _ -> [] 

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let forks = fork ctx lval f arglist in
    let spawn (x,y) = ctx.spawn x y in List.iter spawn forks ;
    match f.vname with
      | "pthread_create" -> begin        
          match arglist with
            | [_; _; _; ptc_arg] -> begin
                [reachable ctx.ask ptc_arg,Cil.integer 1, true]
              end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> [ctx.local,Cil.integer 1, true]

  let startstate v = Dom.bot ()
  let otherstate v = Dom.bot ()
  let exitstate  v = Dom.bot ()
end

module TransparentSignatureHack: Analyses.Spec = Spec

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "escape" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x: lf): MCP.local_state = `Escape x
                let extract_l x = match x with `Escape x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 : Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "escape" (module Spec2 : Spec2)
