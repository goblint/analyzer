open Cil
open Pretty
open Analyses

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Escaped Variables"
  module Dom  = 
  struct 
    include SetDomain.HeadlessSet (Basetype.Variables) 
    let toXML_f sf x = 
      match toXML x with
        | Xml.Element (node, [text, _], elems) -> 
            let summary = "Escaped Variables: " ^ sf Goblintutil.summary_length x in
              Xml.Element (node, [text, summary], elems)
        | x -> x
        
    let toXML s  = toXML_f short s
  end
  module Glob = Global.Make (Lattice.Unit)
  
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
  
  let leave_func ctx (lval:lval option) (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
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

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    match f.vname with
      | "pthread_create" -> begin        
          match arglist with
            | [_; _; _; ptc_arg] -> begin
                [reachable ctx.ask ptc_arg,Cil.integer 1, true]
              end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> [ctx.local,Cil.integer 1, true]

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
    let finish_him () = Messages.bailwith "pthread_create arguments are strange!" in
    match f.vname with
      | "pthread_create" -> begin        
          match args with
            | [_; _; start; ptc_arg] -> begin
                match eval_fv ctx.ask start with
                  | Some v -> [v, reachable ctx.ask ptc_arg]
                  | None -> finish_him ()
              end
            | _ -> finish_him () 
        end
      | _ -> [] 

  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()
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
