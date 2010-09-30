open Cil
open Pretty
open Analyses
module Trivial =  ConcDomain.Simple

module Spec =
struct
  include Analyses.DefaultSpec

  module Dom = Trivial
  module Glob = Global.Make (Lattice.Unit) (* no global state *)

  (* helper functions *)
  
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []
 
  let rec eval_fv ask (exp:Cil.exp): varinfo option = 
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

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
    
  let fork ctx lv f args = 
    let finish_him () = Messages.bailwith "pthread_create arguments are strange!" in
    let pt_create () =
      match args with
        | [_; _; start; ptc_arg] -> begin
            match eval_fv ctx.ask start with
              | Some v -> [v, Dom.get_multi ()]
              | None -> finish_him ()
            end
        | _ -> finish_him ()
    in
    match f.vname with 
       | "pthread_create" -> pt_create ()
       | _ -> [] (* NB! unknown funktion spawns are covered with otherstate *)

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let forks = fork ctx lval f arglist in
    let spawn (x,y) = ctx.spawn x y in List.iter spawn forks ;
    match f.vname with 
       | "pthread_create" -> 
          let new_fl = Dom.join ctx.local (Dom.get_main ()) in
            [new_fl,Cil.integer 1, true]
       | _ -> 
        (* We actually want to spawn threads for some escaped function pointers,
           but lets ignore that for now. *)
        [ctx.local,Cil.integer 1, true]


  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()

  let name = "Thread analysis"
end

module ThreadMCP = (* MCP - master control program, see mCP.ml *)
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "thread" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Thread x
                let extract_l x = match x with `Thread x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
