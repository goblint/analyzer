(** Variables that escape threads using the last argument from pthread_create. *)

open Cil
open Pretty
open Analyses

module M = Messages

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "escape"
  module D = EscapeDomain.EscapedVars
  module C = EscapeDomain.EscapedVars
  module G = Lattice.Unit
  
  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t = 
    match q with
      | Queries.MayEscape v -> `Bool (D.mem v ctx.local)
      | _ -> Queries.Result.top ()
 
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
    [ctx.local,ctx.local]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let rec cut_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (_,o) -> `NoOffset
      | `Field (f,o) -> `Field (f, cut_offset o)
  
  let reachable ask e: D.t = 
    match ask (Queries.ReachableFrom e) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
           (* let to_extra (v,o) set = D.add (Addr.from_var_offset (v, cut_offset o)) set in *)
          let to_extra (v,o) set = D.add v set in
            Queries.LS.fold to_extra a (D.empty ())
      (* Ignore soundness warnings, as invalidation proper will raise them. *)
      | _ -> D.empty ()
 
  let query_lv ask exp = 
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) -> 
          Queries.LS.elements l
      | _ -> []

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

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let forks = fork ctx lval f arglist in
    let spawn (x,y) = ctx.spawn x y in List.iter spawn forks ;
    match f.vname with
      | "pthread_create" -> begin        
          match arglist with
            | [_; _; _; ptc_arg] -> begin
                reachable ctx.ask ptc_arg
              end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ = 
  MCP.register_analysis (module Spec : Spec)
