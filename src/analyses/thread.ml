open Cil
open Pretty
module Trivial =  ConcDomain.Simple

module Spec : Analyses.Spec =
struct
  module Dom  = Trivial
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

  (* queries *)
  let query ask _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = 
    Queries.Result.top ()
 
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
  
  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st:Dom.t) : (Dom.t * Cil.exp * bool) list =
    match f.vname with 
       | "pthread_create" -> 
          let new_fl = Dom.join st (Dom.get_main ()) in
            [new_fl,Cil.integer 1, true]
       | _ -> 
        (* We actually want to spawn threads for some escaped function pointers,
           but lets ignore that for now. *)
        [st,Cil.integer 1, true]

  
  let fork ask lv f args gs ls = 
    let finish_him () = Messages.bailwith "pthread_create arguments are strange!" in
    let pt_create () =
      match args with
        | [_; _; start; ptc_arg] -> begin
            match eval_fv ask start with
              | Some v -> [v, Dom.get_multi ()]
              | None -> finish_him ()
            end
        | _ -> finish_him ()
    in
    match f.vname with 
       | "pthread_create" -> pt_create ()
       | _ -> [] (* NB! unknown funktion spawns are covered with otherstate *)

  let startstate () = Dom.bot ()
  let otherstate () = Dom.top ()

  let get_diff _ = []
  let reset_diff x = x
  
  let name = "Thread analysis"
  let es_to_string f _ = f.svar.vname

  let should_join _ _ = true
  let finalize () = ()
  let init () = ()
end

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
