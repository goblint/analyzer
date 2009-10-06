module LF = LibraryFunctions
module Exp = Exp.Exp

open Cil
open Pretty

(* Note: This is currently more conservative than varEq --- but 
   it should suffice for tests. *)
module Spec : Analyses.Spec =
struct
  exception Top

  module Dom = LockDomain.Symbolic
  module Glob = Global.Make (Lattice.Unit)

  let name = "Symbolic locks"

  let init     () = ()
  let finalize () = ()
  let startstate = Dom.top 
  let otherstate = Dom.top 
  let es_to_string f es = f.svar.vname
  
  let reset_diff x = x
  let get_diff   x = []
  let should_join x y = true
  let eval_funvar a exp glob st = []


  let branch a exp tv glob st = st
  let body   a f glob st = st

  let assign a lval rval glob st =
    Dom.remove a (Cil.Lval lval) st
    
  let return a exp fundec glob st = 
    List.fold_right Dom.remove_var (fundec.sformals@fundec.slocals) st  
    
  let special_fn ask lval f arglist glob st = 
      match f.vname with
   (* | "sem_wait"*)
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_trylock" | "_spin_trylock_irqsave" | "_spin_lock_bh"
      | "mutex_lock" | "mutex_lock_interruptible"
      | "pthread_mutex_lock" ->
          [Dom.add ask (List.hd arglist) st, integer 1, true]
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
      | "mutex_unlock"
      | "pthread_mutex_unlock" ->
          [Dom.remove ask (List.hd arglist) st, integer 1, true]
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> [st, integer 1, true]
            | _ -> [st, integer 1, true]
        end

  let enter_func a lval f args glob st = [(st,st)]
  let leave_func a lval f args glob st1 st2 = st1
  let fork       a lval f args glob st = []  

  let get_locks e st =
    let rec prefix e l =
      match l with
        | Cil.AddrOf  (Cil.Mem l,_) 
        | Cil.StartOf (Cil.Mem l,_) 
        | Cil.Lval    (Cil.Mem l,_) -> Exp.equal e l
        | Cil.CastE (_,l)           -> prefix e l
        | _ -> false
    in
    match e with
      | Cil.Lval (Cil.Mem e, _) ->
          Dom.fold Queries.ES.add (Dom.filter (prefix e) st) (Queries.ES.empty ())
      | _ -> Queries.ES.empty ()

  let query _ _ (x:Dom.t) (q:Queries.t) =
    match q with
      | Queries.PerElementLock e -> `ExprSet (get_locks e x)
      | _ -> Queries.Result.top ()
end