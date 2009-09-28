module Equ = MusteqDomain.Equ
module Equset = LockDomain.LocksetEqu
module LF = LibraryFunctions

open Cil
open Pretty


module Spec : Analyses.Spec =
struct
  exception Top

  module Dom = LockDomain.LocksetEqu
  module Glob = Global.Make (Lattice.Unit)

  let name = "Symbolic locks"

  let init     () = ()
  let finalize () = ()
  let startstate = Dom.top 
  let otherstate = Dom.top 
  let es_to_string f es = f.svar.vname
  
  let exp_equal e1 e2 g s = None
  let query _ _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = Queries.Result.top ()

  let reset_diff x = x
  let get_diff   x = []
  let should_join x y = true


  let assign a lval rval glob st = st
  let branch a exp tv glob st = st
  let return a exp fundec glob st = st
  let body   a f glob st = st
  let special a f arglist glob st = 
      match f.vname with
   (* | "sem_wait"*)
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_trylock" | "_spin_trylock_irqsave" | "_spin_lock_bh"
      | "mutex_lock" | "mutex_lock_interruptible"
      | "pthread_mutex_lock" ->
          let x = List.hd arglist in
          let eq = 
            let c_eq = Equ.bot () in 
              match Equ.eval_rv x with
                | Some e  -> Equset.add e c_eq st
                | _ -> st
          in 
          [eq, integer 1, true]
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
      | "mutex_unlock"
      | "pthread_mutex_unlock" ->
          [Equset.empty (), integer 1, true]
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> 
                (*let written = access_funargs c (fnc arglist) in
                  ((st,eq), add_locks written c (st,eq))*)
                  [st, integer 1, true]
            | _ -> [st, integer 1, true]
        end

  let enter_func a lval f args glob st = [(st,st)]
  let leave_func a lval f args glob st1 st2 = st1
  let special_fn a lval f args glob st = [(st,integer 1,true)]
  let fork       a lval f args glob st = []
  
  let eval_funvar a exp glob st = []

end