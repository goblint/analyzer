module LF = LibraryFunctions
module LP = Exp.LockingPattern
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

  let assign ask lval rval glob st =
    let not_in v xs = not (Exp.contains_var v xs) in
    let remove_simple (v,offs) st =      
      Dom.filter (not_in v) st
    in
    match ask (Queries.MayPointTo (Cil.mkAddrOf lval)) with 
      | `LvalSet rv when not (Queries.LS.is_top rv) -> 
          Queries.LS.fold remove_simple rv st 
      | _ -> Dom.top ()
    
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
    let add_perel x xs =
      match LP.from_exps e x with
        | Some x -> Queries.PS.add x xs
        | None -> xs
    in
    Dom.fold add_perel st (Queries.PS.empty ())

  let get_all_locks ask e st : Queries.PS.t =
    let exps = 
      match ask (Queries.EqualSet e) with
        | `ExprSet a when not (Queries.ES.is_bot a) -> a
        | _ -> Queries.ES.singleton e
    in
    let add_locks x xs = Queries.PS.union (get_locks x st) xs in
    Queries.ES.fold add_locks exps (Queries.PS.empty ())

  let query a _ (x:Dom.t) (q:Queries.t) =
    match q with
      | Queries.PerElementLock e -> 
          let triples = get_all_locks a e x in
(*           Messages.report ((sprint 800 (d_exp () e)) ^ " accesed with " ^ (Queries.PS.short 800 triples)); *)
          `PerElemLock triples
      | _ -> Queries.Result.top ()
end