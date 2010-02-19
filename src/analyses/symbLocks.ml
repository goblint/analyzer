module LF = LibraryFunctions
module LP = Exp.LockingPattern
module Exp = Exp.Exp

open Cil
open Pretty

(* Note: This is currently more conservative than varEq --- but 
   it should suffice for tests. *)
module Spec =
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

  let not_in v xs = not (Exp.contains_var v xs) 
  let remove_simple (v,offs) st = Dom.filter (not_in v) st

  let invalidate_lval ask lv st =
    match ask (Queries.MayPointTo (Cil.mkAddrOf lv)) with 
      | `LvalSet rv when not (Queries.LS.is_top rv) -> 
          Queries.LS.fold remove_simple rv st 
      | _ -> Dom.kill_lval lv st

  let invalidate_exp ask exp st =
    match ask (Queries.MayPointTo exp) with 
      | `LvalSet rv when not (Queries.LS.is_top rv) -> 
          Queries.LS.fold remove_simple rv st 
      | _ -> Dom.top ()

  let assign ask lval rval glob st = invalidate_lval ask lval st
    
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
            | Some fnc -> [List.fold_left (fun st e -> invalidate_exp ask e st) st (fnc `Write arglist), integer 1, true]
            | _ -> [Dom.top (), integer 1, true]
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
(*        | `ExprSet a when not (Queries.ES.is_bot a) -> Queries.ES.add e a *)
        | _ -> Queries.ES.singleton e
    in
    let add_locks x xs = Queries.PS.union (get_locks x st) xs in
    Queries.ES.fold add_locks exps (Queries.PS.empty ())

  let same_unknown_index ask exp slocks =
    let uk_index_equal i1 i2 =
      match Cil.constFold true i1, Cil.constFold true i2 with
        | (Const _), _ 
        | _,(Const _) -> false
        | _ ->
      match ask (Queries.ExpEq (i1, i2)) with
        | `Int i when i <> 0L -> true
        | _ -> false
    in
    let lock_index ei ee x xs =
      match Exp.one_unknown_array_index x with
        | Some (true, i, e) when uk_index_equal ei i ->
            Queries.PS.add (zero, ee, e) xs
        | _ -> xs
    in
    match Exp.one_unknown_array_index exp with
      | Some (false, i, e) -> Dom.fold (lock_index i e) slocks (Queries.PS.empty ())
      | _ -> Queries.PS.empty ()
      
      
  (* Per-element returns a triple of exps, first are the "element" pointers, 
     in the second and third positions are the respectively access and mutex.
     Access and mutex expressions have exactly the given "elements" as "prefixes". 
     
     To know if a access-mutex pair matches our per-element pattern we listify 
     the offset (adding dereferencing to our special offset type). Then we take 
     the longest common prefix till a dereference and check if the rest is "concrete".  
    
     ----     
     Array lockstep also returns a triple of exps. Second and third elements in 
     triples are access and mutex exps. Common index is replaced with *.
     First element is unused.
     
     To find if this pattern matches, we try to separate the base variable and 
     the index from both -- access exp and mutex exp. We check if indexes match
     and the rest is concrete. Then replace the common index with *.
    *)
  let query a _ (x:Dom.t) (q:Queries.t) =
    match q with
      | Queries.PerElementLock e -> 
          `ExpTriples (get_all_locks a e x)
      | Queries.ArrayLockstep e ->
          `ExpTriples (same_unknown_index a e x)
      | _ -> Queries.Result.top ()
end

module SymbLocksMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "symb_locks" 
                type lf = Spec.Dom.t
                let inject_l x = `SymbLocks x
                let extract_l x = match x with `SymbLocks x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)