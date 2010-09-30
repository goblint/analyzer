module LF = LibraryFunctions
module LP = Exp.LockingPattern
module Exp = Exp.Exp
module VarEq = VarEq.Spec

open Cil
open Pretty
open Analyses

(* Note: This is currently more conservative than varEq --- but 
   it should suffice for tests. *)
module Spec =
struct
  include Analyses.DefaultSpec

  exception Top

  module Dom = LockDomain.Symbolic
  module Glob = Global.Make (Lattice.Unit)

  let name = "Symbolic locks"

  let startstate = Dom.top 
  let otherstate = Dom.top 

  let branch ctx exp tv = ctx.local
  let body   ctx f = ctx.local

  let invalidate_exp ask exp st =
    Dom.filter (fun e -> not (VarEq.may_change ask exp e)) st 

  let invalidate_lval ask lv st =
    invalidate_exp ask (mkAddrOf lv) st

  let assign ctx lval rval = invalidate_lval ctx.ask lval ctx.local
    
  let return ctx exp fundec = 
    List.fold_right Dom.remove_var (fundec.sformals@fundec.slocals) ctx.local  
    
  let special_fn ctx lval f arglist = 
      match f.vname with
   (* | "sem_wait"*)
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_trylock" | "_spin_trylock_irqsave" | "_spin_lock_bh"
      | "mutex_lock" | "mutex_lock_interruptible"
      | "pthread_mutex_lock" ->
          [Dom.add ctx.ask (List.hd arglist) ctx.local, integer 1, true]
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
      | "mutex_unlock"
      | "pthread_mutex_unlock" ->
          [Dom.remove ctx.ask (List.hd arglist) ctx.local, integer 1, true]
      | x -> begin
          let st = 
            match lval with
              | Some lv -> invalidate_lval ctx.ask lv ctx.local
              | None -> ctx.local
          in
          let write_args = 
            match LF.get_invalidate_action x with
              | Some fnc -> fnc `Write arglist
              | _ -> arglist
          in
          [List.fold_left (fun st e -> invalidate_exp ctx.ask e st) st write_args, integer 1, true]
        end

  let enter_func ctx lval f args = [(ctx.local,ctx.local)]
  let leave_func ctx lval fexp f args st2 = ctx.local 

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
  let query ctx (q:Queries.t) =
    match q with
      | Queries.PerElementLock e -> 
          `ExpTriples (get_all_locks ctx.ask e ctx.local)
      | Queries.ArrayLockstep e ->
          `ExpTriples (same_unknown_index ctx.ask e ctx.local)
      | _ -> Queries.Result.top ()
end

module SymbLocksMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "symb_locks" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `SymbLocks x
                let extract_l x = match x with `SymbLocks x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)