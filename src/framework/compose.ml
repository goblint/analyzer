(** Path-sensitivity mechanism; OLD. *)
open Analyses
open Pretty
open Cil

module GU = Goblintutil

(** Add path sensitivity to a analysis *)
module PathSensitive (Base: Analyses.Spec with module Glob.Var = Basetype.Variables) 
  : Analyses.Spec 
  with type Dom.t = SetDomain.Make(Base.Dom).t
   and module Glob = Base.Glob
  =
struct    
  (** the domain is a overloaded set with special join, meet & leq*)
  module Dom = 
  struct
    include SetDomain.Make (Base.Dom)
    let name () = "PathSensitive (" ^ name () ^ ")"
    
    (** [leq a b] iff each element in [a] has a [leq] counterpart in [b]*)
    let leq s1 s2 = 
      let p t = exists (fun s -> Base.Dom.leq t s) s2 in
      for_all p s1

    let pretty_diff () ((s1:t),(s2:t)): Pretty.doc = 
      if leq s1 s2 then dprintf "%s: These are fine!" (name ()) else begin
        let p t = not (exists (fun s -> Base.Dom.leq t s) s2) in
        let evil = choose (filter p s1) in
        let other = choose s2 in
          (* dprintf "%s has a problem with %a not leq %a because %a" (name ()) 
            Base.Dom.pretty evil Base.Dom.pretty other 
            Base.Dom.pretty_diff (evil,other) *)
          Base.Dom.pretty_diff () (evil,other)

      end
    
    (** For [join x y] we take a union of [x] & [y] and join elements 
     * which base analysis suggests us to.*)
    let join s1 s2 =
      let rec loop s1 s2 = 
        let f b (ok, todo) =
          let joinable, rest = partition (Base.should_join b) ok in
          if cardinal joinable = 0 then
            (add b ok, todo)
          else
            let joint = fold (Base.Dom.join) joinable b in
            (fold remove joinable ok, add joint todo)
        in
        let (ok, todo) = fold f s2 (s1, empty ()) in
          if is_empty todo then 
            ok
          else
            loop ok todo
      in
      loop s1 s2    
  
    (** carefully add element (because we might have to join something)*)
    let add e s = join s (singleton e)
  
    (** We dont have good info for this operation -- only thing is to [meet] all elements.*)
    let meet s1 s2 = 
      (* Try to not use top, as it is often not implemented. *)
      let fold1 f s =
        let g x = function
          | None -> Some x
          | Some y -> Some (f x y)
        in
        match fold g s None with  
          | Some x -> x
          | None -> Base.Dom.top()
      in
      singleton (fold1 Base.Dom.meet (union s1 s2))
    
    (** Widening operator. We take all possible (growing) paths, do elementwise 
        widenging and join them together. When the used path sensitivity is 
        not overly dynamic then no joining occurs.*)
    let widen s1 s2 = 
      let f e =
        let l = filter (fun x -> Base.Dom.leq x e) s1 in
        let m = map (fun x -> Base.Dom.widen x e) l in
        fold (Base.Dom.join) m e
      in
      map f s2

    (** Narrowing operator. As with [widen] some precision loss might occur.*)
    let narrow s1 s2 = 
      let f e =
        let l = filter (fun x -> Base.Dom.leq x e) s2 in
        let m = map (Base.Dom.narrow e) l in
        fold (Base.Dom.join) m (Base.Dom.bot ())
      in
      map f s1
   end
  
  (** use same global variables as base analysis*)
  module Glob = Base.Glob
  
  type trans_in = Dom.t
  type trans_out = Dom.t
  type transfer = Dom.t -> Dom.t
    
  let name       = "Path sensitive " ^ Base.name
  let startstate v = Dom.singleton (Base.startstate v)
  let otherstate v = Dom.singleton (Base.otherstate v)
  let exitstate  v = Dom.singleton (Base.exitstate  v)
  let init     = Base.init
  let finalize = Base.finalize
  let es_to_string f es  = Base.es_to_string f (Dom.choose es)
  let should_join _ _ = true
  let context_top f x = Dom.map (Base.context_top f) x
  
  let spawner f v d = f v (Dom.singleton d) 

  let query ctx y = 
    let f e b = Queries.Result.meet b (Base.query (Analyses.set_st ctx e spawner) y) in 
    Dom.fold f ctx.local (Queries.Result.top ())
  
  let may_race (ctx1,ac1) (ctx2,ac2) =
    let f x y = 
      Base.may_race (set_st ctx1 x spawner,ac1) (set_st ctx2 y spawner,ac2)
    in
    Dom.exists (fun x -> Dom.exists (f x) ctx2.local) ctx1.local
  
  (** [lift f set] is basically a map, that handles dead-code*)
  let lift f set = 
    let apply_add st = 
      try Dom.add (f st) 
      with Analyses.Deadcode -> fun x -> x (*exception means dead-code so we filter these out*) 
    in   
    let rslt = Dom.fold apply_add set (Dom.bot ()) in
    if Dom.is_bot rslt 
    then raise Analyses.Deadcode
    else rslt
  
  let morphstate v = lift (Base.morphstate v)
  
  let sync ctx = 
    let f l (ls,gs) = 
      let (l',gs') = Base.sync (set_st ctx l spawner) in 
      (Dom.add l' ls, gs' @ gs)
    in
    Dom.fold f ctx.local (Dom.bot (), [])

  let assign ctx lval exp  = lift (fun st -> Base.assign (set_st ctx st spawner) lval exp) ctx.local
  let branch ctx exp br    = lift (fun st -> Base.branch (set_st ctx st spawner) exp br) ctx.local
  let body ctx f           = lift (fun st -> Base.body (set_st ctx st spawner) f) ctx.local
  let return ctx exp f     = lift (fun st -> Base.return (set_st ctx st spawner) exp f) ctx.local
  let intrpt ctx           = lift (fun st -> Base.intrpt (set_st ctx st spawner)) ctx.local

  let special_fn ctx lval f args = 
    let just_d_set (s,_,_) = Dom.singleton s in
    let one_special st xs =
      List.map just_d_set (Base.special_fn (set_st ctx st spawner) lval f args)  @ xs
    in
    let true_exp = (Cil.integer 1) in
    List.map (fun x -> x, true_exp, true) (Dom.fold one_special ctx.local []) 
    
(*  let fork ctx lval fn args = 
    let add_spawn st ss =  
      List.map (fun (x,y) -> x, Dom.singleton y) (Base.fork (set_st ctx st spawner) lval fn args) @ ss
    in
    Dom.fold add_spawn ctx.local []
  *)
  let enter_func ctx lval fn args : (Dom.t * Dom.t) list =
    let sing_pair (x,y) =  Dom.singleton x, Dom.singleton y in
    let add_work wrk_list st = List.map sing_pair (Base.enter_func (set_st ctx st spawner) lval fn args) @ wrk_list in
    List.fold_left add_work [] (Dom.elements ctx.local) 

  let leave_func ctx lval fexp fn args after : Dom.t =
    (* we join as a general case -- but it should have been a singleton anyway *)
    let bbf : Base.Dom.t = Dom.fold (Base.Dom.join) ctx.local (Base.Dom.bot ()) in
    let leave_and_join nst result = Dom.join result (Dom.singleton (Base.leave_func (set_st ctx bbf spawner) lval fexp fn args nst)) in
    Dom.fold leave_and_join after (Dom.bot ())    
end                                  
