(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


open Pretty
open Cil

(** Add path sensitivity to a analysis *)
module PathSensitive (Base: Analyses.Spec) 
  : Analyses.Spec =
struct
  (** the domain is a overloaded set with special join, meet & leq*)
  module Dom = 
  struct
    include SetDomain.Make (Base.Dom)
    
    (** [leq a b] iff each element in [a] has a [leq] counterpart in [b]*)
    let leq s1 s2 = 
      let p t = exists (fun s -> Base.Dom.leq t s) s2 in
      for_all p s1
    
    (** For [join x y] we take a union of [x] & [y] and join elements 
     * which base analysis suggests us to.*)
    let rec join s1 s2 = 
      let f b (ok, todo) =
        let joinable, rest = partition (Base.should_join b) ok in
        if cardinal joinable = 0 then
          (add b ok, todo)
        else
          let joint = fold Base.Dom.join joinable b in
          (fold remove joinable ok, add joint todo)
      in
      let (ok, todo) = fold f s2 (s1, empty ()) in
        if is_empty todo then 
          ok
        else
          join ok todo
  
    (** carefully add element (because we might have to join something)*)
    let add e s = join s (singleton e)
  
    (** We dont have good info for this operation -- only thing is to [meet] all elements.*)
    let meet s1 s2 = 
      singleton (fold Base.Dom.meet (union s1 s2) (Base.Dom.top()))
    
    (** Widening operator. We take all possible (growing) paths, do elementwise 
        widenging and join them together. When the used path sensitivity is 
        not overly dynamic then no joining occurs.*)
    let widen s1 s2 = 
      let f e =
        let l = filter (fun x -> Base.Dom.leq x e) s1 in
        let m = map (fun x -> Base.Dom.widen x e) l in
        fold Base.Dom.join m e
      in
      map f s2

    (** Narrowing operator. As with [widen] some precision loss might occur.*)
    let narrow s1 s2 = 
      let f e =
        let l = filter (fun x -> Base.Dom.leq x e) s2 in
        let m = map (Base.Dom.narrow e) l in
        fold Base.Dom.join m (Base.Dom.bot ())
      in
      map f s1
   end
  
  (** use same global variables as base analysis*)
  module Glob = Base.Glob
  
  type trans_in = Dom.t
  type trans_out = Dom.t
  type transfer = Dom.t -> Dom.t
    
  let name       = "Path sensitive " ^ Base.name
  let startstate () = Dom.singleton (Base.startstate ())
  let otherstate () = Dom.singleton (Base.otherstate ())
  let init     = Base.init
  let finalize = Base.finalize
  let es_to_string f es  = Base.es_to_string f (Dom.choose es)
  let should_join _ _ = true
  
  let query a g s y = 
    let f e b = Queries.Result.meet b (Base.query a g e y) in 
    Dom.fold f s (Queries.Result.bot ())
  
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
  
  let reset_diff x = Dom.map Base.reset_diff x
  let get_diff x = Dom.fold (fun x y -> Base.get_diff x @ y) x []

  let assign a lval exp gs       = lift (Base.assign a lval exp gs)
  let branch a exp br gs         = lift (Base.branch a exp br gs)
  let body a f gs                = lift (Base.body a f gs)
  let return a exp f gs          = lift (Base.return a exp f gs)

  let special_fn a lval f args gs st = 
    let just_d_set (s,_,_) = Dom.singleton s in
    let one_special st xs =
      List.map just_d_set (Base.special_fn a lval f args gs st)  @ xs
    in
    let true_exp = (Cil.integer 1) in
    List.map (fun x -> x, true_exp, true) (Dom.fold one_special st []) 
  
  let eval_funvar a exp gs st  = Dom.fold (fun x xs -> (Base.eval_funvar a exp gs x) @ xs)  st []
  
  let fork a lval fn args gs st = 
    let add_spawn st ss =  
      List.map (fun (x,y) -> x, Dom.singleton y) (Base.fork a lval fn args gs st) @ ss
    in
    Dom.fold add_spawn st []
  
  let enter_func a lval fn args gs st : (Dom.t * Dom.t) list =
    let sing_pair (x,y) =  Dom.singleton x, Dom.singleton y in
    let add_work wrk_list st = List.map sing_pair (Base.enter_func a lval fn args gs st) @ wrk_list in
    List.fold_left add_work [] (Dom.elements st) 

  let leave_func a lval fn args gs before after : Dom.t =
    (* we join as a general case -- but it should have been a singleton anyway *)
    let bbf : Base.Dom.t = Dom.fold Base.Dom.join before (Base.Dom.bot ()) in
    let leave_and_join nst result = Dom.join result (Dom.singleton (Base.leave_func a lval fn args gs bbf nst)) in
    Dom.fold leave_and_join after (Dom.bot ())    
end

                                  