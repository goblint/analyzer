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
        if cardinal joinable == 0 then
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
      
   end
  
  (** use same global variables as base analysis*)
  module Dep = Base.Dep 
  
  
  type trans_in = Dom.t
  type trans_out = Dom.t
  type transfer = Dom.t -> Dom.t
    
  let name       = "Path sensitive " ^ Base.name
  let startstate = Dom.singleton Base.startstate
  let otherstate = Dom.singleton Base.otherstate
  let init     = Base.init
  let finalize = Base.finalize
  let es_to_string f es  = Base.es_to_string f (Dom.choose es)
  let should_join _ _ = true
  
  (** [lift f set] is basically a map, that handles dead-code*)
  let lift f set = 
    let apply_add st = 
      try Dom.add (f st) 
      with _ -> fun x -> x (*exception means dead-code so we filter these out*) 
    in   
    let rslt = Dom.fold apply_add set (Dom.bot ()) in
    if Dom.is_bot rslt 
    then raise Analyses.Deadcode
    else rslt
    
  let assign lval exp          = lift (Base.assign lval exp)
  let branch exp br            = lift (Base.branch exp br)
  let body f                   = lift (Base.body f)
  let return exp f             = lift (Base.return exp f)
  let special_fn lval f args   = lift (Base.special_fn lval f args)
  
  let eval_funvar exp st  = Dom.fold (fun x xs -> Base.eval_funvar exp x @ xs)  st []
  
  let fork lval fn args st = 
    let add_spawn st ss =  
      List.map (fun (x,y) -> x, Dom.singleton y) (Base.fork lval fn args st) @ ss
    in
    Dom.fold add_spawn st []
  
  let enter_func lval fn args st : Dom.t list =
    let add_work wrk_list st = List.map Dom.singleton (Base.enter_func lval fn args st) @ wrk_list in
    List.fold_left add_work [] (Dom.elements st) 

  let leave_func lval fn args before after : Dom.t =
    (* we join as a general case -- but it should have been a singleton anyway *)
    let bbf : Base.Dom.t = Dom.fold Base.Dom.join before (Base.Dom.bot ()) in
    let leave_and_join nst result = Dom.join result (Dom.singleton (Base.leave_func lval fn args bbf nst)) in
    Dom.fold leave_and_join after (Dom.bot ())  
  
  let get_global_dep   st = Dom.fold (fun x xs -> Base.get_global_dep x @ xs) st []
  let reset_global_dep st = Dom.map Base.reset_global_dep st
  
  let filter_globals   st =
    let join_f_globs x xs = Base.Dom.join xs (Base.filter_globals x) in
    Dom.singleton (Dom.fold join_f_globs st (Base.Dom.bot ()))
  
  let insert_globals st gs =
    let el = Dom.fold Base.Dom.join gs (Base.Dom.bot ()) in
    Dom.map (fun x -> Base.insert_globals x el) st
    
  let get_changed_globals st1 st2 =
    let add_cg st ss =
      Dom.fold (fun x xs -> Base.get_changed_globals st x @ xs) st2 ss 
    in
    Dom.fold add_cg st1 []
end

                                  