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

module A = Analyses
module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module Lockset = LockDomain.Lockset
module AD = ValueDomain.AD
(*module BS = Base.Spec*)
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty

(** Data race analyzer *)  
module Spec : Analyses.Spec =
struct  

  (** name for the analysis (btw, it's "Mutex Must") *)
  let name = "Mutex Must"

  (** a strange function *)
  let es_to_string f es = f.svar.vname
  
  (** no init. needed -- call [BS.init] *)
  let init = BS.init 

  (** Add current lockset alongside to the base analysis domain. Global data is collected using dirty side-effecting. *)
  module Dom = Lattice.Prod (BS.Dom) (Lockset)
  
  (** We do not add global state, so just lift from [BS]*)
  module Dep = BS.Dep
  
  (** Access counting is done using side-effect (accesses added in [add_accesses] and read in [finalize]) : *)
  
  (* 
    Access counting using side-effects: ('|->' is a hash-map)
    
    acc     : var |-> (loc, mt_flag, rw_falg, lockset, offset) set
    accKeys : var set
    
    Remark:
    As you can see, [accKeys] is just premature optimization, so we dont have to iterate over [acc] to get all keys.
   *)
  module Acc = Hashtbl.Make (Basetype.Variables)
  module AccKeySet = Set.Make (Basetype.Variables)
  module AccValSet = Set.Make (Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (IntDomain.Booleans)) (Lockset) (Offs))
  let acc     : AccValSet.t Acc.t = Acc.create 100
  let accKeys : AccKeySet.t ref   = ref AccKeySet.empty 
  
  (** Function [add_accesses accs st] fills the hash-map [acc] *)
  let add_accesses (accessed: (varinfo * Offs.t * bool) list) (bst,ust:Dom.t) : unit = 
    let fl = BS.get_fl bst in
    if BS.Flag.is_multi fl then
      let loc = !GU.current_loc in
      let try_add_one (v, o, rv: Cil.varinfo * Offs.t * bool) =
        if (v.vglob) then
          let curr : AccValSet.t = try Acc.find acc v with Not_found -> AccValSet.empty in
          let neww : AccValSet.t = AccValSet.add ((loc,fl,rv),ust,o) curr in
          Acc.replace acc v neww;
          accKeys := AccKeySet.add v !accKeys
      in 
        List.iter try_add_one accessed


  (** First we consider reasonable joining states if locksets are equal, also we don't expect precision if base state is equal*)
  let should_join (x,a) (y,b) = Lockset.equal a b || BS.Dom.equal x y
  
  (** We just lift start state, global and dependecy functions: *)
  
  let startstate = BS.startstate, Lockset.empty ()
  let otherstate = BS.otherstate, Lockset.empty ()
  
  let filter_globals      (b,d)         = (BS.filter_globals b, Lockset.empty ())
  let insert_globals      (b1,d) (b2,_) = (BS.insert_globals b1 b2, d)
  let get_changed_globals (b1,_) (b2,_) = BS.get_changed_globals b1 b2 
  let get_global_dep      (vs,is)       = BS.get_global_dep vs
  let reset_global_dep    (vs,is)       = BS.reset_global_dep vs, is
  
  (** Transfer functions: *)
  
  let assign lval rval (bst,ust: Dom.t) : Dom.t = 
    let accessed = BS.access_one_byval true bst (Lval lval) @ BS.access_one_byval false bst rval in
    add_accesses accessed (bst,ust) ;
    (BS.assign lval rval bst, ust)
    
  let branch exp tv (bst,ust: Dom.t) : Dom.t =
    let accessed = BS.access_one_byval false bst exp in
    add_accesses accessed (bst,ust);
    (BS.branch exp tv bst, ust)
    
  let return exp fundec (bst,ust: Dom.t) : Dom.t =
    begin match exp with 
      | Some exp -> 
          let accessed = BS.access_one_byval false bst exp in
          add_accesses accessed (bst,ust)
      | None -> () end;
      (BS.return exp fundec bst, ust)
        
  let body f (bst,ust: Dom.t) : Dom.t = 
    (BS.body f bst, ust)

  let eval_funvar exp (bst,bl) = 
    let read = BS.access_one_byval false bst exp in
    add_accesses read (bst,bl); 
    BS.eval_funvar exp bst
  
  let special_fn lv f arglist (bst,ls: Dom.t) : Dom.t =
    let eval_exp_addr context exp =
      let v = BS.eval_rv context exp in
        match v with
          | `Address v when not (AD.is_top v) -> AD.fold (fun a b -> a :: b) v []    
          | _                                 -> []
    in
    match f.vname with
   (* | "sem_wait"*)
      | "pthread_mutex_lock" -> BS.special_fn lv f arglist bst, begin
          match arglist with
            | [x] -> begin match  (eval_exp_addr bst x) with 
                             | [e]  -> Lockset.add e ls
                             | _ -> ls
                     end
            | _ -> ls
        end
   (* | "sem_post"*)
      | "pthread_mutex_unlock" -> BS.special_fn lv f arglist bst, begin
          match arglist with
            | [x] -> begin match  (eval_exp_addr bst x) with 
                             | [] -> Lockset.empty ()
                             | e  -> List.fold_right (Lockset.remove) e ls
                     end
            | _ -> ls
        end
      | x -> 
          let read       = BS.access_byval false bst arglist in
          let accessable = BS.access_byref       bst arglist in
          add_accesses (read @ accessable) (bst,ls);
          BS.special_fn lv f arglist bst, ls
          
  let enter_func lv f args (bst,lst) =
    List.map (fun st -> (st,lst)) (BS.enter_func lv f args bst) 

  let leave_func lv f args (bst,bl) (ast,al) = 
    let read = BS.access_byval false bst args in
    add_accesses read (bst,bl); 
    let rslt = BS.leave_func lv f args bst ast in
    (rslt, al)
    
  let fork lv f args (bst,ls) = 
    List.map (fun (f,t) -> (f,(t,ls))) (BS.fork lv f args bst)
  
  
  (** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true

  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsMap = Map.Make (Offs)
  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsSet = Set.Make (Offs)

  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc (gl : Cil.varinfo) =
    (* create mapping from offset to access list; set of offsets  *)
    let create_map (accesses_map: AccValSet.t) =
      let f (((_, _, rw), _, offs) as accs) (map,set) =
        if OffsMap.mem offs map
        then (OffsMap.add offs ([accs] @ (OffsMap.find offs map)) map,
              OffsSet.add offs set)
        else (OffsMap.add offs [accs] map,
              OffsSet.add offs set)
      in
      AccValSet.fold f accesses_map (OffsMap.empty, OffsSet.empty)
    in 
    (* join map elements, that we cannot be sure are logically separate *)
    let regroup_map (map,set) =
      let f offs (group_offs, access_list, new_map) = 
        let new_offs = Offs.definite offs in
        let new_gr_offs = Offs.join new_offs group_offs in
        (* we assume f is called in the right order: we get the greatest offset first (leq'wise) *)
        if (Offs.leq new_offs group_offs || (Offs.is_bot group_offs)) 
        then (new_gr_offs, OffsMap.find offs map @ access_list, new_map) 
        else (   new_offs, OffsMap.find offs map, OffsMap.add group_offs access_list new_map) 
      in
      let (last_offs,last_set, map) = OffsSet.fold f set (Offs.bot (), [], OffsMap.empty) in
        if Offs.is_bot last_offs
        then map
        else OffsMap.add last_offs last_set map
    in
    let is_race acc_list =
      let f locks (_, lock, _) = Lockset.join locks lock in
      let locks = List.fold_left f (Lockset.bot ()) acc_list in
      let rw ((_,_,x),_,_) = x in
      let non_main ((_,x,_),_,_) = BS.Flag.is_bad x in      
             (Lockset.is_empty locks || Lockset.is_top locks) 
          && ((List.length acc_list) > 1)
          && (List.exists rw acc_list) 
          && (List.exists non_main acc_list)
    in
    let report_race offset acc_list =
      if is_race acc_list
        then begin
        race_free := false;
        let warn = "Datarace over variable \"" ^ gl.vname ^ Offs.short 80 offset ^ "\"" in
        let f  ((loc, fl, write), lockset,o) = 
          let lockstr = Lockset.short 80 lockset in
          let action = if write then "write" else "read" in
          let thread = if BS.Flag.is_bad fl then "some thread" else "main thread" in
          let warn = (*gl.vname ^ Offs.short 80 o ^ " " ^*) action ^ " in " ^ thread ^ " with lockset: " ^ lockstr in
            (warn,loc) in 
        let warnings =  List.map f acc_list in
          M.print_group warn warnings
      end
    in 
    let acc_info = create_map (Acc.find acc gl) in
    let acc_map  = regroup_map acc_info in
      OffsMap.iter report_race acc_map
    
  (** postprocess and print races and other output *)
  let finalize () = 
    AccKeySet.iter postprocess_acc !accKeys;
    if !GU.multi_threaded then begin
      match !race_free, !M.soundness with
        | true, true -> print_endline "CONGRATULATIONS!\nYour program has just been certified Free of Data Races!"
        | true, false -> 
            print_endline "Goblint did not find any Data Races in this program!";
            print_endline "However, the code was too complicated for Goblint to understand all of it."
        | _ -> ()
    end else if not !GU.debug then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    BS.finalize ()

end

(*module Trivial = Spec*)
(*module Context = Compose.ContextSensitive (BS) (Spec)*)
module Path = Compose.PathSensitive (Spec)

module Analysis = Multithread.Forward(Path)
module SimpleAnalysis = Multithread.Forward(Spec)

