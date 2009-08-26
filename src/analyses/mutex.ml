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
module ID = ValueDomain.ID
(*module BS = Base.Spec*)
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty

(** only report write races *)
let no_read = ref false
(** Truns off field-sensitivity. *)
let field_insensitive = ref false
(** Avoids the merging of fields, not really sound *)
let unmerged_fields = ref false
(** Take the possible failing of standard locking operations into account. *)
let failing_locks = ref false

(* Some helper functions ... *)
let is_atomic_type (t: typ): bool = match t with
  | TNamed (info, attr) -> info.tname = "atomic_t"
  | _ -> false

let is_atomic lval = 
  let (lval, _) = removeOffsetLval lval in
  let typ = typeOfLval lval in
    is_atomic_type typ

let is_ignorable lval = 
  Base.is_mutex_type (typeOfLval lval) || is_atomic lval

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
  module Glob = BS.Glob
  
  let get_diff (x,_) = BS.get_diff x
  let reset_diff (x,y) = (BS.reset_diff x, y)

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
  
  
  (** Transfer functions: *)
  
  let assign lval rval gs (bst,ust: Dom.t) : Dom.t = 
    let accessed = BS.access_one_byval true gs bst (Lval lval) @ BS.access_one_byval false gs bst rval in
    add_accesses accessed (bst,ust) ;
    (BS.assign lval rval gs bst, ust)
    
  let branch exp tv gs (bst,ust: Dom.t) : Dom.t =
    let accessed = BS.access_one_byval false gs bst exp in
    add_accesses accessed (bst,ust);
    (BS.branch exp tv gs bst, ust)
    
  let return exp fundec gs (bst,ust: Dom.t) : Dom.t =
    begin match exp with 
      | Some exp -> 
          let accessed = BS.access_one_byval false gs bst exp in
          add_accesses accessed (bst,ust)
      | None -> () end;
      (BS.return exp fundec gs bst, ust)
        
  let body f gs (bst,ust: Dom.t) : Dom.t = 
    (BS.body f gs bst, ust)

  let eval_funvar exp gs (bst,bl) = 
    let read = BS.access_one_byval false gs bst exp in
    add_accesses read (bst,bl); 
    BS.eval_funvar exp gs bst
  
  let special_fn lv f arglist gs (bst,ls: Dom.t) : Dom.t list =
    let eval_exp_addr context exp =
      let v = BS.eval_rv gs context exp in
        match v with
          | `Address v when not (AD.is_top v) -> AD.fold (fun a b -> a :: b) v []    
          | _                                 -> []
    in
    let map_bs x = List.map (fun y -> y, x) (BS.special_fn lv f arglist gs bst) in
    let lock rw =
        let lock_one (e:LockDomain.Addr.t) =
          let set_ret v sts = 
            match lv with 
              | None -> sts
              | Some lv ->
                let lv_addr = BS.eval_lv gs bst lv in
                List.map (fun (b,u) -> BS.set gs b lv_addr v, u) sts 
          in 
          set_ret (`Int (ID.of_int 0L)) (map_bs (Lockset.add (e,rw) ls)) @
          if !failing_locks then set_ret (`Int (ID.of_excl_list [0L])) (map_bs ls) else []
        in
        let unknown () = 
          match lv with 
            | None -> map_bs ls
            | Some lv ->  
                let lv_addr = BS.eval_lv gs bst lv in
                List.map (fun (b,u) -> BS.set gs b lv_addr (`Int (ID.top ())), u) (map_bs ls)  
        in
          match arglist with
            | [x] -> begin match  (eval_exp_addr bst x) with 
                             | [e]  -> lock_one e
                             | _ -> unknown ()
                     end
            | _ -> unknown ()
    in
    let remove_rw x st = Lockset.remove (x,true) (Lockset.remove (x,false) st) in
    let unlock remove_fn =
      match arglist with
        | [x] -> begin match  (eval_exp_addr bst x) with 
                        | [] -> map_bs (Lockset.empty ())
                        | es -> map_bs (List.fold_right remove_fn es ls)
                end
        | _ -> map_bs ls
    in
    match f.vname with
   (* | "sem_wait"*)
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_trylock" | "_spin_trylock_irqsave" | "_spin_lock_bh"
      | "mutex_lock" | "mutex_lock_interruptible" | "pthread_mutex_trylock" | "_write_lock"
      | "pthread_mutex_lock" | "pthread_rwlock_wrlock" | "pthread_rwlock_trywrlock"
          -> lock true
      | "pthread_rwlock_tryrdlock" | "pthread_rwlock_rdlock" | "_read_lock" 
          -> lock false
      | "__raw_read_unlock" | "__raw_write_unlock" -> 
          let drop_raw_lock x =
            let rec drop_offs o = 
              match o with
                | `Field ({fname="raw_lock"},`NoOffset) -> `NoOffset
                | `Field (f1,o1) -> `Field (f1, drop_offs o1)
                | `Index (i1,o1) -> `Index (i1, drop_offs o1)
                | `NoOffset -> `NoOffset
            in
            match Addr.to_var_offset x with
              | [(v,o)] -> Addr.from_var_offset (v, drop_offs o)
              | _ -> x
          in
          unlock (fun l -> remove_rw (drop_raw_lock l))
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
      | "mutex_unlock"  
      | "pthread_mutex_unlock" 
          -> unlock remove_rw
      | x -> 
          let arg_acc act = 
            match LF.get_invalidate_action x with
              | Some fnc -> (fnc act arglist) 
              | _ -> []
          in
          let read       = BS.access_byval false gs bst (arg_acc `Read) in
          let accessable = BS.access_byref       gs bst (arg_acc `Write) in
          add_accesses (read @ accessable) (bst,ls);
          map_bs ls
          
  let enter_func lv f args gs (bst,lst) : (Dom.t * Dom.t) list =
    List.map (fun (bf,st) -> (bf,lst),(st,lst)) (BS.enter_func lv f args gs bst) 

  let leave_func lv f args gs (bst,bl) (ast,al) = 
    let read = BS.access_byval false gs bst args in
    add_accesses read (bst,bl); 
    let rslt = BS.leave_func lv f args gs bst ast in
    (rslt, al)
    
  let fork lv f args gs (bst,ls) = 
    List.map (fun (f,t) -> (f,(t,ls))) (BS.fork lv f args gs bst)
  
  
  (** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true

  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsMap = Map.Make (Offs)
  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsSet = Set.Make (Offs)

  type access_status = 
    | Race
    | Guarded of Lockset.t
    | ReadOnly
    | ThreadLocal

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
    let get_common_locks acc_list = 
      let f locks ((_,_,writing), lock, _) = 
        let lock = 
          if writing then
            (* when writing: ignore reader locks *)
            Lockset.filter snd lock 
          else 
            (* when reading: bump reader locks to exclusive as they protect reads *)
            Lockset.map (fun (x,_) -> (x,true)) lock 
        in
          Lockset.join locks lock 
      in
			List.fold_left f (Lockset.bot ()) acc_list
    in
    let is_race acc_list =
      let locks = get_common_locks acc_list in
      let rw ((_,_,x),_,_) = x in
      let non_main ((_,x,_),_,_) = BS.Flag.is_bad x in      
        if not (Lockset.is_empty locks || Lockset.is_top locks) then
          Guarded locks
        else if not (List.exists rw acc_list) then
          ReadOnly
        else if not (List.exists non_main acc_list) then
          ThreadLocal
        else
          Race
    in
    let report_race offset acc_list =
        let f  ((loc, fl, write), lockset,o) = 
          let lockstr = Lockset.short 80 lockset in
          let action = if write then "write" else "read" in
          let thread = if BS.Flag.is_bad fl then "some thread" else "main thread" in
          let warn = (*gl.vname ^ Offs.short 80 o ^ " " ^*) action ^ " in " ^ thread ^ " with lockset: " ^ lockstr in
            (warn,loc) in 
        let warnings =  List.map f acc_list in
            let var_str = gl.vname ^ Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
          match is_race acc_list with
            | Race -> begin
                race_free := false;
                let warn = "Datarace over " ^ var_str in
                  M.print_group warn warnings
              end
            | Guarded locks ->
                let lock_str = Lockset.short 80 locks in
                  if !GU.allglobs then
                    M.print_group (safe_str "common mutex") warnings
                  else 
                    ignore (printf "Found correlation: %s is guarded by lockset %s\n" var_str lock_str)
            | ReadOnly ->
                if !GU.allglobs then
                  M.print_group (safe_str "only read") warnings
            | ThreadLocal ->
                if !GU.allglobs then
                  M.print_group (safe_str "thread local") warnings
    in 
    let rw ((_,_,x),_,_) = x in
    let acc = (Acc.find acc gl) in
    let acc = if !no_read then AccValSet.filter rw acc else acc in
    let acc_info = create_map acc in
    let acc_map = if !unmerged_fields then fst acc_info else regroup_map acc_info in
      OffsMap.iter report_race acc_map
    
  (** postprocess and print races and other output *)
  let finalize () = 
    AccKeySet.iter postprocess_acc !accKeys;
    if !GU.multi_threaded then begin
      match !race_free, !M.soundness with
        | true, true -> 
            print_endline "CONGRATULATIONS!\nYour program has just been certified Free of Data Races!";
            if not (!failing_locks) then print_endline  "(Assuming locking operations always succeed.)"
        | true, false -> 
            print_endline "Goblint did not find any Data Races in this program!";
            print_endline "However, the code was too complicated for Goblint to understand all of it."
        | false, true -> 
            print_endline "And that's all. Goblint is certain there are no other races.";
            if not (!failing_locks) then print_endline  "(Assuming locking operations always succeed.)"
        | _ -> 
            print_endline "And there may be more races ...";
            print_endline "The code was too complicated for Goblint to understand all of it."
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

