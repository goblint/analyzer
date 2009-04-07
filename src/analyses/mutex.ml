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
module H = Hashtbl
module GU = Goblintutil
module CF = Cilfacade
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module Fields = Lval.Fields
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


module Spec =
struct
  exception Top
  let name = "Mutex Must"
  type context = BS.store
  module LD = Lockset

  module AccessType = IntDomain.MakeBooleans (struct 
                                               let truename = "Write" 
                                               let falsename = "Read" end)
  module SLD = Printable.Prod (Lockset) (Offs)                                             
  module Access = Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (SLD)
  module Accesses = SetDomain.SensitiveConf (struct
                                               let expand_fst = false
                                               let expand_snd = true
                                             end) (AccessType) (Access)
  module GLock = Lattice.Prod (Lockset) (Accesses)
  module GD = Global.Make (GLock)

  type domain = LD.t
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  type calls = (varinfo * LD.t) list -> LD.t
  type spawn = (varinfo * LD.t) list -> unit
  type transfer = LD.t * context * glob_fun -> LD.t * glob_diff
  type trans_in = LD.t * context * glob_fun
  type callback = calls * spawn 
  type access_list = (Cil.varinfo * Offs.t * bool) list

  let startstate = LD.top ()
  let otherstate = LD.top ()

  let access_address ((_,fl),_) write addrs =
    if BS.Flag.is_multi fl then begin
      let f (v,o) acc = 
        let o = if !field_insensitive then `NoOffset else o in
        if v.vglob then (v, Offs.from_offset o, write) :: acc else acc 
      in 
      let addr_list = try AD.to_var_offset addrs with _ -> M.warn "Access to unknown address could be global"; [] in
        List.fold_right f addr_list [] 
    end else []

  let rec access rw (st: context) (exp:exp): access_list = 
    match exp with
      (* Integer literals *)
      | Const _ -> []
      (* Variables and address expressions *)
      | Lval lval -> 
          if is_ignorable lval then [] else
          let target = access_address st rw (BS.eval_lv st lval) in
          let derefs = access_lv st lval in
            target @ derefs
      (* Binary operators *)
      | BinOp (op,arg1,arg2,typ) -> 
          let a1 = access rw st arg1 in
          let a2 = access rw st arg2 in
            a1 @ a2
      (* Unary operators *)
      | UnOp (op,arg1,typ) -> access rw st arg1
      (* The address operators, we just check the accesses under them *)
      | AddrOf lval -> access_lv st lval
      | StartOf lval -> access_lv st lval
      (* Most casts are currently just ignored, that's probably not a good idea! *)
      | CastE  (t, exp) -> access rw st exp
      | _ -> []
  (* Accesses during the evaluation of an lval, not the lval itself! *)
  and access_lv st (lval:lval): access_list = 
    let rec access_offset (st: context) (ofs: offset): access_list = 
      match ofs with 
        | NoOffset -> []
        | Field (fld, ofs) -> access_offset st ofs
        | Index (exp, ofs) -> access false st exp @ access_offset st ofs
    in 
      match lval with 
        | Var x, ofs -> access_offset st ofs
        | Mem n, ofs -> access false st n @ access_offset st ofs

  let access_funargs (st:context) (exps: exp list): access_list = 
    (* Find the addresses reachable from some expression, and assume that these
     * can all be written to. *)
    let do_exp e = 
      match BS.eval_rv st e with
        | `Address a when AD.equal a (AD.null_ptr()) -> []
        | `Address a when not (AD.is_top a) -> 
            let f x = access_address st true x in
              List.concat (List.map f (BS.reachable_vars [a] st))
        (* Ignore soundness warnings, as invalidation proper will raise them. *)
        | _-> []
    in
      List.concat (List.map do_exp exps)

  let add_locks accessed c locks  = 
    let fl = BS.get_fl c in
    let loc = !GU.current_loc in
    let f (v, o, rv) = (v, (locks, Accesses.singleton (rv, (loc, fl, (locks,o))))) in 
      List.rev_map f accessed

  let assign lval rval (st,c,gl) = 
    let accessed = access true c (Lval lval) @ access false c rval in
      (st, add_locks accessed c st)
  let branch exp tv (st,c,gl) =
    let accessed = access false c exp in
      (st, add_locks accessed c st)
  let return exp fundec (st,c,gl) =
    match exp with 
      | Some exp -> let accessed = access false c exp in 
          (st, add_locks accessed c st)
      | None -> (st, [])
  let body f (st,c,gl) = (st, [])

  let eval_exp_addr context exp =
    let v = BS.eval_rv context exp in
      match v with
        | `Address v when not (AD.is_top v) -> AD.fold (fun a b -> a :: b) v []    
        | _                                 -> []

  (* Don't forget to add some annotation for the base analysis as well,
   * otherwise goblint will warn that the function is unknown. *)
  let special f arglist (st,c,gl) =
    match f.vname with
   (* | "sem_wait"*)
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_trylock" | "_spin_trylock_irqsave" | "_spin_lock_bh"
      | "mutex_lock" | "mutex_lock_interruptible"
      | "pthread_mutex_lock" ->
          let x = List.hd arglist in
          let st = match (eval_exp_addr c x) with 
            | [e]  -> Lockset.add e st
            | _ -> st 
          in st,[]
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
      | "mutex_unlock"
      | "pthread_mutex_unlock" ->
          let x = List.hd arglist in
          let st = match  (eval_exp_addr c x) with 
            | [] -> Lockset.empty ()
            | e  -> List.fold_right (Lockset.remove) e st
          in st, []
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> 
                let written = access_funargs c (fnc arglist) in
                  (st, add_locks written c st)
            | _ -> (st, [])
        end

  let combine lval f args (fun_st: domain) (st,c,gl: trans_in) =
    let accessed = List.concat (List.map (access false c) (f::args)) in
      (fun_st, add_locks accessed c st)

  let entry f args st = ([],[])

  let es_to_string f es = f.svar.vname

  let init () = ()

  let race_free = ref true

  module OffsMap = Map.Make (Offs)
  module OffsSet = Set.Make (Offs)

  type access_status = 
    | Race
    | Guarded of Lockset.t
    | ReadOnly
    | ThreadLocal

  let postprocess_glob (gl : GD.Var.t) ((_, accesses) : GD.Val.t) = 
    if Base.is_fun_type (gl.vtype) then () else
    (* create mapping from offset to access list; set of offsets  *)
    let create_map access_list =
      let f (map,set)  ((_, (_, _, (_, offs))) as accsess) =
        if OffsMap.mem offs map
        then (OffsMap.add offs ([accsess] @ (OffsMap.find offs map)) map,
              OffsSet.add offs set)
        else (OffsMap.add offs [accsess] map,
              OffsSet.add offs set)
      in
      List.fold_left f (OffsMap.empty, OffsSet.empty) access_list
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
      let f locks ((_, (_, _, (lock, _)))) = Lockset.join locks lock in
        List.fold_left f (Lockset.bot ()) acc_list 
    in
    let is_race acc_list: access_status =
      let locks = get_common_locks acc_list in
      let non_main (_,(_,x,_)) = BS.Flag.is_bad x in      
        if not (Lockset.is_empty locks || Lockset.is_top locks) then
          Guarded locks
        else if not (List.exists fst acc_list) then
          ReadOnly
        else if not (List.exists non_main acc_list) then
          ThreadLocal
        else
          Race
    in
    let report_race offset acc_list =
      let f (write, (loc, fl, (lockset,o))) = 
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
    let acc = Accesses.elements accesses in
    let acc = if !no_read then List.filter fst acc else acc in
    let acc_info = create_map acc in
    let acc_map  = if !unmerged_fields then fst acc_info else regroup_map acc_info in
      OffsMap.iter report_race acc_map

  let finalize () = 
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
    end

end

(*module Trivial = Spec*)
module Context = Compose.ContextSensitive (BS) (Spec)
module Path = struct
  include Compose.PathSensitive (BS) (Spec)
  let special f arglist (st,gl) = 
    match f.vname with
      | "pthread_mutex_lock" | "pthread_mutex_trylock" ->
          let f (cpa,ls) st = 
            let x = List.hd arglist in
            let gf1 x = fst (gl x) in
              match (Spec.eval_exp_addr (cpa,gf1) x) with 
                | [e]  -> 
                    let cpa_succ,_ = BS.set_return (cpa,gf1) (`Int (ID.of_int 0L)) in
                    let cpa_fail,_ = BS.set_return (cpa,gf1) (`Int (ID.of_excl_list [0L])) in
                    let st = LD.add (cpa_succ, Lockset.add e ls) st in
                      if !failing_locks then LD.add (cpa_fail,ls) st else st
                | _ -> 
                    let cpa_unknown,_ = BS.set_return (cpa,gf1) (`Int (ID.top ())) in
                    LD.add (cpa_unknown,ls) st
          in
            LD.fold f st (LD.empty ()), []
      | _ -> special f arglist (st,gl)
end

module Analysis = Multithread.Forward(Path)
module SimpleAnalysis = Multithread.Forward(Context)
