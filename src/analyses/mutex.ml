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
module Equ = AddressDomain.Equ
module Reg = AddressDomain.Reg
module Fields = AddressDomain.Fields
module Lockset = LockDomain.Lockset
module Equset = LockDomain.LocksetEqu
module AD = ValueDomain.AD
module ID = ValueDomain.ID
(*module BS = Base.Spec*)
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty

module Spec =
struct
  exception Top
  let name = "Mutex Must"
  type context = BS.store
  module LD = Lattice.ProdSimple (Lockset) (Equset)

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

  let startstate = LD.top ()
  let otherstate = LD.top ()
  let return_var = 
    let myvar = makeVarinfo false "RETURN" voidType in
      myvar.vid <- -99;
      myvar

  let add_locks accessed c (locks, eqlocks)  = 
    let fl = BS.get_fl c in
    let eq = BS.get_eq c in
    let loc = !GU.current_loc in
    let f ((v,o), eq_acc, exp, rv) = 
      let locks = 
        if !GU.regions then begin
          match exp with 
            | Some fd -> begin
                try 
                  (* let all_locks = Equset.elements eqlocks in *)
                  let (lv,lo) = Equset.choose eqlocks in
                  (* We need a function to skip fields to the first array
                   * indexing while constructing the sequence of field
                   * accesses into which we place our k. *)
                  let rec skip_fields fds k =
                    match fds with
                      | `Left f :: fds -> 
                          let (seq,rest) = skip_fields fds k in
                            `Field (f, seq), rest
                      | _ -> k, fds
                  in
                  let _ ,fd = skip_fields fd `NoOffset in
                  let la,lo = skip_fields lo (`Index (ID.of_int GU.inthack, `NoOffset)) in
                    match fd,lo with
                      | `Right e :: _, [`Right e'] when Basetype.CilExp.equal e e' -> 
                          Lockset.add (Addr.from_var_offset (lv,la)) locks
                      | _ -> locks
                with _ -> locks
              end
            | None -> locks 
        end else locks 
      in
      let locks =
        match eq_acc with 
          | Some eqaddr -> begin 
              try
                let all_addrs = Equ.other_addrs eqaddr eq in
                let all_locks = Equset.elements eqlocks in
                let ptr_to x y = match x with
                  | TPtr (x,_) when Util.equals x y -> true
                  | _ -> false in
                let compat = List.find (fun (x,fl) -> ptr_to x.vtype v.vtype) in
                let (lv,lo) = compat all_locks in
                let (av,ao) = compat all_addrs in
                  if Basetype.Variables.equal lv av then
                    let lo = Fields.to_offs lo (Base.ID.top ()) in
                      Lockset.add (Addr.from_var_offset (v,lo)) locks
                      else locks 
              with _ -> locks
            end
          | None -> locks
      in
        (v, (locks, Accesses.singleton (rv, (loc, fl, (locks,o))))) 
    in 
      List.rev_map f accessed

  let assign lval rval ((st,eq),c,gl) = 
    let accessed = BS.access true c (Lval lval) @ BS.access false c rval in
    let eq = match Equ.eval_lv lval with Some x -> Equset.remove x eq | _ -> eq in
      ((st,eq), add_locks accessed c (st,eq))
  let branch exp tv ((st,eq),c,gl) =
    let accessed = BS.access false c exp in
      ((st,eq), add_locks accessed c (st,eq))
  let return exp fundec ((st,eq),c,gl) =
    let locals = fundec.sformals @ fundec.slocals in
    let eq = Equset.kill_vars locals eq in
    match exp with 
      | Some exp -> let accessed = BS.access false c exp in 
          ((st,eq), add_locks accessed c (st,eq))
      | None -> ((st,eq), [])
  let body f ((st,eq),c,gl) = ((st,eq), [])

  let eval_exp_addr context exp =
    let v = BS.eval_rv context exp in
      match v with
        | `Address v when not (AD.is_top v) -> AD.fold (fun a b -> a :: b) v []    
        | _                                 -> []

  (* Don't forget to add some annotation for the base analysis as well,
   * otherwise goblint will warn that the function is unknown. *)
  let special f arglist ((st,eq),c,gl) =
    match f.vname with
   (* | "sem_wait"*)
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_trylock" | "_spin_trylock_irqsave"
      | "mutex_lock" | "mutex_lock_interruptible"
      | "pthread_mutex_lock" ->
          let x = List.hd arglist in
          let st = match (eval_exp_addr c x) with 
            | [e]  -> Lockset.add e st
            | _ -> st in
          let eq = 
            let c_eq = BS.get_eq c in 
              match Equ.eval_rv x with
                | Some e  -> Equset.add e c_eq eq
                | _ -> eq
          in (st,eq), []
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore"
      | "mutex_unlock"
      | "pthread_mutex_unlock" ->
          let x = List.hd arglist in
          let st, eq = match  (eval_exp_addr c x) with 
            | [] -> Lockset.empty (), Equset.empty ()
            | e  -> List.fold_right (Lockset.remove) e st, Equset.empty ()
          in (st,eq), []
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> 
                let written = BS.access_funargs c (fnc arglist) in
                  ((st,eq), add_locks written c (st,eq))
            | _ -> ((st,eq), [])
        end

  let combine lval f args ((fun_st,fun_eq): domain) ((st,eq),c,gl: trans_in) =
    let accessed = List.concat (List.map (BS.access false c) (f::args)) in
      ((fun_st,fun_eq), add_locks accessed c (st,eq))

  let entry f args st = ([],[])

  let es_to_string f es = f.svar.vname

  let init () = ()

  let race_free = ref true

  module OffsMap = Map.Make (Offs)
  module OffsSet = Set.Make (Offs)

  let postprocess_glob (gl : GD.Var.t) ((_, accesses) : GD.Val.t) = 
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
    let is_race acc_list =
      let f locks ((_, (_, _, (lock, _)))) = Lockset.join locks lock in
      let locks = List.fold_left f (Lockset.bot ()) acc_list in
      let non_main (_,(_,x,_)) = BS.Flag.is_bad x in      
             (Lockset.is_empty locks || Lockset.is_top locks) 
          && (List.exists fst acc_list) 
          && (List.exists non_main acc_list)    
    in
    let report_race offset acc_list =
      let f (write, (loc, fl, (lockset,o))) = 
        let lockstr = Lockset.short 80 lockset in
        let action = if write then "write" else "read" in
        let thread = if BS.Flag.is_bad fl then "some thread" else "main thread" in
        let warn = (*gl.vname ^ Offs.short 80 o ^ " " ^*) action ^ " in " ^ thread ^ " with lockset: " ^ lockstr in
          (warn,loc) in 
      let warnings =  List.map f acc_list in
      if is_race acc_list then begin
        race_free := false;
        let warn = "Datarace over variable \"" ^ gl.vname ^ Offs.short 80 offset ^ "\"" in
          M.print_group warn warnings
      end else if !GU.allglobs then
        let warn = "Safely accessed variable \"" ^ gl.vname ^ Offs.short 80 offset ^ "\"" in
          match gl.vtype with
            | TFun _ -> ()
            | _ -> M.print_group warn warnings
    in 
    let acc = Accesses.elements accesses in
    let acc = if !GU.no_read then List.filter fst acc else acc in
    let acc_info = create_map acc in
    let acc_map  = if !GU.unmerged_fields then fst acc_info else regroup_map acc_info in
      OffsMap.iter report_race acc_map

  let finalize () = 
    if !GU.multi_threaded then begin
      match !race_free, !M.soundness with
        | true, true -> 
            print_endline "CONGRATULATIONS!\nYour program has just been certified Free of Data Races!"
        | true, false -> 
            print_endline "Goblint did not find any Data Races in this program!";
            print_endline "However, the code was too complicated for Goblint to understand all of it."
        | false, true -> 
            print_endline "And that's all. Goblint is certain there are no other races."
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
module Path = Compose.PathSensitive (BS) (Spec)

module Analysis = Multithread.Forward(Path)
module SimpleAnalysis = Multithread.Forward(Context)
