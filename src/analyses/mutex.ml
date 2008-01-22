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
module AD = ValueDomain.AD
module BS = Base.Spec
(*module BS = Base.Main*)
module LF = LibraryFunctions
open Cil
open Pretty

module Spec =
struct
  exception Top
  let name = "Mutex Must"
  type context = BS.store
  module Lockset = SetDomain.ToppedSet (Basetype.Variables) 
                  (struct let topname = "All mutexes" end)
  module LD = Lattice.Reverse (Lockset)

  module AccessType = IntDomain.MakeBooleans (struct 
                                               let truename = "Write" 
                                               let falsename = "Read" end)
  module Access = Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (LD)
  module Accesses = SetDomain.SensitiveConf (struct
                                               let expand_fst = false
                                               let expand_snd = true
                                             end) (AccessType) (Access)
  module GLock = Lattice.Prod (LD) (Accesses)
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
  let return_var = 
    let myvar = makeVarinfo false "RETURN" voidType in
      myvar.vid <- -99;
      myvar

  let add_locks accessed c (locks: domain) = 
    let fl = BS.get_fl c in
    let loc = !GU.current_loc in
    let f (v,rv) = (v,(locks, Accesses.singleton (rv, (loc, fl, locks)))) in 
      List.rev_map f accessed

  let assign lval rval (st,c,gl) = 
    let accessed = BS.access true c (Lval lval) @ BS.access false c rval in
      (st, add_locks accessed c st)
  let branch exp tv (st,c,gl) =
    let accessed = BS.access false c exp in
      (st, add_locks accessed c st)
  let return exp fundec (st,c,gl) =
    match exp with 
      | Some exp -> let accessed = BS.access false c exp in 
          (st, add_locks accessed c st)
      | None -> (st, [])
  let body f (st,c,gl) = (st, [])

  let eval_exp context exp: varinfo option =
    let v = BS.eval_rv context exp in
      match v with
        | `Address v when not (AD.is_top v) && AD.cardinal v = 1 -> Some (List.hd (AD.to_var v))
        | _ -> None

  let special f arglist (st,c,gl) =
    match f.vname with
   (* | "sem_wait"*)
      | "pthread_mutex_lock" -> begin
          match arglist with
            | [AddrOf (Var x, _)] -> (Lockset.add x st,[])
            | [x] -> (match eval_exp c x with
                        | Some x -> Lockset.add x st, []
                        | _ -> st, [])
            | _ -> (st, [])
        end
   (* | "sem_post"*)
      | "pthread_mutex_unlock" -> begin
          match arglist with
            | [AddrOf (Var x, _)] -> (Lockset.remove x st,[])
            (* XXX This isn't sound. You should remove ALL possible addresses! *)
            | [x] -> (match eval_exp c x with
                        | Some x -> Lockset.remove x st, []
                        | _ -> st, [])
            | _ -> (st, [])
        end
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> 
                let written = BS.access_funargs c (fnc arglist) in
                  (st, add_locks written c st)
            | _ -> (st, [])
        end

  let combine lval f args (fun_st: domain) (st,c,gl: trans_in) =
    let accessed = List.concat (List.map (BS.access false c) args) in
      (fun_st, add_locks accessed c st)

  let entry f args st = ([],[])

  let es_to_string f es = f.svar.vname
  let init () = ()

  let postprocess_glob gl (locks, accesses) = 
    let non_main (_,(_,x,_)) = BS.Flag.is_bad x in
    if (Lockset.is_empty locks || Lockset.is_top locks)
    && ((Accesses.cardinal accesses) > 1)
    && (Accesses.exists fst accesses) 
    && (Accesses.exists non_main accesses)
    then 
      let warn = "Datarace over variable \"" ^ gl.vname ^ "\"" in
      let f (write, (loc, fl, lockset)) = 
        let lockstr = LD.short 80 lockset in
        let action = if write then "write" else "read" in
        let thread = if BS.Flag.is_bad fl then "some thread" else "main thread" in
        let warn = action ^ "in" ^ thread ^ " with lockset: " ^ lockstr in
          (warn,loc) in 
      let warnings =  List.map f (Accesses.elements accesses) in
        M.print_group warn warnings
end

(*module Trivial = Spec*)
module Context = Compose.ContextSensitive (BS) (Spec)
module Path = Compose.PathSensitive (BS) (Spec)

module Analysis = Multithread.Forward(Path)
module SimpleAnalysis = Multithread.Forward(Context)
