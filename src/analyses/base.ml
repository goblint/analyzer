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

open Cil
open Pretty
module A = Analyses
module M = Messages
module H = Hashtbl

module GU = Goblintutil
module ID = ValueDomain.ID
module IntSet = SetDomain.Make (IntDomain.Integers)
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module VD = ValueDomain.Compound
module LF = LibraryFunctions


  
module MakeSpec (Flag: ConcDomain.S) =
struct
  module Flag = Flag
  module Interp = CInterpreter.Interpreter (Flag)
  
  include Interp
    
  let name = "Constant Propagation Analysis"
  let startstate = CPA.top (), Flag.bot (), VarSet.bot ()
  let otherstate = CPA.top (), Flag.top (), VarSet.bot ()

  let filter_globals (cpa,fl,gs) : Dom.t = 
    let add_var (v:varinfo) (value) cpa = 
      if v.vglob then cpa else CPA.remove v cpa 
    in
      if Flag.is_multi fl then 
        (CPA.fold add_var cpa cpa) , fl, VarSet.bot ()
      else 
        Dom.bot ()
    
  let insert_globals (ls1,fl,gs) (ls2,_,_) : Dom.t = 
    let insert x v map = CPA.add x v map in
    (CPA.fold insert ls2 ls1) ,fl, gs

  let get_changed_globals (x,_,_) (y,_,_) =
    let add_glob_var (v:varinfo) (value) gs = 
      if v.vglob && not (VD.equal value (CPA.find v y)) then 
        v :: gs 
      else 
        gs
    in
      CPA.fold add_glob_var x []
  let get_global_dep   (_,_,g) = Vars.elements !g
  let reset_global_dep (s,f,g) = s, f, ref (Vars.empty ())
    
  let should_join _ _ = true 
  let get_fl (_,fl,_) = fl
  
  let hash    (x,y,_)             = Hashtbl.hash (x,y)
  let equal   (x1,x2,_) (y1,y2,_) = CPA.equal x1 y1 && Flag.equal x2 y2
  let leq     (x1,x2,_) (y1,y2,_) = CPA.leq   x1 y1 && Flag.leq   x2 y2 
  let compare (x1,x2,_) (y1,y2,_) = 
    match CPA.compare x1 y1 with 
      | 0 -> Flag.compare x2 y2
      | x -> x
    
  let es_to_string f (es,fl,_) = 
    let short_fun x = 
      match x.vtype, CPA.find x es with
        | TPtr (t, attr), `Address a 
	    when (not (AD.is_top a)) 
	      && List.length (AD.to_var_may a) = 1 
	      && not (is_immediate_type t) 
	      -> 
            let cv = List.hd (AD.to_var_may a) in 
              "ref " ^ VD.short 26 (CPA.find cv es)
        | _, v -> VD.short 30 v
    in
    let args_short = List.map short_fun f.sformals in
      Printable.get_short_list (f.svar.vname ^ "(") ")" 80 args_short

 (**************************************************************************
  * Initializing my variables
  **************************************************************************)

  let return_varstore = ref dummyFunDec.svar
  let return_varinfo () = !return_varstore 
  let return_var () = AD.from_var (return_varinfo ())

  let heap_hash = H.create 113 
  let heap_counter = ref 0

  let get_heap_var loc = try 
      H.find heap_hash loc
    with Not_found ->
      let _ = heap_counter := !heap_counter + 1 in
      let name = "heap_" ^ string_of_int !heap_counter in
      let newvar = makeGlobalVar name voidType 
      in
        H.add heap_hash loc newvar;
        newvar

  let heap_var loc = AD.from_var (get_heap_var loc)

  let init () = 
    return_varstore := makeVarinfo false "RETURN" voidType;
    H.clear heap_hash

  let finalize () = ()


 (**************************************************************************
  * Simple defs for the transfer functions 
  **************************************************************************)

  let assign lval rval st = 
    set_savetop st (eval_lv st lval) (eval_rv st rval)

  let branch (exp:exp) (tv:bool) (st: store): store =
    (* First we want to see, if we can determine a dead branch: *)
    match eval_rv st exp with
      (* For a boolean value: *)
      | `Int value when (ID.is_bool value) -> 
          (* to suppress pattern matching warnings: *)
          let fromJust x = match x with Some x -> x | None -> assert false in
          let v = fromJust (ID.to_bool value) in
            (* Eliminate the dead branch and just propagate to the true branch *)
            if v = tv then st else raise A.Deadcode
      (* Otherwise we try to impose an invariant: *)
      | _ -> invariant st exp tv 

  let body f st = 
    (* First we create a variable-initvalue pair for each varaiable *)
    let init_var v = (AD.from_var v, init_value st v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
      set_many st inits

  let return exp fundec st =
    let nst = rem_many st (fundec.sformals @ fundec.slocals) in
      match exp with
        | None -> nst
        | Some exp -> set nst (return_var ()) (eval_rv st exp)





 (**************************************************************************
  * Function calls
  **************************************************************************)
  
  (* todo: check if they are functions *)
  let eval_funvar fval st: varinfo list =
    AD.to_var_may (eval_fv st fval)
    
  
  let special_fn (lv:lval option) (f: varinfo) (args: exp list) (cpa,fl,gl as st:store): store = 
    let heap_var = heap_var !GU.current_loc in
    match f.vname with 
      (* handling thread creations *)
      | "pthread_create" -> 
          GU.multi_threaded := true;
          let new_fl = Flag.join fl (Flag.get_main ()) in
            (cpa,new_fl,gl)
      (* handling thread joins... sort of *)
      | "pthread_join" -> begin
          match args with
            | [id; ret_var] -> begin
		match (eval_rv st ret_var) with
            | `Int n when n = ID.of_int 0L -> (cpa,fl,VarSet.bot ())
		  | _      -> invalidate st [ret_var] end
            | _ -> M.bailwith "pthread_join arguments are strange!"
        end
      | "exit" -> raise A.Deadcode
      | "abort" -> raise A.Deadcode
      | "malloc" | "calloc" -> begin
        match lv with
          | Some lv -> set st (eval_lv st lv) (`Address heap_var)
          | _ -> st
        end
      (* Handling the assertions *)
      | "__assert_rtn" -> raise A.Deadcode (* gcc's built-in assert *) 
      | "assert" -> begin
          match args with
            | [e] -> begin
                (* evaluate the assertion and check if we can refute it *)
                let expr = sprint ~width:80 (d_exp () e) in
                match eval_rv st e with 
                  (* If the assertion is known to be false/true *)
                  | `Int v when ID.is_bool v -> 
                      (* Warn if it was false; ignore if true! The None case
                       * should not happen! *)
                      (match ID.to_bool v with
                        | Some false -> M.warn_each ("Assertion \"" ^ expr ^ "\" will fail")
                        | _ -> ()); 
                      (* Just propagate the state *)
                      st
                  | _ -> begin 
                      if !GU.debug then begin
                        M.warn_each ("Assertion \"" ^ expr ^ "\" is unknown");
                        st
                      end else
                        (* make the state meet the assertion in the rest of the code *)
                        invariant st e true
                    end
              end
            | _ -> M.bailwith "Assert argument mismatch!"
        end
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> invalidate st (fnc args);
            | None -> begin
                M.warn ("Function definition missing for " ^ f.vname);
                let st_expr (v:varinfo) (value) a = 
                  if v.vglob then Cil.mkAddrOf (Var v, NoOffset) :: a else a
                in
                match lv with
                  | None -> invalidate st (CPA.fold st_expr cpa args)
                  | Some lval -> invalidate st (Cil.mkAddrOrStartOf lval :: (CPA.fold st_expr cpa args))
              end
        end

  let enter_func lval fn args (cpa,fl,gl as st: store): Dom.t list = 
    let make_entry pa context =
      (* If we need the globals, add them *)
      let new_cpa = CPA.filter_class 2 cpa in 
      (* Assign parameters to arguments *)
      let new_cpa = CPA.add_list pa new_cpa in
      let new_cpa = CPA.add_list_fun context (fun v -> CPA.find v cpa) new_cpa in
        new_cpa, fl, VarSet.bot () 
    in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv st) args in
    (* List of reachable variables *)
    let reachable = List.concat (List.map AD.to_var_may (reachable_vars (get_ptrs vals) st)) in
    (* generate the entry states *)
    let add_calls_addr f norms =
      let fundec = Cilfacade.getdec fn in
      (* And we prepare the entry state *)
      let entry_state = make_entry (zip fundec.sformals vals) reachable in
        entry_state :: norms
    in
    add_calls_addr fn []


  let fork (lv: lval option) (f: varinfo) (args: exp list) (cpa,fl,gl as st:store) : (varinfo * Dom.t) list = 
    match f.vname with 
      (* handling thread creations *)
      | "pthread_create" -> begin	 
          match args with
            | [_; _; start; ptc_arg] -> begin
                let start_addr = eval_fv st start in
                let start_vari = List.hd (AD.to_var_may start_addr) in
                try
                  (* try to get function declaration *)
                  let _ = Cilfacade.getdec start_vari in 
                  let sts = enter_func None start_vari [ptc_arg] (cpa, Flag.get_multi (), gl) in
                  List.map (fun st -> start_vari, st) sts
                with Not_found -> 
                  M.warn ("creating an thread from unknown function " ^ start_vari.vname);
                  [start_vari,(cpa, Flag.get_multi (), VarSet.bot())]
                end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> []

  let leave_func (lval: lval option) (f: varinfo) (args: exp list) (before: Dom.t) (after: Dom.t) : Dom.t =
    let combine_one (loc,lf,gl as st: Dom.t) (fun_st,fun_fl,_: Dom.t) = 
      (* This function does miscelaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (cpa_s,fl_s) (cpa_d,fl_d,gl) = 
        (* Remove the return value as this is dealt with separately. *)
        let cpa_s = CPA.remove (return_varinfo ()) cpa_s in
        let new_cpa = CPA.fold CPA.add cpa_s cpa_d in
          (new_cpa, fl_s, gl)
      in 
      let return_var = return_var () in
      let return_val = get (fun_st,fun_fl,gl) return_var in
      let st = add_globals (fun_st,fun_fl) st in
        match lval with
          | None      -> st
          | Some lval -> set_savetop st (eval_lv st lval) return_val
     in
     combine_one before after

end

module Spec = MakeSpec (ConcDomain.Trivial)
module Main = MakeSpec (ConcDomain.Simple)


module Analysis = Multithread.Forward(Spec)
