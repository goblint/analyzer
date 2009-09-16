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
 
(*module BS = Base.Spec*)
module M = Messages
module BS = Base.Main
module AD = ValueDomain.AD
module Offs = ValueDomain.Offs

open Cil
open Pretty

module Spec =
struct
  module Addr = ValueDomain.Addr
  module LD = SetDomain.ToppedSet (Addr) (struct let topname = "All" end)
  
  module Dom = Lattice.Prod (LD) (BS.Dom)
  module Glob = BS.Glob
  
  type trans_in  = LD.t * BS.trans_in
  type trans_out = LD.t * BS.trans_out
  type transfer  = trans_in -> trans_out
  type glob_fun  = Glob.Var.t -> Glob.Val.t
  
  let name = "Initialization analysis"  
  
  let startstate : Dom.t = LD.empty () , BS.startstate
  let otherstate : Dom.t = LD.empty () , BS.otherstate  
  let es_to_string (f:fundec) (es:Dom.t) : string = f.svar.vname
  let init = BS.init
  let should_join (a,b : Dom.t) (c,d : Dom.t) = LD.equal a c 
  let finalize = BS.finalize
    
  
  let get_diff (_,x) = BS.get_diff x
  let reset_diff (y,x) = (y,BS.reset_diff x)
  
  (*  queries *)
  let query _ _ (x:Dom.t) (q:Queries.t) : Queries.Result.t = Queries.Result.top ()

  (* list accessed addresses *)
  let varoffs a (rval:exp) (gs:glob_fun) (st:BS.store) =
    let f vs (v,o,_) =
      match o with 
        | Offs.Offs o -> (v,o) :: vs
        | _ -> vs in 
    List.fold_left f [] (BS.access_one_byval a false gs st rval)  

  let vars a (rval:exp) (gs:glob_fun) (st:BS.store) : Addr.t list =
    List.map Addr.from_var_offset (varoffs a rval gs st)

  let is_prefix_of (v1,ofs1: varinfo * (Addr.field,Addr.idx) Lval.offs) (v2,ofs2: varinfo * (Addr.field,Addr.idx) Lval.offs) : bool =
    let rec is_offs_prefix_of pr os = 
      match (pr, os) with
        | (`NoOffset, _) -> true
        | (`Field (f1, o1), `Field (f2,o2)) -> f1 == f2 && is_offs_prefix_of o1 o2
        | (_, _) -> false
    in
    (v1.vid == v2.vid) && is_offs_prefix_of ofs1 ofs2
    

  (* Does it contain non-initialized variables? *)
  let is_expr_initd a (expr:exp) (gs:glob_fun) (st:LD.t) (ct:BS.store): bool = 
    let variables = vars a expr gs ct in
    let raw_vars = List.concat (List.map Addr.to_var_offset variables) in
    let will_addr_init (t:bool) a = 
      let f addr =
        List.exists (is_prefix_of a) (Addr.to_var_offset addr)
      in
      if LD.exists f st then begin 
        Messages.report ("Uninitialized variable " ^ (Addr.short 80 (Addr.from_var_offset a)) ^ " accessed.");
        false
      end else
        t in  
    List.fold_left will_addr_init true raw_vars

  let remove_if_prefix (pr: varinfo * (Addr.field,Addr.idx) Lval.offs) (uis: LD.t) : LD.t =
    let f ad =
      let vals = Addr.to_var_offset ad in
      List.for_all (fun a -> not (is_prefix_of pr a)) vals
    in
    LD.filter f uis

  type lval_offs = (Addr.field,Addr.idx) Lval.offs
  type var_offs  = varinfo * lval_offs    
  
  (* Call to [get_pfx v cx] returns initialized prefixes ... *)
  let rec get_pfx (v:varinfo) (cx:lval_offs) (ofs:lval_offs) (target:typ) (other:typ) : var_offs list =
    let rec cat o i =
      match o with
        | `NoOffset -> i
        | `Field (f, o) -> `Field (f, cat o i)
        | `Index (v, o) -> `Index (v, cat o i)
    in
    let rec rev lo = 
      match lo with
        | `NoOffset -> `NoOffset
        | `Field (f, o) -> cat (rev o) (`Field (f, `NoOffset))
        | `Index (v, o) -> cat (rev o) (`Index (v, `NoOffset))
    in
    let rec bothstruct (t:fieldinfo list) (tf:fieldinfo) (o:fieldinfo list) (no:lval_offs)  : var_offs list =
      match t, o with
        | x::xs, y::ys when x.fcomp.ckey == tf.fcomp.ckey && x.fname == tf.fname ->
          get_pfx v (`Field (y, cx)) no x.ftype y.ftype
        | x::xs, y::ys when x.ftype == y.ftype ->
          bothstruct xs tf ys no
        | x::xs, y::ys ->
          [] (* found a mismatch *)
        | _ -> 
          M.warn ("Failed to analyze union at point " ^ (Addr.short 80 (Addr.from_var_offset (v,rev cx))) ^ " -- did not find " ^ tf.fname);
          []
    in
    let utar, uoth = unrollType target, unrollType other in
    match ofs, utar, uoth with
      |     `NoOffset,              _ ,               _ when utar == uoth  -> [v, rev cx]
      |     `NoOffset,              _ ,    TComp (c2,_) when not c2.cstruct -> 
          (* unroll other (union) *)
          List.concat (List.rev_map (fun oth_f -> get_pfx v (`Field (oth_f, cx)) ofs utar oth_f.ftype) c2.cfields)
      |     `NoOffset,              _ ,               _ -> 
          (* types not same and other is not a struct *)
          [] 
      | `Index (i, o), TArray (t1,_,_), TArray (t2,_,_) -> (* todo: other might be a union*)
          (* step into both indexed *)
          get_pfx v (`Index (i, cx)) o t1 t2    
      | `Index (i, o),    _ ,    TComp (c2,_) when not c2.cstruct ->
          (* step into all other fields *)
          List.concat (List.rev_map (fun oth_f -> get_pfx v (`Field (oth_f, cx)) ofs utar oth_f.ftype) c2.cfields)          
      | `Field (f, o),    TComp (c1,_),    TComp (c2,_) when c1.cstruct && c2.cstruct ->
          (* step into both, but check that types of prefices match*)
          bothstruct c1.cfields f c2.cfields o
      | `Field (f, o),    TComp (c1,_),              _  when not c1.cstruct  ->
          (* step into target but not other (don't care about other) *)
          get_pfx v cx o f.ftype uoth  
      | `Field (f, o),    TComp (c1,_),    TComp (c2,_) when c1.cstruct && not c2.cstruct ->
          (* step into all other fields *)
          List.concat (List.rev_map (fun oth_f -> get_pfx v (`Field (oth_f, cx)) ofs utar oth_f.ftype) c2.cfields)          
      | _ ->
        M.warn ("Failed to analyze union at point " ^ (Addr.short 80 (Addr.from_var_offset (v,rev cx))));
        []
        
    
  (* Call to [init_lval lv gs st] results in state [st] where the variable evaluated form [lv] is initialized. *)
  let init_lval a (lv: lval) (gl:glob_fun)  (gs: BS.Dom.t) (st: LD.t) : LD.t =
    let init_vo (v: varinfo) (ofs: lval_offs) : LD.t = 
      List.fold_right remove_if_prefix (get_pfx v `NoOffset ofs v.vtype v.vtype) st
    in
    let el = BS.eval_lv a gl gs lv in
    if AD.is_top el then st else
    match AD.to_var_offset el with
      | [var,ofs] -> init_vo var ofs
      | _ -> st   
  
  let to_addrs (v:varinfo) : Addr.t list =
    let make_offs = List.fold_left (fun o f -> `Field (f, o)) `NoOffset in
    let rec add_fields (base: Addr.field list) fs acc = 
      match fs with 
        | [] -> acc
        | f :: fs ->
            match unrollType f.ftype with
              | TComp ({cfields=ffs},_) -> add_fields base fs (List.rev_append (add_fields (f::base) ffs []) acc)  
              | _                       -> add_fields base fs ((Addr.from_var_offset (v,make_offs (f::base))) :: acc) 
    
    in
    match unrollType v.vtype with
      | TComp ({cfields=fs},_) -> add_fields [] fs [] 
      | _ -> [Addr.from_var v]
      
  
  let remove_unreachable a (args: exp list) (gl:glob_fun) (gs: BS.Dom.t) (st: LD.t) : LD.t =
    let vals      = List.map (BS.eval_rv a gl gs) args in
    let reachable = BS.reachable_vars (BS.get_ptrs vals) (gl:glob_fun) gs in
    let add_exploded_struct (one: AD.t) (many: AD.t) : AD.t =
      let vars = AD.to_var_may one in
      List.fold_right AD.add (List.concat (List.map to_addrs vars)) many
    in
    let vars = List.fold_right add_exploded_struct reachable (AD.empty ()) in
    if LD.is_top st 
    then LD.top ()
    else LD.filter (fun x -> AD.mem x vars) st     

  (*
    Transfer functions
  *)
  
  let assign a (lval:lval) (rval:exp) (gl:glob_fun) (st,gs:trans_in) : trans_out =
    ignore (is_expr_initd a rval gl st gs);
    (init_lval a lval gl gs st, BS.assign a lval rval gl gs)
        
  let branch a (exp:exp) (tv:bool) (gl:glob_fun) (st,gs:trans_in) : trans_out = 
    ignore (is_expr_initd a exp gl st gs);
    (st,BS.branch a exp tv gl gs)
  
  let body a (f:fundec) (gl:glob_fun) (st,gs:trans_in) : trans_out = 
    let add_var st v = List.fold_right LD.add (to_addrs v) st in
    (List.fold_left add_var st f.slocals, BS.body a f gl gs)
  
  let return a (exp:exp option) (f:fundec) (gl:glob_fun) (st,gs:trans_in) : trans_out = 
    let remove_var x v = 
      List.fold_right LD.remove (to_addrs v) x in
    let nst = List.fold_left remove_var st (f.slocals @ f.sformals) in
    match exp with 
      | Some exp -> ignore (is_expr_initd a exp gl st gs); (nst, BS.return a (Some exp) f gl gs)
      | _ -> (nst,BS.return a exp f gl gs)
  
  
  
  let eval_funvar a fn (gl:glob_fun) (_,st) = BS.eval_funvar a fn gl st    
  
  let enter_func a (lval: lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (st,gs:trans_in) : (Dom.t * Dom.t) list =
    let nst = remove_unreachable a args gl gs st in
    let lift_pair (bf,g) = (st,gs), (nst, g)  in
    List.map lift_pair (BS.enter_func a lval f args gl gs)
  
  let leave_func a (lval:lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (bu,bst:Dom.t) (au,ast:Dom.t) : trans_out =
    ignore (List.map (fun x -> is_expr_initd a x gl bu bst) args);
    let cal_st = remove_unreachable a args gl bst bu in
    let ret_st = LD.union au (LD.diff bu cal_st) in
    let new_u = 
      match lval with
        | None -> ret_st
        | Some lv -> init_lval a lv gl bst ret_st
    in
    new_u, BS.leave_func a lval f args gl bst ast
  
  let special_fn a (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st,gs:trans_in) : (Dom.t * Cil.exp * bool) list =
    let remove_lv lval =
      let addr  = BS.eval_lv a gl gs lval in
      if AD.is_top addr then st else
      let addrl = AD.to_var_offset addr in
      if (List.length addrl) = 1 then
        LD.remove (Addr.from_var_offset (List.hd addrl)) st
      else
        st        
    in  
    let map_bs x = List.map (fun (y,e,t) -> (x, y), e, t) (BS.special_fn a lval f arglist gl gs) in
    match lval with
      | Some lv -> map_bs (remove_lv lv)
      | _ -> map_bs st
      
  let fork a (lval: lval option) (f : varinfo) (args : exp list) (gl:glob_fun) ((univ, cpa) : trans_in) : (varinfo * Dom.t) list =
    let base = BS.fork a lval f args gl cpa in
    let dress (v,d) = v, (LD.top (), d) in 
    List.map dress base

end

module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
