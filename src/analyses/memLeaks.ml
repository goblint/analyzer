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
(*
module M = Messages
module BS = Base.Main
module AD = ValueDomain.AD
module Offs = ValueDomain.Offs

open Cil
open Pretty

  
module Own =
struct
  include Lattice.StdCousot
  type var = AD.t * int64 
  type t = Bot | Owning | NotOwning | Var of var |  Top
    
  let owning = Owning
  let not_owning = NotOwning
  let var x = Var x
  let name () = "ownership"
  let compare (x:t) (y:t) = Pervasives.compare x y
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x = y
  let isSimple _ = true
  let short w x =
    match x with
      | Top -> "Top"
      | Bot -> "Bot"
      | Owning -> "1"
      | NotOwning -> "0"
      | Var (a,i) -> (AD.short max_int a) ^ "_" ^ (Int64.to_string i) 
  let toXML_f f x = Xml.Element ("Leaf", [("text",Str.global_replace (Str.regexp "&") "&amp;" (f Goblintutil.summary_length x))],[])
  let pretty_f f () x = text (f max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x 
    
  let top () = Top
  let is_top x = x = Top
  let bot () = Bot
  let is_bot x = x = Bot
  let leq (x:t) (y:t) = 
    match x,y with 
      | _,Top -> true
      | Top,_ -> false
      | Bot,_ -> true
      | _,Bot -> false
      | Var (x,y), Var (z,w) -> y = w && AD.equal x z
      | Owning, Owning -> true
      | NotOwning, NotOwning -> true
      | _ -> false
  let join x y = 
    match x, y with
      | Top, _ -> Top
      | _ , Top -> Top
      | x, Bot -> x
      | Bot, x -> x
      | Var x, Var y when x = y -> Var x
      | Owning, Owning -> Owning
      | NotOwning, NotOwning -> NotOwning
      | _ -> Top
  let meet x y = 
    match x, y with
      | Bot, _  -> Bot
      | _ , Bot -> Bot
      | x, Top -> x
      | Top, x -> x
      | Var x, Var y when x == y -> Var x
      | Owning, Owning -> Owning
      | NotOwning, NotOwning -> NotOwning
      | _ -> Bot
end
  
  
module MemLeak : Analyses.Spec =
struct
  module Int  = IntDomain.Integers
  module Offs = ValueDomain.Offs 
   
  module Addr = 
  struct 
    include ValueDomain.AD
    let class_name _ = ""
    let classify _ = 0
  end
  
  module OwnSet  = SetDomain.Make   (Own)
  module CurVal  = MapDomain.MapBot (Addr) (Int)
  module CurValL = Lattice.Lift     (CurVal) (struct let top_name = "Top" let bot_name = "Bot" end)
  module Var     = Lattice.Prod     (Addr) (Int)
  module Constr  = MapDomain.MapTop (Var)  (OwnSet)
  module ConstrL = Lattice.LiftBot  (Constr) 
      

  module LD  = 
  struct 
    include Lattice.Prod (CurValL) (ConstrL)
  
    let inc (addr: AD.t) (cv,c) : t =
      match cv with
        | `Lifted cv -> `Lifted (CurVal.add addr (Int.add (Int.of_int 1L) (CurVal.find addr cv)) cv), c
        | _ -> (cv,c)
    
    let cur (addr: AD.t) (cv,c) : Var.t =
      match cv with
        | `Lifted cv -> addr, CurVal.find addr cv
        | _ -> failwith ("Index not known for address " ^ AD.short max_int addr)
    
    let prev (addr: AD.t) (cv,c) : Var.t =
      match cv with
        | `Lifted cv -> addr, Int.sub (CurVal.find addr cv) (Int.of_int 1L)
        | _ -> failwith ("Index not known for address " ^ AD.short max_int addr)
          
    let add_own (var:Var.t) (own: Own.t) (cv,c:t) : t =
      match c with
        | `Lifted c ->
          let old = try Constr.find var c with _ -> OwnSet.empty () in
          cv, `Lifted (Constr.add var (OwnSet.add own old) c) 
        | _ -> cv, c
    
    let add_var (a: AD.t) (cv,c) : t =
      match cv with
        | `Lifted cv -> `Lifted (CurVal.add a 0L cv), c 
        | _ -> cv, c 
          
    let init_addr (locals: AD.t list) (formal: AD.t list) (s:t) : t = 
      let s = List.fold_right add_var locals s in
      let s = List.fold_right add_var formal s in
      let s = List.fold_right (fun a -> add_own (cur a s) Own.not_owning) locals s in
      let s = List.fold_right (fun a -> add_own (cur a s) Own.owning)     formal s in
      s
      
    let malloc (x:AD.t) s =
      List.fold_left (fun x f -> f x ) s 
      [inc x;
       (fun s -> add_own (prev x s) Own.not_owning s);
       (fun s -> add_own (cur  x s) Own.owning s);
      ]
          
    let free (x:AD.t) s =
      List.fold_left (fun x f -> f x ) s 
      [inc x;
       (fun s -> add_own (prev x s) Own.owning s);
       (fun s -> add_own (cur  x s) Own.not_owning s);
      ]

          
    let assign (l:AD.t) (r:AD.t) s =
      List.fold_left (fun x f -> f x ) s 
      [inc l;
       inc r;
       (fun s -> add_own (prev r s) (Own.var (cur r s)) s);
       (fun s -> add_own (prev r s) (Own.var (cur l s)) s);
      ]

      
  end
          
  module Dom = Lattice.Prod (LD) (BS.Dom)
  module Dep = BS.Dep
    
  module Glob = BS.Glob
  
  let get_diff (_,x) = BS.get_diff x
  let reset_diff (y,x) = (y,BS.reset_diff x)
    
  let name = "Memory leaks analysis"
  
  let startstate : Dom.t = (`Lifted (CurVal.bot ()), ConstrL.bot ()), BS.startstate
  let otherstate : Dom.t = (`Lifted (CurVal.bot ()), ConstrL.bot ()), BS.otherstate
  let init = BS.init 
  let finalize = BS.init
(*  let postprocess_last_in_main (_,g) = BS.postprocess_last_in_main g*)
    
  
  let should_join (a,_) (b,_) = LD.equal a b
  let es_to_string (f:fundec) (es:Dom.t) : string = f.svar.vname  
    
  let filter_globals      (d,b)         = (LD.top(),BS.filter_globals b)
  let insert_globals      (d,b1) (_,b2) = (d,BS.insert_globals b1 b2)
  let get_changed_globals (_,b1) (_,b2) = BS.get_changed_globals b1 b2 
  let get_global_dep      (is,vs)       = BS.get_global_dep vs
  let reset_global_dep    (is,vs)       = is, BS.reset_global_dep vs
    
  let branch (exp:exp) (tv:bool) (st,gs: Dom.t) : Dom.t = 
    (st, BS.branch exp tv gs)
  
  let body (f:fundec) (st,gs:Dom.t) : Dom.t = 
    let locals  = List.map (Addr.from_var) f.slocals in
    let formals = List.map (Addr.from_var) f.sformals in
      (LD.init_addr locals formals st, BS.body f gs)
  
  let return (exp:exp option) (fundec:fundec) (st,gs:Dom.t) : Dom.t = 
    (st, BS.return exp fundec gs)
  
  let assign (lval:lval) (rval:exp) (st,gs:Dom.t) : Dom.t =
    let access =
      let add_ad xs (v,i,_) = 
        match i with
          | Offs.Offs i -> AD.from_var_offset (v,i) :: xs
          | _ -> xs
      in
      List.fold_left add_ad [] (BS.access_one_byval true gs rval) 
    in
    let target = BS.eval_lv gs lval in
    let real_ass () =
      if List.length access == 1
      then LD.assign target (List.hd access) st
      else (M.warn "somekind of a problem"; st)
    in
    let new_st =
      match AD.get_type target with
        | TPtr (_,_) -> real_ass ()
        | _ -> st
    in
    (new_st, BS.assign lval rval gs)
    
  let leave_func (lval:lval option) (f:varinfo) (args:exp list) (bu,bg: Dom.t) (au,ag: Dom.t) : Dom.t =
(*    if M.tracing then M.trace "mem" (dprintf "assign: %a.\n" f.vname);**)
    au, BS.leave_func lval f args bg ag
  
  let special_fn (lval:lval option) (f:varinfo) (arglist:exp list) (st,gs:Dom.t) : Dom.t list =
    let map_ngs x = List.map (fun y -> x,y) (BS.special_fn lval f arglist gs) in
    match f.vname, lval with
      | "malloc", Some lv -> map_ngs (LD.malloc (BS.eval_lv gs lv) st)
      | "free", _ -> 
          let add_ad xs (v,i,_) = 
            match i with
              | Offs.Offs i -> AD.from_var_offset (v,i) :: xs
              | _ -> xs
          in
          let xs = List.fold_left add_ad [] (BS.access_one_byval false gs (List.hd arglist))  in
          if List.length xs = 1 
          then map_ngs (LD.free (List.hd xs) st)
          else map_ngs (LD.top ())
      | _ -> map_ngs (LD.bot())

  let eval_funvar exp (_,st) = BS.eval_funvar exp st
    
  let enter_func (lval: lval option) (f:varinfo) (args:exp list) (st,gs:Dom.t) : Dom.t list =
    List.map (fun s -> (st,s)) (BS.enter_func lval f args gs)
      
  let fork (lval: lval option) (f : varinfo) (args : exp list) ((univ, cpa) : Dom.t) : (varinfo * Dom.t) list =
    let base = BS.fork lval f args cpa in
    let dress (v,d) = v, (LD.bot (), d) in 
    List.map dress base
end

module Analysis = Multithread.Forward(Compose.PathSensitive(MemLeak))
*)