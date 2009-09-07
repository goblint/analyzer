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
module AD = ValueDomain.AD
(*module BS = Base.Spec*)
module LF = LibraryFunctions
open Cil
open Pretty


module Spec : Analyses.Spec with type Glob.Val.t = unit =
struct
  exception Top

  module Dom = PartitionDomain.SetSet (Basetype.Variables)
  module Glob = Global.Make (Lattice.Unit)

  let name = "Partition"

  let init () = ()
  let finalize () = ()
  let startstate = Dom.top ()
  let otherstate = Dom.top ()
  let es_to_string f es = f.svar.vname
  
  let exp_equal e1 e2 (g:Glob.Var.t -> Glob.Val.t) s =
    print_string "...";
    match e1, e2 with
      | Lval  (Var v1,NoOffset), Lval (Var v2,NoOffset) -> begin
        match Dom.find_class v1 s with
          | Some ss when Dom.S.mem v2 ss -> print_string ("ok\n"); Some true
          | _ -> print_string ("not in "^(Dom.short 80 s)^"\n"); None
        end
      | _ -> print_string "not lv\n"; None
      
  let reset_diff x = x
  let get_diff   x = []
  let should_join x y = true

  let branch exp tv glob st     = st
  let return exp fundec glob st = Dom.top ()
  let body   f glob st          = Dom.top ()
  let special f arglist glob st = Dom.top ()

  let assign (lval:lval) (rval:exp) (glob:Glob.Var.t -> Glob.Val.t) (st:Dom.t) : Dom.t  = 
    match lval, rval with
      | (Var v1,NoOffset), Lval ((Var v2,NoOffset)) -> Dom.add_eq (v1,v2) st
      | (Var v1,_), _ -> Dom.remove v1 st
      | _ -> st

  let enter_func lval f args glob st = [(st,Dom.top ())]
  let leave_func lval f args glob st1 st2 = Dom.top ()
  let special_fn lval f args glob st = [Dom.top ()]
  let fork       lval f args glob st = []
  
  let eval_funvar exp glob st = []

end

module Analysis = Multithread.Forward(Spec)
