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
open Cil

module type BaseSpec =
sig
  module LD: Lattice.S
  module GD: Global.S
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  val postprocess_glob: GD.Var.t -> GD.Val.t -> unit
  include Analyses.Spec 
    with type domain = LD.t
     and type transfer = LD.t * glob_fun -> LD.t * glob_diff
     and type trans_in = LD.t * glob_fun
  val spawn: varinfo -> exp list -> trans_in -> (varinfo * domain) list
end

module type UserSpec =
sig
  module LD: Lattice.S
  module GD: Global.S
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  val postprocess_glob: GD.Var.t -> GD.Val.t -> unit
  type context
  include Analyses.Spec 
    with type domain = LD.t
     and type transfer = LD.t * context * glob_fun -> LD.t * glob_diff
     and type trans_in = LD.t * context * glob_fun
end  

module ContextSensitive (Base: BaseSpec) (User: UserSpec with 
  module GD.Var = Base.GD.Var and 
  type context = Base.domain * Base.glob_fun) = 
struct
  module LD = Lattice.Prod (Base.LD) (User.LD)
  module GD = Global.Prod (Base.GD) (User.GD)

  type domain = LD.t
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  type transfer = LD.t * glob_fun -> LD.t * glob_diff
  type trans_in = LD.t * glob_fun

  (* Function that combines two transfer functions to work over this combined
   * domain, this function should be applied to all the transfer functions... *)
  let combiner (f1,f2) ((st1,st2),gl) = 
    let gf1 x = fst (gl x) in
    let gf2 x = snd (gl x) in
    let (l1,g1) = f1 (st1, gf1) in
    let (l2,g2) = f2 (st2, (st1,gf1), gf2) in
     ((l1,l2), GD.merge_effects g1 g2) 

  let name = Base.name ^ " and " ^ User.name
  let startstate = (Base.startstate, User.startstate)
  let es_to_string f (es,_) = Base.es_to_string f (es)
  let init () = Base.init (); User.init ()
  let postprocess_glob g (v1,v2) = 
    Base.postprocess_glob g v1;
    User.postprocess_glob g v2

  let assign lval exp  = combiner (Base.assign lval exp, User.assign lval exp)
  let branch exp tv = combiner (Base.branch exp tv, User.branch exp tv)
  let body f = combiner (Base.body f, User.body f)
  let return e f = combiner (Base.return e f, User.return e f)

  let entry exp args ((st1,st2),gl: trans_in) =
    let gf1 x = fst (gl x) in
    let (norms,specs) = Base.entry exp args (st1,gf1) in
    let norms_with_st2 = List.map (fun (f,x) -> (f,(x,st2))) norms in
      (norms_with_st2, specs)

  let spawn f args ((st1,st2),gl) =
    let gf1 x = fst (gl x) in
    let norms = Base.spawn f args (st1,gf1) in
      List.map (fun (f,x) -> (f,(x,st2))) norms

  let special f args = combiner (Base.special f args, User.special f args)

  let combine lv f args (fun_st: domain) (((st1,st2),gl) as st) = 
    let (fun_st1,fun_st2) = fun_st in
    combiner (Base.combine lv f args fun_st1, User.combine lv f args fun_st2) st

end

module PathSensitive (Base: BaseSpec) (User: UserSpec with 
  module GD.Var = Base.GD.Var and 
  type context = Base.domain * Base.glob_fun): Multithread.Spec = 
struct
  module S = ContextSensitive (Base) (User)
  module LD = SetDomain.Sensitive (Base.LD) (User.LD)
  module GD = Global.Prod (Base.GD) (User.GD)

  type domain = LD.t
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  type spawn = (Cil.varinfo * LD.t) list -> unit
  type transfer = LD.t * glob_fun -> LD.t * glob_diff
  type trans_in = LD.t * glob_fun

  let name = S.name
  let startstate = LD.singleton (Base.startstate, User.startstate)
  let es_to_string f es = Base.es_to_string f (fst (LD.choose es))
  let init () = S.init ()
  let postprocess_glob = S.postprocess_glob

  let lifting (f: S.transfer): transfer =
    let the_fun (st,gl) =
      let collect_diffs = ref [] in
      let apply_f st acc = 
        try
          let (st,gd) = f (st,gl) in
          let _ = collect_diffs := gd @ !collect_diffs in
            LD.add st acc
        with A.Deadcode -> acc
      in
      let res = LD.fold apply_f st (LD.empty ()) in
        if LD.is_empty res then
          raise A.Deadcode
        else 
          res, !collect_diffs
    in
      the_fun

  let collapse (x: LD.t): LD.elt = 
    let joiner (x,y) (a,b)  = (Base.LD.join x a, User.LD.join y b) in
      LD.fold joiner x (LD.choose x)

  (* These are here because combiner has been redefined, and well include is not
   * inheritence and ocaml is not java, so here it is: *)
  let assign lval exp = lifting (S.assign lval exp)
  let branch exp tv = lifting (S.branch exp tv)
  let body f = lifting (S.body f)
  let return e f = lifting (S.return e f)

  let combine lv f args fun_st (st,gl) = 
    let collect_diffs = ref [] in
    let each_local (l_st: LD.elt) (acc: LD.t): LD.t = 
      (* Collect each of the fun_st and combine them with the corresonding local
       * state, and then return all of them in the path-sensitive domain. *)
      let each_fun (f_st: LD.elt) acc = 
        try
          let (nst,gd) = S.combine lv f args f_st (l_st,gl) in
          let _ = collect_diffs := gd @ !collect_diffs in
            LD.add nst acc
        with
          | A.Deadcode -> acc
      in
        LD.join (LD.fold each_fun fun_st (LD.empty ())) acc
    in
    (* The result is obtained by applying our function on each of the initial
     * states. *)
    let res = LD.fold each_local st (LD.empty ()) in
      if LD.is_empty res then
        raise A.Deadcode
      else 
        res, !collect_diffs

  let entry exp args (st,gl: trans_in) = 
    let for_each (st1,st2) (s,f) =
      let gf1 x = fst (gl x) in
      let (norms,specs) = Base.entry exp args (st1,gf1) in
      let norms_with_st2 = List.map ( fun (f,x) -> (f, LD.singleton (x,st2)) ) norms in
        (norms_with_st2 @ s, specs @ f)
    in
      LD.fold for_each st ([], [])

  let spawn f args (st,gl: trans_in): (varinfo * domain) list =
    let for_each (st1,st2) =
      let gf1 x = fst (gl x) in
      let norms = Base.spawn f args (st1,gf1) in
        List.map (fun (f,x) -> (f, LD.singleton (x,st2))) norms
    in
    let res = List.map for_each (LD.elements st) in
      List.concat res

  let special f args = lifting (S.special f args)

end
