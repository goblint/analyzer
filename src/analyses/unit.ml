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
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty

module Spec =
struct
  let name = "Unit analysis"
  type context = BS.store
  module LD = Lattice.Unit
  module GD = Global.Make (Lattice.Unit)

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

  let assign lval rval (st,c,gl) = (st,[])

  let branch exp tv (st,c,gl) = (st,[])

  let return exp fundec (st,c,gl) = (st,[])

  let body f (st,c,gl) = (st, [])


  let special f arglist (st,c,gl) = (st,[])

  let combine lval f args (fun_st: domain) (st,c,gl: trans_in) = (fun_st, [])

  let entry f args st = ([],[])

  let es_to_string f es = f.svar.vname
  let init () = ()
  let finalize () = ()
  let postprocess_glob g v = ()

end

module Context = Compose.ContextSensitive (BS) (Spec)
module SimpleAnalysis = Multithread.Forward(Context)

module Path = Compose.PathSensitive (BS) (Spec)
module Analysis = Multithread.Forward(Path)
