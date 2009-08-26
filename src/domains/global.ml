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


module type S =
sig
  module Val: Lattice.S
  module Var: (*VarType with type t = Basetype.Variables.t*)
  sig
    include Hashtbl.HashedType
    val pretty_trace: unit -> t -> Pretty.doc
  end

end


module Make (G: Lattice.S) = 
struct 
  module Val = G
  module Var = Basetype.Variables
end

module Prod (G1: S) (G2: S with module Var = G1.Var) =
struct
  module Val = Lattice.Prod (G1.Val) (G2.Val)
  module Var = G1.Var

  (* Function that merges the side_effects by pairing them with the bottom value
   * of the other domain. *)
  let merge_effects gd1 gd2 = 
    let gd1 = List.map (fun (g,x) -> (g,(x,G2.Val.bot ()))) gd1 in
    let gd2 = List.map (fun (g,x) -> (g,(G1.Val.bot (), x))) gd2 in
      gd1 @ gd2

  let split_effects gd =
    let gs, ds = List.split gd in
    let d1s,d2s = List.split ds in
      List.combine gs d1s, List.combine gs d2s
end
