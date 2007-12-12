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

(** A simple worklist solver *)
module Make (Var: Hashtbl.HashedType) (Dom: Lattice.S) = 
struct
  module HT = Hashtbl.Make(Var)
  type 'a table    = 'a HT.t
  type variable    = Var.t
  type domain      = Dom.t
  type assignment  = Var.t -> Dom.t
  type assignment' = domain table 
  type rhs         = assignment -> Dom.t
  type lhs         = Var.t
  type equation    = lhs * rhs
  type system      = lhs -> rhs
  type solution    = assignment'

  let solve system xs =
    let sigma = HT.create 113 in
    let infl = HT.create 113 in
    let worklist = ref xs in

    let init x = 
      HT.add sigma x (Dom.bot ());
      HT.add infl x [];
      worklist := x :: !worklist in 

    let eval x v =
      if not (HT.mem sigma v) then init v;
      HT.replace infl x (v :: HT.find infl x);
      HT.find sigma v in

    let _ = List.iter init xs in

      while !worklist != [] do
        let x = List.hd !worklist in
        let _ = worklist := List.tl !worklist in
        let (oldval: Dom.t) = HT.find sigma x in
        let (calcval: Dom.t) = system x (eval x) in
        let newval = Dom.join oldval calcval in
          if not (Dom.equal oldval newval) then begin
            HT.replace sigma x newval;
            worklist := HT.find infl x @ !worklist;
          end
      done;
      sigma

end
