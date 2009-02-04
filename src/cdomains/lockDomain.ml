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

module Addr = ValueDomain.Addr
module Equ = MusteqDomain.Equ
module ID = ValueDomain.ID

open Cil
open Pretty

module Lockset =
struct
  module ReverseAddrSet = SetDomain.ToppedSet (Addr) 
                        (struct let topname = "All mutexes" end)
                    
  module AddrSet = Lattice.Reverse (ReverseAddrSet)

  include AddrSet

  let rec concrete_offset offs =
   match offs with
     | `NoOffset -> true
     | `Field (x,y) -> concrete_offset y
     | `Index (x,y) -> 
         if !Goblintutil.regions then 
           ID.equal x (ID.of_int Goblintutil.inthack) 
         else 
           ID.is_int x && concrete_offset y

  let rec may_be_same_offset of1 of2 =
    match of1, of2 with
      | `NoOffset , `NoOffset -> true
      | `Field (x1,y1) , `Field (x2,y2) -> x1.fcomp.ckey = x2.fcomp.ckey && may_be_same_offset y1 y2
      | `Index (x1,y1) , `Index (x2,y2) 
        -> (not (ID.is_int x1) || not (ID.is_int x2)) 
        || ID.equal x1 x2 && may_be_same_offset y1 y2
      | _ -> false

  let add addr set = 
   match (Addr.to_var_offset addr) with
     | [(_,x)] when concrete_offset x -> ReverseAddrSet.add addr set
     | _ -> set

  let remove addr set = 
    let collect_diff_varinfo_with (vi,os) addr =
      match (Addr.to_var_offset addr) with
        | [(v,o)] when vi.vid == v.vid -> not (may_be_same_offset o os)
        | [(v,o)] when vi.vid != v.vid -> true
        | _ -> false
    in
   match (Addr.to_var_offset addr) with
     | [(_,x)] when concrete_offset x -> ReverseAddrSet.remove addr set
     | [x] -> ReverseAddrSet.filter (collect_diff_varinfo_with x) set
     | _   -> AddrSet.top ()

  let empty = ReverseAddrSet.empty
  let is_empty = ReverseAddrSet.is_empty
end

module LocksetEqu = 
struct
  module P = MusteqDomain.EquAddr
  module S = SetDomain.ToppedSet (P) (struct let topname = "All mutexes" end)
  include Lattice.Reverse (S)
  let empty = S.empty
  let is_empty = S.is_empty
  let add (v,fd) eq (s:t): t = 
    let others = Equ.other_addrs (v,fd) eq in
      List.fold_left (fun s vfd -> S.add vfd s) s others
  let remove x = S.filter (fun (y,f) -> not (Basetype.Variables.equal x y || Lval.Fields.occurs x f))
  let kill x = S.filter (fun (y,f) -> not (Lval.Fields.occurs x f))
  let kill_vars vars st = List.fold_right kill vars st
  let elements = S.elements
  let choose = S.choose

end
