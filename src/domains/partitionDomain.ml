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

open Pretty

module type Collapse = sig
  include Printable.S
  val collapse: t -> t -> bool
  val leq: t -> t -> bool
  val join: t -> t -> t
end

module Set (S: Collapse) =
struct
  include SetDomain.Make (S)

  let leq s1 s2 = 
    let p vf1 = exists (fun vf2 -> S.leq vf1 vf2) s2 in
      for_all p s1

  let join (s1:t) (s2:t): t = 
    (* Ok, so for each element vf2 in s2, we check in s1 for elements that
     * collapse with it and join with them. These are put in res and removed
     * from s1 as we don't need to compare with them anymore. *)
    let f vf2 (s1,res) = 
      let (s1_match, s1_rest) = partition (fun vf1 -> S.collapse vf1 vf2) s1 in
      let el = fold S.join s1_match vf2 in
        (s1_rest, add el res)
    in
    let (s1', res) = fold f s2 (s1, empty ()) in
      union s1' res

  let collapse (s1:t) (s2:t): bool = 
    let f vf2 res = 
      res || exists (fun vf1 -> S.collapse vf1 vf2) s1
    in
      fold f s2 false

  let add e s = join s (singleton e)
end

module type CollapseSet = sig
  include SetDomain.S
  val collapse: t -> t -> bool
end

module Make (S: CollapseSet) = 
struct
  include SetDomain.Make (S)
  module S = S
  type set = S.t
  type elem = S.elt

  let short w _ = "Partitions"
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  let leq x y =
    for_all (fun p -> exists (S.leq p) y) x

  let join xs ys =
    let f (x: set) (zs: t): t = 
      let (joinem, rest) = partition (S.collapse x) zs in
      let joined = fold S.join joinem x in
        add joined rest
    in
      fold f xs ys

  let meet xs ys =
    let f (x: set) (zs: t): t = 
      let p z = not (S.is_empty (S.inter x z)) in
      let joinem = filter p ys in
      let joined = fold S.inter joinem x in
        if S.is_empty joined then zs else add joined zs
    in
      fold f xs (empty ())

  let find_class (x:elem) (p:t): set = 
    let s = S.singleton x in try 
      choose (filter (S.collapse s) p) 
    with Not_found -> s

  let closure (p:t) (s:set): set =
    let f x res = S.join (find_class x p) res in
      S.fold f s (S.empty ())

  let add (s:set) (p:t): t = join p (singleton s)

end


module SetSet (Base: Printable.S) = 
struct
  module S = SetDomain.Make (Base)
  module E =  SetDomain.ToppedSet (S) (struct let topname = "Top" end)
  include E
  type set = S.t
  type partition = t

  let short w _ = "Partitions"
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  let leq x y = if is_top y then true else if is_top x then false else
    for_all (fun p -> exists (S.leq p) y) x

  let join xs ys = if is_top xs || is_top ys then top () else
    let f (x: set) (zs: partition): partition = 
      let p z = S.is_empty (S.inter x z) in
      let (rest, joinem) = partition p zs in
      let joined = fold S.union joinem x in
        add joined rest
    in
      fold f xs ys

  let meet xs ys = if is_top xs then ys else if is_top ys then xs else
    let f (x: set) (zs: partition): partition = 
      let p z = not (S.is_empty (S.inter x z)) in
      let joinem = filter p ys in
      let joined = fold S.inter joinem x in
        if S.is_empty joined then zs else add joined zs
    in
      fold f xs (empty ())

  let remove x ss = if is_top ss then ss else
    let f (z: set) (zz: partition) = 
      let res = S.remove x z in
        if S.cardinal res > 1 then add res zz else zz
    in
      fold f ss (empty ())

  let add_eq (x,y) ss = if Base.equal x y then ss else
    let myset = S.add y (S.singleton x) in
      join ss (singleton myset)

  let filter f ss = if is_top ss then ss else
    let f (z: set) (zz: partition) = 
      let res = S.filter f z in
        if S.cardinal res > 1 then add res zz else zz
    in
      fold f ss (empty ())
         
  let find_class (x: Base.t) (ss: t): set option = 
    try Some (E.choose (E.filter (S.mem x) ss)) with _ -> None
end

