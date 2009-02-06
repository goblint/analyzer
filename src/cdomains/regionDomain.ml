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
open MusteqDomain

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

module Reg = 
struct 
  module SS = SetSet (EquAddr)
  module S  = struct
    include SetDomain.Make (EquAddr)
    let short w _ = "Collapsed"
    let toXML s  = toXML_f short s
    let pretty () x = pretty_f short () x
  end
  (* Note that SS is a topped set, while S has no top; this can cause problems
   * that are extremely hard to find... *)

  include Lattice.Prod (S) (SS)
  type elt = EquAddr.t
  type set = S.t
  type equiv = SS.t

  let problematic (v1,fd1) (v2,fd2) = 
    (* Check if two offsets conflict, and return common prefix. *)
    let rec offs fd1 fd2 o =
      match fd1,fd2 with
        | `Right _ :: _, _ | _, `Right _ :: _ -> Some (List.rev o)
(*        | `Right i1 :: fd1, `Right i2 :: fd2 when Basetype.CilExp.equal i1 i2 -> *)
(*            offs fd1 fd2 (`Right i1 :: o)*)
        | `Left f1 :: fd1, `Left f2 :: fd2 when Basetype.CilField.equal f1 f2 -> 
            offs fd1 fd2 (`Left f1 :: o)
        | _ -> None
    in
      if V.equal v1 v2 then begin
        match offs fd1 fd2 [] with
          | Some fd -> Some (v1,fd)
          | None -> None
      end else None

  exception Found of elt
  (* Function to find a problematic prefix. *)
  let find_problem (c,ss: t) = 
    (* Look inside a class for two conflicting elements. *)
    let f (s: set) =
      (* Check if a new element conflicts with what we have visited so far. *)
      let f (e: elt) (seen: set): set = 
        (* As soon as we find a problematic pair, we escape out of the loops. *)
        let check vf1 vf2 = 
          match problematic vf1 vf2 with
            | Some vf -> raise (Found vf)
            | None -> ()
        in
          (* Check if e conflicts with any of the elements we have seen. *)
          S.iter (check e) seen;
          (* If not, then we add it to the seen elements. *)
          S.add e seen
      in
      let _ = SS.S.fold f s c in ()
    in
      SS.iter f ss

  let is_collapsed v (c,p) = S.mem v c

  (* Function for collapsing an array { v } and joining all equivalent classes
   * related to any of its elements. *)
  let rec collapse (vf: elt) (c,p: t): t = 
    if !Goblintutil.die_on_collapse then failwith "An array has collapsed!";
    let c = S.add vf c in
    let p = 
      (* We need a function that looks into each set z of our partitions, and
       * collect those that contain vf into a single class s and the other
       * partitions ss we leave alone. *)
      let f (z: set) (s,ss) = 
        (* We first split the elements in z into two sets, those with a
         * prefix vf and those without it. *)
        let f vf' = match EquAddr.prefix vf vf' with Some _ -> true | _ -> false in
        let (x,y) = SS.S.partition f z in
          if SS.S.is_empty x then 
            (* If this partition does not contain something that was collapsed,
             * then we add it to the undisturbed ones. *)
            (s, SS.add y ss) 
          else 
            (* If something here needs to be collapsed, then we take the other
             * elements and throw it into the class that collects everything
             * related to the array. *)
            (SS.S.union y s, ss)
      in
      let (s,ss) = SS.fold f p (SS.S.empty (), SS.empty ()) in
        SS.add (SS.S.add vf s) ss
    in normalize (c,p)
  (* And we iterate until there are nothing more to collapse. *)
  and normalize cp = 
    try find_problem cp; cp with Found v -> collapse v cp

  let collapse_all (vs: set) (cp: t): t = S.fold collapse vs cp

  (* We join these classes by collapsing what is uncollapsed in the other, and
   * then normalizing. *)
  let join (c1,_ as cp1: t) (c2,_ as cp2: t): t =
    let cp1 = collapse_all (S.diff c2 c1) cp1 in
    let cp2 = collapse_all (S.diff c1 c2) cp2 in
      normalize (join cp1 cp2)

  (* Here, we collapse the array { v }, without joiing equivalence classes.
   * Probably only needed for the leq function. *)
  let flatten v (c,p) = 
    let f (v,fd) = match fd with 
      | [`Right _] -> (v, [])
      | x -> v,x
    in c, SS.map (SS.S.map f) p

  let flatten_all vs cp = S.fold flatten vs cp

  let leq (c1,_ as cp1: t) (c2,_ as cp2: t): bool = 
    let cp1 = flatten_all (S.diff c2 c1) cp1 in
      leq cp1 cp2


  let add_eq (x,y) (c,p) = 
    match problematic x y with
      | Some vf -> collapse vf (c,p)
      | None -> normalize (c, SS.add_eq (x,y) p)

  let remove x (c,p) = c, SS.remove x p

  let remove_vars (vs: varinfo list) (cp:t): t = 
    let f v (c,p) = 
      let not_v (v',_) = not (V.equal v v') in
        S.filter not_v c, SS.filter not_v p
    in
      List.fold_right f vs cp

  let keep_only (vs: varinfo list) (c,p:t): t = 
    let eq_v (pv,_) = pv.vglob || List.mem pv vs  in
      S.filter eq_v c, SS.filter eq_v p

  let kill x (c,p: t): t = 
    let vars =
      let f (v,fd) vs = 
        match F.occurs_where x fd with
          | Some fs -> S.add (v,fs) vs
          | None -> vs
      in
      let f s vs = SS.S.fold f s vs in
        SS.fold f p (S.empty ())
    in
      collapse_all vars (c,p)

  let kill_vars vars st = List.fold_right kill vars st

  let replace x exp (c,st): t = 
    let f (v,fd) = v, F.replace x exp fd in
      (c,SS.map (SS.S.map f) st)


  let update x rval st =
    match rval with 
      | Lval (Var y, NoOffset) when V.equal x y -> st
      | BinOp (PlusA, Lval (Var y, NoOffset), (Const _ as c), typ) when V.equal x y -> 
          replace x (BinOp (MinusA, Lval (Var y, NoOffset), c, typ)) st
      | BinOp (MinusA, Lval (Var y, NoOffset), (Const _ as c), typ) when V.equal x y -> 
          replace x (BinOp (PlusA, Lval (Var y, NoOffset), c, typ)) st
      | _ -> kill x st

  type eval_t = (bool * elt) option
  let eval_exp exp: eval_t = 
    let rec eval_rval deref rval =
      match rval with
        | Lval lval -> eval_lval deref lval 
        | AddrOf lval -> eval_lval deref lval
        | CastE (typ, exp) -> eval_rval deref exp
        | BinOp (MinusPI, p, i, typ) 
        | BinOp (PlusPI, p, i, typ) 
        | BinOp (IndexPI, p, i, typ) -> eval_rval deref p
        | _ -> None
    and eval_lval deref lval =
      match lval with 
        | (Var x, offs) -> Some (deref, (x,F.listify offs))
        | (Mem exp,  _) -> eval_rval true exp
    in
      eval_rval false exp

  let assign (lval: lval) (rval: exp) (c,p as st: t): t =
(*    let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in *)
    let st' = st in
    let st = match eval_exp (Lval lval) with 
      | Some (false, x) -> remove x st
      | _ -> st
    in
    if isPointerType (typeOf rval) then begin
      match eval_exp (Lval lval), eval_exp rval with
        | Some (_, x), Some (_,y) ->
            if EquAddr.equal x y then st' else add_eq (x, y) st
        | _ -> st
    end else if isIntegralType (typeOf rval) then begin
      match lval with 
        | Var x, NoOffset -> update x rval st
        | _ -> st
    end else st

  let related_globals (deref_vfd: eval_t) (st: t): elt list = 
    let is_global (v,fd) = v.vglob in
    match deref_vfd with
      | Some (true, vfd) -> 
          let set = SS.find_class vfd (snd st) in begin
            match set with 
              | Some set -> SS.S.elements (SS.S.filter is_global set)
              | None -> if is_global vfd then [vfd] else []
          end
      | Some (false, vfd) -> 
          if is_global vfd then [vfd] else []
      | None -> Messages.warn "Access to unknown address could be global"; [] 
end
