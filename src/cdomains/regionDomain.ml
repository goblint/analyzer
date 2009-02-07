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

module V = Basetype.Variables
module B = Printable.UnitConf (struct let name = "*fresh*" end) 
module F = Lval.Fields

module VF = 
struct
  include Printable.ProdSimple (V) (F)
  let short w (v,fd) = 
    let v_str = V.short w v in let w = w - String.length v_str in
    let fd_str = F.short w fd in
      v_str ^ fd_str
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  (* Indicates if the two var * offset pairs should collapse or not. *)
  let collapse (v1,f1) (v2,f2) = V.equal v1 v2 && F.collapse f1 f2
  let leq (v1,f1) (v2,f2) = V.equal v1 v2 && F.leq f1 f2
  (* Joins the fields, assuming the vars are equal. *)
  let join (v1,f1) (v2,f2) = (v1, F.join f1 f2)
  let is_glob (v,f) = v.vglob
end

module VFB = 
struct
  include Printable.Either (VF) (B)

  let collapse (x:t) (y:t): bool = 
    match x,y with
      | `Right (), `Right () -> true
      | `Right (), _ | _, `Right () -> false
      | `Left x, `Left y -> VF.collapse x y

  let leq x y = 
    match x,y with
      | `Right (), `Right () -> true
      | `Right (), _ | _, `Right () -> false
      | `Left x, `Left y -> VF.leq x y

  let join x y: t = 
    match x,y with
      | `Right (), _ | _, `Right () -> `Right ()
      | `Left x, `Left y -> `Left (VF.join x y)
end

module RS = PartitionDomain.Set (VF)
module RB = PartitionDomain.Set (VFB)

module P = PartitionDomain.Make  (RS)
module M = MapDomain.MapBot (VF) (RS)

module Reg = 
struct 
  include Lattice.Prod (P) (M) 
  type elt = VF.t

  let closure p m = M.map (P.closure p) m

  let is_global (v,fd) = v.vglob

  let remove v (p,m) = p, M.remove (v,[]) m
  let remove_vars (vs: varinfo list) (cp:t): t = 
    List.fold_right remove vs cp

  let kill_vars vars st = st

  let update x rval st = st

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
        | (Var x, offs) -> Some (deref, (x, F.listify offs))
        | (Mem exp,  _) -> eval_rval true exp
    in
      eval_rval false exp

  let assign (lval: lval) (rval: exp) (st: t): t =
(*    let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in *)
    if isPointerType (typeOf rval) then begin
      match eval_exp (Lval lval), eval_exp rval with
        | Some (deref_x, x), Some (deref_y,y) ->
            if VF.equal x y then st else 
              let (p,m) = st in begin
                match is_global x, deref_x, is_global y with
                  | false, false, true  -> 
                      p, M.add x (P.closure p (P.S.singleton y)) m
                  | false, false, false -> 
                      p, M.add x (M.find y m) m
                  | false, true , true ->
                      let p = P.add (P.S.join (M.find x m) (P.S.singleton y)) p in
                        p, closure p m
                  | false, true , false ->
                      let p = P.add (P.S.join (M.find x m) (M.find y m)) p in
                        p, closure p m
                  | true , _    , true  -> 
                      let p = P.add (P.S.join (P.S.singleton x) (P.S.singleton y)) p in
                        p, closure p m
                  | true , _    , false  -> 
                      let p = P.add (P.S.join (P.S.singleton x) (M.find y m)) p in
                        p, closure p m
              end
        | _ -> st
    end else if isIntegralType (typeOf rval) then begin
      match lval with 
        | Var x, NoOffset -> update x rval st
        | _ -> st
    end else 
      match eval_exp (Lval lval) with 
        | Some (false, (x,_)) -> remove x st
        | _ -> st

  let related_globals (deref_vfd: eval_t) (p,m: t): elt list = 
    match deref_vfd with
      | Some (true, vfd) -> RS.elements (
          if is_global vfd then 
            P.find_class vfd p
          else 
            M.find vfd m)
      | Some (false, vfd) -> 
          if is_global vfd then [vfd] else []
      | None -> Messages.warn "Access to unknown address could be global"; [] 
end
