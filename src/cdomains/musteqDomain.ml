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
module F = Lval.Fields

module EquAddr = 
struct 
  include Printable.ProdSimple (V) (F)
  let short w (v,fd) = 
    let v_str = V.short w v in let w = w - String.length v_str in
    let fd_str = F.short w fd in
      v_str ^ fd_str
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  let prefix (v1,fd1: t) (v2,fd2: t): F.t option = 
    if V.equal v1 v2 then F.prefix fd1 fd2 else None
end

module P = Printable.ProdSimple (V) (V)

module Equ = 
struct
  module PMap = MapDomain.PMap (P) (F)
  include MapDomain.MapTop (P) (F)

  let toXML_f sf mapping =
    let esc = Goblintutil.escape in
    let f ((v1,v2: key), (fd: value)) = 
      let w = Goblintutil.summary_length - 4 in
      let v1_str = V.short w v1 in let w = w - String.length v1_str in
      let v2_str = V.short w v2 in let w = w - String.length v2_str in
      let fd_str = F.short w fd in
      let summary = esc (v1_str ^ " = " ^ v2_str ^ fd_str) in
      let attr = [("text", summary)] in 
        Xml.Element ("Leaf",attr,[])
    in
    let assoclist = fold (fun x y rest -> (x,y)::rest) mapping [] in
    let children = List.rev_map f assoclist in
    let node_attrs = [("text", esc (sf Goblintutil.summary_length mapping));("id","map")] in
      Xml.Element ("Node", node_attrs, children)

  let pretty_f short () mapping = 
    let f (v1,v2) st dok: doc = 
      dok ++ dprintf "%a = %a%a\n" V.pretty v1 V.pretty v2 F.pretty st in
    let content () = fold f mapping nil in
      dprintf "@[%s {\n  @[%t@]}@]" (short 60 mapping) content

  let short _ _ = "Equalities"

  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  let exists x m = try PMap.find x m; true with | Not_found -> false

  let add_old = add
  let rec add (x,y) fd d =
    if V.equal x y || exists (x,y) d then d else
      let add_closure (x,y) fd d = 
        let f (x',y') fd' acc =
          if V.equal y y' then 
            match F.prefix fd fd' with
              | Some rest -> add (x',x) rest acc
              | None -> match F.prefix fd' fd with
                  | Some rest -> add (x,x') rest acc
                  | None -> acc
          else acc
        in
        fold f d (add_old (x,y) fd d)
      in
        if fd = [] then add_closure (y,x) [] (add_closure (x,y) [] d)
        else add_closure (x,y) fd d

  let kill x d = 
    let f (y,z) fd acc = 
      if V.equal x y || V.equal x z || F.occurs x fd then 
        remove (y,z) acc else acc
    in
      fold f d d 

  let kill_vars vars st = List.fold_right kill vars st

  (* Function to find all addresses equal to { vfd } in { eq }. *)
  let other_addrs vfd eq = 
    let rec helper (v,fd) addrs = 
      if List.exists (EquAddr.equal (v,fd)) addrs then addrs else
        let f (x,y) fd' acc = 
          if V.equal v x then
            helper (y, F.append fd' fd) acc
          else if V.equal v y then 
            (match F.prefix fd' fd with
               | Some rest -> helper (x,rest) acc
               | None -> acc)
          else acc
        in
          fold f eq ((v,fd) :: addrs)
    in
      helper vfd []

  let eval_rv rv: EquAddr.t option = 
    match rv with 
      | Lval (Var x, NoOffset) -> Some (x, [])
      | AddrOf (Var x, ofs)
      | AddrOf (Mem (Lval (Var x, NoOffset)),  ofs) -> Some (x, F.listify ofs)
      | _ -> None

  let eval_lv lv = 
    match lv with 
      | Var x, NoOffset -> Some x
      | _ -> None

  let add_eq (x,y) d = add (x,y) [] d

  let assign lval rval st =
    match lval with
      | Var x, NoOffset -> begin 
          let st = kill x st in
          (* let _ = printf "Here: %a\n" (printExp plainCilPrinter) rval in *)
            match rval with
              | Lval (Var y, NoOffset) -> add_eq (x,y) st 
              | AddrOf (Var y, ofs) -> add (x,y) (F.listify ofs) st 
              | AddrOf (Mem (Lval (Var y, NoOffset)),  ofs) -> 
                  add (x,y) (F.listify ofs) st 
              | _ -> st
        end
      | _ -> st

end
