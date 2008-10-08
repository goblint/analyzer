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
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty

module Owner = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "Entire heap" end)

module OwnerClass = 
struct
  include Lattice.Prod (Owner) (SetDomain.Make (Basetype.Variables))
end

module Fields = 
struct
  module Ofs = struct
    include Printable.Liszt (Basetype.CilField)
    let short _ x = 
      let elems = List.map (Basetype.CilField.short max_int) x in
        match elems with 
          | [] -> ""
          | e -> "." ^ String.concat "." elems
    let toXML m = toXML_f short m
    let pretty () x = pretty_f short () x
  end
  include Lattice.Fake (Ofs)

  let rec prefix x y = match x,y with
    | (x::xs), (y::ys) when Basetype.CilField.equal x y -> prefix xs ys
    | [], ys -> Some ys
    | _ -> None
end

module Equ = 
struct
  module V = Basetype.Variables
  module P = Printable.ProdSimple (V) (V)
  module PMap = MapDomain.PMap (P) (Fields)
  include MapDomain.MapTop (P) (Fields)

  let exists x m = try PMap.find x m; true with | Not_found -> false

  let add_old = add
  let rec add (x,y) fd d =
    if V.equal x y || exists (x,y) d then d else
      let add_closure (x,y) fd d = 
        let f (x',y') fd' acc =
          if V.equal y y' then 
            match Fields.prefix fd fd' with
              | Some rest -> add (x',x) rest acc
              | None -> match Fields.prefix fd' fd with
                  | Some rest -> add (x,x') rest acc
                  | None -> acc
          else acc
        in
        let d = fold f d d in
          add_old (x,y) fd d
      in
        if fd = [] then add_closure (y,x) [] (add_closure (x,y) [] d)
        else add_closure (x,y) fd d

  let kill x d = 
    let f (y,z) fd acc = 
      if V.equal x y || V.equal x z then remove (y,z) acc else acc
    in
      fold f d d 

  let add_eq (x,y) d = add (x,y) [] d
  let toXML_f sf mapping =
    let esc = Goblintutil.escape in
    let f ((v1,v2: key), (fd: value)) = 
      let w = Goblintutil.summary_length - 4 in
      let v1_str = V.short w v1 in let w = w - String.length v1_str in
      let v2_str = V.short w v2 in let w = w - String.length v2_str in
      let fd_str = Fields.short w fd in
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
      dok ++ dprintf "%a = %a %a\n" V.pretty v1 V.pretty v2 Fields.pretty st in
    let content () = fold f mapping nil in
      dprintf "@[%s {\n  @[%t@]}@]" (short 60 mapping) content

  let short _ _ = "Equalities"

  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x
end

module Spec =
struct
  exception Top
  let name = "Equalities"
  type context = BS.store
  module LD = Equ 
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
  let return_var = 
    let myvar = makeVarinfo false "RETURN" voidType in
      myvar.vid <- -99;
      myvar

  let assign lval rval (st,c,gl) = 
    let rec listify ofs  = 
      match ofs with 
        | NoOffset -> []
        | Field (x,ofs) -> x :: listify ofs
        | _ -> M.bailwith "Indexing not supported here!"
    in
    let st = match lval with
      | Var x, NoOffset -> begin 
          let st = LD.kill x st in
            match rval with
              | Lval (Var y, NoOffset) -> LD.add_eq (x,y) st 
              | AddrOf (Mem (Lval (Var y, NoOffset)),  ofs) -> 
                  LD.add (x,y) (listify ofs) st 
              | _ -> st
        end
      | _ -> st
    in (st,[])

  let branch exp tv (st,c,gl) = (st,[])

  let return exp fundec (st,c,gl) = (st,[])

  let body f (st,c,gl) = (st, [])


  (* Don't forget to add some annotation for the base analysis as well,
   * otherwise goblint will warn that the function is unknown. *)
  let special f arglist (st,c,gl) = (st,[])

  let combine lval f args (fun_st: domain) (st,c,gl: trans_in) = (fun_st, [])

  let entry f args st = ([],[])

  let es_to_string f es = f.svar.vname
  let init () = ()
  let finalize () = ()
  let postprocess_glob g v = ()

end

(*module Trivial = Spec*)
module Context = Compose.ContextSensitive (BS) (Spec)
module Path = Compose.PathSensitive (BS) (Spec)

(*module Analysis = Multithread.Forward(Path)*)
(*module SimpleAnalysis = Multithread.Forward(Context)*)
module Analysis = Multithread.Forward(Context)
module SimpleAnalysis = Multithread.Forward(Context)
