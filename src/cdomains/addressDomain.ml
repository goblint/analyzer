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

type ('a, 'b) offs = [
  | `NoOffset 
  | `Field of 'a * ('a,'b) offs
  | `Index of 'b * ('a,'b) offs
  ] 

module type S = 
sig
  include Lattice.S
  type idx
  type field
  type heap
  
  val from_var: varinfo -> t
  val from_var_offset: (varinfo * (idx,field) offs) -> t
  val to_var: t -> varinfo list
  val get_type: t -> typ

(*  val from_heap: heap -> t*)
(*  val from_offset_heap: (heap * (idx,field) offs) -> t*)
end

module Address (Idx: Printable.S) = 
struct
  open Basetype
  type heap = unit
  type field = fieldinfo
  type idx = Idx.t
  type t = Addr of (varinfo * (field, idx) offs) | NullPtr | StrPtr
  include Printable.Std

  let from_var x = Addr (x, `NoOffset)
  let from_var_offset x = Addr x
  let to_var a =
    match a with
      | Addr (x,_) -> [x]
      | StrPtr
      | NullPtr    -> []

  let get_type_addr (x, ofs) = 
    let unarray t = match t with
      | TArray (t,_,_) -> t
      | _ -> failwith "C'est Unpossible!"
    in let rec find_type t ofs = match ofs with
      | `NoOffset -> t
      | `Field (fld, ofs) -> find_type fld.ftype ofs
      | `Index (idx, ofs) -> find_type (unarray t) ofs
    in
      find_type x.vtype ofs
  
  let get_type x =
    match x with
      | Addr x  -> get_type_addr x
      | StrPtr  -> charPtrType
      | NullPtr -> voidType

  let copy x = x
  let isSimple _  = true

  let short_addr (x, offs) = 
    let rec off_str ofs = 
      match ofs with
        | `NoOffset -> ""
        | `Field (fld, ofs) -> "." ^ CilField.short Goblintutil.summary_length fld ^ off_str ofs
        | `Index (v, ofs) -> "[" ^ Idx.short Goblintutil.summary_length v ^ "]" ^ off_str ofs
    in
      "&" ^ x.Cil.vname ^ off_str offs

  let short _ x = 
    match x with 
      | Addr x  -> short_addr x
      | StrPtr  -> "STRING"
      | NullPtr -> "NULL"

  let toXML_f_addr sf (x,y) = 
    let esc = Goblintutil.escape in
    let typeinf = esc (Pretty.sprint Goblintutil.summary_length (Cil.d_type () x.Cil.vtype)) in
    let info = "id=" ^ string_of_int x.Cil.vid ^ "; type=" ^ typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int (Addr (x,y)))); ("info", info)],[])

  let toXML_f sf x =
    match x with 
      | Addr x  -> toXML_f_addr sf x
      | StrPtr | NullPtr -> Xml.Element ("Leaf", [("text", short max_int x)],[])

  let pretty_f sf () x = Pretty.text (sf max_int x)

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
end

module AddressSet (Idx: Lattice.S) = 
struct 
  module Addr = Address (Idx)
  module Variables = Basetype.Variables
  include SetDomain.ToppedSet (Addr) (struct let topname = "Anywhere" end)
  type heap = unit
  type field = Basetype.CilField.t
  type idx = Idx.t
  type offs = [`NoOffset | `Field of (field * offs) | `Index of (idx * offs)]

  let null_ptr () = singleton Addr.NullPtr
  let str_ptr () = singleton Addr.StrPtr

  let get_type xs = 
    try Addr.get_type (choose xs) 
    with (* WTF? Returns TVoid when it is unknown and stuff??? *)
      | _ -> voidType

  (* The basic strategy for the join and meet operations is to first just take
   * the union and intersection and then collapse the values. (Does the meet
   * operation actually need any of this? Probably not, but who cares...) 
   * The basic thing is to deal with {&a[3]} join {&a[4]} so the set doesn't
   * grow during loops.  *)
  let merge op x y =
    let merge_addr op (v1,ofs1) (v2,ofs2) =
      let rec merge_offs x y = 
        match x,y with
          | `NoOffset, `NoOffset -> `NoOffset
          | `Field (f1,of1), `Field (_,of2) -> `Field (f1, merge_offs of1 of2)
          | `Index (i1,of1), `Index (i2,of2)-> `Index (op i1 i2, merge_offs of1 of2)
          | x, _ -> x
      in
        v1, merge_offs ofs1 ofs2
    in
    match (x,y) with
      | Addr.Addr x,  Addr.Addr y  -> Addr.Addr (merge_addr op x y)
      | _ -> failwith "This should never happen!"

  (* A function to find the addresses that need to be merged. Those that have
   * the same shape.  *)
  let same_mod_idx x y =
    let same_mod_idx_addr (v1,ofs1) (v2,ofs2) = 
      let rec same_offs x y = 
        match x,y with
          | `NoOffset, `NoOffset -> true
          | `Index (_,x), `Index (_,y) -> same_offs x y
          | `Field (f1,x), `Field (f2,y) when Util.equals f1 f2 -> same_offs x y
          | _ -> false
      in
        Variables.equal v1 v2 && same_offs ofs1 ofs2
    in
    match x,y with
      | Addr.Addr x , Addr.Addr y  -> same_mod_idx_addr x y
      | _ -> false

  let merge_idxs op (s:t) : t = 
    let rec f xs acc = 
      if is_empty xs then begin acc 
      end else 
        let x = choose xs in
        let xs = remove x xs in
        let (fit,rest) =  partition (same_mod_idx x) xs in
        let merged = fold (merge op) fit x in
          f rest (add merged acc)
    in 
      try f s (empty ()) with SetDomain.Unsupported _ -> top ()

  let join (s1:t) (s2:t) = merge_idxs Idx.join  (join s1 s2)
  let meet (s1:t) (s2:t) = merge_idxs Idx.meet  (meet s1 s2)

  let from_var x = singleton (Addr.from_var x)
  let from_var_offset x = singleton (Addr.from_var_offset x)
  let to_var x = List.concat (List.map Addr.to_var (elements x))
end
