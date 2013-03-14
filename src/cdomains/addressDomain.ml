open Cil
open Pretty

module GU = Goblintutil

module type S = 
sig
  include Lattice.S
  type idx
  type field
  
  val from_var: varinfo -> t
  val from_var_offset: (varinfo * (idx,field) Lval.offs) -> t
  val to_var_offset: t -> (varinfo * (idx,field) Lval.offs) list
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  val get_type: t -> typ
end

module AddressSet (Idx: Lattice.S) = 
struct 
  module Addr = Lval.NormalLat (Idx)
  include SetDomain.MacroSet (Addr) (struct let topname = "Anywhere" end)
  
  type field = Addr.field
  type idx = Idx.t
  type offs = [`NoOffset | `Field of (field * offs) | `Index of (idx * offs)]

  let null_ptr () = singleton (Addr.null_ptr ())
  let str_ptr () = singleton (Addr.str_ptr ())
  let safe_ptr () = singleton (Addr.safe_ptr ())
  let unknown_ptr () = singleton (Addr.unknown_ptr ())
  let is_unknown x = cardinal x = 1 && Addr.is_unknown (choose x)

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
    match (Addr.to_var_offset x, Addr.to_var_offset y) with
      | [x],[y]  -> Addr.from_var_offset (merge_addr op x y)
      | _ -> failwith "This should never happen!"

  (* A function to find the addresses that need to be merged. Those that have
   * the same shape.  *)
  let same_mod_idx x y =
    let same_mod_idx_addr (v1,ofs1) (v2,ofs2) = 
      let rec same_offs x y = 
        match x,y with
          | `NoOffset, `NoOffset -> true
          | `Index (_,x), `Index (_,y) -> same_offs x y
          | `Field (f1,x), `Field (f2,y) when f1.fcomp.ckey=f2.fcomp.ckey && f1.fname=f2.fname -> same_offs x y
          | _ -> false
      in
        v1.vid = v2.vid && same_offs ofs1 ofs2
    in
    match Addr.to_var_offset x, Addr.to_var_offset y with
      | [x],[y]  -> same_mod_idx_addr x y
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

  let join (s1:t) (s2:t) = merge_idxs Idx.join (join s1 s2)
  let meet (s1:t) (s2:t) = merge_idxs Idx.meet  (meet s1 s2)
  let leq (s1:t) (s2:t) = match (s1,s2) with
    | _, All -> true
    | All, _ -> false
    | Set s1, Set s2 -> S.for_all (fun x -> S.exists (Addr.leq x) s2) s1

  let from_var x = singleton (Addr.from_var x)
  let from_var_offset x = singleton (Addr.from_var_offset x)
  let to_var_may x = List.concat (List.map Addr.to_var_may (elements x))
  let to_var_must x = List.concat (List.map Addr.to_var_must (elements x))
  let to_var_offset x = List.concat (List.map Addr.to_var_offset (elements x))

  (* strings *)
  let from_string x = singleton (Addr.from_string x)
  let to_string x = List.concat (List.map Addr.to_string (elements x))
  
  (* add an & in front of real addresses *)
  let short_addr w a =
    match Addr.to_var a with
      | [_] -> "&" ^ Addr.short w a
      | _ -> Addr.short w a

  let pretty_f w () x = 
    try
      let elts = elements x in
      let content = List.map (Addr.pretty_f short_addr ()) elts in
      let rec separate x =
        match x with
          | [] -> []
          | [x] -> [x]
          | (x::xs) -> x ++ (text ", ") :: separate xs
      in 
      let separated = separate content in
      let content = List.fold_left (++) nil separated in
         (text "{") ++ content ++ (text "}") 
    with SetDomain.Unsupported _ -> pretty_f w () x

  let short w x : string = 
    try
      let usable_length = w - 5 in
      let all_elems : string list = List.map (short_addr usable_length) (elements x) in
        Printable.get_short_list "{" "}" usable_length all_elems 
    with SetDomain.Unsupported _ -> short w x

  let toXML_f sf x = 
    try
      let esc = Goblintutil.escape in
      let elems = List.map Addr.toXML (elements x) in
        Xml.Element ("Node", [("text", esc (sf max_int x))], elems)
    with SetDomain.Unsupported _ -> toXML_f sf x

  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x
  
end
