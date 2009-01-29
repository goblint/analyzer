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

module type S = 
sig
  include Lattice.S
  type idx
  type field
  
  val from_var: varinfo -> t
  val from_var_offset: (varinfo * (idx,field) Lval.offs) -> t
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  val get_type: t -> typ
end

module AddressSet (Idx: Lattice.S) = 
struct 
  module Addr = Lval.Normal (Idx)
  include SetDomain.ToppedSet (Addr) (struct let topname = "Anywhere" end)
  
  type field = Addr.field
  type idx = Idx.t
  type offs = [`NoOffset | `Field of (field * offs) | `Index of (idx * offs)]

  let null_ptr () = singleton (Addr.null_ptr ())
  let str_ptr () = singleton (Addr.str_ptr ())

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
          | `Field (f1,x), `Field (f2,y) when Util.equals f1 f2 -> same_offs x y
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

  let join (s1:t) (s2:t) = merge_idxs Idx.join  (join s1 s2)
  let meet (s1:t) (s2:t) = merge_idxs Idx.meet  (meet s1 s2)

  let from_var x = singleton (Addr.from_var x)
  let from_var_offset x = singleton (Addr.from_var_offset x)
  let to_var_may x = List.concat (List.map Addr.to_var_may (elements x))
  let to_var_must x = List.concat (List.map Addr.to_var_must (elements x))
  let to_var_offset x = List.concat (List.map Addr.to_var_offset (elements x))
  
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

module Fields = 
struct
  module F = Printable.Either (Basetype.CilField) (Basetype.CilExp)
  module Ofs = struct
    include Printable.Liszt (F)
    let rec short w x = match x with
      | [] -> ""
      | (`Left x :: xs) -> "." ^ Basetype.CilField.short w x ^ short w xs
      | (`Right x :: xs) -> "[" ^ Basetype.CilExp.short w x ^ "]" ^ short w xs

    let toXML m = toXML_f short m
    let pretty () x = pretty_f short () x
  end
  include Lattice.Fake (Ofs)

  let rec prefix x y = match x,y with
    | (x::xs), (y::ys) when F.equal x y -> prefix xs ys
    | [], ys -> Some ys
    | _ -> None

  let append x y: t = x @ y

  let rec listify ofs: t = 
    match ofs with 
      | NoOffset -> []
      | Field (x,ofs) -> `Left x :: listify ofs
      | Index (i,ofs) -> `Right i :: listify ofs

  let rec to_offs (ofs:t) tv = match ofs with 
    | (`Left x::xs) -> `Field (x, to_offs xs tv)
    | (`Right x::xs) -> `Index (tv, to_offs xs tv)
    | [] -> `NoOffset

  let rec occurs v fds = match fds with 
    | (`Left x::xs) -> occurs v xs 
    | (`Right x::xs) -> Basetype.CilExp.occurs v x || occurs v xs
    | [] -> false

  let rec occurs_where v (fds: t): t option = match fds with 
    | (`Right x::xs) when Basetype.CilExp.occurs v x -> Some []
    | (x::xs) -> (match occurs_where v xs with None -> None | Some fd -> Some (x :: fd))
    | [] -> None

  let rec replace x exp ofs = 
    let f o = match o with
      | `Right e -> `Right (Basetype.CilExp.replace x exp e)
      | x -> x
    in 
      List.map f ofs
end

module V = Basetype.Variables
module F = Fields

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
(*        | `Right _ :: fd1, `Right _ :: fd2 -> Some (List.rev o) *)
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

  let eval_exp exp: (bool * elt) option = 
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
        | (Var x, offs) -> Some (deref, (x,Fields.listify offs))
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

  let related_globals (vfd: elt) (st: t): elt list = 
    let is_global (v,fd) = v.vglob in
    let set = SS.find_class vfd (snd st) in
      match set with 
        | Some set -> SS.S. elements (SS.S.filter is_global set)
        | None -> if is_global vfd then [vfd] else []
end
