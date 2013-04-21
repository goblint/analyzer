open Pretty
open Cil

module Exp =
struct
  type t = Cil.exp
  include Printable.Std

  let equal = Expcompare.compareExp
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let classify _ = 0
  let class_name _ = "None" 
  let name () = "Cil expressions"
  
  let pretty = Cil.d_exp
  let short w s = sprint w (Cil.d_exp () s)
  let toXML x = Xml.Element ("Leaf", [("text", Goblintutil.escape (short 80 x))], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
  let rec interesting x =
    match x with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.AlignOfE _
      | Cil.UnOp  _    
      | Cil.BinOp _ -> false
      | Cil.Const _ -> true
      | Cil.AddrOf  (Cil.Var v2,_) 
      | Cil.StartOf (Cil.Var v2,_) 
      | Cil.Lval    (Cil.Var v2,_) -> true
      | Cil.AddrOf  (Cil.Mem e,_) 
      | Cil.StartOf (Cil.Mem e,_) 
      | Cil.Lval    (Cil.Mem e,_)
      | Cil.CastE (_,e)           -> interesting e 
      | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
      
  let  contains_var v e =
    let rec offs_contains o =
      match o with
        | Cil.NoOffset -> false
        | Cil.Field (_,o) -> offs_contains o
        | Cil.Index (e,o) -> cv false e || offs_contains o
    and cv deref e = 
      match e with
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.Const _ 
        | Cil.AlignOfE _ -> false 
        | Cil.UnOp  (_,e,_)     -> cv deref e      
        | Cil.BinOp (_,e1,e2,_) -> cv deref e1 || cv deref e2  
        | Cil.AddrOf  (Cil.Mem e,o) 
        | Cil.StartOf (Cil.Mem e,o) 
        | Cil.Lval    (Cil.Mem e,o) -> cv true e || offs_contains o
        | Cil.CastE (_,e)           -> cv deref e 
        | Cil.Lval    (Cil.Var v2,o) -> v.Cil.vid = v2.Cil.vid || offs_contains o
        | Cil.AddrOf  (Cil.Var v2,o) 
        | Cil.StartOf (Cil.Var v2,o) -> 
          if deref  
          then v.Cil.vid = v2.Cil.vid || offs_contains o 
          else offs_contains o 
        | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in
      cv false e
  
  let contains_field f e =
    let rec offs_contains o =
      match o with
        | Cil.NoOffset -> false
        | Cil.Field (f',o) -> f.Cil.fname = f'.Cil.fname 
        | Cil.Index (e,o) -> cv e || offs_contains o
    and cv e = 
      match e with
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.Const _ 
        | Cil.AlignOfE _ -> false 
        | Cil.UnOp  (_,e,_)     -> cv e      
        | Cil.BinOp (_,e1,e2,_) -> cv e1 || cv e2  
        | Cil.AddrOf  (Cil.Mem e,o) 
        | Cil.StartOf (Cil.Mem e,o) 
        | Cil.Lval    (Cil.Mem e,o) -> cv e || offs_contains o
        | Cil.CastE (_,e)           -> cv e 
        | Cil.Lval    (Cil.Var v2,o) -> offs_contains o
        | Cil.AddrOf  (Cil.Var v2,o) 
        | Cil.StartOf (Cil.Var v2,o) -> offs_contains o 
        | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in
      cv e
      
  let rec is_global_var x =
    match x with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.AlignOfE _ 
      | Cil.UnOp _      
      | Cil.BinOp _ -> None
      | Cil.Const _ -> Some false      
      | Cil.Lval (Cil.Var v,_) -> Some v.Cil.vglob   
      | Cil.Lval (Cil.Mem e,_) -> is_global_var e
      | Cil.CastE (t,e) -> is_global_var e 
      | Cil.AddrOf lval -> Some false  
      | Cil.StartOf lval -> Some false
      | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
  
  let rec conv_offs (offs:(Cil.fieldinfo,Cil.exp) Lval.offs) : Cil.offset =
    match offs with
      | `NoOffset -> Cil.NoOffset
      | `Field (f,o) -> Cil.Field (f, conv_offs o)
      | `Index (e,o) -> Cil.Index (e, conv_offs o)

  let of_clval (v,offs) = Cil.Lval (Cil.Var v, conv_offs offs)
  
  let rec fold_offs c =
    match c with
      | Cil.AddrOf (Cil.Mem e,o) -> begin
          match fold_offs e with
            | Some (v, o') -> Some (v, Cil.addOffset o o')
            | x -> x
          end
      | Cil.AddrOf (Cil.Var v,o)  
      | Cil.Lval (Cil.Var v,o) -> 
          Some (v, o)
      | _ -> None
    
  let rec off_eq x y =
    match x, y with
      | Cil.NoOffset, Cil.NoOffset -> true
      | Cil.Field (f1, o1), Cil.Field (f2, o2) -> f1.Cil.fname = f2.Cil.fname && off_eq o1 o2
      | Cil.Index (e1, o1), Cil.Index (e2, o2) -> simple_eq e1 e2 && off_eq o1 o2
      | _ -> false
  and simple_eq x y =
    match x, y with
      | Cil.Lval (Cil.Var v1,o1)   , Cil.Lval (Cil.Var v2,o2)
      | Cil.AddrOf (Cil.Var v1,o1) , Cil.AddrOf (Cil.Var v2,o2)             
      | Cil.StartOf (Cil.Var v1,o1), Cil.StartOf (Cil.Var v2,o2) 
          -> v1.Cil.vid = v2.Cil.vid && off_eq o1 o2
      | Cil.Lval (Cil.Mem e1,o1)   , Cil.Lval (Cil.Mem e2,o2)
      | Cil.AddrOf (Cil.Mem e1,o1) , Cil.AddrOf (Cil.Mem e2,o2) 
      | Cil.StartOf (Cil.Mem e1,o1), Cil.StartOf (Cil.Mem e2,o2)
          -> simple_eq e1 e2 && off_eq o1 o2
      | _ -> false
    
  let rec replace_base (v,offs) q exp =
    match exp with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.AlignOfE _ 
      | Cil.UnOp _      
      | Cil.BinOp _ 
      | Cil.Const _ 
      | Cil.Lval (Cil.Var _,_) 
      | Cil.AddrOf (Cil.Var _,_)              
      | Cil.StartOf (Cil.Var _,_) -> exp
      | Cil.Lval (Cil.Mem e,o)    when simple_eq e q -> Cil.Lval (Cil.Var v, Cil.addOffset o (conv_offs offs))
      | Cil.Lval (Cil.Mem e,o)                       -> Cil.Lval (Cil.Mem (replace_base (v,offs) q e), o)
      | Cil.AddrOf (Cil.Mem e,o)  when simple_eq e q -> Cil.AddrOf (Cil.Var v, Cil.addOffset o (conv_offs offs))
      | Cil.AddrOf (Cil.Mem e,o)                     -> Cil.AddrOf (Cil.Mem (replace_base (v,offs) q e), o)
      | Cil.StartOf (Cil.Mem e,o) when simple_eq e q -> Cil.StartOf (Cil.Var v, Cil.addOffset o (conv_offs offs))
      | Cil.StartOf (Cil.Mem e,o)                    -> Cil.StartOf (Cil.Mem (replace_base (v,offs) q e), o)
      | Cil.CastE (t,e) -> Cil.CastE (t, replace_base (v,offs) q e)
      | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."

  let rec base_compinfo q exp =
    match exp with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.AlignOfE _ 
      | Cil.UnOp _      
      | Cil.BinOp _ 
      | Cil.Const _ 
      | Cil.Lval (Cil.Var _,_) 
      | Cil.AddrOf (Cil.Var _,_)              
      | Cil.StartOf (Cil.Var _,_) -> None
      | Cil.Lval (Cil.Mem e,Cil.Field (f,_)) when simple_eq e q -> Some f.Cil.fcomp
      | Cil.Lval (Cil.Mem e,o) -> base_compinfo q e
      | Cil.AddrOf (Cil.Mem e,Cil.Field (f,_)) when simple_eq e q -> Some f.Cil.fcomp
      | Cil.AddrOf (Cil.Mem e,o) -> base_compinfo q e
      | Cil.StartOf (Cil.Mem e,Cil.Field (f,_)) when simple_eq e q -> Some f.Cil.fcomp
      | Cil.StartOf (Cil.Mem e,o) -> base_compinfo q e
      | Cil.CastE (t,e) -> base_compinfo q e
      | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
  
  let rec conc i = 
    match i with
      | NoOffset -> true
      | Index (i,o) -> isConstant i && conc o
      | Field (_,o) -> conc o
  
  let rec one_unknown_array_index exp = 
    let rec separate_fields_index o = 
      match o with
        | NoOffset -> None
        | Index (ie,o) -> Some ((fun x -> x),ie,o)
        | Field (f,o) -> 
      match separate_fields_index o with
        | Some (osf, ie,o) -> Some ((fun o -> Field (f,o)), ie, o)
        | x -> x
    in 
    let star = kinteger64 IInt Goblintutil.inthack in
    match exp with
      | Lval (Mem (Lval (Var v, io)),o) when conc o -> 
          begin match separate_fields_index io with
            | Some (osf, ie, io) when conc io -> 
                Some (false, ie, Lval (Mem (Lval (Var v, osf (Index (star,io)))),o))
            | _ ->
                None
          end
      | Lval (Var v, io) -> 
          begin match separate_fields_index io with
            | Some (osf, ie, o) when conc o -> 
                Some (false, ie, Lval (Var v, osf (Index (star,o))))
            | _ ->
                None
          end        
      | AddrOf (Var v, io) -> 
          begin match separate_fields_index io with
            | Some (osf, ie, o) when conc o -> 
                Some (true, ie, AddrOf (Var v, osf (Index (star,o)))) 
            | _ ->
                None
          end                     
      | StartOf (Var v, io) -> 
          begin match separate_fields_index io with
            | Some (osf, ie, o) when conc o -> 
                Some (true, ie, StartOf (Var v, osf (Index (star,o))))
            | _ ->
                None
          end                     
      | AddrOf (Mem e, NoOffset) -> one_unknown_array_index e
      | StartOf (Mem e, NoOffset) -> one_unknown_array_index e
      | CastE (t,e) -> one_unknown_array_index e
      | _ -> None
end

module LockingPattern =
struct
  include Printable.Std
  type t = Exp.t * Exp.t * Exp.t
  
  let equal = Util.equals
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let classify _ = 0
  let class_name _ = "None" 
  let name () = "Per-Element locking triple"
    
  let pretty () (x,y,z) = text "(" ++ Cil.d_exp () x ++ text ", "++ Cil.d_exp () y ++ text ", "++ Cil.d_exp () z ++ text ")"
  let short w (x,y,z) = sprint w (dprintf "(%a,%a,%a)" Cil.d_exp x Cil.d_exp y Cil.d_exp z)
  let toXML x = Xml.Element ("Leaf", [("text", Goblintutil.escape (short 80 x))], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
  type ee = Var of Cil.varinfo
          | Addr
          | Deref
          | Field of Cil.fieldinfo
          | Index of Cil.exp

  let ee_equal x y = 
    match x, y with
      | Var v1, Var v2 -> v1.Cil.vid = v2.Cil.vid
      | Addr, Addr -> true 
      | Deref, Deref -> true
      | Field f1, Field f2 -> f1.Cil.fname = f2.Cil.fname 
      | Index e1, Index e2 -> Exp.simple_eq e1 e2
      | _ -> false
  
  let ee_to_str x = 
    match x with
      | Var v -> v.Cil.vname
      | Addr -> "&"
      | Deref -> "*"
      | Field f -> f.Cil.fname 
      | Index e -> Pretty.sprint 80 (Cil.d_exp () e)
  
  let ees_to_str xs = List.fold_right (fun x xs -> " " ^ (ee_to_str x) ^ xs ) xs ""

  exception NotSimpleEnough

  let rec ees_to_offs = function
	| [] 		-> `NoOffset 	
(*	| Addr :: x ->
	| Deref :: x ->
*)	| Addr :: Deref :: x -> ees_to_offs x
	| Deref :: Addr :: x -> ees_to_offs x
	| Field f :: x -> `Field (f,ees_to_offs x)
	| Index (Cil.Const (CInt64 (i,_,_))) :: x -> `Index (ValueDomain.IndexDomain.of_int i,ees_to_offs x)
	| Index i :: x -> `NoOffset 
	| x  -> raise NotSimpleEnough
  
  let toEl exp = 
    let rec conv_o o =
      match o with
        | Cil.NoOffset -> []
        | Cil.Index (e,o) -> Index e :: conv_o o
        | Cil.Field (f,o) -> Field f :: conv_o o
    in
    let rec helper exp =
      match exp with
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.AlignOfE _ 
        | Cil.UnOp _      
        | Cil.BinOp _ 
        | Cil.StartOf _
        | Cil.Const _ -> raise NotSimpleEnough 
        | Cil.Lval (Cil.Var v, os) -> Var v :: conv_o os  
        | Cil.Lval (Cil.Mem e, os) -> helper e @ [Deref] @ conv_o os
        | Cil.AddrOf (Cil.Var v, os) -> Var v :: conv_o os @ [Addr]
        | Cil.AddrOf (Cil.Mem e, os) -> helper e @ [Deref] @ conv_o os @ [Addr]
        | Cil.CastE (_,e) -> helper e 
        | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in
      try helper exp 
      with NotSimpleEnough -> []
  
  let rec fromEl xs ex =
    match xs, ex with
      | []           ,             _ -> ex      
      | Deref::xs    ,             _ -> fromEl xs (Cil.Lval (Cil.Mem ex, Cil.NoOffset))
      | Var v::xs    ,             _ -> fromEl xs (Cil.Lval (Cil.Var v, Cil.NoOffset)) 
      | Field f::xs  , Cil.Lval lv   -> fromEl xs (Cil.Lval (Cil.Mem (Cil.AddrOf lv), Cil.Field (f, Cil.NoOffset)))
      | Index i::xs  , Cil.Lval lv   -> fromEl xs (Cil.Lval (Cil.Mem (Cil.AddrOf lv), Cil.Index (i, Cil.NoOffset)))
      | Addr::xs     , Cil.Lval lv   -> fromEl xs (Cil.AddrOf lv)
      | _            ,             _ -> raise (Invalid_argument "")
  
  let strip_fields e =
    let rec sf e fs = 
      match e with
        | Field f :: es -> sf es (Field f::fs)
        | Deref :: Addr :: es -> sf es fs
        | _ -> e, fs
    in
    let el, fs = sf (List.rev e) [] in
    List.rev el, fs
  
  let from_exps a l : t option =
    let a, l = toEl a, toEl l in
    (*let rec fold_left2 f a xs ys =
      match xs, ys with
        | x::xs, y::ys -> fold_left2 f (f a x y) xs ys
        | _ -> a
    in*)
    let rec fold_advance_prefix xs x y = 
      match xs, x, y with
        | `Todo (zs,fs,gs), x::xs, y::ys when ee_equal x y -> fold_advance_prefix (`Todo (x :: zs,fs,gs)) xs ys
        | `Todo (zs,fs,gs), _, _ 
        | `Done (zs,fs,gs), _, _ ->  `Done (zs,fs,List.rev y@gs)
    in
    let dummy = Cil.integer 42 in
    let is_concrete = 
      let is_concrete x =
        match x with
          | Var v -> true
          | Addr -> true
          | Deref -> true
          | Field f -> true
          | Index e -> false
      in
      List.for_all is_concrete 
    in
    try match fold_advance_prefix (`Todo ([],[],[])) a l with
      | `Done ([],_,_) 
      | `Todo ([],_,_) -> None
      | `Todo (zs,xs,ys) 
      | `Done (zs,xs,ys) when is_concrete xs && is_concrete ys ->
          let elem = fromEl (List.rev (Addr::zs)) dummy in
          Some (elem, fromEl a dummy, fromEl l dummy)
      | _ -> None
    with Invalid_argument _ -> None

end
