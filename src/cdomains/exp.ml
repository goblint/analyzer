open Pretty
open Cil

module Exp =
struct
  type t = exp
  include Printable.Std

  let equal = Expcompare.compareExp
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let classify _ = 0
  let class_name _ = "None" 
  let name () = "Cil expressions"
  
  let pretty = d_exp
  let short w s = sprint w (d_exp () s)
  let toXML x = Xml.Element ("Leaf", [("text", Goblintutil.escape (short 80 x))], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
  let rec interesting x =
    match x with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | AlignOfE _
      | UnOp  _    
      | BinOp _ -> false
      | Const _ -> true
      | AddrOf  (Var v2,_) 
      | StartOf (Var v2,_) 
      | Lval    (Var v2,_) -> true
      | AddrOf  (Mem e,_) 
      | StartOf (Mem e,_) 
      | Lval    (Mem e,_)
      | CastE (_,e)           -> interesting e 
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
      
  let  contains_var v e =
    let rec offs_contains o =
      match o with
        | NoOffset -> false
        | Field (_,o) -> offs_contains o
        | Index (e,o) -> cv false e || offs_contains o
    and cv deref e = 
      match e with
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | Const _ 
        | AlignOfE _ -> false 
        | UnOp  (_,e,_)     -> cv deref e      
        | BinOp (_,e1,e2,_) -> cv deref e1 || cv deref e2  
        | AddrOf  (Mem e,o) 
        | StartOf (Mem e,o) 
        | Lval    (Mem e,o) -> cv true e || offs_contains o
        | CastE (_,e)           -> cv deref e 
        | Lval    (Var v2,o) -> v.vid = v2.vid || offs_contains o
        | AddrOf  (Var v2,o) 
        | StartOf (Var v2,o) -> 
          if deref  
          then v.vid = v2.vid || offs_contains o 
          else offs_contains o 
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in
      cv false e
  
  let contains_field f e =
    let rec offs_contains o =
      match o with
        | NoOffset -> false
        | Field (f',o) -> f.fname = f'.fname 
        | Index (e,o) -> cv e || offs_contains o
    and cv e = 
      match e with
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | Const _ 
        | AlignOfE _ -> false 
        | UnOp  (_,e,_)     -> cv e      
        | BinOp (_,e1,e2,_) -> cv e1 || cv e2  
        | AddrOf  (Mem e,o) 
        | StartOf (Mem e,o) 
        | Lval    (Mem e,o) -> cv e || offs_contains o
        | CastE (_,e)           -> cv e 
        | Lval    (Var v2,o) -> offs_contains o
        | AddrOf  (Var v2,o) 
        | StartOf (Var v2,o) -> offs_contains o 
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in
      cv e
      
  let rec is_global_var x =
    match x with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | AlignOfE _ 
      | UnOp _      
      | BinOp _ -> None
      | Const _ -> Some false      
      | Lval (Var v,_) -> Some v.vglob   
      | Lval (Mem e,_) -> is_global_var e
      | CastE (t,e) -> is_global_var e 
      | AddrOf lval -> Some false  
      | StartOf lval -> Some false
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
  
  let rec conv_offs (offs:(fieldinfo,exp) Lval.offs) : offset =
    match offs with
      | `NoOffset -> NoOffset
      | `Field (f,o) -> Field (f, conv_offs o)
      | `Index (e,o) -> Index (e, conv_offs o)

  let of_clval (v,offs) = Lval (Var v, conv_offs offs)
  
  let rec fold_offs c =
    match c with
      | AddrOf (Mem e,o) -> begin
          match fold_offs e with
            | Some (v, o') -> Some (v, addOffset o o')
            | x -> x
          end
      | AddrOf (Var v,o)  
      | Lval (Var v,o) -> 
          Some (v, o)
      | _ -> None
    
  let rec off_eq x y =
    match x, y with
      | NoOffset, NoOffset -> true
      | Field (f1, o1), Field (f2, o2) -> f1.fname = f2.fname && off_eq o1 o2
      | Index (e1, o1), Index (e2, o2) -> simple_eq e1 e2 && off_eq o1 o2
      | _ -> false
  and simple_eq x y =
    match x, y with
      | Lval (Var v1,o1)   , Lval (Var v2,o2)
      | AddrOf (Var v1,o1) , AddrOf (Var v2,o2)             
      | StartOf (Var v1,o1), StartOf (Var v2,o2) 
          -> v1.vid = v2.vid && off_eq o1 o2
      | Lval (Mem e1,o1)   , Lval (Mem e2,o2)
      | AddrOf (Mem e1,o1) , AddrOf (Mem e2,o2) 
      | StartOf (Mem e1,o1), StartOf (Mem e2,o2)
          -> simple_eq e1 e2 && off_eq o1 o2
      | _ -> false
    
  let rec replace_base (v,offs) q exp =
    match exp with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | AlignOfE _ 
      | UnOp _      
      | BinOp _ 
      | Const _ 
      | Lval (Var _,_) 
      | AddrOf (Var _,_)              
      | StartOf (Var _,_) -> exp
      | Lval (Mem e,o)    when simple_eq e q -> Lval (Var v, addOffset o (conv_offs offs))
      | Lval (Mem e,o)                       -> Lval (Mem (replace_base (v,offs) q e), o)
      | AddrOf (Mem e,o)  when simple_eq e q -> AddrOf (Var v, addOffset o (conv_offs offs))
      | AddrOf (Mem e,o)                     -> AddrOf (Mem (replace_base (v,offs) q e), o)
      | StartOf (Mem e,o) when simple_eq e q -> StartOf (Var v, addOffset o (conv_offs offs))
      | StartOf (Mem e,o)                    -> StartOf (Mem (replace_base (v,offs) q e), o)
      | CastE (t,e) -> CastE (t, replace_base (v,offs) q e)
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."

  let rec base_compinfo q exp =
    match exp with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | AlignOfE _ 
      | UnOp _      
      | BinOp _ 
      | Const _ 
      | Lval (Var _,_) 
      | AddrOf (Var _,_)              
      | StartOf (Var _,_) -> None
      | Lval (Mem e,Field (f,_)) when simple_eq e q -> Some f.fcomp
      | Lval (Mem e,o) -> base_compinfo q e
      | AddrOf (Mem e,Field (f,_)) when simple_eq e q -> Some f.fcomp
      | AddrOf (Mem e,o) -> base_compinfo q e
      | StartOf (Mem e,Field (f,_)) when simple_eq e q -> Some f.fcomp
      | StartOf (Mem e,o) -> base_compinfo q e
      | CastE (t,e) -> base_compinfo q e
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
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

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>%s\n</data>\n</value>\n" (short 800 x)
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
    
  let pretty () (x,y,z) = text "(" ++ d_exp () x ++ text ", "++ d_exp () y ++ text ", "++ d_exp () z ++ text ")"
  let short w (x,y,z) = sprint w (dprintf "(%a,%a,%a)" d_exp x d_exp y d_exp z)
  let toXML x = Xml.Element ("Leaf", [("text", Goblintutil.escape (short 80 x))], [])
  let isSimple _ = true
  let pretty_f _ = pretty
  let toXML_f _ = toXML
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
  type ee = EVar of varinfo
          | EAddr
          | EDeref
          | EField of fieldinfo
          | EIndex of exp

  let ee_equal x y = 
    match x, y with
      | EVar v1, EVar v2 -> v1.vid = v2.vid
      | EAddr, EAddr -> true 
      | EDeref, EDeref -> true
      | EField f1, EField f2 -> f1.fname = f2.fname 
      | EIndex e1, EIndex e2 -> Exp.simple_eq e1 e2
      | _ -> false
  
  let ee_to_str x = 
    match x with
      | EVar v -> v.vname
      | EAddr -> "&"
      | EDeref -> "*"
      | EField f -> f.fname 
      | EIndex e -> Pretty.sprint 80 (d_exp () e)
  
  let ees_to_str xs = List.fold_right (fun x xs -> " " ^ (ee_to_str x) ^ xs ) xs ""

  exception NotSimpleEnough

  let rec ees_to_offs = function
	| [] 		-> `NoOffset 	
(*	| Addr :: x ->
	| Deref :: x ->
*)	| EAddr :: EDeref :: x -> ees_to_offs x
	| EDeref :: EAddr :: x -> ees_to_offs x
	| EField f :: x -> `Field (f,ees_to_offs x)
	| EIndex (Const (CInt64 (i,_,_))) :: x -> `Index (ValueDomain.IndexDomain.of_int i,ees_to_offs x)
	| EIndex i :: x -> `NoOffset 
	| x  -> raise NotSimpleEnough
  
  let toEl exp = 
    let rec conv_o o =
      match o with
        | NoOffset -> []
        | Index (e,o) -> EIndex e :: conv_o o
        | Field (f,o) -> EField f :: conv_o o
    in
    let rec helper exp =
      match exp with
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | AlignOfE _ 
        | UnOp _      
        | BinOp _ 
        | StartOf _
        | Const _ -> raise NotSimpleEnough 
        | Lval (Var v, os) -> EVar v :: conv_o os  
        | Lval (Mem e, os) -> helper e @ [EDeref] @ conv_o os
        | AddrOf (Var v, os) -> EVar v :: conv_o os @ [EAddr]
        | AddrOf (Mem e, os) -> helper e @ [EDeref] @ conv_o os @ [EAddr]
        | CastE (_,e) -> helper e 
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in
      try helper exp 
      with NotSimpleEnough -> []
  
  let rec fromEl xs ex =
    match xs, ex with
      | []           ,             _ -> ex      
      | EDeref::xs    ,             _ -> fromEl xs (Lval (Mem ex, NoOffset))
      | EVar v::xs    ,             _ -> fromEl xs (Lval (Var v, NoOffset)) 
      | EField f::xs  , Lval lv   -> fromEl xs (Lval (Mem (AddrOf lv), Field (f, NoOffset)))
      | EIndex i::xs  , Lval lv   -> fromEl xs (Lval (Mem (AddrOf lv), Index (i, NoOffset)))
      | EAddr::xs     , Lval lv   -> fromEl xs (AddrOf lv)
      | _            ,             _ -> raise (Invalid_argument "")
  
  let strip_fields e =
    let rec sf e fs = 
      match e with
        | EField f :: es -> sf es (EField f::fs)
        | EDeref :: EAddr :: es -> sf es fs
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
    let dummy = integer 42 in
    let is_concrete = 
      let is_concrete x =
        match x with
          | EVar v -> true
          | EAddr -> true
          | EDeref -> true
          | EField f -> true
          | EIndex e -> false
      in
      List.for_all is_concrete 
    in
    try match fold_advance_prefix (`Todo ([],[],[])) a l with
      | `Done ([],_,_) 
      | `Todo ([],_,_) -> None
      | `Todo (zs,xs,ys) 
      | `Done (zs,xs,ys) when is_concrete xs && is_concrete ys ->
          let elem = fromEl (List.rev (EAddr::zs)) dummy in
          Some (elem, fromEl a dummy, fromEl l dummy)
      | _ -> None
    with Invalid_argument _ -> None
    let printXml f (x,y,z) = BatPrintf.fprintf f "<value>\n<map>\n<key>1</key>\n%a<key>2</key>\n%a<key>3</key>\n%a</map>\n</value>\n" Exp.printXml x Exp.printXml y Exp.printXml z
end
