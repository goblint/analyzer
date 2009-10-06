include Pretty

module Exp =
struct
  type t = Cil.exp

  let equal = Util.equals
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
      
  let rec contains_var v e =
    match e with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.Const _ 
      | Cil.AlignOfE _ -> false 
      | Cil.UnOp  (_,e,_)     -> contains_var v e      
      | Cil.BinOp (_,e1,e2,_) -> contains_var v e1 || contains_var v e2  
      | Cil.AddrOf  (Cil.Var v2,_) 
      | Cil.StartOf (Cil.Var v2,_) -> false 
      | Cil.Lval    (Cil.Var v2,_) -> v.Cil.vid = v2.Cil.vid 
      | Cil.AddrOf  (Cil.Mem e,_) 
      | Cil.StartOf (Cil.Mem e,_) 
      | Cil.Lval    (Cil.Mem e,_)
      | Cil.CastE (_,e)           -> contains_var v e 
    
      
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
  
  let rec conv_offs (offs:(Cil.fieldinfo,Cil.exp) Lval.offs) : Cil.offset =
    match offs with
      | `NoOffset -> Cil.NoOffset
      | `Field (f,o) -> Cil.Field (f, conv_offs o)
      | `Index (e,o) -> Cil.Index (e, conv_offs o)

  let of_clval (v,offs) = Cil.Lval (Cil.Var v, conv_offs offs)
end