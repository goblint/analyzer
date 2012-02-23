open Cil
open Lval
open Pretty
(*
type pth = Base  of (varinfo * (fieldinfo, int64) offs) option
         | Deref of pth * (fieldinfo, int64) offs
         | Star  of pth
         | Ref   of pth

type acc = AccBase  of (varinfo * (fieldinfo, int64) offs) option
         | AccDeref of acc * (fieldinfo, int64) offs
         | AccStar  of acc
         | AccRef   of acc
         | AccEqual of acc * varinfo * (fieldinfo, int64) offs
         
let is_global (a: Queries.ask) (v: varinfo): bool = 
  v.vglob || match a (Queries.MayEscape v) with `Bool tv -> tv | _ -> false
  
module Path = 
struct
  type t = pth option

  include Lattice.StdCousot
  
  let name () = "Path"
  
  let rec eq_offs x y = 
    match x, y with
      | `NoOffset     , `NoOffset -> true
      | `Index (i,o)  , `Index (j,u) when i=j-> eq_offs o u
      | `Field (f1,o) , `Field (f2,u) 
          when f1.fcomp.ckey=f2.fcomp.ckey && f1.fname = f2.fname 
          -> eq_offs o u
      | _ -> false
  
  let lval_eq x y = 
    match x, y with
      | None, None             -> true
      | Some (x,o), Some (y,i) -> x.vid=y.vid && eq_offs o i
      | _                      -> false
  
  let rec offs_from_cil ask o =
    match o with
      | NoOffset    -> `NoOffset
      | Field (f,o) -> `Field (f,offs_from_cil ask o)
      | Index (i,o) -> 
    match ask (Queries.EvalInt i) with
      | `Int i -> `Index (i, offs_from_cil ask o)
      | _      -> `NoOffset

  let rec append_ofs v o =
    match v with
      | `NoOffset     -> o
      | `Index (x, v) -> `Index (x, append_ofs v o)
      | `Field (x, v) -> `Field (x, append_ofs v o)
      
  let rec equal' x y =
    match x, y with
      | Base x     , Base  y     -> lval_eq x y
      | Deref (x,o), Deref (y,i) -> eq_offs o i && equal' x y
      | Star x     , Star y      -> equal' x y
      | Ref x      , Ref y       -> equal' x y
      | _ -> false
 
  let equal (x:t) (y:t) =
    match x, y with
      | None , None    -> true
      | Some x, Some y -> equal' x y
      | _ -> false
    
  let hash (x:t) = Hashtbl.hash x
  
  let compare (x:t) (y:t) = Pervasives.compare x y
  
  let isSimple _ = true
  
  let rec offs_short tt x = 
    match x with 
      | `NoOffset -> ""
      | `Index (x,o) -> "[" ^ (Int64.to_string x) ^ "]" ^ (offs_short true o) 
      | `Field (x,o) -> (if tt then "." else "") ^ (x.fname) ^ (offs_short true o) 
  
  let short n x =
    let rec to_str x =
      match x with 
        | Base (Some (x, o)) -> x.vname  ^ offs_short true o
        | Base None          -> "*" 
        | Deref (x,`NoOffset)-> "(*"^to_str x^")"
        | Deref (x,o)        -> to_str x ^ "→" ^ offs_short false o
        | Star x             -> to_str x ^ "→*"
        | Ref (Base x)       -> "&" ^ to_str (Base x) 
        | Ref x              -> "(&" ^ to_str x ^ ")"
    in
    match x with
      | None   -> "⊥"
      | Some x -> to_str x
   
  let pretty_f sf () x = Pretty.text (sf 80 x)
  let pretty = pretty_f short
  
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf 80 x)],[]) 
  let toXML = toXML_f (fun n x -> Goblintutil.escape (short n x))
  
  let pretty_diff () (x,y) = 
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
    
  let top ()   = Some (Base None)
  let is_top x = Some (Base None) = x
  
  let bot ()   = None
  let is_bot x = None = x
  
  
  let rec leq' (x:pth) (y:pth) =
    let r = 
    match x, y with
      | _            , Base None   -> true
      | Base x       , Base y      -> lval_eq x y 
      | Base (Some _), Deref (x,i) -> false
      | Base (Some x), Star y      -> leq' (Base (Some x)) y      
      | Deref (x,o)  , Deref (y,i) -> eq_offs i o && leq' x y
      | Star x       , Star y      -> List.exists (fun (x,y) -> leq' x y) [x,y;x,Star y]   
      | Star x       , Deref (y,i) -> false
      | Deref (x,o)  , Star y      -> List.exists (fun (x,y) -> leq' x y) [Deref (x,o),y;x,Star y]
      | Ref x        , Ref y       -> leq' x y
      | Ref x        , Star y      -> leq' (Ref x) y
      | _ -> false (*is this ok*)
    in
(*     let short' x y = short x (Some y) in *)
(*     print_endline (short' 80 x ^ " ⊑ " ^ short' 80 y ^ " = " ^ (if r then "true" else "false") ); *)
    r

  let leq x y =
    match x, y with
      | Some x, Some y -> leq' x y
      | None  , _      -> true
      | _     , None   -> false 
       
  let min x =
    let rec min' x =
      match x with
        | [] -> failwith "incorrect use of PathDomain.min --- argument list must not be nil" 
        | [x] -> x
        | x::y::ys -> if leq' x y then min' (x::ys) else min' (y::ys) 
    in
    let m = min' x in
(*     print_string "min: "; *)
(*     List.iter (fun x -> print_string (short 80 (Some x) ^ " ")) x; *)
(*     print_endline (" = "^short 80 (Some m)); *)
    m

  let add_star x =
    match x with
      | Base None -> Base None
      | Star x -> Star x
      | x      -> Star x
  
  let add_ref x =
    match x with
      | Base None -> Base None
      | x -> Ref x
  
  let join x y =
    let rec join' x y =
      let r = 
      match x, y with
        | Base (Some (x,o)), Base (Some (y,i)) 
          when eq_offs o i&&x.vid=y.vid -> Base (Some (x,o))
        | Base x      , Deref (y,_)-> add_star (join' (Base x) y)
        | Deref (x,_) , Base y     -> add_star (join' x (Base y))
        | Base x      , Star y     -> add_star (join' (Base x) y)
        | Star x      , Base y     -> add_star (join' x (Base y))
        | Deref (x,o) , Deref (y,i) when eq_offs i o -> Deref (join' x y, i)
        | Deref (x,o) , Deref (y,i)-> min (List.map add_star [join' x  (Deref (y,i)); join' (Deref (x,o)) y])
        | Deref (x,o) , Star y     -> min (List.map add_star [join' (Deref (x,o)) y; join' x (Star y)])
        | Star  x     , Deref (y,i)-> min (List.map add_star [join' x (Deref (y,i)); join' (Star x) y])
        | Star  x     , Star y     -> min [add_star (join' x y); join' x (Star y); join' (Star x) y]
        | Ref x       , Ref y      -> add_ref (join' x y)
        | Ref x       , Star y      -> add_star (join' (Ref x) y)
        | Ref x       , Deref (y,_) -> add_star (join' (Ref x) y)
        | Star x      , Ref y       -> add_star (join' x (Ref y))
        | Deref (x,_) , Ref y       -> add_star (join' x (Ref y))
        | _ -> Base None
      in
      r
    in
    let r =
    match x, y with
      | None  , y      -> y
      | x     , None   -> x
      | Some x, Some y -> Some (join' x y)
    in
(*     print_endline (sprint 80 (dprintf "%a ⊔ %a = %a" pretty x pretty y pretty r)); *)
    r
  
  let meet' x y = failwith "Path.meet not implemented"

  let meet x y =
    match x, y with
      | Some x, Some y -> Some (meet' x y)
      | _ -> None
      
  let from_lval ask (lv:lval) : t =
    match lv with
      | Var x, o -> Some (Base (Some (x,offs_from_cil ask o)))
      | Mem e, o -> bot ()
  
  let add_ofs x o =
    match x with
      | Base (Some (x,u)) -> Base (Some (x, append_ofs u o))
      | Base None   -> Base None
      | Deref (x,u) -> Deref (x, append_ofs u o)
      | Star x      -> Star  x
      | Ref x       -> Base None
  
  (*todo: deal with offsets*)
  let rec subst_base_var x (v,o) y =
    let rec subst_base_var_ref x y =
      match x with
        | Deref (Base (Some (v',o')),oa) 
            when v.vid = v'.vid && eq_offs o o' 
            -> add_ofs y oa 
        | Star (Base (Some (v',o'))) 
            when v.vid = v'.vid && eq_offs o o' 
            -> add_star y
        | Base (Some (v',o')) 
            when v.vid = v'.vid && eq_offs o o'
            -> Base None 
        | Base x      -> Base x
        | Deref (x,o) -> Deref (subst_base_var_ref x y, o)
        | Star x      -> Star  (subst_base_var_ref x y)      
        | Ref x       -> Base None
    in
    let rec subst_base_var' x y =
      match x with
        | Ref (Base (Some (_,_))) -> x
        | Base (Some (v',o')) when v.vid = v'.vid && eq_offs o o' -> y  
        | Base x      -> Base x
        | Deref (x,o) -> Deref (subst_base_var' x y, o)
        | Star x      -> Star  (subst_base_var' x y)   
        | Ref x       -> Ref   (subst_base_var' x y)
    in
    let r =
    match x, y with
      | None  , _     -> None
      | _     , None  -> None 
      | Some (Ref x)                  , Some y -> Some (Ref (subst_base_var' x y))
      | Some (Base (Some (v',o')))    , Some (Ref _) when v.vid = v'.vid && eq_offs o o' -> y 
      | Some x                        , Some y -> Some (subst_base_var' x y)      
    in
(*     print_endline (sprint 80 (dprintf "%a[%s↦%a] = %a" pretty x (v.vname^offs_short true o) pretty y pretty r)); *)
    r
  
    
  let rec from_exp ask (e:exp) : t =
    let addr e o =
      match from_exp ask e with
        | Some (Base None) -> Some (Deref (Base None,offs_from_cil ask o))
        | None   -> None
        | Some x -> Some (Deref (x,offs_from_cil ask o))
    in
    let add_ref x =
      match x with Some x -> Some (add_ref x) | x -> x 
    in
    match e with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.AlignOfE _
      | Cil.Const _ -> top ()
      | Cil.UnOp  (_,e,_) -> from_exp ask e   
      | Cil.BinOp (Cil.IndexPI, e, i, _) when isConstant i ->
          let io = 
            match ask (Queries.EvalInt i) with 
              | `Int i -> `Index (i, `NoOffset)
              | _ -> `NoOffset
          in
          begin match from_exp ask e with
            | Some (Base (Some (x, v))) -> Some (Base (Some (x, append_ofs v io)))
            | e -> e      
          end
      | Cil.BinOp (_,e1,e2,_) ->
          let e1p = from_exp ask e1 in
          let e2p = from_exp ask e2 in
          begin match isConstant e1, isConstant e2 with
            | true , _    -> e2p
            | _    , true -> e1p
            | _ -> top ()
          end
      | Cil.AddrOf  (Cil.Var v,o) 
      | Cil.StartOf (Cil.Var v,o) -> Some (Ref (Base (Some (v,offs_from_cil ask o))))
      | Cil.Lval    (Cil.Var v,o) -> Some (Base (Some (v,offs_from_cil ask o)))
      | Cil.AddrOf  (Cil.Mem e,o) 
      | Cil.StartOf (Cil.Mem e,o) -> add_ref (addr e o) 
      | Cil.Lval    (Cil.Mem e,o) -> addr e o
      | Cil.CastE (_,e)           -> from_exp ask e 

end      

module Acc =
struct
  type t = acc
  
  let hash (x:t) = Hashtbl.hash x
  
  let compare (x:t) (y:t) = Pervasives.compare x y
  
  let name () = "Access"
  
  let rec eq_offs x y = 
    match x, y with
      | `NoOffset     , `NoOffset -> true
      | `Index (i,o)  , `Index (j,u) when i=j-> eq_offs o u
      | `Field (f1,o) , `Field (f2,u) 
          when f1.fcomp.ckey=f2.fcomp.ckey && f1.fname = f2.fname 
          -> eq_offs o u
      | _ -> false
  
  let lval_eq x y = 
    match x, y with
      | None, None             -> true
      | Some (x,o), Some (y,i) -> x.vid=y.vid && eq_offs o i
      | _                      -> false

  let rec equal x y =
    match x, y with
      | AccBase x       , AccBase  y       -> lval_eq x y
      | AccDeref (x,o)  , AccDeref (y,i)   -> eq_offs o i && equal x y
      | AccStar x       , AccStar y        -> equal x y
      | AccRef x        , AccRef y         -> equal x y
      | AccEqual (x,z,y), AccEqual (a,b,c) -> z.vid = b.vid && eq_offs y c && equal x a
      | _ -> false

  let isSimple _ = true
  
  let rec short n x =
    let rec offs_short tt x = 
      match x with 
        | `NoOffset -> ""
        | `Index (x,o) -> "[" ^ (Int64.to_string x) ^ "]" ^ (offs_short true o) 
        | `Field (x,o) -> (if tt then "." else "") ^ (x.fname) ^ (offs_short true o) 
    in
    match x with 
      | AccRef (AccBase (Some (x, o))) -> "(&" ^ short n (AccBase (Some (x, o))) ^")"
      | AccDeref (AccBase (Some (x, o)),`NoOffset)-> "*"^short n (AccBase (Some (x, o)))
      | AccBase (Some (x, o)) -> x.vname  ^ offs_short true o
      | AccBase None          -> "*" 
      | AccDeref (x,`NoOffset)-> "(*"^short n  x^")"
      | AccDeref (x,o)        -> short n  x ^ "→" ^ offs_short false o
      | AccStar x             -> short n  x ^ "→*"
      | AccRef x              -> "(&" ^ short n  x ^ ")"
      | AccEqual (a,v,o)      -> "("^short n a^"≡"^v.vname^offs_short true o^")"


  let pretty_f sf () x = Pretty.text (sf 80 x)
  let pretty = pretty_f short
  
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf 80 x)],[]) 
  let toXML = toXML_f (fun n x -> Goblintutil.escape (short n x))
  
  let pretty_diff () (x,y) = 
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let guaranteed_local ask x =
    let local v = not (is_global ask v) in 
    let rec refs n x =
      match x with
        | AccRef x             -> refs (n+1) x
        | AccDeref (x,_)       -> refs (n-1) x
        | AccEqual (x,v,_)     -> (n=0 && local v) || refs n x
        | AccBase (Some (x,_)) -> (n=0 && local x) 
        | AccStar x            -> refs (n-1) x
        | AccBase None         -> false
    in
    refs 0 x
  
  let rec norm x = 
    match x with
      | AccEqual (e, _, _) -> norm e 
      | AccDeref (e, `NoOffset) ->
          begin match norm e with
            | AccRef x -> x
            | e -> AccDeref (e, `NoOffset)
          end
      | AccDeref (e, o) ->
          begin match norm e with
            | AccRef (AccDeref (x, u))       -> AccDeref (x, Path.append_ofs u o)
            | AccRef (AccBase (Some (x, u))) -> AccBase (Some (x, Path.append_ofs u o))
            | e -> AccDeref (e, o)
          end
      | AccRef x -> AccRef (norm x)
      | x -> x 

  let rec must_compiletime_constant (x:t) : bool = 
    let rec f x = 
      match norm x with
        | AccRef _         -> true      
        | AccEqual (x,_,_) -> f x
        | _                -> false
    in
    f x
(*     if f x then ((Messages.report (sprint 80 (pretty () x)));true) else false *)
 
  let may_alias x z =
    let x, z = norm x, norm z in
(*    print_string (sprint 80 (pretty () x));
    print_string " vs. " ;
    print_endline (sprint 80 (pretty () z));*)
    let rec alias_offs x y = 
      match x, y with
        | `NoOffset     , _         -> true
        | _             , `NoOffset -> true
        | `Index (i,o)  , `Index (j,u) when i=j-> eq_offs o u
        | `Field (f1,o) , `Field (f2,u) 
            when f1.fcomp.ckey=f2.fcomp.ckey 
              && f1.fname=f2.fname 
            -> eq_offs o u
        | `Field (f1,o) , _ when not f1.fcomp.cstruct -> true
        | _, `Field (f1,o)  when not f1.fcomp.cstruct -> true
        | _ -> false
    in
    match x, z with
      | AccBase (Some (x,o))  , AccBase (Some (z,u))   -> x.vid=z.vid && alias_offs o u
      | AccDeref (x,`NoOffset), AccDeref (z,_)         -> true
      | AccDeref (x,_)        , AccDeref (z,`NoOffset) -> true
      | AccDeref (x,o)        , AccDeref (z,u)         -> alias_offs o u
      | AccDeref (x,`NoOffset), AccBase (Some _)       -> true
      | AccDeref (x,o)        , AccBase (Some (_,`NoOffset)) -> false
      | AccDeref (x,o)        , AccBase (Some _)             -> true
      | AccBase (Some _)            , AccDeref (y,`NoOffset) -> true
      | AccBase (Some (x,`NoOffset)), AccDeref (y,o)         -> false
      | AccBase (Some _)            , AccDeref (y,o)         -> true
      | _ -> true
  

end

module Access =
struct 
  module Lvals   = Lval.Normal (IntDomain.Integers)
  module MustMap = MapDomain.MapTop (Lval.Normal (IntDomain.Integers)) (Path)
  module Accs    = SetDomain.ToppedSet (Acc) (struct let topname = "totally unsound" end)
  module MaySet  = SetDomain.ToppedSet (Lvals) (struct let topname = "all variables" end)  
  module GlobDom = Lattice.Prod (Path) (MaySet)
  module Diff    = SetDomain.ToppedSet (Printable.Prod (Basetype.Variables) (GlobDom)) (struct let topname = "Unknown diff" end)
  module MayMap  = MapDomain.MapTop (Lval.Normal (IntDomain.Integers)) (MaySet)
  

  include Lattice.LiftBot (Lattice.Prod3 (Lattice.Prod3 (MustMap) (Diff) (MayMap)) (Accs) (Accs))
  
  let startstate () : t = `Lifted ((MustMap.top (), Diff.bot (), MayMap.top ()), Accs.bot (), Accs.bot ())

  let lift_fun (f:MustMap.t -> Diff.t -> MustMap.t * Diff.t) (mp:t) : t = 
    match mp with
    | `Bot -> `Bot
    | `Lifted ((mp,d,mm),a,b) ->  
        let nmp, nd = f mp d in
        `Lifted ((nmp, nd, mm), a, b)

  let rec const_equal c1 c2 =
    match c1, c2 with
      | CStr s1  , CStr s2   -> s1 = s2
      | CWStr is1, CWStr is2 -> is1 = is2
      | CChr c1  , CChr c2   -> c1 = c2
      | CInt64 (v1,k1,_), CInt64 (v2,k2,_) -> v1 = v2 && k1 = k2
      | CReal (f1,k1,_) , CReal (f2,k2,_)  -> f1 = f2 && k1 = k2
      | CEnum (_,n1,e1), CEnum (_,n2,e2) -> n1 = n2 && e1.ename = e2.ename  
      | _ -> false

  let option_eq f x y =
    match x, y with
      | Some x, Some y -> f x y
      | None, None -> true
      | _ -> false 
  
  let rec typ_equal t1 t2 =
    let args_eq (s1,t1,_) (s2,t2,_) = s1 = s2 && typ_equal t1 t2 in
    let eitem_eq (s1,e1,l1) (s2,e2,l2) = s1 = s2 && l1 = l2 && exp_equal e1 e2 in
    match t1, t2 with
      | TVoid _, TVoid _ -> true
      | TInt (k1,_), TInt (k2,_) -> k1 = k2
      | TFloat (k1,_), TFloat (k2,_) -> k1 = k2
      | TPtr (t1,_), TPtr (t2,_) -> typ_equal t1 t2
      | TArray (t1,d1,_), TArray (t2,d2,_) -> option_eq exp_equal d1 d2 && typ_equal t1 t2
      | TFun (rt1, arg1, _,  b1), TFun (rt2, arg2, _, b2) -> b1 = b2 && typ_equal rt1 rt2 && option_eq (List.for_all2 args_eq) arg1 arg2
      | TNamed (ti1, _), TNamed (ti2, _) -> ti1.tname = ti2.tname && typ_equal ti1.ttype ti2.ttype
      | TComp (c1,_), TComp (c2,_) -> c1.ckey = c2.ckey
      | TEnum (e1,_), TEnum (e2,_) -> e1.ename = e2.ename & List.for_all2 eitem_eq e1.eitems e2.eitems 
      | TBuiltin_va_list _, TBuiltin_va_list _ -> true
      | _ -> false

  and lval_equal (l1,o1) (l2,o2) =
    let rec offs_equal o1 o2 =
      match o1, o2 with
        | NoOffset, NoOffset -> true
        | Field (f1, o1), Field (f2,o2) -> f1.fcomp.ckey = f2.fcomp.ckey && f1.fname = f2.fname && offs_equal o1 o2
        | Index (i1,o1), Index (i2,o2) -> exp_equal i1 i2 && offs_equal o1 o2   
        | _ -> false     
    in
       offs_equal o1 o2 
    && match l1, l2 with
         | Var v1, Var v2 -> v1.vid = v2.vid
         | Mem m1, Mem m2 -> exp_equal m1 m2
         | _ -> false
  
  and exp_equal e1 e2 =
    match e1, e2 with
      | Const c1, Const c2 -> const_equal c1 c2
      | AddrOf l1,  AddrOf l2   
      | StartOf l1, StartOf l2 
      | Lval l1 , Lval  l2 -> lval_equal l1 l2
      | SizeOf t1,  SizeOf t2 -> typ_equal t1 t2
      | SizeOfE e1, SizeOfE e2 -> exp_equal e1 e2  
      | SizeOfStr s1, SizeOfStr s2 -> s1 = s2
      | AlignOf t1, AlignOf t2 -> typ_equal t1 t2
      | AlignOfE e1,  AlignOfE e2 -> exp_equal e1 e2
      | UnOp (o1,e1,t1),  UnOp (o2,e2,t2) -> o1 = o2 && typ_equal t1 t2 && exp_equal e1 e2
      | BinOp (o1,e11,e21,t1),  BinOp(o2,e12,e22,t2) -> o1 = o2 && typ_equal t1 t2 && exp_equal e11 e12 && exp_equal e21 e22     
      | CastE (t1,e1),  CastE (t2,e2) -> typ_equal t1 t2 && exp_equal e1 e2
      | _ -> false
      
  (* kill predicate for must-equality kind of analyses*)
  let may_change_t (b:exp) (a:exp) : bool =
    let rec type_may_change_t a bt =
      let rec may_change_t_offset o =
        match o with  
          | NoOffset -> false
          | Index (e,o) -> type_may_change_t e bt || may_change_t_offset o
          | Field (_,o) -> may_change_t_offset o
      in
      let at = typeOf a in
      (isIntegralType at && isIntegralType bt) || (typ_equal at bt) ||
      match a with
        | Cil.Const _ 
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.AlignOfE _ -> false
        | Cil.UnOp (_,e,_) -> type_may_change_t e bt      
        | Cil.BinOp (_,e1,e2,_) -> type_may_change_t e1 bt || type_may_change_t e2 bt
        | Cil.Lval (Cil.Var _,o) 
        | Cil.AddrOf (Cil.Var _,o)              
        | Cil.StartOf (Cil.Var _,o) -> may_change_t_offset o
        | Cil.Lval (Cil.Mem e,o)    
        | Cil.AddrOf (Cil.Mem e,o)  
        | Cil.StartOf (Cil.Mem e,o) -> may_change_t_offset o || type_may_change_t e bt
        | Cil.CastE (t,e) -> type_may_change_t e bt
    in
    let bt =  unrollTypeDeep (typeOf b) in
    type_may_change_t a bt
    
  let may_change_pt ask (b:exp) (a:exp) : bool =
    let pt e = 
      match ask (Queries.MayPointTo e) with
        | `LvalSet ls -> ls
        | _ -> Queries.LS.top ()
    in
    let rec lval_may_change_pt a bl : bool =
      let rec may_change_pt_offset o =
        match o with  
          | NoOffset -> false
          | Index (e,o) -> lval_may_change_pt e bl || may_change_pt_offset o
          | Field (_,o) -> may_change_pt_offset o
      in
      let als = pt a in
      Queries.LS.is_top als || Queries.LS.mem bl als ||
      match a with
        | Cil.Const _ 
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.AlignOfE _ -> false
        | Cil.UnOp (_,e,_) -> lval_may_change_pt e bl      
        | Cil.BinOp (_,e1,e2,_) -> lval_may_change_pt e1 bl || lval_may_change_pt e2 bl
        | Cil.Lval (Cil.Var _,o) 
        | Cil.AddrOf (Cil.Var _,o)              
        | Cil.StartOf (Cil.Var _,o) -> may_change_pt_offset o
        | Cil.Lval (Cil.Mem e,o)    
        | Cil.AddrOf (Cil.Mem e,o)  
        | Cil.StartOf (Cil.Mem e,o) -> may_change_pt_offset o || lval_may_change_pt e bl 
        | Cil.CastE (t,e) -> lval_may_change_pt e bl
    in 
    let bls = pt b in
    if Queries.LS.is_top bls
    then true
    else Queries.LS.exists (lval_may_change_pt a) bls
  
  let may_change ask (b:exp) (a:exp) : bool =
    (*b should be an address of something that changes*)
    let pt e = 
      match ask (Queries.MayPointTo e) with
        | `LvalSet ls -> ls
        | _ -> Queries.LS.top ()
    in
    let bls = pt b in
    let bt = 
      match unrollTypeDeep (typeOf b) with
        | TPtr (t,_) -> t
        | _ -> voidType
    in (* type of thing that changed: typeof( *b ) *)
    let rec type_may_change_apt a = 
      (* With abstract points-to (like in type invariants in accesses). 
         Here we implement it in part --- minimum to protect local integers. *)
       match a, b with
         | Cil.Lval (Cil.Var _,NoOffset), Cil.AddrOf (Cil.Mem(Cil.Lval _),Field(_, NoOffset)) -> 
            (* lval *.field changes -> local var stays the same *)
            false
         | _ -> 
            type_may_change_t false a
    and type_may_change_t deref a =
      let rec may_change_t_offset o =
        match o with  
          | NoOffset -> false
          | Index (e,o) -> type_may_change_apt e || may_change_t_offset o
          | Field (_,o) -> may_change_t_offset o
      in
      let at = 
        match unrollTypeDeep (typeOf a) with
          | TPtr (t,a) -> t
          | at -> at 
      in
(*      Messages.report 
        ( sprint 80 (d_type () at)
        ^ " : "
        ^ sprint 80 (d_type () bt)
        ^ (if bt = voidType || (isIntegralType at && isIntegralType bt) || (deref && typ_equal (TPtr (at,[]) ) bt) || typ_equal at bt then ": yes" else ": no"));
*)      bt = voidType || (isIntegralType at && isIntegralType bt) || (deref && typ_equal (TPtr (at,[]) ) bt) || typ_equal at bt ||
      match a with
        | Cil.Const _ 
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.AlignOfE _ -> false
        | Cil.UnOp (_,e,_) -> type_may_change_t deref e      
        | Cil.BinOp (_,e1,e2,_) -> type_may_change_t deref e1 || type_may_change_t deref e2
        | Cil.Lval (Cil.Var _,o) 
        | Cil.AddrOf (Cil.Var _,o)              
        | Cil.StartOf (Cil.Var _,o) -> may_change_t_offset o
        | Cil.Lval (Cil.Mem e,o)    -> (*Messages.report "Lval" ;*) may_change_t_offset o || type_may_change_t true e    
        | Cil.AddrOf (Cil.Mem e,o)  -> (*Messages.report "Addr" ;*) may_change_t_offset o || type_may_change_t false e  
        | Cil.StartOf (Cil.Mem e,o) -> (*Messages.report "Start";*) may_change_t_offset o || type_may_change_t false e
        | Cil.CastE (t,e) -> type_may_change_t deref e 
    
    and lval_may_change_pt a bl : bool =
      let rec may_change_pt_offset o =
        match o with  
          | NoOffset -> false
          | Index (e,o) -> lval_may_change_pt e bl || may_change_pt_offset o
          | Field (_,o) -> may_change_pt_offset o
      in
      let rec addrOfExp e = 
        match e with
          | Cil.Lval    (Cil.Var v,o) -> Some (AddrOf (Var v,o)) 
          | Cil.AddrOf  (Cil.Var _,_) -> None              
          | Cil.StartOf (Cil.Var _,_) -> None
          | Cil.Lval    (Cil.Mem e,o) -> Some (AddrOf (Mem e, o)) 
          | Cil.AddrOf  (Cil.Mem e,o) -> (match addrOfExp e with Some e -> Some (AddrOf (Mem e, o)) | x -> x)
          | Cil.StartOf (Cil.Mem e,o) -> (match addrOfExp e with Some e -> Some (AddrOf (Mem e, o)) | x -> x)
          | Cil.CastE   (t,e) -> addrOfExp e
          | _ -> None
      in      
      let lval_is_not_disjoint (v,o) als = 
        let rec oleq o s = 
          match o, s with
            | `NoOffset, _ -> true
            | `Field (f1,o), `Field (f2,s) when f1.fname = f2.fname -> oleq o s
            | `Index (i1,o), `Index (i2,s) when exp_equal i1 i2     -> oleq o s
            | _ -> false
        in
        if Queries.LS.is_top als
        then false
        else Queries.LS.exists (fun (u,s) ->  v.vid = u.vid && oleq o s) als
      in
      let (als, test) = 
      match addrOfExp a with
        | None -> (Queries.LS.bot (), false)
        | Some e -> 
            let als = pt e in 
            (als, lval_is_not_disjoint bl als)   
      in
(*      Messages.report 
        ( sprint 80 (Lval.CilLval.pretty () bl)
        ^ " in PT("
        ^ sprint 80 (d_exp () a)
        ^ ") = "
        ^ sprint 80 (Queries.LS.pretty () als)
        ^ (if Queries.LS.is_top als || test then ": yes" else ": no"));
*)      if (Queries.LS.is_top als) 
      then type_may_change_apt a 
      else test ||
      match a with
        | Cil.Const _ 
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.AlignOfE _ -> false
        | Cil.UnOp (_,e,_) -> lval_may_change_pt e bl      
        | Cil.BinOp (_,e1,e2,_) -> lval_may_change_pt e1 bl || lval_may_change_pt e2 bl
        | Cil.Lval (Cil.Var _,o) 
        | Cil.AddrOf (Cil.Var _,o)              
        | Cil.StartOf (Cil.Var _,o) -> may_change_pt_offset o
        | Cil.Lval (Cil.Mem e,o)    
        | Cil.AddrOf (Cil.Mem e,o)  
        | Cil.StartOf (Cil.Mem e,o) -> may_change_pt_offset o || lval_may_change_pt e bl 
        | Cil.CastE (t,e) -> lval_may_change_pt e bl
    in 
    let r =
    if Queries.LS.is_top bls
    then ((*Messages.report "No PT-set: switching to types ";*) type_may_change_apt a)
    else Queries.LS.exists (lval_may_change_pt a) bls
    in
(*    if r 
    then (Messages.report ("Kill " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)); r)
    else (Messages.report ("Keep " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)); r) 
    Messages.report (sprint 80 (Exp.pretty () b) ^" changed lvalues: "^sprint 80 (Queries.LS.pretty () bls)); 
*)    r
  
  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, _, 
          [ Xml.Element 
            ( _, _, 
                 (Xml.Element (xn,_,xss))
              :: _
              :: (Xml.Element (yn,_,yss)) :: _
            )
          ; Xml.Element (sn, _, ss)
          ; Xml.Element (tn, _, ts)]) 
        -> 
            Xml.Element (node, ["text", "Accesses"], 
              [ Xml.Element (sn, ["text", "Reads"], ss)
              ; Xml.Element (tn, ["text", "Writes"], ts)
              ; Xml.Element (xn, ["text", "HA-Must"], xss)
              ; Xml.Element (yn, ["text", "May Point to"], yss)]
              )
      | x -> x
      
  let toXML s = toXML_f short s 
  
(*   exception AccPathBot *)
  (* extend the Base as long as possible *)
  let to_acc ask (p : Path.t) mp (f: Basetype.Variables.t -> GlobDom.t) : acc list =
    let rec to_acc' p (b:bool) : acc list  =
      match p with
        | Base None -> []
        | Deref (Base (Some (x, u)),`NoOffset) when is_global ask x ->
            begin match f x with             
              | Some (Base None), s when not (MaySet.is_top s)-> 
                  begin match List.flatten (List.map Lvals.to_var_offset (MaySet.elements s)) with
                    | [] -> AccDeref (AccBase (Some (x, u)), `NoOffset) :: []
                    | xs -> 
                      List.map (fun (v,o) -> AccBase (Some (v, Path.append_ofs u o))) xs
                  end 
              | _ -> List.map (fun x -> AccDeref (x, `NoOffset)) (to_acc' (Base (Some (x, u))) false) 
            end
        | Base (Some (x, `NoOffset)) when is_global ask x -> 
            begin match f x with             
              | None, mp-> 
                  []
              | Some (Base None), s when not (MaySet.is_top s) -> 
                  begin match List.flatten (List.map Lvals.to_var_offset (MaySet.elements s)) with
                    | [] -> AccBase (Some (x, `NoOffset)) :: []
                    | xs -> 
                      (if b then fun z -> AccBase (Some (x, `NoOffset)) :: z else fun z -> z )
                      (List.map (fun x -> AccRef (AccBase (Some x))) xs)
                  end               
              | Some (Base None), mp -> 
                  AccBase (Some (x, `NoOffset)) :: []
              | Some xp, _ ->
                  List.map (fun y -> AccEqual (y, x, `NoOffset)) (to_acc' xp b)
            end
        | Base (Some (x, o)) -> 
            begin match MustMap.find (Lvals.from_var_offset (x,o)) mp with
              | Some (Base None) -> AccBase (Some (x, o)) :: []
              | Some xp          -> List.map (fun y -> AccEqual (y, x, o)) (to_acc' xp b)
              | None             -> []
            end
        | Star x      -> List.map (fun y -> AccStar  y)     (to_acc' x true)
        | Deref (x,o) -> List.map (fun y -> AccDeref (y,o)) (to_acc' x false)
        | Ref x       -> List.map (fun y -> AccRef   y)     (to_acc' x true)
    in
      match p with
        | Some p -> to_acc' p true
        | None   -> [] 
   

  (* remove *-s from right hand sides in the map *)
  let kill_tops' mp d =
    let rem_top k v m =
      if Path.is_top v
      then m
      else MustMap.add k v m
    in
    MustMap.fold rem_top mp (MustMap.top ()) , d
    
  let kill_tops = lift_fun kill_tops'

  (*todo: kill when prefix matches*)
  let kill' v mp d = 
    let old_val = 
      try MustMap.find v mp
      with Not_found -> Path.top ()
    in
    match Lvals.to_var_offset v with
      | [op] ->
          let subst_old x =
            Path.subst_base_var x op old_val
          in
          MustMap.map subst_old (MustMap.remove v mp) , d
      | _ -> MustMap.top (), d
  
  let kill v mp = 
    match mp with
      | `Bot -> `Bot
      | `Lifted ((mp,d,mm),a,b) ->  
          let nmp, nd = kill' v mp d in
          let nmm = MayMap.remove v mm in 
          `Lifted ((nmp, nd, nmm), a, b)

  (* Remove elements, that would change if the given lval would change.*)
  let remove_exp' ask (e:exp) (st:MustMap.t) (d:Diff.t) : MustMap.t * Diff.t =
    let filter_key fn (mp, d) =
      let takeNotFn k v (xs,d) =
        if fn k
        then kill' k xs d
        else (xs, d)
      in
      MustMap.fold takeNotFn mp (mp, d)
    in
    let change x = 
      try may_change ask e (Lvals.to_exp (kinteger64 IInt) x) 
      with Lattice.BotValue -> false 
        |  Lattice.TopValue -> true      
    in    
    filter_key change (st, d)

  let remove_exp ask e = lift_fun (remove_exp' ask e)

  (* normalize --- remove *-s from right hand sides *)
  let join x y = 
    let r = kill_tops (join x y) in
(*     print_endline (Pretty.sprint 80 (Pretty.dprintf "join: \n%a U\n%a\n=%a\n\n" pretty  x pretty y pretty r)); *)
    r

  (* Modifies path-map to set lhs = rhs.  If lhs \in rhs then then lhs is replaced  
     in rhs with its(lhs) previous value.*)
  let assign' ask (lhs:lval) (rhs:exp) (mp:MustMap.t) (d:Diff.t) : MustMap.t * Diff.t =
    let rec offs_from_cil o =
      match o with
        | NoOffset    -> `NoOffset
        | Field (f,o) -> `Field (f,offs_from_cil o)
        | Index (i,o) -> `Index (0L, offs_from_cil o)
    in 
    let get_val lv mp : pth option =
      try MustMap.find lv mp
      with Not_found -> Path.top ()
    in
    match lhs with
      | (Var v,NoOffset) when is_global ask v -> 
        let rlv = (v, `NoOffset) in
        let lv = Lvals.from_var_offset rlv in 
        let r_exp = Path.from_exp ask rhs in
        let new_val = Path.subst_base_var r_exp rlv (get_val lv mp) in
        kill' lv mp (Diff.add (v, (new_val,MaySet.bot ())) d)
      | (Var v,n) when is_global ask v -> 
        mp, (Diff.add (v, (Path.top (),MaySet.bot ())) d)
      | (Var v,o) -> 
        begin
          let rlv = (v, offs_from_cil o) in
          let lv = Lvals.from_var_offset rlv in 
          let r_exp = Path.from_exp ask rhs in
          let new_val = Path.subst_base_var r_exp rlv (get_val lv mp) in
          let r = 
            if Path.is_top new_val
            then kill' lv mp d
            else let e, d = remove_exp' ask (AddrOf lhs) mp d in MustMap.add lv new_val e, d
          in
(*                  print_endline (Pretty.sprint 80 (Pretty.dprintf "assign %s <- %a (%a) \n%a \n = \n%a\n" v.vname Path.pretty new_val Path.pretty r_exp MustMap.pretty mp MustMap.pretty r)); *)
          r
        end
      | (Mem e, o) -> 
        let v = remove_exp' ask (mkAddrOf lhs) mp d in
(*          print_endline (Pretty.sprint 80 (Pretty.dprintf "assign %a \n %a \n = \n%a\n" d_lval lhs MustMap.pretty mp MustMap.pretty v)); *)
        v

  let assign_may ask (lhs:lval) (rhs:exp) (mp:MayMap.t) gs df : MayMap.t * Diff.t =
    let rec from_exp e = 
      match e with
        | AlignOf _ | AlignOfE _
        | SizeOf _  | SizeOfE _ | SizeOfStr _
        | Const _ ->
            MaySet.bot ()
        | CastE (_,e) -> from_exp e 
        | AddrOf (Var v, o) -> 
            MaySet.singleton (Lvals.from_var_offset (v, Path.offs_from_cil ask o))
        | Lval (Var v, o) when not (is_global ask v) ->
            MayMap.find (Lvals.from_var_offset (v, Path.offs_from_cil ask o)) mp
        | Lval (Var v, NoOffset) ->
            snd (gs v)
        | Lval (Mem e, o) -> MaySet.top ()
        | AddrOf (Mem e, o) ->
            let io = Path.offs_from_cil ask o in
            MaySet.map (fun x -> Lvals.add_offset x io) (from_exp e)
        | BinOp (IndexPI, e, i, _) ->
            let io = 
              match ask (Queries.EvalInt i) with 
                | `Int i -> `Index (i, `NoOffset)
                | _ -> `NoOffset
            in
            MaySet.map (fun x -> Lvals.add_offset x io) (from_exp e)         
        | BinOp (_, e1, e2, _) ->
            MaySet.join (from_exp e1) (from_exp e2)         
        | _ -> 
            Messages.report ("AccessDomain::assign_may:: " ^ sprint 80 (d_plainexp () e));
            MaySet.top ()
    in
    match lhs with
      | (Var v,NoOffset) when is_global ask v -> 
        mp, Diff.add (v, (Path.bot (), from_exp rhs)) df
      | (Var v,n) when is_global ask v -> 
        mp, (Diff.add (v, (Path.bot (), MaySet.top ())) df)
      | (Var v,o) ->
        begin
          let rlv = (v, Path.offs_from_cil ask o) in
          let lv = Lvals.from_var_offset rlv in           
          let r_exp = from_exp rhs in
          MayMap.add lv r_exp mp, df
        end
      | (Mem e, o) -> 
          let l_exp = from_exp e in
          let r_exp = from_exp rhs in
          let add (v:Lvals.t) (mp,df) = 
            match Lvals.to_var_offset v with
              | [vv, o] when is_global ask vv ->
                  mp, Diff.add (vv, (Path.bot (), r_exp)) df
              | [vv, o] ->
                  MayMap.add v r_exp mp, df
              | _ -> 
                  Messages.report "now what?";
                  mp, df
          in
            if MaySet.is_top l_exp
            then (Messages.warn ("Value '" ^ (sprint 80 (d_lval () lhs)) ^ "' may point anywhere. Ignoring." ); mp, df)
            else MaySet.fold add l_exp (mp, df)
         
        
  let assign ask (lhs:lval) (rhs:exp) (gs) mp = 
    match mp with
      | `Bot -> `Bot
      | `Lifted ((mp,d,mm),a,b) ->  
          let nmp, nd = assign' ask lhs rhs mp d in
          let mm , nd = assign_may ask lhs rhs mm gs nd in
          `Lifted ((nmp, nd, mm), a, b)
  
  (* set access sets to empty *)
  let reset_accs (x : t) : t =
    match x with
      | `Bot -> `Bot
      | `Lifted (m,_,_) -> `Lifted (m,Accs.empty (),Accs.empty ())
  
  (* recursively add accesses to state *)
  let rec add_access ask exp read gs st : t =
    let exp = stripCasts exp in
    let f (mp:MustMap.t) (st:Accs.t) : Accs.t =
      let acp = Path.from_exp ask exp in
      let accs = to_acc ask acp mp gs in
      (*let accs_str = List.fold_left (fun xs x -> xs ^ " " ^ Acc.short 80 x) "" accs in 
      Messages.report (sprint 80 (Path.pretty () acp) ^ accs_str);
      *)let f st x = 
        if Acc.guaranteed_local ask x
        then st
        else Accs.add x st
      in
      List.fold_left f st accs
    in
    let rec add_idx o st =
      match o with
        | NoOffset -> st
        | Field (_,o) -> add_idx o st
        | Index (i,o) -> add_idx o (add_access ask i true gs st)
    in
    let add_next st =
      match exp with
        | Cil.UnOp  (_,e,_) -> add_access ask e true gs st
        | Cil.BinOp (_,e1,e2,_) -> add_access ask e1 true gs (add_access ask e2 true gs st)
        | Cil.AddrOf  (Cil.Var v2,o) 
        | Cil.StartOf (Cil.Var v2,o) -> st
        | Cil.Lval    (Cil.Var v2,o) -> add_idx o st
        | Cil.AddrOf  (Cil.Mem e,o) 
        | Cil.StartOf (Cil.Mem e,o) 
        | Cil.Lval    (Cil.Mem e,o) -> add_idx o (add_access ask e true gs st) 
        | _ -> st
    in
    if isConstant exp 
    then st
    else match st, read with
      | `Bot, _ -> `Bot
      | `Lifted ((m1,d, m2),a,b), true  -> add_next (`Lifted ((m1,d,m2),f m1 a,b)) 
      | `Lifted ((m1,d, m2),a,b), false -> add_next (`Lifted ((m1,d,m2),a,f m1 b))    
  
  let add_escaped ask exp write gs st =
    match st with
      | `Bot -> `Bot
      | `Lifted ((mst,d,may),a,b) ->
    let rec reachable_from (e : exp) =  
      match e with
        | AlignOf _ | AlignOfE _
        | SizeOf _  | SizeOfE _ | SizeOfStr _
        | Const _ -> MaySet.bot ()            
        | CastE (_,e) -> reachable_from e 
        | AddrOf (Var v, o) -> 
            MaySet.singleton (Lvals.from_var_offset (v, Path.offs_from_cil ask o))
        | AddrOf (Mem e, o) ->
            let io = Path.offs_from_cil ask o in
            MaySet.map (fun x -> Lvals.add_offset x io) (reachable_from e)
        | Lval (Var v, o) when not (is_global ask v) ->
            MayMap.find (Lvals.from_var_offset (v, Path.offs_from_cil ask o)) may
        | Lval (Var v, NoOffset) ->
            snd (gs v)
        | Lval (Mem e, o) ->
            let io = Path.offs_from_cil ask o in
            MaySet.map (fun x -> Lvals.add_offset x io) (reachable_from e)
        | BinOp (IndexPI, e, i, _) ->
            begin match ask (Queries.EvalInt i) with 
              | `Int i -> MaySet.map (fun x -> Lvals.add_offset x (`Index (i, `NoOffset))) (reachable_from e)
              | _ -> MaySet.join (reachable_from e) (reachable_from i)
            end
        | BinOp (_, e1, e2, _) -> MaySet.join (reachable_from e1) (reachable_from e2)
        | _ -> Messages.report ("AccessDomain::add_escaped:: " ^ sprint 80 (d_plainexp () e));
               MaySet.top ()    
    in
    let reachables = reachable_from exp in
    let f x =
      let g x =  
        match Lvals.to_var_offset x with
          | [var] -> Accs.add (AccBase (Some var))
          | _     -> fun x -> x
      in
      if MaySet.is_top reachables
      then (Messages.report ("AccessDomain::add_escaped:: set of reachable from "^(sprint 80 (d_exp () exp))^" is top"); x)
      else MaySet.fold g reachables x
    in
    if write 
    then (`Lifted ((mst, d, may), f a, b))
    else add_access ask exp true gs (`Lifted ((mst, d, may), a  , f b))

  let init_local_var v st =
    match st with
      | `Bot -> `Bot
      | `Lifted ((mst,d,may),a,b) -> `Lifted ((mst,d,MayMap.add (Lvals.from_var v) (MaySet.bot ()) may),a,b)    
  
  let get_acc write d : Acc.t list =
    let to_acc_list (a:Accs.t) mp =
      if Accs.is_top a
      then (Messages.warn "Access domain broken? It should not be top." ; [])
      else List.filter (fun x -> not (Acc.must_compiletime_constant x)) (Accs.elements a) 
    in
    match d, write with
      | `Bot, _ -> []
      | `Lifted ((mp,d,mm),r,w), true  -> to_acc_list w mp
      | `Lifted ((mp,d,mm),r,w), false -> to_acc_list r mp
    
    
end
*)