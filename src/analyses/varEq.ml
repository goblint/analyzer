module A = Analyses
module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module AD = ValueDomain.AD
module Exp = Exp.Exp
(*module BS = Base.Spec*)
module LF = LibraryFunctions
open Cil
open Pretty


module Spec =
struct
  exception Top
  
  module Dom =
  struct
    include PartitionDomain.ExpPartitions
    let toXML_f sf x = 
      match toXML x with
        | Xml.Element (node, [text, _], elems) -> Xml.Element (node, [text, "Variable Equalities"], elems)
        | x -> x
        
    let toXML s  = toXML_f short s
  end
  
  module Glob = Global.Make (Lattice.Unit)

  let name = "Partition"

  let init () = ()
  let finalize () = ()
  let startstate = Dom.top 
  let otherstate = Dom.top 
  let es_to_string f es = f.svar.vname
      
  let reset_diff x = x
  let get_diff   x = []
  let should_join x y = true

  (* Remove elements, that would change if the given lval would change.*)
  let remove ask (e:lval) (st:Dom.t) : Dom.t =
    let not_in v xs = not (Exp.contains_var v xs) in
    let remove_simple (v,offs) st =      
      Dom.filter (not_in v) st
    in
    match ask (Queries.MayPointTo (Cil.mkAddrOf e)) with 
      | `LvalSet rv when not (Queries.LS.is_top rv) -> 
          Queries.LS.fold remove_simple rv st 
      | _ -> Dom.top ()
  
  (* Set given lval equal to the result of given expression. On doubt do nothing. *)
  let add_eq ask (lv:lval) (rv:Exp.t) st =
    let is_local x = 
      match x with (Var v,_) -> not v.vglob | _ -> false
    in
    let st = 
      if is_local lv && Exp.interesting rv 
      then Dom.add_eq (rv,Lval lv) (remove ask lv st)
      else remove ask lv st 
    in
    match rv with
      | Lval rlval -> begin
          match ask (Queries.MayPointTo (Cil.mkAddrOf rlval)) with 
            | `LvalSet rv when not (Queries.LS.is_top rv) && Queries.LS.cardinal rv = 1 -> 
                let rv = Exp.of_clval (Queries.LS.choose rv) in
                if is_local lv && Exp.is_global_var rv = Some false 
                then Dom.add_eq (rv,Lval lv) st
                else st
            | _ -> st
          end
      | _ -> st
  
  (* Give the set of reachables from argument. *)
  let reachables ask es = 
    let reachable e st = 
      match st with 
        | None -> None
        | Some st ->
      match ask (Queries.ReachableFrom e) with
        | `LvalSet vs -> Some (Queries.LS.join vs st)
        | _ -> None
    in
    List.fold_right reachable es (Some (Queries.LS.empty ()))   

  let rec reachable_from (r:Queries.LS.t) e =
    if Queries.LS.is_top r then true else
    let rec is_prefix x1 x2 =
      match x1, x2 with
        | _, `NoOffset -> true
        | Cil.Field (f1,o1), `Field (f2,o2) when f1.Cil.fname = f2.Cil.fname -> is_prefix o1 o2
        | Cil.Index (_,o1), `Index (_,o2) -> is_prefix o1 o2
        | _ -> false
    in
    let has_reachable_prefix v1 ofs =
      let suitable_prefix (v2,ofs2) = 
             v1.Cil.vid = v2.Cil.vid 
          && is_prefix ofs ofs2
      in
      Queries.LS.exists suitable_prefix r
    in
    match e with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.Const _ 
      | Cil.AlignOfE _ 
      | Cil.UnOp  _ 
      | Cil.BinOp _ -> true
      | Cil.AddrOf  (Cil.Var v2,ofs) 
      | Cil.StartOf (Cil.Var v2,ofs) 
      | Cil.Lval    (Cil.Var v2,ofs) -> has_reachable_prefix v2 ofs
      | Cil.AddrOf  (Cil.Mem e,_) 
      | Cil.StartOf (Cil.Mem e,_) 
      | Cil.Lval    (Cil.Mem e,_)
      | Cil.CastE (_,e)           -> reachable_from r e 
      
  (* Probably ok as is. *)
  let body a f glob st = st
  let fork a lval f args glob st = []
  
  (* this makes it grossly unsound *)
  let eval_funvar a exp glob st = []

  (* Branch could be improved to set invariants like base tries to do. *)
  let branch a exp tv glob st     = st
  
  (* Just remove things that go out of scope. *)
  let return a exp fundec glob st = 
    let rm v = remove a (Var v,NoOffset) in
    List.fold_right rm (fundec.sformals@fundec.slocals) st   

  (* removes all equalities with lval and then tries to make a new one: lval=rval *)
  let assign ask (lval:lval) (rval:exp) (glob:Glob.Var.t -> Glob.Val.t) (st:Dom.t) : Dom.t  = 
    let rval = Cil.constFold true (Cil.stripCasts rval) in
    add_eq ask lval rval st

  (* First assign arguments to parameters. Then join it with reachables, to get
     rid of equalities that are not reachable. *)
  let enter_func a lval f args glob st = 
    let rec fold_left2 f r xs ys =
      match xs, ys with
        | x::xs, y::ys -> fold_left2 f (f r x y) xs ys
        | _ -> r
    in
    let assign_one_param st lv exp = 
      let rm = remove a (Var lv, NoOffset) st in 
      add_eq a (Var lv, NoOffset) exp rm 
    in
    let f = Cilfacade.getdec f in    
    let nst = 
      try fold_left2 assign_one_param st f.sformals args 
      with SetDomain.Unsupported _ -> (* ignore varargs fr now *) Dom.top ()
    in
    match Dom.is_bot st with
      | true -> raise Analyses.Deadcode
      | false -> [st,nst]
  
  let leave_func ask lval f args glob st1 st2 = 
    match Dom.is_bot st1 with
      | true -> raise Analyses.Deadcode
      | false -> 
      match lval with
        | Some lval -> remove ask lval st2
        | None -> st2    

  (* remove all variables that are reachable from arguments *)
  let special_fn ask lval f args glob st = 
    let args =
      match LF.get_invalidate_action f.vname with
        | Some fnc -> fnc `Write args
        | _ -> args
    in
    let es = 
      match lval with
        | Some l -> mkAddrOf l :: args
        | None -> args
    in
    match Dom.is_bot st with
      | true -> raise Analyses.Deadcode
      | false -> 
    let true_exp = Cil.integer 1 in
    match reachables ask es with
      | None -> [Dom.top (), true_exp, true]
      | Some rs -> 
        let remove_reachable1 es st =
          let remove_reachable2 e st =
            if reachable_from rs e then Dom.remove e st else st
          in
          Dom.S.fold remove_reachable2 es st
        in
        [Dom.fold remove_reachable1 st st, true_exp, true]
    
  (* query stuff *)
  
  let rec const_equal c1 c2 =
    match c1, c2 with
      |	CStr s1  , CStr s2	 -> s1 = s2
      |	CWStr is1, CWStr is2 -> is1 = is2
      |	CChr c1  , CChr c2   -> c1 = c2
      |	CInt64 (v1,k1,_), CInt64 (v2,k2,_) -> v1 = v2 && k1 = k2
      |	CReal (f1,k1,_) , CReal (f2,k2,_)  -> f1 = f2 && k1 = k2
      |	CEnum (_,n1,e1), CEnum (_,n2,e2) -> n1 = n2 && e1.ename = e2.ename  
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
      |	Const c1,	Const c2 -> const_equal c1 c2
      |	AddrOf l1,	AddrOf l2   
      |	StartOf l1,	StartOf l2 
      |	Lval l1 ,	Lval  l2 -> lval_equal l1 l2
      |	SizeOf t1,	SizeOf t2 -> typ_equal t1 t2
      |	SizeOfE e1,	SizeOfE e2 -> exp_equal e1 e2  
      |	SizeOfStr s1,	SizeOfStr s2 -> s1 = s2
      |	AlignOf t1,	AlignOf t2 -> typ_equal t1 t2
      |	AlignOfE e1,	AlignOfE e2 -> exp_equal e1 e2
      |	UnOp (o1,e1,t1),	UnOp (o2,e2,t2) -> o1 = o2 && typ_equal t1 t2 && exp_equal e1 e2
      |	BinOp (o1,e11,e21,t1),	BinOp(o2,e12,e22,t2) -> o1 = o2 && typ_equal t1 t2 && exp_equal e11 e12 && exp_equal e21 e22     
      |	CastE (t1,e1),	CastE (t2,e2) -> typ_equal t1 t2 && exp_equal e1 e2
      | _ -> false
    
  (* helper to decide equality *)
  let exp_equal ask e1 e2 (g:Glob.Var.t -> Glob.Val.t) s =
    let e1 = Cil.constFold false (Cil.stripCasts e1) in
    let e2 = Cil.constFold false (Cil.stripCasts e2) in
    if exp_equal e1 e2 then true else
    match Dom.find_class e1 s with
      | Some ss when Dom.S.mem e2 ss -> true
      | _ -> false
  
  let eq_set (e:Cil.exp) s =
    match Dom.find_class e s with
      | None -> Queries.ES.empty ()
      | Some e when Dom.S.is_bot e -> Queries.ES.bot ()
      | Some e -> Dom.S.fold Queries.ES.add e (Queries.ES.empty ())
  
  let rec eq_set_clos e s =
    match e with
      | Cil.SizeOf _
      | Cil.SizeOfE _
      | Cil.SizeOfStr _
      | Cil.AlignOf _  
      | Cil.Const _ 
      | Cil.AlignOfE _ 
      | Cil.UnOp _
      | Cil.BinOp _ 
      | Cil.AddrOf  (Cil.Var _,_) 
      | Cil.StartOf (Cil.Var _,_) 
      | Cil.Lval    (Cil.Var _,_) -> eq_set e s
      | Cil.AddrOf  (Cil.Mem e,ofs) -> 
          Queries.ES.map (fun e -> mkAddrOf (Cil.mkMem e ofs)) (eq_set_clos e s)
      | Cil.StartOf (Cil.Mem e,ofs) -> 
          Queries.ES.map (fun e -> mkAddrOrStartOf (Cil.mkMem e ofs)) (eq_set_clos e s)
      | Cil.Lval    (Cil.Mem e,ofs) -> 
          Queries.ES.map (fun e -> Cil.Lval (Cil.mkMem e ofs)) (eq_set_clos e s)
      | Cil.CastE (t,e) -> 
          Queries.ES.map (fun e -> Cil.CastE (t,e)) (eq_set_clos e s)
      
      
  let query a g s x = 
    match x with 
      | Queries.ExpEq (e1,e2) when exp_equal a e1 e2 g s -> `Int (Queries.ID.of_bool true)
      | Queries.EqualSet e -> 
        let r = eq_set_clos e s in 
(*         Messages.report ("equset of "^(sprint 80 (Cil.d_exp () e))^" is "^(Queries.ES.short 80 r)); *)
        `ExprSet r
      | _ -> `Top

end

module VarEqMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "var_eq" 
                type lf = Spec.Dom.t
                let inject_l x = `VarEq x
                let extract_l x = match x with `VarEq x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Analysis = Multithread.Forward(Spec)
