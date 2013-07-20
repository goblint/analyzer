(** Variable equalities neccessary for per-element patterns. *)

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
open Analyses


module Spec =
struct
  exception Top
  
  include Analyses.DefaultSpec
  
  module D =
  struct
    include PartitionDomain.ExpPartitions
    let toXML_f sf x = 
      match toXML x with
        | Xml.Element (node, [text, _], elems) -> Xml.Element (node, [text, "Variable Equalities"], elems)
        | x -> x
        
    let toXML s  = toXML_f short s
  end

  module C = D
  module G = Lattice.Unit

  let name = "var_eq"

  let startstate v = D.top ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
    
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
  let query_exp_equal ask e1 e2 g s =
    let e1 = constFold false (stripCasts e1) in
    let e2 = constFold false (stripCasts e2) in
    if exp_equal e1 e2 then true else
    match D.find_class e1 s with
      | Some ss when D.B.mem e2 ss -> true
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
        | Const _ 
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | AlignOfE _ -> false
        | UnOp (_,e,_) -> type_may_change_t e bt      
        | BinOp (_,e1,e2,_) -> type_may_change_t e1 bt || type_may_change_t e2 bt
        | Lval (Var _,o) 
        | AddrOf (Var _,o)              
        | StartOf (Var _,o) -> may_change_t_offset o
        | Lval (Mem e,o)    
        | AddrOf (Mem e,o)  
        | StartOf (Mem e,o) -> may_change_t_offset o || type_may_change_t e bt
        | CastE (t,e) -> type_may_change_t e bt
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
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
      Queries.LS.is_top als || Queries.LS.mem (dummyFunDec.svar, `NoOffset) als || Queries.LS.mem bl als ||
      match a with
        | Const _ 
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | AlignOfE _ -> false
        | UnOp (_,e,_) -> lval_may_change_pt e bl      
        | BinOp (_,e1,e2,_) -> lval_may_change_pt e1 bl || lval_may_change_pt e2 bl
        | Lval (Var _,o) 
        | AddrOf (Var _,o)              
        | StartOf (Var _,o) -> may_change_pt_offset o
        | Lval (Mem e,o)    
        | AddrOf (Mem e,o)  
        | StartOf (Mem e,o) -> may_change_pt_offset o || lval_may_change_pt e bl 
        | CastE (t,e) -> lval_may_change_pt e bl
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
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
(*       Messages.report ("a: "^sprint 80 (d_plainexp () a)); *)
(*       Messages.report ("b: "^sprint 80 (d_plainexp () b)); *)
      match a, b with
         | Lval (Var _,NoOffset), AddrOf (Mem(Lval _),Field(_, _)) -> 
            (* lval *.field changes -> local var stays the same *)
            false
(*         | dr, Lval (Var lv,NoOffset) when (isIntegralType (typeOf dr)) && (isPointerType (lv.vtype)) && not (isIntegralType (typeOf (Lval (Mem (Lval (Var lv,NoOffset)),NoOffset)))) -> 
            (* lval *x changes -> local var stays the same *)
            false*)
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
        | Const _ 
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | AlignOfE _ -> false
        | UnOp (_,e,_) -> type_may_change_t deref e      
        | BinOp (_,e1,e2,_) -> type_may_change_t deref e1 || type_may_change_t deref e2
        | Lval (Var _,o) 
        | AddrOf (Var _,o)              
        | StartOf (Var _,o) -> may_change_t_offset o
        | Lval (Mem e,o)    -> (*Messages.report "Lval" ;*) may_change_t_offset o || type_may_change_t true e    
        | AddrOf (Mem e,o)  -> (*Messages.report "Addr" ;*) may_change_t_offset o || type_may_change_t false e  
        | StartOf (Mem e,o) -> (*Messages.report "Start";*) may_change_t_offset o || type_may_change_t false e
        | CastE (t,e) -> type_may_change_t deref e 
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    
    and lval_may_change_pt a bl : bool =
      let rec may_change_pt_offset o =
        match o with  
          | NoOffset -> false
          | Index (e,o) -> lval_may_change_pt e bl || may_change_pt_offset o
          | Field (_,o) -> may_change_pt_offset o
      in
      let rec addrOfExp e = 
        match e with
          | Lval    (Var v,o) -> Some (AddrOf (Var v,o)) 
          | AddrOf  (Var _,_) -> None              
          | StartOf (Var _,_) -> None
          | Lval    (Mem e,o) -> Some (AddrOf (Mem e, o)) 
          | AddrOf  (Mem e,o) -> (match addrOfExp e with Some e -> Some (AddrOf (Mem e, o)) | x -> x)
          | StartOf (Mem e,o) -> (match addrOfExp e with Some e -> Some (AddrOf (Mem e, o)) | x -> x)
          | CastE   (t,e) -> addrOfExp e
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
*)      if (Queries.LS.is_top als) || Queries.LS.mem (dummyFunDec.svar, `NoOffset) als
      then type_may_change_apt a 
      else test ||
      match a with
        | Const _ 
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _  
        | AlignOfE _ -> false
        | UnOp (_,e,_) -> lval_may_change_pt e bl      
        | BinOp (_,e1,e2,_) -> lval_may_change_pt e1 bl || lval_may_change_pt e2 bl
        | Lval (Var _,o) 
        | AddrOf (Var _,o)              
        | StartOf (Var _,o) -> may_change_pt_offset o
        | Lval (Mem e,o)    
        | AddrOf (Mem e,o)  
        | StartOf (Mem e,o) -> may_change_pt_offset o || lval_may_change_pt e bl 
        | CastE (t,e) -> lval_may_change_pt e bl
        | Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern."
    in 
    let r =
    if Queries.LS.is_top bls || Queries.LS.mem (dummyFunDec.svar, `NoOffset) bls
    then ((*Messages.report "No PT-set: switching to types ";*) type_may_change_apt a )
    else Queries.LS.exists (lval_may_change_pt a) bls
    in
(*    if r 
    then (Messages.report ("Kill " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)); r)
    else (Messages.report ("Keep " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)); r) 
    Messages.report (sprint 80 (Exp.pretty () b) ^" changed lvalues: "^sprint 80 (Queries.LS.pretty () bls)); 
*)    r

  (* Remove elements, that would change if the given lval would change.*)
  let remove_exp ask (e:exp) (st:D.t) : D.t =
    D.filter (fun x -> not (may_change ask e x)) st

  let remove ask (e:lval) (st:D.t) : D.t =
    remove_exp ask (mkAddrOf e) st 
    (*
    let not_in v xs = not (Exp.contains_var v xs) in
    let remove_simple (v,offs) st =      
      D.filter (not_in v) st
    in
    match ask (Queries.MayPointTo (mkAddrOf e)) with 
      | `LvalSet rv when not (Queries.LS.is_top rv) -> 
          Queries.LS.fold remove_simple rv st 
      | _ -> D.top ()
    *)
    
  (* Set given lval equal to the result of given expression. On doubt do nothing. *)
  let add_eq ask (lv:lval) (rv:Exp.t) st =
(*    let is_local x = 
      match x with (Var v,_) -> not v.vglob | _ -> false
    in
    let st = 
*)  let lvt = typeOf (Lval lv) in
(*     Messages.report (sprint 80 (d_type () lvt)); *)
      if Exp.is_global_var (Lval lv) = Some false 
      && Exp.interesting rv 
      && Exp.is_global_var rv = Some false
      && (isArithmeticType lvt || isPointerType lvt)
      then D.add_eq (rv,Lval lv) (remove ask lv st)
      else remove ask lv st 
(*    in
    match rv with
      | Lval rlval -> begin
          match ask (Queries.MayPointTo (mkAddrOf rlval)) with 
            | `LvalSet rv when not (Queries.LS.is_top rv) && Queries.LS.cardinal rv = 1 -> 
                let rv = Exp.of_clval (Queries.LS.choose rv) in
                if is_local lv && Exp.is_global_var rv = Some false 
                then D.add_eq (rv,Lval lv) st
                else st
            | _ -> st
          end
      | _ -> st
*)  
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
        | Field (f1,o1), `Field (f2,o2) when f1.fname = f2.fname -> is_prefix o1 o2
        | Index (_,o1), `Index (_,o2) -> is_prefix o1 o2
        | _ -> false
    in
    let has_reachable_prefix v1 ofs =
      let suitable_prefix (v2,ofs2) = 
             v1.vid = v2.vid 
          && is_prefix ofs ofs2
      in
      Queries.LS.exists suitable_prefix r
    in
    match e with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ 
      | UnOp  _ 
      | BinOp _ -> true
      | AddrOf  (Var v2,ofs) 
      | StartOf (Var v2,ofs) 
      | Lval    (Var v2,ofs) -> has_reachable_prefix v2 ofs
      | AddrOf  (Mem e,_) 
      | StartOf (Mem e,_) 
      | Lval    (Mem e,_)
      | CastE (_,e)           -> reachable_from r e 
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
      
  (* Probably ok as is. *)
  let body ctx f = ctx.local

  (* Branch could be improved to set invariants like base tries to do. *)
  let branch ctx exp tv = ctx.local
  
  (* Just remove things that go out of scope. *)
  let return ctx exp fundec  = 
    let rm v = remove ctx.ask (Var v,NoOffset) in
    List.fold_right rm (fundec.sformals@fundec.slocals) ctx.local   

  (* removes all equalities with lval and then tries to make a new one: lval=rval *)
  let assign ctx (lval:lval) (rval:exp) : D.t  = 
    let rval = constFold true (stripCasts rval) in
    add_eq ctx.ask lval rval ctx.local

  (* First assign arguments to parameters. Then join it with reachables, to get
     rid of equalities that are not reachable. *)
  let enter ctx lval f args = 
    let rec fold_left2 f r xs ys =
      match xs, ys with
        | x::xs, y::ys -> fold_left2 f (f r x y) xs ys
        | _ -> r
    in
    let assign_one_param st lv exp = 
      let rm = remove ctx.ask (Var lv, NoOffset) st in 
      add_eq ctx.ask (Var lv, NoOffset) exp rm 
    in
    let f = Cilfacade.getdec f in    
    let nst = 
      try fold_left2 assign_one_param ctx.local f.sformals args 
      with SetDomain.Unsupported _ -> (* ignore varargs fr now *) D.top ()
    in
    match D.is_bot ctx.local with
      | true -> raise Analyses.Deadcode
      | false -> [ctx.local,nst]
  
  let combine ctx lval fexp f args st2 = 
    match D.is_bot ctx.local with
      | true -> raise Analyses.Deadcode
      | false -> 
      match lval with
        | Some lval -> remove ctx.ask lval st2
        | None -> st2    

  let unknown_fn ctx lval f args =
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
    match D.is_bot ctx.local with
      | true -> raise Analyses.Deadcode
      | false -> 
    match reachables ctx.ask es with
      | None -> D.top ()
      | Some rs -> 
        let remove_reachable1 es st =
          let remove_reachable2 e st =
            if reachable_from rs e && not (isConstant e) then remove_exp ctx.ask e st else st
          in
          D.B.fold remove_reachable2 es st
        in
        D.fold remove_reachable1 ctx.local ctx.local

  let safe_fn = function
    | "memcpy" -> true
    | _ -> false

  (* remove all variables that are reachable from arguments *)
  let special ctx lval f args = 
    match f.vname with
      | "spinlock_check" -> 
        begin match lval with
          | Some x -> assign ctx x (List.hd args)
          | None -> unknown_fn ctx lval f args
        end
      | x when safe_fn x -> ctx.local
      | _ -> unknown_fn ctx lval f args
  (* query stuff *)
    
  let eq_set (e:exp) s =
    match D.find_class e s with
      | None -> Queries.ES.empty ()
      | Some es when D.B.is_bot es -> Queries.ES.bot ()
      | Some es -> 
        let et = typeOf e in
        let add x xs =
          Queries.ES.add (CastE (et,x)) xs
        in
        D.B.fold add es (Queries.ES.empty ())
  
  let rec eq_set_clos e s =
    match e with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ 
      | UnOp _
      | BinOp _ 
      | AddrOf  (Var _,_) 
      | StartOf (Var _,_) 
      | Lval    (Var _,_) -> eq_set e s
      | AddrOf  (Mem e,ofs) -> 
          Queries.ES.map (fun e -> mkAddrOf (mkMem e ofs)) (eq_set_clos e s)
      | StartOf (Mem e,ofs) -> 
          Queries.ES.map (fun e -> mkAddrOrStartOf (mkMem e ofs)) (eq_set_clos e s)
      | Lval    (Mem e,ofs) -> 
          Queries.ES.map (fun e -> Lval (mkMem e ofs)) (eq_set_clos e s)
      | CastE (t,e) -> 
          Queries.ES.map (fun e -> CastE (t,e)) (eq_set_clos e s)
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
      
      
  let query ctx x = 
    match x with 
      | Queries.ExpEq (e1,e2) when query_exp_equal ctx.ask e1 e2 ctx.global ctx.local -> 
          `Int (Queries.ID.of_bool true)
      | Queries.EqualSet e -> 
        let r = eq_set_clos e ctx.local in 
(*          Messages.report ("equset of "^(sprint 80 (d_exp () e))^" is "^(Queries.ES.short 80 r));  *)
        `ExprSet r
      | _ -> `Top

end

let _ = 
  MCP.register_analysis (module Spec : Spec)
