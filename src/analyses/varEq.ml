(** Variable equalities necessary for per-element patterns. *)

module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module AD = ValueDomain.AD
module Exp = CilType.Exp
module LF = LibraryFunctions
open Prelude.Ana
open Analyses


module Spec =
struct
  exception Top

  include Analyses.DefaultSpec

  module D =
  struct
    include PartitionDomain.ExpPartitions

    let invariant ~scope ss =
      fold (fun s a ->
          if B.mem MyCFG.unknown_exp s then
            a
          else
            let module B_prod = BatSet.Make2 (Exp) (Exp) in
            let s_prod = B_prod.cartesian_product s s in
            let i = B_prod.Product.fold (fun (x, y) a ->
                if Exp.compare x y < 0 && not (InvariantCil.exp_contains_tmp x) && not (InvariantCil.exp_contains_tmp y) && InvariantCil.exp_is_in_scope scope x && InvariantCil.exp_is_in_scope scope y then (* each equality only one way, no self-equalities *)
                  let eq = BinOp (Eq, x, y, intType) in
                  Invariant.(a && of_exp eq)
                else
                  a
              ) s_prod (Invariant.top ())
            in
            Invariant.(a && i)
        ) ss (Invariant.top ())
  end

  module C = D

  let name () = "var_eq"

  let startstate v = D.top ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let const_equal c1 c2 =
    match c1, c2 with
    |	CStr (s1,_)  , CStr (s2,_)	 -> s1 = s2
    |	CWStr (is1,_), CWStr (is2,_) -> is1 = is2
    |	CChr c1  , CChr c2   -> c1 = c2
    |	CInt (v1,k1,_), CInt (v2,k2,_) -> Cilint.compare_cilint v1 v2 = 0 && k1 = k2
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
    | TFun (rt1, arg1, _,  b1), TFun (rt2, arg2, _, b2) -> b1 = b2 && typ_equal rt1 rt2 && option_eq (GobList.equal args_eq) arg1 arg2
    | TNamed (ti1, _), TNamed (ti2, _) -> ti1.tname = ti2.tname && typ_equal ti1.ttype ti2.ttype
    | TComp (c1,_), TComp (c2,_) -> CilType.Compinfo.equal c1 c2
    | TEnum (e1,_), TEnum (e2,_) -> e1.ename = e2.ename && GobList.equal eitem_eq e1.eitems e2.eitems
    | TBuiltin_va_list _, TBuiltin_va_list _ -> true
    | _ -> false

  and lval_equal (l1,o1) (l2,o2) =
    let rec offs_equal o1 o2 =
      match o1, o2 with
      | NoOffset, NoOffset -> true
      | Field (f1, o1), Field (f2,o2) -> CilType.Fieldinfo.equal f1 f2 && offs_equal o1 o2
      | Index (i1,o1), Index (i2,o2) -> exp_equal i1 i2 && offs_equal o1 o2
      | _ -> false
    in
    offs_equal o1 o2
    && match l1, l2 with
    | Var v1, Var v2 -> CilType.Varinfo.equal v1 v2
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

  let contains_float_subexp e =
    let visitor = object
      inherit Cil.nopCilVisitor

      method! vexpr e =
        let t = Cil.unrollType (Cilfacade.typeOf e) in
        begin match t with
          | TFloat _ -> raise Exit
          | _ -> ()
        end;
        DoChildren
    end
    in
    match Cil.visitCilExpr visitor e with
    | _ -> false
    | exception Exit -> true
  let exp_equal e1 e2 =
    exp_equal e1 e2 && not (contains_float_subexp e1)

  (* TODO: what does interesting mean? *)
  let rec interesting x =
    match x with
    | AddrOf (Mem (BinOp (IndexPI, a, _i, _)), _os) ->
      interesting a
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _
    | UnOp  _
    | BinOp _
    | Question _
    | Real _
    | Imag _
    | AddrOfLabel _ -> false
    | Const _ -> true
    | AddrOf  (Var v2,_)
    | StartOf (Var v2,_)
    | Lval    (Var v2,_) -> true
    | AddrOf  (Mem e,_)
    | StartOf (Mem e,_)
    | Lval    (Mem e,_)
    | CastE (_,e)           -> interesting e

  (* helper to decide equality *)
  let query_exp_equal ask e1 e2 g s =
    let e1 = constFold false (stripCasts e1) in
    let e2 = constFold false (stripCasts e2) in
    if exp_equal e1 e2 then true else
      match D.find_class e1 s with
      | Some ss when D.B.mem e2 ss -> true
      | _ -> false

  (* kill predicate for must-equality kind of analyses*)
  (* TODO: why unused? how different from below? *)
  let may_change_t (b:exp) (a:exp) : bool =
    let rec type_may_change_t a bt =
      let rec may_change_t_offset o =
        match o with
        | NoOffset -> false
        | Index (e,o) -> type_may_change_t e bt || may_change_t_offset o
        | Field (_,o) -> may_change_t_offset o
      in
      let at = Cilfacade.typeOf a in
      (isIntegralType at && isIntegralType bt) || (typ_equal at bt) ||
      match a with
      | Const _
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | AlignOfE _
      | AddrOfLabel _ -> false (* TODO: some may contain exps? *)
      | UnOp (_,e,_)
      | Real e
      | Imag e -> type_may_change_t e bt
      | BinOp (_,e1,e2,_) -> type_may_change_t e1 bt || type_may_change_t e2 bt
      | Lval (Var _,o)
      | AddrOf (Var _,o)
      | StartOf (Var _,o) -> may_change_t_offset o
      | Lval (Mem e,o)
      | AddrOf (Mem e,o)
      | StartOf (Mem e,o) -> may_change_t_offset o || type_may_change_t e bt
      | CastE (t,e) -> type_may_change_t e bt
      | Question (b, t, f, _) -> type_may_change_t b bt || type_may_change_t t bt || type_may_change_t f bt
    in
    let bt =  unrollTypeDeep (Cilfacade.typeOf b) in
    type_may_change_t a bt

  (* TODO: why unused? how different from below? *)
  let may_change_pt ask (b:exp) (a:exp) : bool =
    let pt e = ask (Queries.MayPointTo e) in
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
      | AlignOfE _
      | AddrOfLabel _ -> false (* TODO: some may contain exps? *)
      | UnOp (_,e,_)
      | Real e
      | Imag e -> lval_may_change_pt e bl
      | BinOp (_,e1,e2,_) -> lval_may_change_pt e1 bl || lval_may_change_pt e2 bl
      | Lval (Var _,o)
      | AddrOf (Var _,o)
      | StartOf (Var _,o) -> may_change_pt_offset o
      | Lval (Mem e,o)
      | AddrOf (Mem e,o)
      | StartOf (Mem e,o) -> may_change_pt_offset o || lval_may_change_pt e bl
      | CastE (t,e) -> lval_may_change_pt e bl
      | Question (b, t, f, _) -> lval_may_change_pt t bl || lval_may_change_pt t bl || lval_may_change_pt f bl
    in
    let bls = pt b in
    if Queries.LS.is_top bls
    then true
    else Queries.LS.exists (lval_may_change_pt a) bls

  let may_change (ask: Queries.ask) (b:exp) (a:exp) : bool =
    (*b should be an address of something that changes*)
    let pt e = ask.f (Queries.MayPointTo e) in
    let bls = pt b in
    let bt =
      match unrollTypeDeep (Cilfacade.typeOf b) with
      | TPtr (t,_) -> t
      | exception Cilfacade.TypeOfError _
      | _ -> voidType
    in (* type of thing that changed: typeof( *b ) *)
    let rec type_may_change_apt a =
      (* With abstract points-to (like in type invariants in accesses).
         Here we implement it in part --- minimum to protect local integers. *)
      match a, b with
      | Lval (Var _,NoOffset), AddrOf (Mem(Lval _),Field(_, _)) ->
        (* lval *.field changes -> local var stays the same *)
        false
      (*         | dr, Lval (Var lv,NoOffset) when (isIntegralType (Cilfacade.typeOf dr)) && (isPointerType (lv.vtype)) && not (isIntegralType (Cilfacade.typeOfLval (Mem (Lval (Var lv,NoOffset)),NoOffset))) ->
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
        match unrollTypeDeep (Cilfacade.typeOf a) with
        | TPtr (t,a) -> t
        | at -> at
      in
      bt = voidType || (isIntegralType at && isIntegralType bt) || (deref && typ_equal (TPtr (at,[]) ) bt) || typ_equal at bt ||
              match a with
              | Const _
              | SizeOf _
              | SizeOfE _
              | SizeOfStr _
              | AlignOf _
              | AlignOfE _
              | AddrOfLabel _ -> false (* TODO: some may contain exps? *)
              | UnOp (_,e,_)
              | Real e
              | Imag e -> type_may_change_t deref e
              | BinOp (_,e1,e2,_) -> type_may_change_t deref e1 || type_may_change_t deref e2
              | Lval (Var _,o)
              | AddrOf (Var _,o)
              | StartOf (Var _,o) -> may_change_t_offset o
              | Lval (Mem e,o)    -> may_change_t_offset o || type_may_change_t true e
              | AddrOf (Mem e,o)  -> may_change_t_offset o || type_may_change_t false e
              | StartOf (Mem e,o) -> may_change_t_offset o || type_may_change_t false e
              | CastE (t,e) -> type_may_change_t deref e
              | Question (b, t, f, _) -> type_may_change_t deref b || type_may_change_t deref t || type_may_change_t deref f

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
          | `Field (f1,o), `Field (f2,s) when CilType.Fieldinfo.equal f1 f2 -> oleq o s
          | `Index (i1,o), `Index (i2,s) when exp_equal i1 i2     -> oleq o s
          | _ -> false
        in
        if Queries.LS.is_top als
        then false
        else Queries.LS.exists (fun (u,s) -> CilType.Varinfo.equal v u && oleq o s) als
      in
      let (als, test) =
        match addrOfExp a with
        | None -> (Queries.LS.bot (), false)
        | Some e ->
          let als = pt e in
          (als, lval_is_not_disjoint bl als)
      in
      if (Queries.LS.is_top als) || Queries.LS.mem (dummyFunDec.svar, `NoOffset) als
      then type_may_change_apt a
      else test ||
           match a with
           | Const _
           | SizeOf _
           | SizeOfE _
           | SizeOfStr _
           | AlignOf _
           | AlignOfE _
           | AddrOfLabel _ -> false (* TODO: some may contain exps? *)
           | UnOp (_,e,_)
           | Real e
           | Imag e -> lval_may_change_pt e bl
           | BinOp (_,e1,e2,_) -> lval_may_change_pt e1 bl || lval_may_change_pt e2 bl
           | Lval (Var _,o)
           | AddrOf (Var _,o)
           | StartOf (Var _,o) -> may_change_pt_offset o
           | Lval (Mem e,o)
           | AddrOf (Mem e,o)
           | StartOf (Mem e,o) -> may_change_pt_offset o || lval_may_change_pt e bl
           | CastE (t,e) -> lval_may_change_pt e bl
           | Question (b, t, f, _) -> lval_may_change_pt b bl || lval_may_change_pt t bl || lval_may_change_pt f bl
    in
    let r =
      if Cil.isConstant b then false
      else if Queries.LS.is_top bls || Queries.LS.mem (dummyFunDec.svar, `NoOffset) bls
      then ((*Messages.warn ~category:Analyzer "No PT-set: switching to types ";*) type_may_change_apt a )
      else Queries.LS.exists (lval_may_change_pt a) bls
    in
    (*    if r
          then (Messages.warn ~category:Analyzer ~msg:("Kill " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)) (); r)
          else (Messages.warn ~category:Analyzer ~msg:("Keep " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)) (); r)
          Messages.warn ~category:Analyzer ~msg:(sprint 80 (Exp.pretty () b) ^" changed lvalues: "^sprint 80 (Queries.LS.pretty () bls)) ();
    *)
    if M.tracing then M.tracel "var_eq" "may_change %a %a = %B\n" CilType.Exp.pretty b CilType.Exp.pretty a r;
    r

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
      | rv when not (Queries.LS.is_top rv) ->
          Queries.LS.fold remove_simple rv st
      | _ -> D.top ()
    *)

  let rec is_global_var (ask: Queries.ask) x =
    match x with
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _
    | UnOp _
    | BinOp _
    | Question _
    | AddrOfLabel _
    | Real _
    | Imag _ -> None
    | Const _ -> Some false
    | Lval (Var v,_) ->
      Some (v.vglob || (ask.f (Queries.IsMultiple v)))
    | Lval (Mem e, _) ->
      begin match ask.f (Queries.MayPointTo e) with
        | ls when not (Queries.LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) ls) ->
          Some (Queries.LS.exists (fun (v, _) -> is_global_var ask (Lval (var v)) = Some true) ls)
        | _ -> Some true
      end
    | CastE (t,e) -> is_global_var ask e
    | AddrOf (Var v,_) -> Some (ask.f (Queries.IsMultiple v)) (* Taking an address of a global is fine*)
    | AddrOf lv -> Some false (* TODO: sound?! *)
    | StartOf (Var v,_) ->  Some (ask.f (Queries.IsMultiple v)) (* Taking an address of a global is fine*)
    | StartOf lv -> Some false (* TODO: sound?! *)

  (* Set given lval equal to the result of given expression. On doubt do nothing. *)
  let add_eq ask (lv:lval) (rv:Exp.t) st =
    let lvt = unrollType @@ Cilfacade.typeOfLval lv in
    if M.tracing then (
      M.tracel "var_eq" "add_eq is_global_var %a = %B\n" d_plainlval lv (is_global_var ask (Lval lv) = Some false);
      M.tracel "var_eq" "add_eq interesting %a = %B\n" d_plainexp rv (interesting rv);
      M.tracel "var_eq" "add_eq is_global_var %a = %B\n" d_plainexp rv (is_global_var ask rv = Some false);
      M.tracel "var_eq" "add_eq type %a = %B\n" d_plainlval lv ((isArithmeticType lvt && match lvt with | TFloat _ -> false | _ -> true ) || isPointerType lvt);
    );
    if is_global_var ask (Lval lv) = Some false
    && interesting rv
    && is_global_var ask rv = Some false
    && ((isArithmeticType lvt && match lvt with | TFloat _ -> false | _ -> true ) || isPointerType lvt)
    then D.add_eq (rv,Lval lv) (remove ask lv st)
    else remove ask lv st
  (*    in
        match rv with
        | Lval rlval -> begin
            match ask (Queries.MayPointTo (mkAddrOf rlval)) with
              | rv when not (Queries.LS.is_top rv) && Queries.LS.cardinal rv = 1 ->
                  let rv = Lval.CilLval.to_exp (Queries.LS.choose rv) in
                  if is_local lv && Exp.is_global_var rv = Some false
                  then D.add_eq (rv,Lval lv) st
                  else st
              | _ -> st
            end
        | _ -> st
  *)
  (* Give the set of reachables from argument. *)
  let reachables ~deep (ask: Queries.ask) es =
    let reachable e st =
      match st with
      | None -> None
      | Some st ->
        let q = if deep then Queries.ReachableFrom e else Queries.MayPointTo e in
        let vs = ask.f q in
        if Queries.LS.is_top vs then
          None
        else
          Some (Queries.LS.join vs st)
    in
    List.fold_right reachable es (Some (Queries.LS.empty ()))


  (* Probably ok as is. *)
  let body ctx f = ctx.local

  (* Branch could be improved to set invariants like base tries to do. *)
  let branch ctx exp tv = ctx.local

  (* Just remove things that go out of scope. *)
  let return ctx exp fundec  =
    let rm v = remove (Analyses.ask_of_ctx ctx) (Var v,NoOffset) in
    List.fold_right rm (fundec.sformals@fundec.slocals) ctx.local

  (* removes all equalities with lval and then tries to make a new one: lval=rval *)
  let assign ctx (lval:lval) (rval:exp) : D.t  =
    let rval = constFold true (stripCasts rval) in
    add_eq (Analyses.ask_of_ctx ctx) lval rval ctx.local

  (* First assign arguments to parameters. Then join it with reachables, to get
     rid of equalities that are not reachable. *)
  let enter ctx lval f args =
    let rec fold_left2 f r xs ys =
      match xs, ys with
      | x::xs, y::ys -> fold_left2 f (f r x y) xs ys
      | _ -> r
    in
    let assign_one_param st lv exp =
      let rm = remove (Analyses.ask_of_ctx ctx) (Var lv, NoOffset) st in
      add_eq (Analyses.ask_of_ctx ctx) (Var lv, NoOffset) exp rm
    in
    let nst =
      try fold_left2 assign_one_param ctx.local f.sformals args
      with SetDomain.Unsupported _ -> (* ignore varargs fr now *) D.top ()
    in
    match D.is_bot ctx.local with
    | true -> raise Analyses.Deadcode
    | false -> [ctx.local,nst]

  let combine ctx lval fexp f args fc st2 =
    match D.is_bot ctx.local with
    | true -> raise Analyses.Deadcode
    | false ->
      match lval with
      | Some lval -> remove (Analyses.ask_of_ctx ctx) lval st2
      | None -> st2

  let remove_reachable ~deep ask es st =
    match reachables ~deep ask es with
    | None -> D.top ()
    | Some rs ->
      (* Prior to https://github.com/goblint/analyzer/pull/694 checks were done "in the other direction":
         each expression in st was checked for reachability from es/rs using very conservative but also unsound reachable_from.
         It is unknown, why that was necessary. *)
      Queries.LS.fold (fun lval st ->
          remove ask (Lval.CilLval.to_lval lval) st
        ) rs st

  let unknown_fn ctx lval f args =
    let desc = LF.find f in
    let shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } args in
    let deep_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } args in
    let shallow_args =
      match lval with
      | Some l -> mkAddrOf l :: shallow_args
      | None -> shallow_args
    in
    match D.is_bot ctx.local with
    | true -> raise Analyses.Deadcode
    | false ->
      let ask = Analyses.ask_of_ctx ctx in
      ctx.local
      |> remove_reachable ~deep:false ask shallow_args
      |> remove_reachable ~deep:true ask deep_args

  (* remove all variables that are reachable from arguments *)
  let special ctx lval f args =
    let desc = LibraryFunctions.find f in
    match desc.special args with
    | Identity e ->
      begin match lval with
        | Some x -> assign ctx x e
        | None -> unknown_fn ctx lval f args
      end
    | ThreadCreate { arg; _ } ->
      begin match D.is_bot ctx.local with
      | true -> raise Analyses.Deadcode
      | false -> remove_reachable ~deep:true (Analyses.ask_of_ctx ctx) [arg] ctx.local
      end
    | _ -> unknown_fn ctx lval f args
  (* query stuff *)

  let eq_set (e:exp) s =
    match D.find_class e s with
    | None -> Queries.ES.empty ()
    | Some es when D.B.is_bot es -> Queries.ES.bot ()
    | Some es ->
      let et = Cilfacade.typeOf e in
      let add x xs =
        Queries.ES.add (CastE (et,x)) xs
      in
      D.B.fold add es (Queries.ES.empty ())

  let rec eq_set_clos e s =
    if M.tracing then M.traceli "var_eq" "eq_set_clos %a\n" d_plainexp e;
    let r = match e with
      | AddrOf (Mem (BinOp (IndexPI, a, i, _)), os) ->
        (* convert IndexPI to Index offset *)
        (* TODO: this applies eq_set_clos under the offset, unlike cases below; should generalize? *)
        Queries.ES.fold (fun e acc -> (* filter_map *)
            match e with
            | CastE (_, StartOf a') -> (* eq_set adds casts *)
              let e' = AddrOf (Cil.addOffsetLval (Index (i, os)) a') in (* TODO: re-add cast? *)
              Queries.ES.add e' acc
            | _ -> acc
          ) (eq_set_clos a s) (Queries.ES.empty ())
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | Const _
      | AlignOfE _
      | UnOp _
      | BinOp _
      | Question _
      | AddrOfLabel _
      | Real _
      | Imag _
      | AddrOf  (Var _,_)
      | StartOf (Var _,_)
      | Lval    (Var _,_) -> eq_set e s
      | AddrOf  (Mem e,ofs) ->
        Queries.ES.map (fun e -> mkAddrOf (mkMem ~addr:e ~off:ofs)) (eq_set_clos e s)
      | StartOf (Mem e,ofs) ->
        Queries.ES.map (fun e -> mkAddrOrStartOf (mkMem ~addr:e ~off:ofs)) (eq_set_clos e s)
      | Lval    (Mem e,ofs) ->
        Queries.ES.map (fun e -> Lval (mkMem ~addr:e ~off:ofs)) (eq_set_clos e s)
      | CastE (t,e) ->
        Queries.ES.map (fun e -> CastE (t,e)) (eq_set_clos e s)
    in
    if M.tracing then M.traceu "var_eq" "eq_set_clos %a = %a\n" d_plainexp e Queries.ES.pretty r;
    r


  let query ctx (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.EvalInt (BinOp (Eq, e1, e2, t)) when query_exp_equal (Analyses.ask_of_ctx ctx) e1 e2 ctx.global ctx.local ->
      Queries.ID.of_bool (Cilfacade.get_ikind t) true
    | Queries.EqualSet e ->
      let r = eq_set_clos e ctx.local in
      if M.tracing then M.tracel "var_eq" "equalset %a = %a\n" d_plainexp e Queries.ES.pretty r;
      r
    | Queries.Invariant context ->
      let scope = Node.find_fundec ctx.node in
      D.invariant ~scope ctx.local
    | _ -> Queries.Result.top x

end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
