(** Symbolic expression equalities analysis ([var_eq]). *)

module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module AD = ValueDomain.AD
module Exp = CilType.Exp
module LF = LibraryFunctions
open GoblintCil
open Analyses


module Spec =
struct
  include Analyses.DefaultSpec

  module D =
  struct
    include PartitionDomain.ExpPartitions

    (* TODO: Should string constants not be added to D in the first place, rather than filtering them for witness invariants? *)
    let rec is_str_constant = function
      | Const (CStr _ | CWStr _) -> true
      | CastE (_, _, e) -> is_str_constant e
      | _ -> false

    let invariant ~scope ss =
      fold (fun s a ->
          if B.mem MyCFG.unknown_exp s then
            a
          else (
            let s' = B.filter (fun x -> InvariantCil.exp_is_suitable ~scope x && not (is_str_constant x)) s in
            if B.cardinal s' >= 2 then (
              (* instead of returning quadratically many pairwise equalities from a cluster,
                 output linear number of equalities with just one expression *)
              let lhs = B.choose s' in (* choose arbitrary expression for lhs *)
              let rhss = B.remove lhs s' in (* and exclude it from rhs-s (no point in reflexive equality) *)
              let i = B.fold (fun rhs a ->
                  let eq = BinOp (Eq, lhs, rhs, intType) in
                  Invariant.(a && of_exp eq)
                ) rhss (Invariant.top ())
              in
              Invariant.(a && i)
            )
            else (* cannot output any equalities between just 0 or 1 usable expressions *)
              a
          )
        ) ss (Invariant.top ())
  end

  include Analyses.ValueContexts(D)

  let name () = "var_eq"

  let startstate v = D.top ()
  let threadenter man ~multiple lval f args = [D.top ()]
  let threadspawn man ~multiple lval f args fman = man.local
  let exitstate  v = D.top ()

  let typ_equal = CilType.Typ.equal (* TODO: Used to have equality checking, which ignores attributes. Is that needed? *)

  let contains_float_subexp e =
    let visitor = object
      inherit Cil.nopCilVisitor

      method! vexpr e =
        if Cilfacade.isFloatType (Cilfacade.typeOf e) then
          raise Stdlib.Exit;
        DoChildren
    end
    in
    match Cil.visitCilExpr visitor e with
    | _ -> false
    | exception Stdlib.Exit -> true
  let exp_equal e1 e2 =
    CilType.Exp.equal e1 e2 && not (contains_float_subexp e1)

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
    | CastE (_,_,e)           -> interesting e

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
      | CastE (_,t,e) -> type_may_change_t e bt
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
      | CastE (_,t,e) -> lval_may_change_pt e bl
      | Question (b, t, f, _) -> lval_may_change_pt t bl || lval_may_change_pt t bl || lval_may_change_pt f bl
    in
    let bls = pt b in
    if Queries.LS.is_top bls
    then true
    else Queries.LS.exists (lval_may_change_pt a) bls

  let may_change (ask: Queries.ask) (b:exp) (a:exp) : bool =
    (*b should be an address of something that changes*)
    let pt e = ask.f (Queries.MayPointTo e) in
    let bad = pt b in
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
      | CastE (_,t,e) -> type_may_change_t deref e
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
        | CastE   (_,t,e) -> addrOfExp e
        | _ -> None
      in
      let lval_is_not_disjoint (v,o) aad =
        let rec oleq o s =
          match o, s with
          | `NoOffset, _ -> true
          | `Field (f1,o), `Field (f2,s) when CilType.Fieldinfo.equal f1 f2 -> oleq o s
          | `Index (i1,o), `Index (i2,s) when exp_equal i1 i2     -> oleq o s
          | _ -> false
        in
        if Queries.AD.is_top aad
        then false
        else Queries.AD.exists (function
            | Addr (u,s) -> CilType.Varinfo.equal v u && oleq o (Addr.Offs.to_exp s) (* TODO: avoid conversion? *)
            | _ -> false
          ) aad
      in
      let (aad, test) =
        match addrOfExp a with
        | None -> (Queries.AD.bot (), false)
        | Some e ->
          let aad = pt e in
          (aad, lval_is_not_disjoint bl aad)
      in
      if Queries.AD.is_top aad
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
           | CastE (_,t,e) -> lval_may_change_pt e bl
           | Question (b, t, f, _) -> lval_may_change_pt b bl || lval_may_change_pt t bl || lval_may_change_pt f bl
    in
    let r =
      if Cil.isConstant b || Cil.isConstant a then false
      else if Queries.AD.is_top bad
      then ((*Messages.warn ~category:Analyzer "No PT-set: switching to types ";*) type_may_change_apt a )
      else Queries.AD.exists (function
          | Addr (v,o) -> lval_may_change_pt a (v, Addr.Offs.to_exp o) (* TODO: avoid conversion? *)
          | _ -> false
        ) bad
    in
    (*    if r
          then (Messages.warn ~category:Analyzer ~msg:("Kill " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)) (); r)
          else (Messages.warn ~category:Analyzer ~msg:("Keep " ^sprint 80 (Exp.pretty () a)^" because of "^sprint 80 (Exp.pretty () b)) (); r)
          Messages.warn ~category:Analyzer ~msg:(sprint 80 (Exp.pretty () b) ^" changed lvalues: "^sprint 80 (Queries.LS.pretty () bls)) ();
    *)
    if M.tracing then M.tracel "var_eq" "may_change %a %a = %B" CilType.Exp.pretty b CilType.Exp.pretty a r;
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
      Some (v.vglob || (ask.f (Queries.IsMultiple v) || BaseUtil.is_global ask v))
    | Lval (Mem e, _) ->
      begin match ask.f (Queries.MayPointTo e) with
        | ad when not (Queries.AD.is_top ad) ->
          Some (Queries.AD.exists (function
              | Addr (v,_) -> is_global_var ask (Lval (var v)) = Some true
              | _ -> false
            ) ad)
        | _ -> Some true
      end
    | CastE (_,t,e) -> is_global_var ask e
    | AddrOf (Var v,_) -> Some (ask.f (Queries.IsMultiple v)) (* Taking an address of a global is fine*)
    | AddrOf lv -> Some false (* TODO: sound?! *)
    | StartOf (Var v,_) ->  Some (ask.f (Queries.IsMultiple v)) (* Taking an address of a global is fine*)
    | StartOf lv -> Some false (* TODO: sound?! *)

  (* Set given lval equal to the result of given expression. On doubt do nothing. *)
  let add_eq ask (lv:lval) (rv:Exp.t) st =
    let lvt = unrollType @@ Cilfacade.typeOfLval lv in
    if M.tracing then (
      M.tracel "var_eq" "add_eq is_global_var %a = %B" d_plainlval lv (is_global_var ask (Lval lv) = Some false);
      M.tracel "var_eq" "add_eq interesting %a = %B" d_plainexp rv (interesting rv);
      M.tracel "var_eq" "add_eq is_global_var %a = %B" d_plainexp rv (is_global_var ask rv = Some false);
      M.tracel "var_eq" "add_eq type %a = %B" d_plainlval lv (isIntegralType lvt || isPointerType lvt);
    );
    if is_global_var ask (Lval lv) = Some false
    && interesting rv
    && is_global_var ask rv = Some false
    && (isIntegralType lvt || isPointerType lvt)
    then D.add_eq (rv,Lval lv) st
    else st
  (*    in
        match rv with
        | Lval rlval -> begin
            match ask (Queries.MayPointTo (mkAddrOf rlval)) with
              | rv when not (Queries.LS.is_top rv) && Queries.LS.cardinal rv = 1 ->
                  let rv = Mval.Exp.to_cil_exp (Queries.LS.choose rv) in
                  if is_local lv && Exp.is_global_var rv = Some false
                  then D.add_eq (rv,Lval lv) st
                  else st
              | _ -> st
            end
        | _ -> st
  *)

  (* removes all equalities with lval and then tries to make a new one: lval=rval *)
  let assign_eq ask lv rv st =
    add_eq ask lv rv (remove ask lv st)

  (* Give the set of reachables from argument. *)
  let reachables ~deep (ask: Queries.ask) es =
    let reachable acc e =
      let q = if deep then Queries.ReachableFrom e else Queries.MayPointTo e in
      let ad = ask.f q in
      Queries.AD.join ad acc
    in
    List.fold_left reachable (Queries.AD.empty ()) es


  (* Probably ok as is. *)
  let body man f = man.local

  (* Assume equalities from expression. *)
  let rec assume ask exp st =
    match exp with
    | BinOp (Eq, e1, e2, t) ->
      (* Pointer equalities have casts on both sides. Strip them to get to the actual Lval. *)
      begin match stripCasts e1, stripCasts e2 with
        | Lval lval, exp
        | exp, Lval lval ->
          add_eq ask lval exp st
        | _, _ ->
          st
      end
    | BinOp (LAnd, e1, e2, _) -> (* Handle for unassume. *)
      assume ask e2 (assume ask e1 st)
    | BinOp (LOr, e1, e2, _) -> (* Handle for unassume. *)
      D.join (assume ask e1 st) (assume ask e2 st)
    | _ -> st

  let branch man exp tv =
    if tv then
      assume (Analyses.ask_of_man man) exp man.local
    else
      (* TODO: support != from false branch. *)
      man.local

  (* Just remove things that go out of scope. *)
  let return man exp fundec  =
    let rm acc v = remove (Analyses.ask_of_man man) (Cil.var v) acc in
    List.fold_left rm man.local (fundec.sformals@fundec.slocals)

  let assign man (lval:lval) (rval:exp) : D.t  =
    let rval = constFold true (stripCasts rval) in
    assign_eq (Analyses.ask_of_man man) lval rval man.local

  (* First assign arguments to parameters. Then join it with reachables, to get
     rid of equalities that are not reachable. *)
  let enter man lval f args =
    let rec fold_left2 f r xs ys =
      match xs, ys with
      | x::xs, y::ys -> fold_left2 f (f r x y) xs ys
      | _ -> r
    in
    let assign_one_param st v exp =
      assign_eq (Analyses.ask_of_man man) (Cil.var v) exp st
    in
    let nst =
      try fold_left2 assign_one_param man.local f.sformals args
      with SetDomain.Unsupported _ -> (* ignore varargs fr now *) D.top ()
    in
    match D.is_bot man.local with
    | true -> raise Analyses.Deadcode
    | false -> [man.local,nst]

  let combine_env man lval fexp f args fc au (f_ask: Queries.ask) =
    let tainted = f_ask.f Queries.MayBeTainted in
    let d_local =
      (* if we are multithreaded, we run the risk, that some mutex protected variables got unlocked, so in this case caller state goes to top
         TODO: !!Unsound, this analysis does not handle this case -> regtest 63 08!! *)
      if Queries.AD.is_top tainted || not (man.ask (Queries.MustBeSingleThreaded {since_start = true})) then
        D.top ()
      else
        let taint_exp =
          Queries.AD.to_mval tainted
          |> List.map Addr.Mval.to_cil_exp
          |> Queries.ES.of_list
        in
        D.filter (fun exp -> not (Queries.ES.mem exp taint_exp)) man.local
    in
    let d = D.meet au d_local in
    match D.is_bot man.local with
    | true -> raise Analyses.Deadcode
    | false -> d

  let combine_assign man lval fexp f args fc st2 (f_ask : Queries.ask) =
    match lval with
    | Some lval -> remove (Analyses.ask_of_man man) lval man.local
    | None -> man.local

  let remove_reachable ~deep ask es st =
    let rs = reachables ~deep ask es in
    if M.tracing then M.tracel "var_eq" "remove_reachable %a: %a" (Pretty.d_list ", " d_exp) es AD.pretty rs;
    (* Prior to https://github.com/goblint/analyzer/pull/694 checks were done "in the other direction":
       each expression in st was checked for reachability from es/rs using very conservative but also unsound reachable_from.
       It is unknown, why that was necessary. *)
    Queries.AD.fold (fun addr st ->
        match addr with
        | Queries.AD.Addr.Addr mval -> remove ask (ValueDomain.Mval.to_cil mval) st
        | UnknownPtr -> D.top ()
        | _ -> st
      ) rs st

  let unknown_fn man lval f args =
    let desc = LF.find f in
    let shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } args in
    let deep_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } args in
    let shallow_args =
      match lval with
      | Some l -> mkAddrOf l :: shallow_args
      | None -> shallow_args
    in
    match D.is_bot man.local with
    | true -> raise Analyses.Deadcode
    | false ->
      let ask = Analyses.ask_of_man man in
      man.local
      |> remove_reachable ~deep:false ask shallow_args
      |> remove_reachable ~deep:true ask deep_args

  (* remove all variables that are reachable from arguments *)
  let special man lval f args =
    let desc = LibraryFunctions.find f in
    match desc.special args with
    | Identity e ->
      begin match lval with
        | Some x -> assign man x e
        | None -> unknown_fn man lval f args
      end
    | ThreadCreate { arg; _ } ->
      begin match D.is_bot man.local with
        | true -> raise Analyses.Deadcode
        | false -> remove_reachable ~deep:true (Analyses.ask_of_man man) [arg] man.local
      end
    | _ -> unknown_fn man lval f args
  (* query stuff *)

  let eq_set (e:exp) s =
    match D.find_class e s with
    | None -> Queries.ES.empty ()
    | Some es when D.B.is_bot es -> Queries.ES.bot ()
    | Some es ->
      let et = Cilfacade.typeOf e in
      let add x xs =
        Queries.ES.add (CastE (Internal,et,x)) xs
      in
      D.B.fold add es (Queries.ES.empty ())

  let rec eq_set_clos e s =
    if M.tracing then M.traceli "var_eq" "eq_set_clos %a" d_plainexp e;
    let r = match e with
      | AddrOf (Mem (BinOp (IndexPI, a, i, _)), os) ->
        (* convert IndexPI to Index offset *)
        (* TODO: this applies eq_set_clos under the offset, unlike cases below; should generalize? *)
        Queries.ES.fold (fun e acc -> (* filter_map *)
            match e with
            | CastE (_, _, StartOf a') -> (* eq_set adds casts *)
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
      | CastE (k,t,e) ->
        Queries.ES.map (fun e -> CastE (k,t,e)) (eq_set_clos e s)
    in
    if M.tracing then M.traceu "var_eq" "eq_set_clos %a = %a" d_plainexp e Queries.ES.pretty r;
    r


  let query man (type a) (x: a Queries.t): a Queries.result =
    match x with
    | Queries.EvalInt (BinOp (Eq, e1, e2, t)) when query_exp_equal (Analyses.ask_of_man man) e1 e2 man.global man.local ->
      Queries.ID.of_bool (Cilfacade.get_ikind t) true
    | Queries.EqualSet e ->
      let r = eq_set_clos e man.local in
      if M.tracing then M.tracel "var_eq" "equalset %a = %a" d_plainexp e Queries.ES.pretty r;
      r
    | Queries.Invariant context when GobConfig.get_bool "ana.var_eq.invariant.enabled" && GobConfig.get_bool "witness.invariant.exact" -> (* only exact equalities here *)
      let scope = Node.find_fundec man.node in
      D.invariant ~scope man.local
    | _ -> Queries.Result.top x

  let event man e oman =
    match e with
    | Events.Unassume {exp; _} ->
      (* Unassume must forget equalities,
         otherwise var_eq may still have a numeric first iteration equality
         while base has unassumed, causing unnecessary extra evals. *)
      Basetype.CilExp.get_vars exp
      |> List.map Cil.var
      |> List.fold_left (fun st lv ->
          remove (Analyses.ask_of_man man) lv st
        ) man.local
      |> assume (Analyses.ask_of_man man) exp (* Still naive unassume to not lose precision unassuming equalities which we know. *)
      |> D.join man.local
    | Events.Escape vars ->
      if EscapeDomain.EscapedVars.is_top vars then
        D.top ()
      else
        let ask = Analyses.ask_of_man man in
        let remove_var st v =
          remove ask (Cil.var v) st
        in
        List.fold_left remove_var man.local (EscapeDomain.EscapedVars.elements vars)
    | Events.Longjmped {lval} ->
      BatOption.map_default (fun lv ->
          let ask = Analyses.ask_of_man man in
          remove ask lv man.local)
        man.local lval
    | _ ->
      man.local
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
