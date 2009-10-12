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

module Spec : Analyses.Spec with type Glob.Val.t = unit =
struct
  exception Top

  module Dom = PartitionDomain.SetSet (Exp)
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
            | `LvalSet rv when Queries.LS.cardinal rv == 1 -> 
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
    let assign_one_param st lv exp = add_eq a (Var lv, NoOffset) exp st in
    let f = Cilfacade.getdec f in
    if List.length args != List.length f.sformals 
    then [st, Dom.top ()]
    else
    let nst = 
      try List.fold_left2 assign_one_param st f.sformals args 
      with SetDomain.Unsupported _ -> (* ignore varargs fr now *) st
    in
    match Dom.is_bot st with
      | true -> raise Analyses.Deadcode
      | false -> 
    match reachables a args with
      | None -> [st, Dom.top ()]
      | Some xs ->
        let add x = Dom.S.add (Exp.of_clval x) in
        let rst = Queries.LS.fold add xs (Dom.S.empty ()) in
        let rst = List.fold_left (fun st v -> Dom.S.add (Lval (Var v,NoOffset)) st) rst f.sformals in
        [st,Dom.join nst (Dom.singleton rst)]
  
  
  let leave_func ask lval f args glob st1 st2 = 
    let remove_ret st =
      match lval with
        | Some lval -> remove ask lval st
        | None -> st
    in
    match Dom.is_bot st1 with
      | true -> raise Analyses.Deadcode
      | false -> 
    match reachables ask args with
      | None -> Dom.top ()
      | Some rs -> 
        let remove_reachable1 (es:Dom.S.t) (st:Dom.t) = 
          let remove_reachable2 e st =
            if reachable_from rs e then Dom.remove e st else st
          in
          Dom.S.fold remove_reachable2 es st
        in
        remove_ret (Dom.meet (Dom.fold remove_reachable1 st1 st1) st2)
    
  (* remove all variables that are reachable from arguments *)
  let special_fn ask lval f args glob st = 
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
  
  (* helper to decide equality *)
  let exp_equal ask e1 e2 (g:Glob.Var.t -> Glob.Val.t) s =
    let e1 = Cil.constFold true (Cil.stripCasts e1) in
    let e2 = Cil.constFold true (Cil.stripCasts e2) in
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

module Analysis = Multithread.Forward(Spec)
