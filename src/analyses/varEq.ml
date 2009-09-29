module A = Analyses
module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module AD = ValueDomain.AD
(*module BS = Base.Spec*)
module LF = LibraryFunctions
open Cil
open Pretty


module Spec : Analyses.Spec with type Glob.Val.t = unit =
struct
  exception Top

  module Dom = PartitionDomain.SetSet (Lval.CilLval)
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

  (* This is a very conservative remove operation, it removes all elements that
     share the same toplevel variable *)
  let remove (v,_) st = 
    let removel1 es st =
      let removel2 ((v2,_) as e) st =
        if v.vid = v2.vid then Dom.remove e st else st
      in
      Dom.S.fold removel2 es st
    in
    if Dom.is_bot st
    then st
    else Dom.fold removel1 st st

  (* Set given lval equal to the result of given expression. On doubt do nothing. *)
  let add_eq ask lv rv st =
    let is_global (v,_) = v.vglob in 
    let st = remove lv st in
    match rv with
      | Lval rlval -> begin
          let rv  = ask (Queries.MayPointTo (Cil.mkAddrOf rlval)) in
          match rv with 
            | `LvalSet rv when Queries.LS.cardinal rv == 1 -> 
                let rv = Queries.LS.choose rv in
                if not (is_global lv || is_global rv) 
                then Dom.add_eq (rv,lv) st
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

  (* Probably ok as is. *)
  let body a f glob st = st
  let fork a lval f args glob st = []
  
  (* this makes it grossly unsound *)
  let eval_funvar a exp glob st = []

  (* Branch could be improved to set invariants like base tries to do. *)
  let branch a exp tv glob st     = st
  
  (* Just remove things that go out of scope. *)
  let return a exp fundec glob st = 
    let rm v = remove (v,`NoOffset) in
    List.fold_right rm (fundec.sformals@fundec.slocals) st   

  (* removes all equalities with lval and then tries to make a new one: lval=rval *)
  let assign ask (lval:lval) (rval:exp) (glob:Glob.Var.t -> Glob.Val.t) (st:Dom.t) : Dom.t  = 
    let l = ask (Queries.MayPointTo (Cil.mkAddrOf lval)) in   
    match l with
      | `LvalSet l when Queries.LS.cardinal l == 1 -> 
          let l = Queries.LS.choose l in
          add_eq ask l rval st
      | `LvalSet l when not (Queries.LS.is_top l) ->
          Queries.LS.fold remove l st 
      | _ -> Dom.top ()

  (* First assign arguments to parameters. Then join it with reachables, to get
     rid of equalities that are not reachable. *)
  let enter_func a lval f args glob st = 
    let assign_one_param st lv exp = add_eq a (lv, `NoOffset) exp st in
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
        let rst = Queries.LS.fold Dom.S.add xs (Dom.S.empty ()) in
        let rst = List.fold_left (fun st v -> Dom.S.add (v,`NoOffset) st) rst f.sformals in
        [st,Dom.join nst (Dom.singleton rst)]
  
  
  let leave_func ask lval f args glob st1 st2 = 
    let st1 =
      match lval with
        | Some lval -> begin
            match ask (Queries.MayPointTo (Cil.mkAddrOf lval)) with
              | `LvalSet l when not (Queries.LS.is_top l) ->
                  Queries.LS.fold remove l st1 
              | _ -> Dom.top ();
           end
        | None -> st1 
    in
    match Dom.is_bot st1 with
      | true -> raise Analyses.Deadcode
      | false -> 
    match reachables ask args with
      | None -> Dom.top ()
      | Some rs -> 
        let remove_reachable1 es st =
          let remove_reachable2 e st =
            if Queries.LS.mem e rs then remove e st else st
          in
          Dom.S.fold remove_reachable2 es st
        in
        Dom.meet (Dom.fold remove_reachable1 st1 st1) st2
    
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
            if Queries.LS.mem e rs then remove e st else st
          in
          Dom.S.fold remove_reachable2 es st
        in
        [Dom.fold remove_reachable1 st st, true_exp, true]
    
  (* query stuff *)
  
  (* helper to decide equality *)
  let exp_equal ask e1 e2 (g:Glob.Var.t -> Glob.Val.t) s =
    match e1, e2 with
      | Lval  llval, Lval rlval -> begin
        let v1 = ask (Queries.MayPointTo (Cil.mkAddrOf llval)) in
        let v2 = ask (Queries.MayPointTo (Cil.mkAddrOf rlval)) in
        match v1, v2 with
          | `LvalSet v1, `LvalSet v2 when 
                 Queries.LS.cardinal v1 == 1 
              && Queries.LS.cardinal v2 == 1 -> begin
            let v1 = Queries.LS.choose v1 in
            let v2 = Queries.LS.choose v2 in          
            match Dom.find_class v1 s with
              | Some ss when Dom.S.mem v2 ss -> true
              | _ -> false
            end 
           | _ -> false end
      | _ -> false
      
  let query a g s x = 
    match x with 
      | Queries.ExpEq (e1,e2) when exp_equal a e1 e2 g s -> `Int (Queries.ID.of_bool true)
      | _ -> `Top

end

module Analysis = Multithread.Forward(Spec)
