module M = Messages
module BS = Base.Main
module AD = ValueDomain.AD
module Offs = ValueDomain.Offs

open Cil
open Pretty

module Spec : Analyses.Spec =
struct
  module Addr = ValueDomain.Addr
  module AddrSet = SetDomain.ToppedSet (Addr) (struct let topname = "All" end)
  
  module Dom  = Lattice.Prod (AddrSet) (BS.Dom)   
  module Glob = BS.Glob
  
  type glob_fun = Glob.Var.t -> Glob.Val.t
  
  let get_diff (_,x) = BS.get_diff x
  let reset_diff (y,x) = (y,BS.reset_diff x)
  
  
  (*
    Addr set functions: 
  *)
  
  let is_prefix_of (v1,ofs1: varinfo * (Addr.field,Addr.idx) Lval.offs) (v2,ofs2: varinfo * (Addr.field,Addr.idx) Lval.offs) : bool =
    let rec is_offs_prefix_of pr os = 
      match (pr, os) with
        | (`NoOffset, `NoOffset) -> true
        | (`NoOffset, _) -> false
        | (`Field (f1, o1), `Field (f2,o2)) -> f1 == f2 && is_offs_prefix_of o1 o2
        | (_, _) -> false
    in
    (v1.vid == v2.vid) && is_offs_prefix_of ofs1 ofs2
  
  let warn_lval (st,gs:Dom.t) (v :varinfo * (Addr.field,Addr.idx) Lval.offs) : unit =
    try 
      if AddrSet.exists (fun x -> List.exists (fun x -> is_prefix_of x v) (Addr.to_var_offset x)) st
      then 
        let var = Addr.from_var_offset v in
        Messages.report ("Possible dereferencing of null on variable '" ^ (Addr.short 80 var) ^ "'.")
    with SetDomain.Unsupported _ -> ()
  
  let rec warn_deref_exp (gl:glob_fun) (_,s as st:Dom.t) (e:exp): unit =
    match e with
      | Lval (Var v, offs) -> ()
      | Lval (Mem e, offs) -> 
          warn_deref_exp gl st e;
(*          begin try List.iter (warn_lval st) (AD.to_var_offset (BS.eval_lv gl s (Mem e, offs)))
          with SetDomain.Unsupported _ -> () end;
*)          begin match e with
            | Lval (Var v, offs) -> warn_lval st (v, BS.convert_offset gl s offs)
            | Lval (Mem e, offs) -> 
                        begin try List.iter (warn_lval st) (AD.to_var_offset (BS.eval_lv gl s (Mem e, offs)))
                        with SetDomain.Unsupported _ -> () end;
            | _ -> () end
      | BinOp (_,e1,e2,_) -> 
          warn_deref_exp gl st e1;
          warn_deref_exp gl st e2
      | UnOp (_,e,_) 
      | SizeOfE e 
      | AlignOfE e
      | AddrOf (Mem e, _) 
      | StartOf (Mem e, _) 
      | CastE  (_,e) ->
          warn_deref_exp gl st e 
      | _ -> ()
  
  let may (f: 'a -> 'b) (x: 'a option) : unit =
    match x with
      | Some x -> f x; ()
      | None -> ()

  let to_addrs (v:varinfo) : Addr.t list =
    let make_offs = List.fold_left (fun o f -> `Field (f, o)) `NoOffset in
    let rec add_fields (base: Addr.field list) fs acc = 
      match fs with 
        | [] -> acc
        | f :: fs ->
            match unrollType f.ftype with
              | TComp ({cfields=ffs},_) -> add_fields base fs (List.rev_append (add_fields (f::base) ffs []) acc)  
              | _                       -> add_fields base fs ((Addr.from_var_offset (v,make_offs (f::base))) :: acc) 
    in
    match unrollType v.vtype with
      | TComp ({cfields=fs},_) -> add_fields [] fs [] 
      | _ -> [Addr.from_var v]

    let remove_unreachable (args: exp list) (gl:glob_fun) (gs: BS.Dom.t) (st: AddrSet.t) : AddrSet.t =
      if AddrSet.is_top st then st else
      let vals      = List.map (BS.eval_rv gl gs) args in
      let reachable = BS.reachable_vars (BS.get_ptrs vals) (gl:glob_fun) gs in
      let add_exploded_struct (one: AD.t) (many: AD.t) : AD.t =
        let vars = AD.to_var_may one in
        List.fold_right AD.add (List.concat (List.map to_addrs vars)) many
      in
      let vars = List.fold_right add_exploded_struct reachable (AD.empty ()) in
      AddrSet.filter (fun x -> AD.mem x vars) st     

  let get_concrete_lval (lval:lval) (gl:glob_fun) (st,gs:Dom.t)  =
    let lval_v = BS.eval_lv gl gs lval in
      if AD.is_top lval_v then None else
      match AD.to_var_offset lval_v with
        | [(vt,ot)] -> Some (vt,ot)
        | _ -> None 
    
  let get_concrete_exp (exp:exp) (gl:glob_fun) (st,gs:Dom.t) =
    match Cil.constFold true exp with
       | CastE (_,Lval (Var v, offs))
       | Lval (Var v, offs) -> Some (v,offs)
       | _ -> None

  let might_be_null (v,offs) gl (st,gs) =
    AddrSet.exists (fun x -> List.exists (fun x -> is_prefix_of (v, BS.convert_offset gl gs offs) x) (Addr.to_var_offset x)) st 
  
  (*
    Transfer functions and alike 
  *)
  
  (* One step tf-s *)
  
  let assign (lval:lval) (rval:exp) (gl:glob_fun) (st,gs:Dom.t) : Dom.t =
    warn_deref_exp gl (st,gs) (Lval lval) ;
    warn_deref_exp gl (st,gs) rval;
    match get_concrete_exp rval gl (st,gs), get_concrete_lval lval gl (st,gs) with
      | Some rv , Some (vt,ot) when might_be_null rv gl (st,gs) -> 
          AddrSet.add (Addr.from_var_offset (vt,ot)) st, BS.assign lval rval gl gs
      | _ -> st, BS.assign lval rval gl gs
      
  let branch (exp:exp) (tv:bool) (gl:glob_fun) (st,gs:Dom.t) : Dom.t = 
    warn_deref_exp gl (st,gs) exp;
    (st,BS.branch exp tv gl gs)
  
  let body (f:fundec) (gl:glob_fun) (st,gs:Dom.t) : Dom.t = 
    (st, BS.body f gl gs)
  
  let return_addr_ = ref (Addr.null_ptr ())
  let return_addr () = !return_addr_
  
  let return (exp:exp option) (f:fundec) (gl:glob_fun) (st,gs:Dom.t) : Dom.t = 
    let remove_var x v = List.fold_right AddrSet.remove (to_addrs v) x in
    let nst = List.fold_left remove_var st (f.slocals @ f.sformals) in
    match exp with
      | Some ret ->
          warn_deref_exp gl (st,gs) ret;
          begin match get_concrete_exp ret gl (st,gs) with
            | Some ev when might_be_null ev gl (st,gs) ->
                AddrSet.add (return_addr ()) nst, BS.return exp f gl gs
            | _ -> (nst,BS.return exp f gl gs)  end
      | None -> (nst,BS.return exp f gl gs)  
  
  (* Function calls *)
  
  let eval_funvar (fv:exp) (gl:glob_fun) (st,gs:Dom.t) : varinfo list = 
    warn_deref_exp gl (st,gs) fv;
    BS.eval_funvar fv gl gs
  
  let enter_func (lval: lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (st,gs:Dom.t) : (Dom.t * Dom.t) list =
    let nst = remove_unreachable args gl gs st in
    may (fun x -> warn_deref_exp gl (st,gs) (Lval x)) lval;
    List.iter (warn_deref_exp gl (st,gs)) args;
    List.map (fun (b,x) -> (st,b), (nst, x)) (BS.enter_func lval f args gl gs)          
  
  let leave_func (lval:lval option) (f:varinfo) (args:exp list) (gl:glob_fun) (bu,bst:Dom.t) (au,ast:Dom.t) : Dom.t =
    let cal_st = remove_unreachable args gl bst bu in
    let ret_st = AddrSet.union au (AddrSet.diff bu cal_st) in
    let new_u = 
      match lval, AddrSet.mem (return_addr ()) ret_st with
        | Some lv, true -> 
            begin match get_concrete_lval lv gl (bu,bst) with
                    | Some (v,ofs) -> AddrSet.remove (return_addr ()) (AddrSet.add (Addr.from_var_offset (v,ofs)) ret_st)
                    | _ -> ret_st end
        | _ -> ret_st
    in
    new_u, BS.leave_func lval f args gl bst ast
  
  let special_fn (lval: lval option) (f:varinfo) (arglist:exp list) (gl:glob_fun) (st,gs:Dom.t) : Dom.t list =
    may (fun x -> warn_deref_exp gl (st,gs) (Lval x)) lval;
    List.iter (warn_deref_exp gl (st,gs)) arglist;
    let map_gs x = List.map (fun y -> x, y) (BS.special_fn lval f arglist gl gs) in
    let null_it add x = BS.set gl x add (`Address (AD.null_ptr ()))  in
    match f.vname, lval with
      | "malloc", Some lv ->
        Messages.report "malloc";
        begin
          let addr = BS.eval_lv gl gs lv in
          match AD.to_var_offset addr with
           | [vo] -> map_gs st @ List.map (fun (x,y) -> x, null_it addr y) (map_gs (AddrSet.add (Addr.from_var_offset vo) st))
           | _ -> map_gs st
        end
      | _ -> map_gs st 
  
  let fork (lval: lval option) (f : varinfo) (args : exp list) (gl:glob_fun) (univ,cpa: Dom.t) : (varinfo * Dom.t) list =
    let dress (v,d) = v, (AddrSet.top (), d) in 
    List.map dress (BS.fork lval f args gl cpa)
  
  let name = "Malloc null"

  let finalize = BS.finalize
  let should_join (x,_) (y,_) = AddrSet.equal x y
  let startstate = AddrSet.empty () , BS.startstate
  let otherstate = AddrSet.empty () , BS.otherstate
  let es_to_string fd (_,d) = BS.es_to_string fd d
  let init () = 
    BS.init ();
    return_addr_ :=  Addr.from_var (makeVarinfo false "RETURN" voidType)
  
end


module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)
