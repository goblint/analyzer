(** Path-sensitive analysis that verifies checking the result of the malloc function. *)

module M = Messages
module BS = Base.Main
module AD = ValueDomain.AD
module IdxDom = ValueDomain.IndexDomain
module Offs = ValueDomain.Offs

open Cil
open Pretty
open Analyses
open GobConfig

module Spec =
struct
  include Analyses.DefaultSpec

  module Addr = ValueDomain.Addr
  module Dom  = ValueDomain.AddrSetDomain
  module Glob = Glob.Make (Lattice.Unit)
  
  type glob_fun = Glob.Var.t -> Glob.Val.t
  
  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with the string "unknown" on all non-concrete cases. *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (IdxDom.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (IdxDom.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)

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
  
  (* We just had to dereference an lval --- warn if it was null *)
  let warn_lval (st:Dom.t) (v :varinfo * (Addr.field,Addr.idx) Lval.offs) : unit =
    try 
      if Dom.exists (fun x -> List.exists (fun x -> is_prefix_of x v) (Addr.to_var_offset x)) st
      then 
        let var = Addr.from_var_offset v in
        Messages.report ("Possible dereferencing of null on variable '" ^ (Addr.short 80 var) ^ "'.")
    with SetDomain.Unsupported _ -> ()
  
  (* Warn null-lval dereferences, but not normal (null-) lvals*)
  let rec warn_deref_exp a (st:Dom.t) (e:exp): unit =
    let warn_lval_mem e offs = 
(*      begin try List.iter (warn_lval st) (AD.to_var_offset (BS.eval_lv gl s (Mem e, offs)))
      with SetDomain.Unsupported _ -> () end;*)
      match e with
        | Lval (Var v, offs) ->            
            begin match a (Queries.MayPointTo (mkAddrOf (Var v,offs))) with
                    | `LvalSet a when not (Queries.LS.is_top a) 
                                   && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
                        Queries.LS.iter (fun (v,o) -> warn_lval st (v, conv_offset o)) a
                    | _ -> ()
            end
        | _ -> ()          
    in
    match e with
      | Lval (Var v, offs) -> ()
      | AddrOf (Var _, _) 
      | StartOf (Var _, _) ->  warn_lval_mem e NoOffset
      | AddrOf (Mem e, offs) 
      | StartOf (Mem e, offs) 
      | Lval (Mem e, offs) -> 
          warn_deref_exp a st e;
          warn_lval_mem e offs
      | BinOp (_,e1,e2,_) -> 
          warn_deref_exp a st e1;
          warn_deref_exp a st e2
      | UnOp (_,e,_) 
      | CastE  (_,e) ->
          warn_deref_exp a st e 
      | _ -> ()
  
  let may (f: 'a -> 'b) (x: 'a option) : unit =
    match x with
      | Some x -> f x; ()
      | None -> ()

  (* Generate addresses to all points in an given varinfo. (Depends on type) *)
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

  (* Remove null values from state that are unreachable from exp.*)
  let remove_unreachable ask (args: exp list) (st: Dom.t) : Dom.t =
  let reachable = 
    let do_exp e = 
      match ask (Queries.ReachableFrom e) with
        | `LvalSet a when not (Queries.LS.is_top a)  -> 
          let to_extra (v,o) xs = AD.from_var_offset (v,(conv_offset o)) :: xs  in
          Queries.LS.fold to_extra a [] 
        (* Ignore soundness warnings, as invalidation proper will raise them. *)
        | _ -> []
    in
      List.concat (List.map do_exp args)
  in
  let add_exploded_struct (one: AD.t) (many: AD.t) : AD.t =
    let vars = AD.to_var_may one in
    List.fold_right AD.add (List.concat (List.map to_addrs vars)) many
  in
  let vars = List.fold_right add_exploded_struct reachable (AD.empty ()) in
  if Dom.is_top st 
  then Dom.top ()
  else Dom.filter (fun x -> AD.mem x vars) st       

  let get_concrete_lval ask (lval:lval) =
    match ask (Queries.MayPointTo (mkAddrOf lval)) with
      | `LvalSet a when Queries.LS.cardinal a = 1 
                     && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
          let v, o = Queries.LS.choose a in
          Some (Var v, conv_offset o)
      | _ -> None 

  let get_concrete_exp (exp:exp) (gl:glob_fun) (st:Dom.t) =
    match Cil.constFold true exp with
       | CastE (_,Lval (Var v, offs))
       | Lval (Var v, offs) -> Some (Var v,offs)
       | _ -> None

  let might_be_null ask lv gl st =
    match ask (Queries.MayPointTo (mkAddrOf lv)) with
      | `LvalSet a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
          let one_addr_might (v,o) = 
            Dom.exists (fun x -> List.exists (fun x -> is_prefix_of (v, conv_offset o) x) (Addr.to_var_offset x)) st
          in
          Queries.LS.exists one_addr_might a 
      | _ -> false   
  
  (*
    Transfer functions and alike 
  *)
  
  (* One step tf-s *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    warn_deref_exp ctx.ask ctx.local (Lval lval) ;
    warn_deref_exp ctx.ask ctx.local rval;
    match get_concrete_exp rval ctx.global ctx.local, get_concrete_lval ctx.ask lval with
      | Some rv , Some (Var vt,ot) when might_be_null ctx.ask rv ctx.global ctx.local -> 
          Dom.add (Addr.from_var_offset (vt,ot)) ctx.local
      | _ -> ctx.local
      
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    warn_deref_exp ctx.ask ctx.local exp;
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    ctx.local
  
  let return_addr_ = ref (Addr.null_ptr ())
  let return_addr () = !return_addr_
  
  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    let remove_var x v = List.fold_right Dom.remove (to_addrs v) x in
    let nst = List.fold_left remove_var ctx.local (f.slocals @ f.sformals) in
    match exp with
      | Some ret ->
          warn_deref_exp ctx.ask ctx.local ret;
          begin match get_concrete_exp ret ctx.global ctx.local with
            | Some ev when might_be_null ctx.ask ev ctx.global ctx.local ->
                Dom.add (return_addr ()) nst
            | _ -> nst  end
      | None -> nst
  
  (* Function calls *)
  
  let eval_funvar ctx (fv:exp) : varinfo list = 
    warn_deref_exp ctx.ask ctx.local fv;
    []
    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let nst = remove_unreachable ctx.ask args ctx.local in
    may (fun x -> warn_deref_exp ctx.ask ctx.local (Lval x)) lval;
    List.iter (warn_deref_exp ctx.ask ctx.local) args;
    [ctx.local,nst]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    let cal_st = remove_unreachable ctx.ask args ctx.local in
    let ret_st = Dom.union au (Dom.diff ctx.local cal_st) in
    let new_u = 
      match lval, Dom.mem (return_addr ()) ret_st with
        | Some lv, true -> 
            begin match get_concrete_lval ctx.ask lv with
                    | Some (Var v,ofs) -> Dom.remove (return_addr ()) (Dom.add (Addr.from_var_offset (v,ofs)) ret_st)
                    | _ -> ret_st end
        | _ -> ret_st
    in
    new_u
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    may (fun x -> warn_deref_exp ctx.ask ctx.local (Lval x)) lval;
    List.iter (warn_deref_exp ctx.ask ctx.local) arglist;
    match f.vname, lval with
      | "malloc", Some lv ->
        begin
          match get_concrete_lval ctx.ask lv with
            | Some (Var v, offs) ->
                [ctx.local, Lval lv, true
                ;Dom.add (Addr.from_var_offset (v,offs)) ctx.local, Lval lv, false]
           | _ -> [ctx.local, Cil.integer 1, true]
        end
      | _ -> [ctx.local, Cil.integer 1, true]

  let name = "Malloc null"

  let startstate v = Dom.empty ()
  let otherstate v = Dom.empty ()
  let exitstate  v = Dom.empty ()
  
  let init () = 
    set_bool "exp.malloc-fail" true;
    return_addr_ :=  Addr.from_var (makeVarinfo false "RETURN" voidType)
end

module VarEqMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "malloc_null" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Malloc_null x
                let extract_l x = match x with `Malloc_null x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "malloc_null" (module Spec2 : Spec2)
