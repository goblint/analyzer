(** Local variable initialization analysis. *)

module M = Messages
module BS = Base.Main
module AD = ValueDomain.AD
module IdxDom = ValueDomain.IndexDomain
module Offs = ValueDomain.Offs

open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Addr = ValueDomain.Addr
  
  module D = ValueDomain.AddrSetDomain
  module C = ValueDomain.AddrSetDomain
  module G = Lattice.Unit
  
  type trans_in  = D.t 
  type trans_out = D.t 
  type transfer  = trans_in -> trans_out
  
  let name = "uninit"
  
  let startstate v : D.t = D.empty () 
  let otherstate v : D.t = D.empty ()
  let exitstate  v : D.t = D.empty ()

  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with the string "unknown" on all non-concrete cases. *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (IdxDom.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (IdxDom.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)

  let access_address ask write lv : BS.extra =
    match ask (Queries.MayPointTo (AddrOf lv)) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
          let to_extra (v,o) xs = (v, Base.Offs.from_offset (conv_offset o), write) :: xs  in
          Queries.LS.fold to_extra a [] 
      | _ -> 
          M.warn "Access to unknown address could be global"; []

  let rec access_one_byval a rw (exp:exp): BS.extra = 
    match exp with 
      (* Integer literals *)
      | Const _ -> []
      (* Variables and address expressions *)
      | Lval lval -> access_address a rw lval @ (access_lv_byval a lval)
      (* Binary operators *)
      | BinOp (op,arg1,arg2,typ) -> 
          let a1 = access_one_byval a rw arg1 in
          let a2 = access_one_byval a rw arg2 in
            a1 @ a2
      (* Unary operators *)
      | UnOp (op,arg1,typ) -> access_one_byval a rw arg1
      (* The address operators, we just check the accesses under them *)
      | AddrOf lval -> access_lv_byval a lval
      | StartOf lval -> access_lv_byval a lval
      (* Most casts are currently just ignored, that's probably not a good idea! *)
      | CastE  (t, exp) -> access_one_byval a rw exp
      | _ -> []    
  (* Accesses during the evaluation of an lval, not the lval itself! *)
  and access_lv_byval a (lval:lval): BS.extra = 
    let rec access_offset (ofs: offset): BS.extra = 
      match ofs with 
        | NoOffset -> []
        | Field (fld, ofs) -> access_offset ofs
        | Index (exp, ofs) -> access_one_byval a false exp @ access_offset ofs
    in 
      match lval with 
        | Var x, ofs -> access_offset ofs
        | Mem n, ofs -> access_one_byval a false n @ access_offset ofs

   let access_byval a (rw: bool) (exps: exp list): BS.extra =
     List.concat (List.map (access_one_byval a rw) exps)

   let access_byref ask (exps: exp list) = 
     (* Find the addresses reachable from some expression, and assume that these
      * can all be written to. *)
     let do_exp e = 
       match ask (Queries.ReachableFrom e) with
         | `LvalSet a when not (Queries.LS.is_top a) -> 
            let to_extra (v,o) xs = (v, Base.Offs.from_offset (conv_offset o), true) :: xs  in
            Queries.LS.fold to_extra a [] 
         (* Ignore soundness warnings, as invalidation proper will raise them. *)
         | _ -> []
     in
       List.concat (List.map do_exp exps)
       
  (* list accessed addresses *)
  let varoffs a (rval:exp) =
    let f vs (v,o,_) =
      match o with 
        | Offs.Offs o -> (v,o) :: vs
        | _ -> vs in 
    List.fold_left f [] (access_one_byval a false rval)  

  let vars a (rval:exp) : Addr.t list =
    List.map Addr.from_var_offset (varoffs a rval)

  let is_prefix_of (v1,ofs1: varinfo * (Addr.field,Addr.idx) Lval.offs) (v2,ofs2: varinfo * (Addr.field,Addr.idx) Lval.offs) : bool =
    let rec is_offs_prefix_of pr os = 
      match (pr, os) with
        | (`NoOffset, _) -> true
        | (`Field (f1, o1), `Field (f2,o2)) -> f1 == f2 && is_offs_prefix_of o1 o2
        | (_, _) -> false
    in
    (v1.vid == v2.vid) && is_offs_prefix_of ofs1 ofs2
    

  (* Does it contain non-initialized variables? *)
  let is_expr_initd a (expr:exp) (st:D.t) : bool = 
    let variables = vars a expr in
    let raw_vars = List.concat (List.map Addr.to_var_offset variables) in
    let will_addr_init (t:bool) a = 
      let f addr =
        List.exists (is_prefix_of a) (Addr.to_var_offset addr)
      in
      if D.exists f st then begin 
        Messages.report ("Uninitialized variable " ^ (Addr.short 80 (Addr.from_var_offset a)) ^ " accessed.");
        false
      end else
        t in  
    List.fold_left will_addr_init true raw_vars

  let remove_if_prefix (pr: varinfo * (Addr.field,Addr.idx) Lval.offs) (uis: D.t) : D.t =
    let f ad =
      let vals = Addr.to_var_offset ad in
      List.for_all (fun a -> not (is_prefix_of pr a)) vals
    in
    D.filter f uis

  type lval_offs = (Addr.field,Addr.idx) Lval.offs
  type var_offs  = varinfo * lval_offs    
  
  (* Call to [get_pfx v cx] returns initialized prefixes ... *)
  let rec get_pfx (v:varinfo) (cx:lval_offs) (ofs:lval_offs) (target:typ) (other:typ) : var_offs list =
    let rec cat o i =
      match o with
        | `NoOffset -> i
        | `Field (f, o) -> `Field (f, cat o i)
        | `Index (v, o) -> `Index (v, cat o i)
    in
    let rec rev lo = 
      match lo with
        | `NoOffset -> `NoOffset
        | `Field (f, o) -> cat (rev o) (`Field (f, `NoOffset))
        | `Index (v, o) -> cat (rev o) (`Index (v, `NoOffset))
    in
    let rec bothstruct (t:fieldinfo list) (tf:fieldinfo) (o:fieldinfo list) (no:lval_offs)  : var_offs list =
      match t, o with
        | x::xs, y::ys when x.fcomp.ckey == tf.fcomp.ckey && x.fname == tf.fname ->
          get_pfx v (`Field (y, cx)) no x.ftype y.ftype
        | x::xs, y::ys when x.ftype == y.ftype ->
          bothstruct xs tf ys no
        | x::xs, y::ys ->
          [] (* found a mismatch *)
        | _ -> 
          M.warn ("Failed to analyze union at point " ^ (Addr.short 80 (Addr.from_var_offset (v,rev cx))) ^ " -- did not find " ^ tf.fname);
          []
    in
    let utar, uoth = unrollType target, unrollType other in
    match ofs, utar, uoth with
      |     `NoOffset,              _ ,               _ when utar == uoth  -> [v, rev cx]
      |     `NoOffset,              _ ,    TComp (c2,_) when not c2.cstruct -> 
          (* unroll other (union) *)
          List.concat (List.rev_map (fun oth_f -> get_pfx v (`Field (oth_f, cx)) ofs utar oth_f.ftype) c2.cfields)
      |     `NoOffset,              _ ,               _ -> 
          (* types not same and other is not a struct *)
          [] 
      | `Index (i, o), TArray (t1,_,_), TArray (t2,_,_) -> (* todo: other might be a union*)
          (* step into both indexed *)
          get_pfx v (`Index (i, cx)) o t1 t2    
      | `Index (i, o),    _ ,    TComp (c2,_) when not c2.cstruct ->
          (* step into all other fields *)
          List.concat (List.rev_map (fun oth_f -> get_pfx v (`Field (oth_f, cx)) ofs utar oth_f.ftype) c2.cfields)          
      | `Field (f, o),    TComp (c1,_),    TComp (c2,_) when c1.cstruct && c2.cstruct ->
          (* step into both, but check that types of prefices match*)
          bothstruct c1.cfields f c2.cfields o
      | `Field (f, o),    TComp (c1,_),              _  when not c1.cstruct  ->
          (* step into target but not other (don't care about other) *)
          get_pfx v cx o f.ftype uoth  
      | `Field (f, o),    TComp (c1,_),    TComp (c2,_) when c1.cstruct && not c2.cstruct ->
          (* step into all other fields *)
          List.concat (List.rev_map (fun oth_f -> get_pfx v (`Field (oth_f, cx)) ofs utar oth_f.ftype) c2.cfields)          
      | _ ->
        M.warn ("Failed to analyze union at point " ^ (Addr.short 80 (Addr.from_var_offset (v,rev cx))));
        []
        
    
  (* Call to [init_lval lv st] results in state [st] where the variable evaluated form [lv] is initialized. *)
  let init_lval a (lv: lval) (st: D.t) : D.t =
    let init_vo (v: varinfo) (ofs: lval_offs) : D.t = 
      List.fold_right remove_if_prefix (get_pfx v `NoOffset ofs v.vtype v.vtype) st
    in
    match a (Queries.MayPointTo (AddrOf lv)) with
      | `LvalSet a when Queries.LS.cardinal a = 1 ->  begin
            let var, ofs = Queries.LS.choose a in        
            init_vo var (conv_offset ofs) 
          end
      | _ -> st 
   
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
      
  
  let remove_unreachable ask (args: exp list) (st: D.t) : D.t =
    let reachable = 
      let do_exp e = 
        match ask (Queries.ReachableFrom e) with
          | `LvalSet a when not (Queries.LS.is_top a) -> 
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
    if D.is_top st 
    then D.top ()
    else D.filter (fun x -> AD.mem x vars) st     

  (*
    Transfer functions
  *)
  let assign ctx (lval:lval) (rval:exp) : trans_out =
    ignore (is_expr_initd ctx.ask rval ctx.local);
    init_lval ctx.ask lval ctx.local
        
  let branch ctx (exp:exp) (tv:bool) : trans_out = 
    ignore (is_expr_initd ctx.ask exp ctx.local);
    ctx.local
  
  let body ctx (f:fundec) : trans_out = 
    let add_var st v = List.fold_right D.add (to_addrs v) st in
    List.fold_left add_var ctx.local f.slocals
  
  let return ctx (exp:exp option) (f:fundec) : trans_out = 
    let remove_var x v = 
      List.fold_right D.remove (to_addrs v) x in
    let nst = List.fold_left remove_var ctx.local (f.slocals @ f.sformals) in
    match exp with 
      | Some exp -> ignore (is_expr_initd ctx.ask exp ctx.local); nst
      | _ -> nst
  
  
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let nst = remove_unreachable ctx.ask args ctx.local in
    [ctx.local, nst]
  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : trans_out =
    ignore (List.map (fun x -> is_expr_initd ctx.ask x ctx.local) args);
    let cal_st = remove_unreachable ctx.ask args ctx.local in
    let ret_st = D.union au (D.diff ctx.local cal_st) in
    match lval with
      | None -> ret_st
      | Some lv -> init_lval ctx.ask lv ret_st

  
  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match lval with
      | Some lv -> init_lval ctx.ask lv ctx.local
      | _ -> ctx.local
      
(*  let fork ctx (lval: lval option) (f : varinfo) (args : exp list) : (varinfo * D.t) list =
    [] (* thats wrong: should be [None, top ()] *)*)

end

let _ = 
  MCP.register_analysis (module Spec : Spec)
