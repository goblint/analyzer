open Cil
open Pretty
open Analyses
open MyCFG
open Batteries_uni 



module type NeedBasedSpec = 
sig
  module B : Spec2
  module N : SetDomain.S with type elt = varinfo
  
  val restrict : B.C.t -> N.t -> B.C.t
  
  val special : N.t -> lval option -> varinfo -> exp list -> N.t
  val enter   : N.t -> lval option -> varinfo -> exp list -> (N.t * N.t) list 
  val combine : N.t -> lval option -> exp -> varinfo -> exp list -> N.t -> N.t
end

module NeedVarsContext (S:NeedBasedSpec) 
  : Printable.S with type t = [ `F of S.B.C.t * S.N.t
                              | `B of S.B.C.t * S.N.t
                              | `C of node * fundec * S.B.C.t * S.N.t ]
  =
struct
  open S
  
  type t = [ `F of B.C.t * N.t
           | `B of B.C.t * N.t
           | `C of node * fundec * B.C.t * N.t ]
           
  let name () = "need context"
  let isSimple _ = true
           
  let equal (x:t) (y:t) =
    match x, y with
      | `F (cf1,cb1), `F (cf2,cb2) -> B.C.equal cf1 cf2 && N.equal cb1 cb2
      | `B (cf1,cb1), `B (cf2,cb2) -> B.C.equal cf1 cf2 && N.equal cb1 cb2
      | `C (n21,f1,cf1,cb1), `C (n22,f2,cf2,cb2) -> Node.equal n21 n22
                                                    && f1.svar.vid=f2.svar.vid
                                                    && B.C.equal cf1 cf2
                                                    && N.equal cb1 cb2
      | _, _ -> false
      
  let hash = function
    | `F (cf1,cb1) -> Hashtbl.hash (1, B.C.hash cf1, N.hash cb1)
    | `B (cf1,cb1) -> Hashtbl.hash (3, B.C.hash cf1, N.hash cb1)
    | `C (n21,f1,cf1,cb1) -> Hashtbl.hash (5, Node.hash n21, B.C.hash cf1, N.hash cb1)
   
  let compare x y = 
    match x, y with
      | `F _, `B _ -> 1
      | `F _, `C _ -> 1
      | `B _, `C _ -> 1
      | `F (cf1,cb1), `F (cf2,cb2) -> 
          let cf = B.C.compare cf1 cf2 in
          if cf<>0 then cf else
          N.compare cb1 cb2           
      | `B (cf1,cb1), `B (cf2,cb2) -> 
          let cf = B.C.compare cf1 cf2 in
          if cf<>0 then cf else
          N.compare cb1 cb2           
      | `C (n21,f1,cf1,cb1), `C (n22,f2,cf2,cb2) -> 
          let n2 = node_compare n21 n22 in
          if n2<>0 then n2 else 
          let f = Basetype.CilFundec.compare f1 f2 in
          if f<>0 then f else
          let cf = B.C.compare cf1 cf2 in
          if cf<>0 then cf else
          N.compare cb1 cb2      
      | _ -> -1     
       
  let short w = function 
    | `F (cf1,cb1)        -> sprint w (dprintf "F<%a,%a>" B.C.pretty cf1 N.pretty cb1)
    | `B (cf1,cb1)        -> sprint w (dprintf "B<%a,%a>" B.C.pretty cf1 N.pretty cb1)
    | `C (n21,f1,cf1,cb1) -> sprint w (dprintf "C<%a,%s,%a,%a>" pretty_node n21 f1.svar.vname B.C.pretty cf1 N.pretty cb1)
  
  let pretty_f s () = function 
    | `F (cf1,cb1)         -> dprintf "F<%a,%a>" B.C.pretty cf1 N.pretty cb1
    | `B (cf1,cb1)         -> dprintf "B<%a,%a>" B.C.pretty cf1 N.pretty cb1
    | `C (n21,f1,cf1,cb1) -> dprintf "C<%a,%s,%a,%a>" pretty_node n21 f1.svar.vname B.C.pretty cf1 N.pretty cb1
  let pretty = pretty_f short
  
  let toXML_f sf =
    let combine s xs = Xml.Element ("Node", [("text", s )], xs)
    in function 
      | `F (cf1,cb1)        -> combine "F" [B.C.toXML cf1; N.toXML cb1]
      | `B (cf1,cb1)        -> combine "B" [B.C.toXML cf1; N.toXML cb1]
      | `C (n21,f1,cf1,cb1) -> combine ("C-"^f1.svar.vname) [B.C.toXML cf1; N.toXML cb1]
  let toXML = toXML_f short

  let pretty_diff () _ = dprintf "not implemented"
  
end

module FromSpec 
  (S:NeedBasedSpec) 
  (Cfg:CfgBidir) 
  =
struct
  include S
  
  module C = NeedVarsContext(S)
  module LVar = VarF (C) 
  module GVar = Basetype.Variables
  module D = Lattice.Lift2 (B.D) (N) (Printable.DefaultNames)
  module G = B.G

  type lv = LVar.t  
  type ld = D.t
  
  type gv = varinfo
  type gd = B.G.t
    
  let context = function 
    | `Lifted1 f -> `Lifted1 (B.context f)
    | `Lifted2 b -> `Lifted2 b
    | `Top -> `Top
    | `Bot -> `Bot
    
  let call_descr f = function 
    | `Lifted1 d -> B.call_descr f d
    | _ -> f.svar.vname

  let common_ctx (v,c) u getl sidel getg sideg = 
    let pval = getl (u,c) in     
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now watch this ... *)
    let rec ctx = 
      { ask2     = query
      ; local2   = pval
      ; global2  = getg
      ; presub2  = []
      ; postsub2 = []
      ; spawn2   = (fun f d -> let c = B.context d in 
                               sidel (FunctionEntry f, c) d; 
                               ignore (getl (Function f, c)))
      ; split2   = (fun d _ _ -> sidel (v,c) d)
      ; sideg2   = sideg
      } 
    and query x = B.query ctx x in
    (* ... nice, right! *)
    let pval, diff = B.sync ctx in
    let _ = List.iter (uncurry sideg) diff in
    { ctx with local2 = pval }
    

  let tf_loop (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in B.intrpt ctx
  
  let tf_assign lv e (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in B.assign ctx lv e
    
  let tf_ret ret fd (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in B.return ctx ret fd
    
  let tf_entry fd (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in B.body ctx fd

  let tf_test e tv (v,c) u getl sidel getg sideg =
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in B.branch ctx e tv
    
    

  let tf_normal_call ctx lv e f args  getl sidel getg sideg =
    let combine (cd, fd) = B.combine {ctx with local2 = cd} lv e f args fd in
    let paths = B.enter ctx lv f args in
    let _     = List.iter (fun (c,v) -> sidel (FunctionEntry f, B.context v) v) paths in
    let paths = List.map (fun (c,v) -> (c, getl (Function f, B.context v))) paths in
    let paths = List.filter (fun (c,v) -> B.D.is_bot v = false) paths in
    let paths = List.map combine paths in
      List.fold_left B.D.join (B.D.bot ()) paths
      
  let tf_special_call ctx lv f args = B.special ctx lv f args 

  let tf_proc lv e args (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg in 
    let functions = 
      match ctx.ask2 (Queries.EvalFunvar e) with 
        | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
        | `Bot -> []
        | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () e)))
    in
    let one_function f = 
      let has_dec = try ignore (Cilfacade.getdec f); true with Not_found -> false in        
      if has_dec && not (LibraryFunctions.use_special f.vname) 
      then tf_normal_call ctx lv e f args getl sidel getg sideg
      else tf_special_call ctx lv f args
    in
    let funs = List.map one_function functions in
    List.fold_left B.D.join (B.D.bot ()) funs

  
  (*let tf_f (v,cf,cb) (edge, u) = 
    begin match edge with
      | Assign (lv,rv) -> tf_assign lv rv
      | Proc (r,f,ars) -> tf_proc r f ars
      | Entry f        -> tf_entry f
      | Ret (r,fd)     -> tf_ret r fd
      | Test (p,b)     -> tf_test p b
      | ASM _          -> fun _ _ getl _ _ _ -> ignore (warn "ASM statement ignored."); getl (u,cf,cb)
      | Skip           -> fun _ _ getl _ _ _ -> getl (u,cf,cb)
      | SelfLoop       -> tf_loop 
    end (v,cf,cb) u*)
    
  let rec add_vars d = function
    | UnOp  (_,e,_)     -> add_vars d e
    | BinOp (_,e1,e2,_) -> add_vars (add_vars d e1) e2
    | AddrOf  (Cil.Mem e,o) 
    | StartOf (Cil.Mem e,o) 
    | Lval    (Cil.Mem e,o) -> add_vars (add_vars_ofs d o) e
    | CastE (_,e)           -> add_vars d e
    | Lval    (Cil.Var v2,o) 
    | AddrOf  (Cil.Var v2,o) 
    | StartOf (Cil.Var v2,o) -> add_vars_ofs (N.add v2 d) o
    | _ -> d
  and add_vars_ofs d = function
    | NoOffset -> d
    | Field (_,o) -> add_vars_ofs d o
    | Index (e,o) -> add_vars (add_vars_ofs d o) e
    
  let rec get_vars d = function
    | UnOp  (_,e,_)          -> get_vars d e
    | BinOp (_,e1,e2,_)      -> get_vars (get_vars d e1) e2
    | CastE (_,e)            -> get_vars d e
    | AddrOf  (Cil.Mem e,o) 
    | StartOf (Cil.Mem e,o) 
    | Lval    (Cil.Mem e,o)  -> get_vars (get_vars_ofs d o) e
    | Lval    (Cil.Var v2,o) 
    | AddrOf  (Cil.Var v2,o) 
    | StartOf (Cil.Var v2,o) -> get_vars_ofs (v2::d) o
    | _ -> d
  and get_vars_ofs d = function
    | NoOffset    -> d
    | Field (_,o) -> get_vars_ofs d o
    | Index (e,o) -> get_vars (get_vars_ofs d o) e
  
  

  let tf_b_assign lv e (v,cf,cb) u (getl:lv -> ld) sidel getg sideg = 
    let old = match getl (u, `B (cf,cb)) with `Lifted2 d -> d | `Bot -> N.empty () | _ -> failwith "1"  in
    let lv_vars = get_vars [] (Lval lv) in
    if List.exists (fun x -> N.mem x old) lv_vars then
      `Lifted2 (add_vars old e)
    else 
      `Lifted2 old    
  
  let tf_b_proc lv e args (v,cf,cb) u (getl:lv -> ld) sidel getg sideg =
    match e, args with
      | Lval(Var f, NoOffset), [Lval(Var x, NoOffset)] when f.vname = "important" -> 
          begin
            match getl (u, `B (cf,cb)) with
              | `Bot -> `Lifted2 (N.singleton x)
              | `Lifted2 d -> `Lifted2 (N.add x d)
              | d -> d
          end
      | _ -> getl (u, `B (cf,cb))

  let tf_b_entry  c = undefined c
  let tf_b_ret    c _ = undefined c

  let tf_b (v,cf,cb) (edge, u) : (lv -> ld) -> (lv -> ld -> unit) -> (gv -> gd) -> (gv -> gd -> unit) -> ld = 
    begin match edge with
      | Assign (lv,rv) -> tf_b_assign lv rv
      | Proc (r,f,ars) -> tf_b_proc r f ars
      (*| Entry f        -> tf_b_entry f
      | Ret (r,fd)     -> tf_b_ret r fd*)
      | _              -> fun _ _ getl _ _ _ -> getl (u, `B (cf,cb))
    end (v,cf,cb) u
    
  (*let tf_f (v,cf,cb) (e,u) getl sidel getg sideg =
    let old_loc = !Tracing.current_loc in
    let _       = Tracing.current_loc := getLoc u in
    let d       = try tf_f (v,cf,cb) (e,u) getl sidel getg sideg 
                  with M.StopTheWorld -> B.D.bot ()
                     | M.Bailure s -> Messages.warn_each s; (getl (u,cf,cb))  in
    let _       = Tracing.current_loc := old_loc in 
      d*)

  let tf_b (v,cf,cb) (e,u) getl sidel getg sideg : ld =
    let old_loc = !Tracing.current_loc in
    let _       = Tracing.current_loc := getLoc v in
    let d       = try tf_b (v,cf,cb) (e,u) getl sidel getg sideg 
                  with M.StopTheWorld -> `Lifted2 (N.bot ())
                     | M.Bailure s -> Messages.warn_each s; (getl (u, `B (cf,cb)))  in
    let _       = Tracing.current_loc := old_loc in 
      d
  
  let system : lv -> ((lv -> ld) -> (lv -> ld -> unit) -> (gv -> gd) -> (gv -> gd -> unit) -> ld) list = function
    | (n , `F (cf,cb))      -> [](*List.map (tf_f (n,cf,cb)) (Cfg.prev v)*)
    | (n , `B (cf,cb))      -> List.map (tf_b (n,cf,cb)) (Cfg.next n)
    | (n1, `C (n2,f,cf,cb)) -> []
end

let _ = 
  let module FromSpec' 
        (S:NeedBasedSpec) 
        (Cfg:CfgBidir) 
        : GlobConstrSys with module LVar = VarF (NeedVarsContext(S)) 
                         and module GVar = Basetype.Variables
                         and module D = Lattice.Lift2 (S.B.D) (S.N) (Printable.DefaultNames)
                         and module G = S.B.G
        = FromSpec (S) (Cfg)
  in ()

module VVVSpec =
struct
  module B = Constraints.Spec2OfSpec (Base.Spec)
  module N = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "everything needed" end)
  
  let restrict x _ = x
  let special d l f a = d
  let enter _ _ _ _ = []
  let combine _ _ _ _ _ x = x
end

module System = FromSpec (VVVSpec)