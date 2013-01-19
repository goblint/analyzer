open Cil
open MyCFG
open Pretty
open Generic
open Analyses
open GobConfig
open Batteries_uni


module FromSpec (S:Spec2) (Cfg:CfgBackward)
  : GlobConstrSys with type lv = node * S.D.t
                   and type gv = varinfo
                   and type ld = S.D.t
                   and type gd = S.G.t
                   and type c  = S.D.t
                   and module C = S.D
                   and module LVar = VarF (S.D)
                   and module GVar = Basetype.Variables
                   and module D = S.D
                   and module G = S.G
  =
struct
  type lv = MyCFG.node * S.D.t
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  type c  = S.D.t
  module C = S.D
  module LVar = VarF (S.D)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G
  
  let context = fun x -> x

  let common_ctx (v,c) u (getl:lv -> ld) sidel getg sideg : (D.t, G.t) ctx2 = 
    let pval = getl (u,c) in 
    (* now wach this ... *)
    let rec ctx = 
      { ask2     = query
      ; local2   = pval
      ; global2  = getg
      ; presub2  = []
      ; postsub2 = []
      ; spawn2   = (fun f d -> sidel (Function f, d) (D.bot ()))
      ; split2   = (fun (d:D.t) _ _ -> sidel (v,c) d)
      ; sideg2   = sideg
      } 
    and query x = S.query ctx x in
    (* ... nice, right! *)
    ctx

  let tf_loop (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in S.intrpt ctx
  
  let tf_assign lv e (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in S.assign ctx lv e
    
  let tf_ret ret fd (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in S.return ctx ret fd
    
  let tf_entry fd (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in S.body ctx fd

  let tf_test e tv (v,c) u getl sidel getg sideg =
    let ctx = common_ctx (v,c) u getl sidel getg sideg 
    in S.branch ctx e tv

  let tf_proc lv e args (v,c) u getl sidel getg sideg = 
    let ctx = common_ctx (v,c) u getl sidel getg sideg in 
    let functions = 
      match ctx.ask2 (Queries.EvalFunvar e) with 
        | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
        | _ -> Messages.bailwith ("ProcCall: Failed to evaluate function expression "^(sprint 80 (d_exp () e)))
    in
    let one_function f = 
      let combine (cd, fd) = S.combine {ctx with local2 = cd} lv e f args fd in
      let paths = S.enter ctx lv f args in
      let paths = List.map (fun (c,v) -> (c, getl (Function f,v))) paths in
      let paths = List.filter (fun (c,v) -> D.is_bot v = false) paths in
      let paths = List.map combine paths in
      List.fold_left D.join (D.bot ()) paths
    in
    let funs = List.map one_function functions in
    List.fold_left D.join (D.bot ()) funs

  
  let tf (v,c) (edge, u) = 
    begin match edge with
      | Assign (lv,rv) -> tf_assign lv rv
      | Proc (r,f,ars) -> tf_proc r f ars
      | Entry f        -> tf_entry f
      | Ret (r,fd)     -> tf_ret r fd
      | Test (p,b)     -> tf_test p b
      | ASM _          -> fun _ _ getl _ _ _ -> ignore (warn "ASM statement ignored."); getl (u,c)
      | Skip           -> fun _ _ getl _ _ _ -> getl (u,c)
      | SelfLoop       -> tf_loop 
    end (v,c) u
    
  let tf (v,c) (e,u) getl sidel getg sideg =
    let old_loc = !Tracing.current_loc in
    let _       = Tracing.current_loc := getLoc u in
    let d       = tf (v,c) (e,u) getl sidel getg sideg in
    let _       = Tracing.current_loc := old_loc in 
      d
  
  let system (v,c) = List.map (tf (v,c)) (Cfg.prev v)
end

(** The selected specification. *)
module Spec = MCP.Path
  
(** Our local domain and variables. *)
module LD   = Analyses.Dom (Spec.Dom)
module HCLD = Lattice.HConsed (LD)
module LV   = Analyses.VarF (HCLD)

(** Our global domain and variables. *)
module GD   = Spec.Glob.Val
module GV   = Spec.Glob.Var

(** Combined variables for the solver. *)
module Var 
  : Analyses.VarType 
    with type t = [ `L of LV.t  | `G of GV.t ]
  = 
struct
  type t = [ `L of LV.t  | `G of GV.t ]
  
  let equal x y =
    match x, y with
      | `L a, `L b -> LV.equal a b
      | `G a, `G b -> GV.equal a b
      | _ -> false
  
  let hash = function
    | `L a -> LV.hash a
    | `G a -> 113 * GV.hash a
    
  let compare x y =
    match x, y with
      | `L a, `L b -> LV.compare a b
      | `G a, `G b -> GV.compare a b
      | `L a, _ -> -1 | _ -> 1
      
  let category = function
    | `L a -> LV.category a
    | `G _ -> -1
    
  let pretty_trace () = function
    | `L a -> LV.pretty_trace () a
    | `G a -> dprintf "Global %a" GV.pretty_trace a
      
  let line_nr = function
    | `L a -> LV.line_nr a
    | `G a -> a.Cil.vdecl.Cil.line
    
  let file_name = function
    | `L a -> LV.file_name a
    | `G a -> a.Cil.vdecl.Cil.file
    
  let description n = sprint 80 (pretty_trace () n)
  let context () _ = Pretty.nil
  let loopSep _ = true
end

(** Combine lattices. *)
module Dom = Lattice.Either (LD) (GD) 

let cfg = ref (fun _ -> [])

(** Our main constraint system. *)
module System 
  : IneqConstrSys 
  with type v = Var.t
   and type d = Dom.t 
   and module Var = Var
   and module Dom = Dom
  =
struct  
  open Messages
  open MyCFG
  
  type v = Var.t
  type d = Dom.t
  
  module Var = Var
  module Dom = Dom
  
  let box _ x y =
    match x, y with
      | `Left a , `Left b -> `Left (LD.join a b)
      | `Right a, `Right b -> `Right (GD.join a b)
      | `Right a , `Left b -> y
      | _ -> Dom.top ()
      

  let getctx v y get set = 
    let top_query _ = Queries.Result.top () in
    let theta g = 
      match get (`G g) with
        | `Right x -> x
        | _ -> failwith "Global domain out of range!"
    in
    let add_diff g d = set (`G g) (`Right d) in 
      Analyses.context top_query v theta [] y add_diff (fun _ -> ())
      
  
  let tf_ret    ret fd    pval get set = pval
  let tf_entry  fd        pval get set = pval (*SD.lift (Spec.body (getctx pre add_var)) pval*)
  let tf_assign lv e      pval get set = pval
  let tf_loop             pval get set = pval
  let tf_test   e tv      pval get set = pval
  let tf_proc   lv e args pval get set = pval
  let tf_asm              pval get set = warn "ASM statement ignored."; pval
  
  let edge_tf es v (e,u) get set = 
    let pval = get (`L (u,es)) in
    try
      match e with
        | Ret    (ret,fd)    -> tf_ret    ret fd    pval get set 
        | Entry  fd          -> tf_entry  fd        pval get set 
        | Assign (lv,e)      -> tf_assign lv e      pval get set 
        | SelfLoop           -> tf_loop             pval get set 
        | Test   (e,tv)      -> tf_test   e tv      pval get set 
        | Proc   (lv,e,args) -> tf_proc   lv e args pval get set     
        | ASM _              -> tf_asm              pval get set 
        | Skip               -> pval 
    with
      | Messages.StopTheWorld
      | Analyses.Deadcode  -> Dom.bot ()
      | Messages.Bailure s -> Messages.warn_each s; pval 
      | x -> Messages.warn_urgent "Oh noes! Something terrible just happened"; raise x
        
  let one_edge es v (e,u) get set =
    let old_loc = !Tracing.current_loc in
    let _       = Tracing.current_loc := getLoc u in
    let d       = edge_tf es v (e,u) get set in
    let _       = Tracing.current_loc := old_loc in 
      d
    
  let system = function 
    | `G _ -> []
    | `L (v,es) -> List.map (one_edge es v) (!cfg v)
    
end

(* this should end up in another file *)

module EQSys = Generic.NormalSysConverter (System) 
module HT    = BatHashtbl.Make (EQSys.Var)
module Slvr  = Generic.DirtyBoxSolver (EQSys) (HT) 


module RT = Analyses.ResultType (Spec) (Spec.Dom) (LD)
module LT = SetDomain.HeadlessSet (RT)
module RC = struct let result_name = "Analysis" end
module Result = Analyses.Result (LT) (RC)
    

(** convert result that can be out-put *)
let solver2source_result h : Result.t =
  (* processed result *)
  let res = Result.create 113 in
    
  (* Adding the state at each system variable to the final result *)
  let add_local_var (n,es) state =
    let loc = MyCFG.getLoc n in
    if loc <> Cil.locUnknown then try 
      let (_,_, fundec) as p = loc, n, MyCFG.getFun n in
      if Result.mem res p then 
        (* If this source location has been added before, we look it up
         * and add another node to it information to it. *)
        let prev = Result.find res p in
        Result.replace res p (LT.add (LD.unlift (HCLD.unlift es),state,fundec) prev)
      else 
        Result.add res p (LT.singleton (LD.unlift (HCLD.unlift es),state,fundec))
      (* If the function is not defined, and yet has been included to the
       * analysis result, we generate a warning. *)
    with Not_found -> Messages.warn ("Undefined function has escaped.")
  in
  
  let collect_locals (k,n) v =
    match k, v with
      | `L k, `Left d -> add_local_var k d
      | _ -> () 
  in
    HT.iter collect_locals h;
    res
    
(** exctract global xml from result *)
let make_global_xml g =
  let one_glob k v = 
    let k = Xml.PCData k.Cil.vname in
    let varname = Xml.Element ("td",[],[k]) in
    let varvalue = Xml.Element ("td",[],[Spec.Glob.Val.toXML v]) in
    Xml.Element ("tr",[],[varname; varvalue])
  in
  let head = 
    Xml.Element ("tr",[],[Xml.Element ("th",[],[Xml.PCData "var"])
                         ;Xml.Element ("th",[],[Xml.PCData "value"])])
  in 
  let collect_globals (k,_:System.Var.t*int) v b =
    match k, v with
      | `G k, `Right v -> one_glob k v :: b
      | _ -> b
  in
    Xml.Element ("table", [], head :: HT.fold collect_globals g [])


let do_global_inits file = LD.lift (Spec.startstate ()), []

let analyze (file: Cil.file) (startfuns, exitfuns, otherfuns: Analyses.fundecs) = 
  Spec.init ();
    
  if (get_bool "dbg.verbose") then print_endline "Generating constraints."; 
  cfg := MyCFG.getCFG file true;
  
	let startstate, more_funs = 
    if (get_bool "dbg.verbose") then print_endline "Initializing globals.";
    Stats.time "initializers" do_global_inits file 
  in
  
  let otherfuns = if get_bool "kernel" then otherfuns @ more_funs else otherfuns in
  
  let enter_with st fd =
    let top_query _ = Queries.Result.top () in
    let args = List.map (fun x -> MyCFG.unknown_exp) fd.Cil.sformals in
    let theta x = Spec.Glob.Val.bot () in
    let ignore2 _ _ = () in 
    let error _ = failwith "Bug: Using enter_func for toplevel functions with 'otherstate'." in 
    let ctx = Analyses.context top_query st theta [] ignore2 error error in
    let ents = Spec.enter_func ctx None fd.Cil.svar args in
      List.map (fun (_,s) -> fd.Cil.svar, LD.lift s) ents  
  in
  
  let _ = try MyCFG.dummy_func.Cil.svar.Cil.vdecl <- (List.hd otherfuns).Cil.svar.Cil.vdecl with Failure _ -> () in
  
  let startvars = 
    if startfuns = [] 
    then [[MyCFG.dummy_func.Cil.svar, startstate]]
    else List.map (enter_with (LD.unlift startstate)) startfuns 
  in
   
  let exitvars = List.map (enter_with (Spec.exitstate ())) exitfuns in
  let othervars = List.map (enter_with (Spec.otherstate ())) otherfuns in
  let startvars = List.concat (startvars @ exitvars @ othervars) in
  
  let _ = 
  if startvars = [] 
    then failwith "BUG: Empty set of start variables; may happen if\
       enter_func of any analysis returns an empty list." 
  in
  
  let context_fn f = if get_bool "exp.full-context" then fun x->x else Spec.context_top f in
  
  let startvars' = 
    List.map (fun (n,e) -> (`L (MyCFG.Function n, HCLD.lift (LD.lift (context_fn n (LD.unlift e)))),0)) startvars in
  
  let entrystates = List.map2 (fun (`L (_,e),i) (n,d) -> ((`L (MyCFG.FunctionEntry n, e), i), `Left d)) startvars' startvars in
  (*
  let entrystatesq = List.map2 (fun (_,e) (n,d) -> (MyCFG.FunctionEntry n, e), d) startvars' startvars in
  let startvars'' = if !Goblintutil.forward then [] else startvars' in
  *)
  
  let local_xml = ref (Result.create 0) in
  let global_xml = ref (Xml.PCData "not-ready" ) in
  let do_analyze () =
    let h = Slvr.solve EQSys.box entrystates startvars' in
    local_xml := solver2source_result h;
    global_xml := make_global_xml h
  in
  Spec.finalize ();
  
  Goblintutil.timeout do_analyze () (float_of_int (get_int "dbg.timeout"))
    (fun () -> Messages.waitWhat "Timeout reached!");
    
  Result.output (lazy !local_xml) (lazy (!global_xml :: [])) file
  
    
    
