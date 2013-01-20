open Cil
open MyCFG
open Pretty
open Generic
open Analyses
open GobConfig
open Batteries_uni



module HashconsLifter (S:Spec2)
  : Spec2 with module D = Lattice.HConsed (S.D)
           and module G = S.G
           and module C = Printable.HConsed (S.C)
  = 
struct
  module D = Lattice.HConsed (S.D)
  module G = S.G
  module C = Printable.HConsed (S.C)
  
  let name = S.name^" hashconsed"
  
  let init = S.init
  let finalize = S.finalize
  
  let startstate () = D.lift (S.startstate ())
  let exitstate () = D.lift (S.exitstate ())
  let otherstate () = D.lift (S.otherstate ())

  let context = C.lift -| S.context -| D.unlift
  let call_descr f = S.call_descr f -| D.unlift

  let conv ctx = 
    { ctx with local2 = D.unlift ctx.local2 
             ; spawn2 = (fun v -> ctx.spawn2 v -| D.lift )
             ; split2 = (fun d e tv -> ctx.split2 (D.lift d) e tv )
    }
  
  let sync ctx = 
    let d, diff = S.sync (conv ctx) in
    D.lift d, diff
    
  let query ctx q = 
    S.query (conv ctx) q
    
  let assign ctx lv e = 
    D.lift **> S.assign (conv ctx) lv e
    
  let branch ctx e tv = 
    D.lift **> S.branch (conv ctx) e tv
    
  let body ctx f = 
    D.lift **> S.body (conv ctx) f
    
  let return ctx r f = 
    D.lift **> S.return (conv ctx) r f
    
  let intrpt ctx = 
    D.lift **> S.intrpt (conv ctx)
    
  let enter ctx r f args = 
    List.map (fun (x,y) -> D.lift x, D.lift y) **> S.enter (conv ctx) r f args
    
  let special ctx r f args = 
    D.lift **> S.special (conv ctx) r f args
      
  let combine ctx r fe f args es = 
    D.lift **> S.combine (conv ctx) r fe f args (D.unlift es) 
end 

module DeadCodeLifter (S:Spec2)
  : Spec2 with module D = Dom (S.D)
           and module G = S.G
           and module C = S.C
  = 
struct
  module D = Dom (S.D)
  module G = S.G
  module C = S.C
  
  let name = S.name^" lifted"
  
  let init = S.init
  let finalize = S.finalize
  
  let startstate () = `Lifted (S.startstate ())
  let exitstate () = `Lifted (S.exitstate ())
  let otherstate () = `Lifted (S.otherstate ())

  let context = S.context -| D.unlift
  let call_descr f = S.call_descr f 

  let conv ctx = 
    { ctx with local2 = D.unlift ctx.local2 
             ; spawn2 = (fun v -> ctx.spawn2 v -| D.lift )
             ; split2 = (fun d e tv -> ctx.split2 (D.lift d) e tv )
    }
    
  let lift_fun ctx f g h b =
    try f **> h (g (conv ctx)) 
    with Deadcode -> b
  
  let sync ctx = 
    let liftpair (x,y) = D.lift x, y in
    lift_fun ctx liftpair S.sync identity (`Bot, [])

  let enter ctx r f args = 
    let liftmap = List.map (fun (x,y) -> D.lift x, D.lift y) in
    lift_fun ctx liftmap S.enter ((|>) args -| (|>) f -| (|>) r) []
    
  let query ctx q     = lift_fun ctx identity S.query  ((|>) q)            `Bot    
  let assign ctx lv e = lift_fun ctx D.lift   S.assign ((|>) e -| (|>) lv) `Bot
  let branch ctx e tv = lift_fun ctx D.lift   S.branch ((|>) tv -| (|>) e) `Bot
  let body ctx f      = lift_fun ctx D.lift   S.body   ((|>) f)            `Bot
  let return ctx r f  = lift_fun ctx D.lift   S.return ((|>) f -| (|>) r)  `Bot
  let intrpt ctx      = lift_fun ctx D.lift   S.intrpt identity            `Bot
  let special ctx r f args       = lift_fun ctx D.lift S.special ((|>) args -| (|>) f -| (|>) r)        `Bot
  let combine ctx r fe f args es = lift_fun ctx D.lift S.combine (fun p -> p r fe f args (D.unlift es)) `Bot
  
end 



module Spec2OfSpec (S:Spec with module Glob.Var = Basetype.Variables) 
  : Spec2 with module D = S.Dom
           and module G = S.Glob.Val 
           and module C = S.Dom 
  =
struct
  module D = S.Dom
  module G = S.Glob.Val
  module C = S.Dom
  
  let name = S.name
  
  let init = S.init
  let finalize = S.finalize
  
  let startstate = S.startstate
  let exitstate = S.exitstate
  let otherstate = S.otherstate

  let context = S.context_top dummyFunDec.svar
  let call_descr = S.es_to_string
  
  let conv_ctx ctx2 =
    { ask = ctx2.ask2
    ; local = ctx2.local2
    ; global = ctx2.global2
    ; sub = ctx2.postsub2
    ; spawn = ctx2.spawn2
    ; geffect = ctx2.sideg2
    ; precomp = []
    ; preglob = []
    ; report_access = (fun _ -> ())
    }
  
  let sync   = S.sync   -| conv_ctx
  let query  = S.query  -| conv_ctx
  let assign = S.assign -| conv_ctx
  let branch = S.branch -| conv_ctx
  let body   = S.body   -| conv_ctx
  let return = S.return -| conv_ctx
  let intrpt = S.intrpt -| conv_ctx
  

  let enter   = S.enter_func -| conv_ctx
  let combine = S.leave_func -| conv_ctx

  let special ctx2 r f args = 
    match S.special_fn (conv_ctx ctx2) r f args with
     | (d,exp,tv)::[] when tv && isInteger exp = Some 1L -> d
     | xs -> List.iter (fun (d,e,tv) -> ctx2.split2 d e tv) xs; D.bot ()
end


module FromSpec (S:Spec2) (Cfg:CfgBackward)
  : GlobConstrSys with type lv = node * S.C.t
                   and type gv = varinfo
                   and type ld = S.D.t
                   and type gd = S.G.t
                   and type c  = S.C.t
                   and module C = S.C
                   and module LVar = VarF (S.C)
                   and module GVar = Basetype.Variables
                   and module D = S.D
                   and module G = S.G
  =
struct
  type lv = MyCFG.node * S.C.t
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  type c  = S.C.t
  module C = S.C
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G
  
  let context = S.context

  let common_ctx (v,c) u (getl:lv -> ld) sidel getg sideg : (D.t, G.t) ctx2 = 
    let pval = getl (u,c) in     
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now wach this ... *)
    let rec ctx = 
      { ask2     = query
      ; local2   = pval
      ; global2  = getg
      ; presub2  = []
      ; postsub2 = []
      ; spawn2   = (fun f d -> let c = S.context d in 
                                sidel (FunctionEntry f, c) d; 
                                ignore (getl (Function f, c)))
      ; split2   = (fun (d:D.t) _ _ -> sidel (v,c) d)
      ; sideg2   = sideg
      } 
    and query x = S.query ctx x in
    (* ... nice, right! *)
    let pval, diff = S.sync ctx in
    let _ = List.iter (uncurry sideg) diff in
    { ctx with local2 = pval }
    

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

  let tf_normal_call ctx lv e f args  getl sidel getg sideg =
    let combine (cd, fd) = S.combine {ctx with local2 = cd} lv e f args fd in
    let paths = S.enter ctx lv f args in
    let _     = List.iter (fun (c,v) -> sidel (FunctionEntry f, S.context v) v) paths in
    let paths = List.map (fun (c,v) -> (c, getl (Function f, S.context v))) paths in
    let paths = List.filter (fun (c,v) -> D.is_bot v = false) paths in
    let paths = List.map combine paths in
      List.fold_left D.join (D.bot ()) paths
      
  let tf_special_call ctx lv f args = S.special ctx lv f args 

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
    let d       = try tf (v,c) (e,u) getl sidel getg sideg 
                  with M.StopTheWorld -> D.bot ()
                     | M.Bailure s -> Messages.warn_each s; (getl (u,c))  in
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
  