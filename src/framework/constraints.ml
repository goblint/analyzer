open Cil
open MyCFG
open Pretty
open Analyses
open GobConfig
open Batteries


(** Lifts a [Spec2] so that the domain and the context are [Hashcons]d. *)
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
  
  let should_join x y = S.should_join (D.unlift x) (D.unlift y)
  
  let startstate () = D.lift (S.startstate ())
  let exitstate () = D.lift (S.exitstate ())
  let otherstate () = D.lift (S.otherstate ())

  let context = C.lift % S.context % D.unlift
  let call_descr f = S.call_descr f % D.unlift

  let conv ctx = 
    { ctx with local2 = D.unlift ctx.local2 
             ; spawn2 = (fun v -> ctx.spawn2 v % D.lift )
             ; split2 = (fun d e tv -> ctx.split2 (D.lift d) e tv )
    }
  
  let sync ctx = 
    let d, diff = S.sync (conv ctx) in
    D.lift d, diff
    
  let query ctx q = 
    S.query (conv ctx) q
    
  let assign ctx lv e = 
    D.lift @@ S.assign (conv ctx) lv e
    
  let branch ctx e tv = 
    D.lift @@ S.branch (conv ctx) e tv
    
  let body ctx f = 
    D.lift @@ S.body (conv ctx) f
    
  let return ctx r f = 
    D.lift @@ S.return (conv ctx) r f
    
  let intrpt ctx = 
    D.lift @@ S.intrpt (conv ctx)
    
  let enter ctx r f args = 
    List.map (fun (x,y) -> D.lift x, D.lift y) @@ S.enter (conv ctx) r f args
    
  let special ctx r f args = 
    D.lift @@ S.special (conv ctx) r f args
      
  let combine ctx r fe f args es = 
    D.lift @@ S.combine (conv ctx) r fe f args (D.unlift es) 
end 

(** Lifts a [Spec2] with a special bottom element that represent unreachable code. *)
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
  
  let should_join x y = 
    match x, y with 
      | `Lifted a, `Lifted b -> S.should_join a b
      | _ -> true
  
  let startstate () = `Lifted (S.startstate ())
  let exitstate () = `Lifted (S.exitstate ())
  let otherstate () = `Lifted (S.otherstate ())

  let context = S.context % D.unlift
  let call_descr f = S.call_descr f 

  let conv ctx = 
    { ctx with local2 = D.unlift ctx.local2 
             ; spawn2 = (fun v -> ctx.spawn2 v % D.lift )
             ; split2 = (fun d e tv -> ctx.split2 (D.lift d) e tv )
    }
    
  let lift_fun ctx f g h b =
    try f @@ h (g (conv ctx)) 
    with Deadcode -> b
  
  let sync ctx = 
    let liftpair (x,y) = D.lift x, y in
    lift_fun ctx liftpair S.sync identity (`Bot, [])

  let enter ctx r f args = 
    let liftmap = List.map (fun (x,y) -> D.lift x, D.lift y) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r) []
    
  let query ctx q     = lift_fun ctx identity S.query  ((|>) q)            `Bot    
  let assign ctx lv e = lift_fun ctx D.lift   S.assign ((|>) e % (|>) lv) `Bot
  let branch ctx e tv = lift_fun ctx D.lift   S.branch ((|>) tv % (|>) e) `Bot
  let body ctx f      = lift_fun ctx D.lift   S.body   ((|>) f)            `Bot
  let return ctx r f  = lift_fun ctx D.lift   S.return ((|>) f % (|>) r)  `Bot
  let intrpt ctx      = lift_fun ctx D.lift   S.intrpt identity            `Bot
  let special ctx r f args       = lift_fun ctx D.lift S.special ((|>) args % (|>) f % (|>) r)        `Bot
  let combine ctx r fe f args es = lift_fun ctx D.lift S.combine (fun p -> p r fe f args (D.unlift es)) `Bot
  
end 


(** Translate the old [Spec] into the new [Spec2] system. *)
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
  
  let should_join = S.should_join
  
  let startstate = S.startstate
  let exitstate = S.exitstate
  let otherstate = S.otherstate

  let context = S.context_top dummyFunDec.svar
  let call_descr = S.es_to_string
  
  let translate_sub = List.map (fun (n,x) -> List.assoc n !MCP.objInjectLocal x)
  
  let conv_ctx ctx2 =
    { ask = ctx2.ask2
    ; local = ctx2.local2
    ; global = ctx2.global2
    ; sub = translate_sub ctx2.postsub2
    ; presub = translate_sub ctx2.presub2
    ; spawn = ctx2.spawn2
    ; geffect = ctx2.sideg2
    ; precomp = []
    ; preglob = []
    ; report_access = (fun _ -> ())
    }
  
  let sync   = S.sync   % conv_ctx
  let query  = S.query  % conv_ctx
  let assign = S.assign % conv_ctx
  let branch = S.branch % conv_ctx
  let body   = S.body   % conv_ctx
  let return = S.return % conv_ctx
  let intrpt = S.intrpt % conv_ctx
  

  let enter   = S.enter_func % conv_ctx
  let combine = S.leave_func % conv_ctx

  let special ctx2 r f args = 
    match S.special_fn (conv_ctx ctx2) r f args with
     | (d,exp,tv)::[] when tv && isInteger exp = Some 1L -> d
     | xs -> List.iter (fun (d,e,tv) -> ctx2.split2 d e tv) xs; raise Deadcode
end

(** The main point of this file---generating a [GlobConstrSys] from a [Spec2]. *)
module FromSpec (S:Spec2) (Cfg:CfgBackward)
  : GlobConstrSys with module LVar = VarF (S.C)
                   and module GVar = Basetype.Variables
                   and module D = S.D
                   and module G = S.G
  =
struct
  type lv = MyCFG.node * S.C.t
  type gv = varinfo
  type ld = S.D.t
  type gd = S.G.t
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  let common_ctx (v,c) u (getl:lv -> ld) sidel getg sideg : (D.t, G.t) ctx2 = 
    let pval = getl (u,c) in     
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now watch this ... *)
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



(** Combined variables so that we can also use the more common [IneqConstrSys], and [EqConstrSys]
    that use only one kind of a variable. *)
module Var2 (LV:VarType) (GV:VarType)
  : VarType 
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
    | `G a -> GV.pretty_trace () a
      
  let line_nr = function
    | `L a -> LV.line_nr a
    | `G a -> GV.line_nr a
    
  let file_name = function
    | `L a -> LV.file_name a
    | `G a -> GV.file_name a
    
  let description n = sprint 80 (pretty_trace () n)
  let context () _ = Pretty.nil
  let loopSep _ = true
end

(** Translate a [GlobConstrSys] into a [IneqConstrSys] *)
module IneqConstrSysFromGlobConstrSys (S:GlobConstrSys)
  : IneqConstrSys with type v = Var2(S.LVar)(S.GVar).t 
                   and type d = Lattice.Either(S.D)(S.G).t
                   and module Var = Var2(S.LVar)(S.GVar)
                   and module Dom = Lattice.Either(S.D)(S.G)
  =
struct
  module Var = Var2(S.LVar)(S.GVar)
  module Dom = Lattice.Either(S.D)(S.G)
  
  type v = Var.t 
  type d = Dom.t
  
  let box f x y = if Dom.leq y x then Dom.narrow x y else Dom.widen x (Dom.join x y)
  
  let getL = function
    | `Left x -> x
    | `Right _ -> S.D.bot ()
    | _ -> failwith "IneqConstrSysFromGlobConstrSys broken: Left!"

  let getR = function
    | `Right x -> x
    | _ -> failwith "IneqConstrSysFromGlobConstrSys broken: Right!"
    
  let l, g = (fun x -> `L x), (fun x -> `G x)  
  let le, ri = (fun x -> `Left x), (fun x -> `Right x)  
  
  let conv f get set = 
    f (getL % get % l) (fun x v -> set (l x) (le v)) 
      (getR % get % g) (fun x v -> set (g x) (ri v)) 
    |> le
  
  let system = function
    | `G _ -> []
    | `L x -> List.map conv (S.system x)
end


(*module GlobSolverFromEqSolverWhat (Sol:GenericEqBoxSolver)
  : GenericGlobSolver 
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.lv) ->
    functor (GH:Hash.H with type key=S.gv) ->
struct
  module IneqSys = IneqConstrSysFromGlobConstrSys (S)
  module EqSys = Generic.SimpleSysConverter (IneqSys)
  
  module VH : Hash.H with type key=IneqSys.v and type 'a t = 'a LH.t * 'a GH.t =
  struct
    type key = IneqSys.Var.t
    type 'a t = ('a LH.t) * ('a GH.t)
    let create n = (LH.create n, GH.create n)
    let clear (l,g) = LH.clear l; GH.clear g
    let copy (l,g) = (LH.copy l, GH.copy g)
        
    let lift (f:'a LH.t -> S.lv -> 'b) (h:'a GH.t -> S.gv -> 'b) 
             (l,g:'a t) : [`L of S.lv | `G of S.gv] -> 'b  = function
      | `L x -> f l x
      | `G x -> h g x
    
    let add          x = lift LH.add          GH.add          x
    let remove       x = lift LH.remove       GH.remove       x
    let find         x = lift LH.find         GH.find         x
    let find_default x = lift LH.find_default GH.find_default x
    let find_all     x = lift LH.find_all     GH.find_all     x
    let replace      x = lift LH.replace      GH.replace      x
    let mem          x = lift LH.mem          GH.mem          x
    let find_all     x = lift LH.find_all     GH.find_all     x
    let find_all     x = lift LH.find_all     GH.find_all     x
    let find_all     x = lift LH.find_all     GH.find_all     x
    
    let iter f (l,g) =
      LH.iter (fun k v -> f (`L k) v) l;
      GH.iter (fun k v -> f (`G k) v) g
      
    let fold f (l,g) x =
      let gx = LH.fold (fun k v a -> f (`L k) v a) l x in
      let rx = GH.fold (fun k v a -> f (`G k) v a) g gx in
      rx
      
    let length (l,g) = LH.length l + GH.length g
  end
  
  module Sol' = Sol (EqSys) (VH)

  let getL = function
    | `Left x -> x
    | _ -> undefined ()

  let getR = function
    | `Right x -> x
    | _ -> undefined ()

  let solve ls gs l = 
    let vs = List.map (fun (x,v) -> `L x, `Left v) ls @ List.map (fun (x,v) -> `G x, `Right v) gs in 
    let l, g = Sol'.solve IneqSys.box vs [] in
    (* one could 'magic' it so no copying would be necessary *)
    let l' = LH.create (LH.length l) in
    let g' = GH.create (GH.length g) in
    LH.iter (fun k v -> LH.add l' k (getL v)) l;
    GH.iter (fun k v -> GH.add g' k (getR v)) g;
    (l', g')
end*)

(** Transforms a [GenericEqBoxSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromEqSolver (Sol:GenericEqBoxSolver)
  : GenericGlobSolver 
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.LVar.t) ->
    functor (GH:Hash.H with type key=S.GVar.t) ->
struct
  let lh_find_default h k d = try LH.find h k with Not_found -> d
  let gh_find_default h k d = try GH.find h k with Not_found -> d

  module IneqSys = IneqConstrSysFromGlobConstrSys (S)
  module EqSys = Generic.NormalSysConverter (IneqSys)
  
  module VH : Hash.H with type key=EqSys.v = Hashtbl.Make(EqSys.Var)
  module Sol' = Sol (EqSys) (VH)

  let getL = function
    | `Left x -> x
    | _ -> undefined ()

  let getR = function
    | `Right x -> x
    | _ -> undefined ()

  let solve ls gs l = 
    let vs = List.map (fun (x,v) -> EqSys.conv (`L x), `Left v) ls 
           @ List.map (fun (x,v) -> EqSys.conv (`G x), `Right v) gs in 
    let sv = List.map (fun x -> EqSys.conv (`L x)) l in
    let hm = Sol'.solve EqSys.box vs sv in
    let l' = LH.create 113 in
    let g' = GH.create 113 in
    let split_vars = function
      | (`L x,_) -> fun y -> LH.replace l' x (S.D.join (getL y) (lh_find_default l' x (S.D.bot ())))
      | (`G x,_) -> fun y -> GH.replace g' x (getR y)
    in
    VH.iter split_vars hm;
    (l', g')
end

(** Add path sensitivity to a analysis *)
module PathSensitive2 (S:Spec2) 
  : Spec2 
  with type D.t = SetDomain.Make(S.D).t
   and module G = S.G
   and module C = S.C
  =
struct    
  module D = 
  struct
    include SetDomain.Make (S.D)
    let name () = "PathSensitive (" ^ name () ^ ")"
    
    (** [leq a b] iff each element in [a] has a [leq] counterpart in [b]*)
    let leq s1 s2 = 
    let p t = exists (fun s -> S.D.leq t s) s2 in
    for_all p s1

    let pretty_diff () ((s1:t),(s2:t)): Pretty.doc = 
    if leq s1 s2 then dprintf "%s: These are fine!" (name ()) else begin
     let p t = not (exists (fun s -> S.D.leq t s) s2) in
     let evil = choose (filter p s1) in
     let other = choose s2 in
       (* dprintf "%s has a problem with %a not leq %a because %a" (name ()) 
         S.D.pretty evil S.D.pretty other 
         S.D.pretty_diff (evil,other) *)
       S.D.pretty_diff () (evil,other)

    end
    
    (** For [join x y] we take a union of [x] & [y] and join elements 
    * which base analysis suggests us to.*)
    let join s1 s2 =
    let rec loop s1 s2 = 
     let f b (ok, todo) =
       let joinable, rest = partition (S.should_join b) ok in
       if cardinal joinable = 0 then
         (add b ok, todo)
       else
         let joint = fold (S.D.join) joinable b in
         (fold remove joinable ok, add joint todo)
     in
     let (ok, todo) = fold f s2 (s1, empty ()) in
       if is_empty todo then 
         ok
       else
         loop ok todo
    in
    loop s1 s2    
  
    (** carefully add element (because we might have to join something)*)
    let add e s = join s (singleton e)
  
    (** We dont have good info for this operation -- only thing is to [meet] all elements.*)
    let meet s1 s2 = 
    (* Try to not use top, as it is often not implemented. *)
    let fold1 f s =
     let g x = function
       | None -> Some x
       | Some y -> Some (f x y)
     in
     match fold g s None with  
       | Some x -> x
       | None -> S.D.top()
    in
    singleton (fold1 S.D.meet (union s1 s2))
    
    (** Widening operator. We take all possible (growing) paths, do elementwise 
       widenging and join them together. When the used path sensitivity is 
       not overly dynamic then no joining occurs.*)
    let widen s1 s2 = 
      let f e =
      let l = filter (fun x -> S.D.leq x e) s1 in
      let m = map (fun x -> S.D.widen x e) l in
      fold (S.D.join) m e
      in
        map f s2

    (** Narrowing operator. As with [widen] some precision loss might occur.*)
    let narrow s1 s2 = 
      let f e =
      let l = filter (fun x -> S.D.leq x e) s2 in
      let m = map (S.D.narrow e) l in
      fold (S.D.join) m (S.D.bot ())
      in
        map f s1
  end
  
  module G = S.G
  module C = S.C
  
  let name = "PathSensitive2("^S.name^")"
  
  let init = S.init
  let finalize = S.finalize
  
  let should_join x y = true
  
  let otherstate () = D.singleton (S.otherstate ())
  let exitstate  () = D.singleton (S.exitstate  ())
  let startstate () = D.singleton (S.startstate ())
  
  let call_descr = S.call_descr 
  
  let context l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive2.context must be called with a singleton set."
    else
      S.context @@ D.choose l
      
  let conv ctx x = 
    let rec ctx' = { ctx with ask2   = query
                            ; local2 = x
                            ; spawn2 = (fun v -> ctx.spawn2 v % D.singleton )
                            ; split2 = (ctx.split2 % D.singleton) }
    and query x = S.query ctx' x in
    ctx'
          
  let map ctx f g =
    let h x xs = 
      try D.add (g (f (conv ctx x))) xs
      with Deadcode -> xs
    in
    let d = D.fold h ctx.local2 (D.empty ()) in
    if D.is_bot d then raise Deadcode else d
    
  let assign ctx l e    = map ctx S.assign  (fun h -> h l e )
  let body   ctx f      = map ctx S.body    (fun h -> h f   )
  let return ctx e f    = map ctx S.return  (fun h -> h e f )
  let branch ctx e tv   = map ctx S.branch  (fun h -> h e tv)
  let intrpt ctx        = map ctx S.intrpt  identity
  let special ctx l f a = map ctx S.special (fun h -> h l f a)
  
  let fold ctx f g h a =
    let k x a = 
      try h a @@ g @@ f @@ conv ctx x 
      with Deadcode -> a
    in
    let d = D.fold k ctx.local2 a in
    if D.is_bot d then raise Deadcode else d

  let fold' ctx f g h a =
    let k x a = 
      try h a @@ g @@ f @@ conv ctx x 
      with Deadcode -> a
    in
    D.fold k ctx.local2 a 
  
  let sync ctx = 
    fold' ctx S.sync identity (fun (a,b) (a',b') -> D.add a' a, b'@b) (D.empty (), [])

  let query ctx q = 
    fold' ctx S.query identity (fun x f -> Queries.Result.meet x (f q)) `Top
    
  let enter ctx l f a =
    let g xs ys = (List.map (fun (x,y) -> D.singleton x, D.singleton y) ys) @ xs in   
    fold' ctx S.enter (fun h -> h l f a) g []

  let combine ctx l fe f a d =
    assert (D.cardinal ctx.local2 = 1);
    let cd = D.choose ctx.local2 in
    let k x y = 
      try D.add (S.combine (conv ctx cd) l fe f a x) y
      with Deadcode -> y 
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d

end  

(** Verify if the hashmap pair is really a (partial) solution. *)
module Verify2 
  (S:GlobConstrSys) 
  (LH:Hash.H with type key=S.LVar.t) 
  (GH:Hash.H with type key=S.GVar.t) 
  =
struct
  open S
  
  let verify (sigma:D.t LH.t) (theta:G.t GH.t) =
    Goblintutil.in_verifying_stage := true;
    let correct = ref true in
    let complain_l (v:LVar.t) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Fixpoint not reached at %a (%s:%d)\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" 
                LVar.pretty_trace v (LVar.file_name v) (LVar.line_nr v) D.pretty lhs D.pretty rhs D.pretty_diff (rhs,lhs))
    in
    let complain_g v (g:GVar.t) lhs rhs = 
      correct := false; 
      ignore (Pretty.printf "Unsatisfied constraint for global %a at variable %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\n@]" 
                GVar.pretty_trace g LVar.pretty_trace v G.pretty lhs G.pretty rhs)
    in
    (* For each variable v which has been assigned value d', would like to check
     * that d' satisfied all constraints. *)
    let verify_var v d' = 
      let verify_constraint rhs =
        let sigma' x = LH.find sigma x in
        let theta' x = GH.find theta x in
        (* First check that each (global) delta is included in the (global)
         * invariant. *)
        let check_local l lv =
          let lv' = sigma' l in 
            if not (D.leq lv lv') then 
              complain_l l lv' lv  
        in
        let check_glob g gv = 
          let gv' = theta' g in 
            if not (G.leq gv gv') then 
              complain_g v g gv' gv  
        in    
        let d = rhs sigma' check_local theta' check_glob in
        (* Then we check that the local state satisfies this constraint. *)
          if not (D.leq d d') then
            complain_l v d' d
      in
      let rhs = system v in
        List.iter verify_constraint rhs
    in
      LH.iter verify_var sigma;
      Goblintutil.in_verifying_stage := false
end
