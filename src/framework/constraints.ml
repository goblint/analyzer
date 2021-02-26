(** How to generate constraints for a solver using specifications described in [Analyses]. *)

open Prelude
open Cil
open MyCFG
open Pretty
open Analyses
open GobConfig

module M = Messages

(** Lifts a [Spec] so that the domain is [Hashcons]d *)
module HashconsLifter (S:Spec)
  : Spec with module D = Lattice.HConsed (S.D)
          and module G = S.G
          and module C = S.C
=
struct
  module D = Lattice.HConsed (S.D)
  module G = S.G
  module C = S.C

  let name () = S.name () ^" hashconsed"

  let init = S.init
  let finalize = S.finalize

  let should_join x y = S.should_join (D.unlift x) (D.unlift y)

  let startstate v = D.lift (S.startstate v)
  let exitstate  v = D.lift (S.exitstate  v)
  let morphstate v d = D.lift (S.morphstate v (D.unlift d))

  let val_of = D.lift % S.val_of
  let context = S.context % D.unlift
  let call_descr = S.call_descr

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d e tv -> ctx.split (D.lift d) e tv )
    }

  let sync ctx =
    let d, diff = S.sync (conv ctx) in
    D.lift d, diff

  let query ctx q =
    S.query (conv ctx) q

  let assign ctx lv e =
    D.lift @@ S.assign (conv ctx) lv e

  let vdecl ctx v =
    D.lift @@ S.vdecl (conv ctx) v

  let branch ctx e tv =
    D.lift @@ S.branch (conv ctx) e tv

  let body ctx f =
    D.lift @@ S.body (conv ctx) f

  let return ctx r f =
    D.lift @@ S.return (conv ctx) r f

  let intrpt ctx =
    D.lift @@ S.intrpt (conv ctx)

  let asm ctx =
    D.lift @@ S.asm (conv ctx)

  let skip ctx =
    D.lift @@ S.skip (conv ctx)

  let enter ctx r f args =
    List.map (fun (x,y) -> D.lift x, D.lift y) @@ S.enter (conv ctx) r f args

  let special ctx r f args =
    D.lift @@ S.special (conv ctx) r f args

  let combine ctx r fe f args fc es =
    D.lift @@ S.combine (conv ctx) r fe f args fc (D.unlift es)

  let threadenter ctx lval f args =
    D.lift @@ S.threadenter (conv ctx) lval f args

  let threadspawn ctx lval f args fctx =
    D.lift @@ S.threadspawn (conv ctx) lval f args (conv fctx)
end

(** Lifts a [Spec] so that the context is [Hashcons]d. *)
module HashconsContextLifter (S:Spec)
  : Spec with module D = S.D
          and module G = S.G
          and module C = Printable.HConsed (S.C)
=
struct
  module D = S.D
  module G = S.G
  module C = Printable.HConsed (S.C)

  let name () = S.name () ^" context hashconsed"

  let init = S.init
  let finalize = S.finalize

  let should_join = S.should_join

  let startstate = S.startstate
  let exitstate  = S.exitstate
  let morphstate = S.morphstate

  let val_of = S.val_of % C.unlift
  let context = C.lift % S.context
  let call_descr f = S.call_descr f % C.unlift

  let conv ctx =
    { ctx with context = (fun () -> C.unlift (ctx.context ())) }

  let sync ctx =
    let d, diff = S.sync (conv ctx) in
    d, diff

  let query ctx q =
    match q with
    | Queries.IterPrevVars f ->
      let g i (n, c, j) e = f i (n, Obj.repr (C.lift (Obj.obj c)), j) e in
      S.query (conv ctx) (Queries.IterPrevVars g)
    | _ -> S.query (conv ctx) q

  let assign ctx lv e =
    S.assign (conv ctx) lv e

  let vdecl ctx v =
    S.vdecl (conv ctx) v

  let branch ctx e tv =
    S.branch (conv ctx) e tv

  let body ctx f =
    S.body (conv ctx) f

  let return ctx r f =
    S.return (conv ctx) r f

  let intrpt ctx =
    S.intrpt (conv ctx)

  let asm ctx =
    S.asm (conv ctx)

  let skip ctx =
    S.skip (conv ctx)

  let enter ctx r f args =
    S.enter (conv ctx) r f args

  let special ctx r f args =
    S.special (conv ctx) r f args

  let combine ctx r fe f args fc es =
    S.combine (conv ctx) r fe f args (C.unlift fc) es

  let threadenter ctx lval f args =
    S.threadenter (conv ctx) lval f args

  let threadspawn ctx lval f args fctx =
    S.threadspawn (conv ctx) lval f args (conv fctx)
end

(* see option ana.opt.equal *)
module OptEqual (S: Spec) = struct
  module D = struct include S.D let equal x y = x == y || equal x y end
  module G = struct include S.G let equal x y = x == y || equal x y end
  module C = struct include S.C let equal x y = x == y || equal x y end
  include (S : Spec with module D := D and module G := G and module C := C)
end

(** If dbg.slice.on, stops entering functions after dbg.slice.n levels. *)
module LevelSliceLifter (S:Spec)
  : Spec with module D = Lattice.Prod (S.D) (Lattice.Reverse (IntDomain.Lifted))
          and module G = S.G
          and module C = S.C
=
struct
  module D = Lattice.Prod (S.D) (Lattice.Reverse (IntDomain.Lifted))
  module G = S.G
  module C = S.C

  let name () = S.name ()^" level sliced"

  let start_level = ref (`Top)
  let error_level = ref (`Lifted  0L)

  let init () =
    if get_bool "dbg.slice.on" then
      start_level := `Lifted (Int64.of_int (get_int "dbg.slice.n"));
    S.init ()

  let finalize = S.finalize

  let should_join (x,_) (y,_) = S.should_join x y

  let startstate v = (S.startstate v, !start_level)
  let exitstate  v = (S.exitstate  v, !start_level)
  let morphstate v (d,l) = (S.morphstate v d, l)

  let val_of d = (S.val_of d, !error_level)
  let context (d,_) = S.context d
  let call_descr f = S.call_descr f

  let conv ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d e tv -> ctx.split (d, snd ctx.local) e tv )
    }

  let lift_fun ctx f g h =
    f @@ h (g (conv ctx))

  let sync ctx =
    let liftpair (x, y) = (x, snd ctx.local), y in
    lift_fun ctx liftpair S.sync identity

  let enter' ctx r f args =
    let liftmap = List.map (fun (x,y) -> (x, snd ctx.local), (y, snd ctx.local)) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r)

  let lift ctx d = (d, snd ctx.local)
  let lift_start_level d = (d, !start_level)

  let query' ctx q    = lift_fun ctx identity   S.query  ((|>) q)
  let assign ctx lv e = lift_fun ctx (lift ctx) S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx (lift ctx) S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx (lift ctx) S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx (lift ctx) S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx (lift ctx) S.return ((|>) f % (|>) r)
  let intrpt ctx      = lift_fun ctx (lift ctx) S.intrpt identity
  let asm ctx         = lift_fun ctx (lift ctx) S.asm    identity
  let skip ctx        = lift_fun ctx (lift ctx) S.skip   identity
  let special ctx r f args        = lift_fun ctx (lift ctx) S.special ((|>) args % (|>) f % (|>) r)
  let combine' ctx r fe f args fc es = lift_fun ctx (lift ctx) S.combine (fun p -> p r fe f args fc (fst es))

  let threadenter ctx lval f args = lift_fun ctx lift_start_level S.threadenter ((|>) args % (|>) f % (|>) lval)
  let threadspawn ctx lval f args fctx = lift_fun ctx (lift ctx) S.threadspawn ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)

  let leq0 = function
    | `Top -> false
    | `Lifted x -> x <= 0L
    | `Bot -> true

  let sub1 = function
    | `Lifted x -> `Lifted (Int64.sub x 1L)
    | x -> x

  let add1 = function
    | `Lifted x -> `Lifted (Int64.add x 1L)
    | x -> x

  let enter ctx r f args =
    let (d,l) = ctx.local in
    if leq0 l then
      [ctx.local, D.bot ()]
    else
      enter' {ctx with local=(d, sub1 l)} r f args

  let combine ctx r fe f args fc es =
    let (d,l) = ctx.local in
    let l = add1 l in
    if leq0 l then
      (d, l)
    else
      let d',_ = combine' ctx r fe f args fc es in
      (d', l)

  let query ctx = function
    | Queries.EvalFunvar e ->
      let (d,l) = ctx.local in
      if leq0 l then
        `LvalSet (Queries.LS.empty ())
      else
        query' ctx (Queries.EvalFunvar e)
    | q -> query' ctx q
end


(** Limits the number of widenings per node. *)
module LimitLifter (S:Spec) =
struct
  include (S : module type of S with module D := S.D)

  let name () = S.name ()^" limited"

  let limit = ref 0

  let init () =
    limit := get_int "dbg.limit.widen";
    S.init ()

  module H = MyCFG.H
  let h = H.create 13
  let incr k =
    H.modify_def 1 k (fun v ->
        if v >= !limit then failwith ("LimitLifter: Reached limit ("^string_of_int !limit^") for node "^Ana.sprint MyCFG.pretty_short_node (Option.get !MyCFG.current_node));
        v+1
      ) h;
  module D = struct
    include S.D
    let widen x y = Option.may incr !MyCFG.current_node; widen x y (* when is this None? *)
  end
end


(* widening on contexts, keeps contexts for calls in context *)
module WidenContextLifter (S:Spec)
=
struct
  module DD =
  struct
    include S.D
    let printXml f d = BatPrintf.fprintf f "<value>%a</value>" printXml d
  end
  module M = MapDomain.MapBot (Basetype.Variables) (DD) (* should be CilFun -> S.C, but CilFun is not Groupable, and S.C is no Lattice *)
  module D = struct
    include Lattice.Prod (S.D) (M)
    let printXml f (d,m) = BatPrintf.fprintf f "\n%a<analysis name=\"widen-context\">\n%a\n</analysis>" S.D.printXml d M.printXml m
  end
  module G = S.G
  module C = Printable.Prod (S.C) (M)

  let name () = S.name ()^" with widened contexts"

  let init = S.init
  let finalize = S.finalize

  let should_join (x,_) (y,_) = S.should_join x y

  let inj f x = f x, M.bot ()

  let startstate = inj S.startstate
  let exitstate  = inj S.exitstate
  let morphstate v (d,m) = S.morphstate v d, m

  let val_of (c,m) =
    Messages.(if tracing then tracel "widen-context" "val_of with context %a\n" S.C.pretty c);
    S.val_of c, m
  let context (d,m) =
    Messages.(if tracing then tracel "widen-context" "context with child domain %a\nand map %a\n" S.D.pretty d M.pretty m);
    S.context d, m
  let call_descr f (c,m) = S.call_descr f c

  let conv ctx =
    { ctx with context = (fun () -> fst (ctx.context ()))
             ; local = fst ctx.local
             ; split = (fun d e tv -> ctx.split (d, snd ctx.local) e tv )
    }
  let lift_fun ctx f g = g (f (conv ctx)), snd ctx.local

  let sync ctx        = let d, ds = S.sync (conv ctx) in (d, snd ctx.local), ds
  let query ctx       = S.query (conv ctx)
  let assign ctx lv e = lift_fun ctx S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx S.return ((|>) f % (|>) r)
  let intrpt ctx      = lift_fun ctx S.intrpt identity
  let asm ctx         = lift_fun ctx S.asm    identity
  let skip ctx        = lift_fun ctx S.skip   identity
  let special ctx r f args       = lift_fun ctx S.special ((|>) args % (|>) f % (|>) r)

  let threadenter ctx lval f args = lift_fun ctx S.threadenter ((|>) args % (|>) f % (|>) lval)
  let threadspawn ctx lval f args fctx = lift_fun ctx S.threadspawn ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)

  let enter ctx r f args =
    let m = snd ctx.local in
    let d' v_cur =
      let v_old = M.find f m in
      let v_new = S.D.widen v_old (S.D.join v_old v_cur) in
      Messages.(if tracing && not (S.D.equal v_old v_new) then tracel "widen-context" "enter results in new context for function %s\n" f.vname);
      v_new, M.add f v_new m
    in
    S.enter (conv ctx) r f args |> List.map (fun (c,v) -> (c,m), d' v)

  let combine ctx r fe f args fc es = lift_fun ctx S.combine (fun p -> p r fe f args (fst fc) (fst es))
end


(* widening on contexts, keeps contexts for calls only in D (needs exp.full-context to be false to work) *)
module WidenContextLifterSide (S:Spec)
=
struct
  module B = WidenContextLifter (S)
  include (B : module type of B with module C := B.C)
  (* same as WidenContextLifter, but with a different C *)
  module C = S.C

  let val_of = inj S.val_of (* empty map when generating value from context *)
  let context (d,m) = S.context d (* just the child analysis' context *)
  let call_descr = S.call_descr

  (* copied from WidenContextLifter... *)
  let conv ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d e tv -> ctx.split (d, snd ctx.local) e tv )
    }
  let lift_fun ctx f g = g (f (conv ctx)), snd ctx.local

  let sync ctx        = let d, ds = S.sync (conv ctx) in (d, snd ctx.local), ds
  let query ctx       = S.query (conv ctx)
  let assign ctx lv e = lift_fun ctx S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx S.return ((|>) f % (|>) r)
  let intrpt ctx      = lift_fun ctx S.intrpt identity
  let asm ctx         = lift_fun ctx S.asm    identity
  let skip ctx        = lift_fun ctx S.skip   identity
  let special ctx r f args       = lift_fun ctx S.special ((|>) args % (|>) f % (|>) r)

  let threadenter ctx lval f args = lift_fun ctx S.threadenter ((|>) args % (|>) f % (|>) lval)
  let threadspawn ctx lval f args fctx = lift_fun ctx S.threadspawn ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)

  let enter ctx r f args =
    let m = snd ctx.local in
    let d' v_cur =
      let v_old = M.find f m in
      let v_new = S.D.widen v_old (S.D.join v_old v_cur) in
      Messages.(if tracing && not (S.D.equal v_old v_new) then tracel "widen-context" "enter results in new context for function %s\n" f.vname);
      v_new, M.add f v_new m
    in
    S.enter (conv ctx) r f args |> List.map (fun (c,v) -> (c,m), d' v)

  let combine ctx r fe f args fc es = lift_fun ctx S.combine (fun p -> p r fe f args fc (fst es))
end


(** Lifts a [Spec] with a special bottom element that represent unreachable code. *)
module DeadCodeLifter (S:Spec)
  : Spec with module D = Dom (S.D)
          and module G = S.G
          and module C = S.C
=
struct
  module D = Dom (S.D)
  module G = S.G
  module C = S.C

  let name () = S.name ()^" lifted"

  let init = S.init
  let finalize = S.finalize

  let should_join x y =
    match x, y with
    | `Lifted a, `Lifted b -> S.should_join a b
    | _ -> true

  let startstate v = `Lifted (S.startstate v)
  let exitstate  v = `Lifted (S.exitstate  v)
  let morphstate v d = try `Lifted (S.morphstate v (D.unlift d)) with Deadcode -> d

  let val_of = D.lift % S.val_of
  let context = S.context % D.unlift
  let call_descr f = S.call_descr f

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d e tv -> ctx.split (D.lift d) e tv )
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
  let vdecl ctx v     = lift_fun ctx D.lift   S.vdecl  ((|>) v)            `Bot
  let branch ctx e tv = lift_fun ctx D.lift   S.branch ((|>) tv % (|>) e) `Bot
  let body ctx f      = lift_fun ctx D.lift   S.body   ((|>) f)            `Bot
  let return ctx r f  = lift_fun ctx D.lift   S.return ((|>) f % (|>) r)  `Bot
  let intrpt ctx      = lift_fun ctx D.lift   S.intrpt identity            `Bot
  let asm ctx         = lift_fun ctx D.lift   S.asm    identity           `Bot
  let skip ctx        = lift_fun ctx D.lift   S.skip   identity           `Bot
  let special ctx r f args       = lift_fun ctx D.lift S.special ((|>) args % (|>) f % (|>) r)        `Bot
  let combine ctx r fe f args fc es = lift_fun ctx D.lift S.combine (fun p -> p r fe f args fc (D.unlift es)) `Bot

  let threadenter ctx lval f args = lift_fun ctx D.lift S.threadenter ((|>) args % (|>) f % (|>) lval) `Bot
  let threadspawn ctx lval f args fctx = lift_fun ctx D.lift S.threadspawn ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval) `Bot
end

module type Increment =
sig
  val increment: increment_data
end

(** The main point of this file---generating a [GlobConstrSys] from a [Spec]. *)
module FromSpec (S:Spec) (Cfg:CfgBackward) (I: Increment)
  : sig
    include GlobConstrSys with module LVar = VarF (S.C)
                           and module GVar = Basetype.Variables
                           and module D = S.D
                           and module G = S.G
    val tf : MyCFG.node * S.C.t -> (Cil.location * MyCFG.edge) list * MyCFG.node -> ((MyCFG.node * S.C.t) -> S.D.t) -> (MyCFG.node * S.C.t -> S.D.t -> unit) -> (Cil.varinfo -> G.t) -> (Cil.varinfo -> G.t -> unit) -> D.t
  end
=
struct
  type lv = MyCFG.node * S.C.t
  (* type gv = varinfo *)
  type ld = S.D.t
  (* type gd = S.G.t *)
  module LVar = VarF (S.C)
  module GVar = Basetype.Variables
  module D = S.D
  module G = S.G

  let full_context = get_bool "exp.full-context"
  (* Dummy module. No incremental analysis supported here*)
  let increment = I.increment
  let common_ctx var edge prev_node pval (getl:lv -> ld) sidel getg sideg : (D.t, G.t, S.C.t) ctx * D.t list ref =
    let r = ref [] in
    if !Messages.worldStopped then raise M.StopTheWorld;
    (* now watch this ... *)
    let rec ctx =
      { ask     = query
      ; node    = fst var
      ; prev_node = prev_node
      ; control_context = snd var
      ; context = snd var |> Obj.obj
      ; edge    = edge
      ; local   = pval
      ; global  = getg
      ; presub  = []
      ; postsub = []
      ; spawn   = spawn
      ; split   = (fun (d:D.t) _ _ -> r := d::!r)
      ; sideg   = sideg
      ; assign = (fun ?name _    -> failwith "Cannot \"assign\" in common context.")
      }
    and query x = S.query ctx x
    and spawn lval f args =
      (* TODO: adjust ctx node/edge? *)
      let d = S.threadenter ctx lval f args in
      let c = S.context d in
      let rec fctx =
        { ctx with
          ask = fquery
        ; local = d
        }
      and fquery x = S.query fctx x
      in
      r := S.threadspawn ctx lval f args fctx :: !r;
      if not full_context then sidel (FunctionEntry f, c) d;
      ignore (getl (Function f, c))
    in
    (* ... nice, right! *)
    let pval, diff = S.sync ctx in
    let _ = List.iter (uncurry sideg) diff in
    { ctx with local = pval }, r

  let rec bigsqcup = function
    | []    -> D.bot ()
    | [x]   -> x
    | x::xs -> D.join x (bigsqcup xs)

  let tf_loop var edge prev_node getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.intrpt ctx)::!r)

  let tf_assign var edge prev_node lv e getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.assign ctx lv e)::!r)

  let tf_vdecl var edge prev_node v getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.vdecl ctx v)::!r)

  let normal_return r fd ctx sideg =
    let spawning_return = S.return ctx r fd in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let toplevel_kernel_return r fd ctx sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.local else S.return ctx r fd in
    let spawning_return = S.return {ctx with local = st} None MyCFG.dummy_func in
    let nval, ndiff = S.sync { ctx with local = spawning_return } in
    List.iter (fun (x,y) -> sideg x y) ndiff;
    nval

  let tf_ret var edge prev_node ret fd getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    let d =
      if (fd.svar.vid = MyCFG.dummy_func.svar.vid ||
          List.mem fd.svar.vname (List.map Json.string (get_list "mainfun"))) &&
         (get_bool "kernel" || get_string "ana.osek.oil" <> "")
      then toplevel_kernel_return ret fd ctx sideg
      else normal_return ret fd ctx sideg
    in
    bigsqcup (d::!r)

  let tf_entry var edge prev_node fd getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.body ctx fd)::!r)

  let tf_test var edge prev_node e tv getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.branch ctx e tv)::!r)

  let tf_normal_call ctx lv e f args  getl sidel getg sideg =
    let combine (cd, fc, fd) = S.combine {ctx with local = cd} lv e f args fc fd in
    let paths = S.enter ctx lv f args in
    let paths = List.map (fun (c,v) -> (c, S.context v, v)) paths in
    let _     = if not full_context then List.iter (fun (c,fc,v) -> if not (S.D.is_bot v) then sidel (FunctionEntry f, fc) v) paths in
    let paths = List.map (fun (c,fc,v) -> (c, fc, if S.D.is_bot v then v else getl (Function f, fc))) paths in
    let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in
    let paths = List.map combine paths in
    List.fold_left D.join (D.bot ()) paths

  let tf_special_call ctx lv f args = S.special ctx lv f args

  let tf_proc var edge prev_node lv e args getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    let functions =
      match ctx.ask (Queries.EvalFunvar e) with
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
    if [] = functions then
      d (* because LevelSliceLifter *)
    else
      let funs = List.map one_function functions in
      bigsqcup (funs @ !r)

  let tf_asm var edge prev_node getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.asm ctx)::!r)

  let tf_skip var edge prev_node getl sidel getg sideg d =
    let ctx, r = common_ctx var edge prev_node d getl sidel getg sideg in
    bigsqcup ((S.skip ctx)::!r)

  let tf var getl sidel getg sideg prev_node edge d =
    begin match edge with
      | Assign (lv,rv) -> tf_assign var edge prev_node lv rv
      | VDecl (v)      -> tf_vdecl var edge prev_node v
      | Proc (r,f,ars) -> tf_proc var edge prev_node r f ars
      | Entry f        -> tf_entry var edge prev_node f
      | Ret (r,fd)     -> tf_ret var edge prev_node r fd
      | Test (p,b)     -> tf_test var edge prev_node p b
      | ASM (_, _, _)  -> tf_asm var edge prev_node (* TODO: use ASM fields for something? *)
      | Skip           -> tf_skip var edge prev_node
      | SelfLoop       -> tf_loop var edge prev_node
    end getl sidel getg sideg d

  let tf var getl sidel getg sideg prev_node (_,edge) d (f,t) =
    let old_loc  = !Tracing.current_loc in
    let old_loc2 = !Tracing.next_loc in
    let _       = Tracing.current_loc := f in
    let _       = Tracing.next_loc := t in
    let d       = tf var getl sidel getg sideg prev_node edge d in
    let _       = Tracing.current_loc := old_loc in
    let _       = Tracing.next_loc := old_loc2 in
    d

  let tf (v,c) (edges, u) getl sidel getg sideg =
    let pval = getl (u,c) in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (getLoc v,[]) in
    List.fold_left2 (|>) pval (List.map (tf (v,Obj.repr (fun () -> c)) getl sidel getg sideg u) edges) locs

  let tf (v,c) (e,u) getl sidel getg sideg =
    let old_node = !current_node in
    let _       = current_node := Some u in
    let d       = try tf (v,c) (e,u) getl sidel getg sideg
      with M.StopTheWorld -> D.bot ()
         | M.Bailure s -> Messages.warn_each s; (getl (u,c))  in
    let _       = current_node := old_node in
    d

  let system (v,c) =
    match v with
    | FunctionEntry _ when full_context ->
      [fun _ _ _ _ -> S.val_of c]
    | _ -> List.map (tf (v,c)) (Cfg.prev v)
end

(** Combined variables so that we can also use the more common [IneqConstrSys], and [EqConstrSys]
    that use only one kind of a variable. *)
module Var2 (LV:VarType) (GV:VarType)
  : VarType
    with type t = [ `L of LV.t  | `G of GV.t ]
=
struct
  type t = [ `L of LV.t  | `G of GV.t ]
  let relift = function
    | `L x -> `L (LV.relift x)
    | `G x -> `G (GV.relift x)

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

  let printXml f = function
    | `L a -> LV.printXml f a
    | `G a -> GV.printXml f a

  let var_id = function
    | `L a -> LV.var_id a
    | `G a -> GV.var_id a

  let line_nr = function
    | `L a -> LV.line_nr a
    | `G a -> GV.line_nr a

  let file_name = function
    | `L a -> LV.file_name a
    | `G a -> GV.file_name a

  let node = function
    | `L a -> LV.node a
    | `G a -> GV.node a
end

(** Translate a [GlobConstrSys] into a [IneqConstrSys] *)
module IneqConstrSysFromGlobConstrSys (S:GlobConstrSys)
  : IneqConstrSys with type v = Var2(S.LVar)(S.GVar).t
                   and type d = Lattice.Either(S.G)(S.D).t
                   and module Var = Var2(S.LVar)(S.GVar)
                   and module Dom = Lattice.Either(S.G)(S.D)
=
struct
  module Var = Var2(S.LVar)(S.GVar)
  module Dom =
  struct
    include Lattice.Either(S.G)(S.D)
    let printXml f = function
      | `Left  a -> S.G.printXml f a
      | `Right a -> S.D.printXml f a
  end
  let increment = S.increment
  type v = Var.t
  type d = Dom.t

  let box f x y = if Dom.leq y x then Dom.narrow x y else Dom.widen x (Dom.join x y)

  let getR = function
    | `Left x -> x
    | `Right _ -> S.G.bot ()
    | _ -> failwith "IneqConstrSysFromGlobConstrSys broken: Right!"

  let getL = function
    | `Right x -> x
    | `Left _ -> S.D.top ()
    | _ -> failwith "IneqConstrSysFromGlobConstrSys broken: Left!"

  let l, g = (fun x -> `L x), (fun x -> `G x)
  let le, ri = (fun x -> `Right x), (fun x -> `Left x)

  let conv f get set =
    f (getL % get % l) (fun x v -> set (l x) (le v))
      (getR % get % g) (fun x v -> set (g x) (ri v))
    |> le

  let system = function
    | `G _ -> []
    | `L x -> List.map conv (S.system x)
end


(** Transforms a [GenericEqBoxSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromEqSolver (Sol:GenericEqBoxSolver)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.LVar.t) ->
    functor (GH:Hash.H with type key=S.GVar.t) ->
    struct
      module IneqSys = IneqConstrSysFromGlobConstrSys (S)
      module EqSys = Generic.NormalSysConverter (IneqSys)

      module VH : Hash.H with type key=EqSys.v = Hashtbl.Make(EqSys.Var)
      module Sol' = Sol (EqSys) (VH)

      let getR v = function
        | `Left x -> x
        | `Right x ->
          ignore @@ Pretty.printf "GVar %a has local value %a\n" S.GVar.pretty_trace v S.D.pretty x;
          undefined ()

      let getL v = function
        | `Right x -> x
        | `Left x ->
          ignore @@ Pretty.printf "LVar %a has global value %a\n" S.LVar.pretty_trace v S.G.pretty x;
          undefined ()

      let solve ls gs l =
        let vs = List.map (fun (x,v) -> EqSys.conv (`L x), `Right v) ls
                 @ List.map (fun (x,v) -> EqSys.conv (`G x), `Left  v) gs in
        let sv = List.map (fun x -> EqSys.conv (`L x)) l in
        let hm = Sol'.solve EqSys.box vs sv in
        let l' = LH.create 113 in
        let g' = GH.create 113 in
        let split_vars = function
          | (`L x,_) -> fun y -> LH.replace l' x (getL x y)
          | (`G x,_) -> fun y -> GH.replace g' x (getR x y)
        in
        VH.iter split_vars hm;
        (l', g')
    end

(** Transforms a [GenericIneqBoxSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromIneqSolver (Sol:GenericIneqBoxSolver)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hash.H with type key=S.LVar.t) ->
    functor (GH:Hash.H with type key=S.GVar.t) ->
    struct
      module IneqSys = IneqConstrSysFromGlobConstrSys (S)

      module VH : Hash.H with type key=IneqSys.v = Hashtbl.Make(IneqSys.Var)
      module Sol' = Sol (IneqSys) (VH)

      let getG v = function
        | `Left x -> x
        | `Right x ->
          ignore @@ Pretty.printf "GVar %a has local value %a\n" S.GVar.pretty_trace v S.D.pretty x;
          (* undefined () *) (* TODO this only happens for test 17/02 arinc/unique_proc *)
          S.G.bot ()

      let getL v = function
        | `Right x -> x
        | `Left x ->
          ignore @@ Pretty.printf "LVar %a has global value %a\n" S.LVar.pretty_trace v S.G.pretty x;
          undefined ()

      let solve ls gs l =
        let vs = List.map (fun (x,v) -> `L x, `Right v) ls
                 @ List.map (fun (x,v) -> `G x, `Left v) gs in
        let sv = List.map (fun x -> `L x) l in
        let hm = Sol'.solve IneqSys.box vs sv in
        let l' = LH.create 113 in
        let g' = GH.create 113 in
        let split_vars = function
          | `L x -> fun y -> LH.replace l' x (getL x y)
          | `G x -> fun y -> GH.replace g' x (getG x y)
        in
        VH.iter split_vars hm;
        (l', g')
    end

module N = struct let topname = "Top" end

(** Add path sensitivity to a analysis *)
module PathSensitive2 (Spec:Spec)
  : Spec
    with type D.t = SetDomain.Hoare(Spec.D)(N).t
     and module G = Spec.G
     and module C = Spec.C
=
struct
  module D =
  struct
    include SetDomain.Hoare (Spec.D) (N) (* TODO is it really worth it to check every time instead of just using sets and joining later? *)
    let name () = "PathSensitive (" ^ name () ^ ")"

    let pretty_diff () ((s1:t),(s2:t)): Pretty.doc =
      if leq s1 s2 then dprintf "%s (%d and %d paths): These are fine!" (name ()) (cardinal s1) (cardinal s2) else begin
        try
          let p t = not (mem t s2) in
          let evil = choose (filter p s1) in
          let other = choose s2 in
          (* dprintf "%s has a problem with %a not leq %a because %a" (name ())
             Spec.D.pretty evil Spec.D.pretty other
             Spec.D.pretty_diff (evil,other) *)
          Spec.D.pretty_diff () (evil,other)
        with _ ->
          dprintf "choose failed b/c of empty set s1: %d s2: %d"
          (cardinal s1)
          (cardinal s2)
      end

    let printXml f x =
      let print_one x =
        BatPrintf.fprintf f "\n<path>%a</path>" Spec.D.printXml x
      in
      iter print_one x

    (* join elements in the same partition (specified by should_join) *)
    let join_reduce a =
      let rec loop js = function
        | [] -> js
        | x::xs -> let (j,r) = List.fold_left (fun (j,r) x ->
            if Spec.should_join x j then Spec.D.join x j, r else j, x::r
          ) (x,[]) xs in
          loop (j::js) r
      in
      apply_list (loop []) a

    let leq a b =
      leq a b || leq (join_reduce a) (join_reduce b)

    let binop op a b = op a b |> join_reduce

    let join = binop join
    let meet = binop meet
    let widen = binop widen
    let narrow = binop narrow

    let invariant c s = fold (fun x a ->
        Invariant.(a || Spec.D.invariant c x) (* TODO: || correct? *)
      ) s Invariant.none
  end

  module G = Spec.G
  module C = Spec.C

  let name () = "PathSensitive2("^Spec.name ()^")"

  let init = Spec.init
  let finalize = Spec.finalize

  let should_join x y = true

  let exitstate  v = D.singleton (Spec.exitstate  v)
  let startstate v = D.singleton (Spec.startstate v)
  let morphstate v d = D.map' (Spec.morphstate v) d

  let call_descr = Spec.call_descr

  let val_of = D.singleton % Spec.val_of
  let context l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive2.context must be called with a singleton set."
    else
      Spec.context @@ D.choose l

  let conv ctx x =
    let rec ctx' = { ctx with ask   = query
                            ; local = x
                            ; split = (ctx.split % D.singleton) }
    and query x = Spec.query ctx' x in
    ctx'

  let map ctx f g =
    let h x xs =
      try D.add (g (f (conv ctx x))) xs
      with Deadcode -> xs
    in
    let d = D.fold h ctx.local (D.empty ()) in
    if D.is_bot d then raise Deadcode else d

  let assign ctx l e    = map ctx Spec.assign  (fun h -> h l e )
  let vdecl ctx v       = map ctx Spec.vdecl   (fun h -> h v)
  let body   ctx f      = map ctx Spec.body    (fun h -> h f   )
  let return ctx e f    = map ctx Spec.return  (fun h -> h e f )
  let branch ctx e tv   = map ctx Spec.branch  (fun h -> h e tv)
  let intrpt ctx        = map ctx Spec.intrpt  identity
  let asm ctx           = map ctx Spec.asm     identity
  let skip ctx          = map ctx Spec.skip    identity
  let special ctx l f a = map ctx Spec.special (fun h -> h l f a)

  let threadenter ctx lval f args = map ctx Spec.threadenter (fun h -> h lval f args)
  let threadspawn ctx lval f args fctx =
    let fd1 = D.choose fctx.local in
    map ctx Spec.threadspawn (fun h -> h lval f args (conv fctx fd1))

  let fold ctx f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    let d = D.fold k ctx.local a in
    if D.is_bot d then raise Deadcode else d

  let fold' ctx f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    D.fold k ctx.local a

  let sync ctx =
    fold' ctx Spec.sync identity (fun (a,b) (a',b') -> D.add a' a, b'@b) (D.empty (), [])

  let query ctx q =
    (* join results so that they are sound for all paths *)
    fold' ctx Spec.query identity (fun x f -> Queries.Result.join x (f q)) `Bot

  let enter ctx l f a =
    let g xs ys = (List.map (fun (x,y) -> D.singleton x, D.singleton y) ys) @ xs in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let combine ctx l fe f a fc d =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      try D.add (Spec.combine (conv ctx cd) l fe f a fc x) y
      with Deadcode -> y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d
end

module Compare
    (S:Spec)
    (Sys:GlobConstrSys with module LVar = VarF (S.C)
                        and module GVar = Basetype.Variables
                        and module D = S.D
                        and module G = S.G)
    (LH:Hash.H with type key=Sys.LVar.t)
    (GH:Hash.H with type key=Sys.GVar.t)
=
struct
  open S

  module PP = Hashtbl.Make (MyCFG.Node)

  let compare_globals g1 g2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      let v2 = try GH.find g2 k with Not_found -> G.bot () in
      let b1 = G.leq v1 v2 in
      let b2 = G.leq v2 v1 in
      if b1 && b2 then
        f_eq ()
      else if b1 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "Global %a is more precise using left:\n%a\n" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2));
        f_le ()
      end else if b2 then begin
        if get_bool "solverdiffs" then
          ignore (Pretty.printf "Global %a is more precise using right:\n%a\n" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2));
        f_gr ()
      end else
        f_uk ()
    in
    GH.iter f g1;
    Printf.printf "globals:\tequal = %d\tleft = %d\tright = %d\tincomparable = %d\n" !eq !le !gr !uk

  let compare_locals h1 h2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f k v1 =
      if not (PP.mem h2 k) then () else
        let v2 = PP.find h2 k in
        let b1 = D.leq v1 v2 in
        let b2 = D.leq v2 v1 in
        if b1 && b2 then
          incr eq
        else if b1 then begin
          if get_bool "solverdiffs" then
            ignore (Pretty.printf "%a @@ %a is more precise using left:\n%a\n" pretty_node k d_loc (getLoc k) D.pretty_diff (v1,v2));
          incr le
        end else if b2 then begin
          if get_bool "solverdiffs" then
            ignore (Pretty.printf "%a @@ %a is more precise using right:\n%a\n" pretty_node k d_loc (getLoc k) D.pretty_diff (v1,v2));
          incr gr
        end else
          incr uk
    in
    PP.iter f h1;
    (* let k1 = Set.of_enum @@ PP.keys h1 in
    let k2 = Set.of_enum @@ PP.keys h2 in
    let o1 = Set.cardinal @@ Set.diff k1 k2 in
    let o2 = Set.cardinal @@ Set.diff k2 k1 in
    Printf.printf "locals: \tequal = %d\tleft = %d[%d]\tright = %d[%d]\tincomparable = %d\n" !eq !le o1 !gr o2 !uk *)
    Printf.printf "locals: \tequal = %d\tleft = %d\tright = %d\tincomparable = %d\n" !eq !le !gr !uk

  let compare_locals_ctx h1 h2 =
    let eq, le, gr, uk, no2 = ref 0, ref 0, ref 0, ref 0, ref 0 in
    let f_eq () = incr eq in
    let f_le () = incr le in
    let f_gr () = incr gr in
    let f_uk () = incr uk in
    let f k v1 =
      if not (LH.mem h2 k) then incr no2 else
        let v2 = LH.find h2 k in
        let b1 = D.leq v1 v2 in
        let b2 = D.leq v2 v1 in
        if b1 && b2 then
          f_eq ()
        else if b1 then begin
          (* if get_bool "solverdiffs" then *)
          (*   ignore (Pretty.printf "%a @@ %a is more precise using left:\n%a\n" pretty_node k d_loc (getLoc k) D.pretty_diff (v1,v2)); *)
          f_le ()
        end else if b2 then begin
          (* if get_bool "solverdiffs" then *)
          (*   ignore (Pretty.printf "%a @@ %a is more precise using right:\n%a\n" pretty_node k d_loc (getLoc k) D.pretty_diff (v1,v2)); *)
          f_gr ()
        end else
          f_uk ()
    in
    LH.iter f h1;
    (* let k1 = Set.of_enum @@ PP.keys h1 in *)
    (* let k2 = Set.of_enum @@ PP.keys h2 in *)
    (* let o1 = Set.cardinal @@ Set.diff k1 k2 in *)
    (* let o2 = Set.cardinal @@ Set.diff k2 k1 in *)
    Printf.printf "locals_ctx:\tequal = %d\tleft = %d\tright = %d\tincomparable = %d\tno_ctx_in_right = %d\n" !eq !le !gr !uk !no2

  let compare (name1,name2) (l1,g1) (l2,g2) =
    let one_ctx (n,_) v h =
      PP.replace h n (try D.join v (PP.find h n) with Not_found -> v);
      h
    in
    (* these contain results where the contexts per node have been joined *)
    let h1 = PP.create 113 in
    let h2 = PP.create 113 in
    let _  = LH.fold one_ctx l1 h1 in
    let _  = LH.fold one_ctx l2 h2 in
    Printf.printf "\nComparing precision of %s (left) with %s (right):\n" name1 name2;
    compare_globals g1 g2;
    compare_locals h1 h2;
    compare_locals_ctx l1 l2;
    print_newline ();
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
    Goblintutil.verified := Some true;
    let complain_l (v:LVar.t) lhs rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a (%s:%d)\n @[Solver computed:\n%a\nRight-Hand-Side:\n%a\nDifference: %a\n@]"
                LVar.pretty_trace v (LVar.file_name v) (LVar.line_nr v) D.pretty lhs D.pretty rhs D.pretty_diff (rhs,lhs))
    in
    let complain_sidel v1 (v2:LVar.t) lhs rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a (%s:%d)\nOrigin: %a (%s:%d)\n @[Solver computed:\n%a\nSide-effect:\n%a\nDifference: %a\n@]"
      LVar.pretty_trace v2 (LVar.file_name v2) (LVar.line_nr v2)
      LVar.pretty_trace v1 (LVar.file_name v1) (LVar.line_nr v1)
      D.pretty lhs D.pretty rhs D.pretty_diff (rhs,lhs))
    in
    let complain_sideg v (g:GVar.t) lhs rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached. Unsatisfied constraint for global %a at variable %a (%s:%d)\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\n@]"
                GVar.pretty_trace g LVar.pretty_trace v (LVar.file_name v) (LVar.line_nr v) G.pretty lhs G.pretty rhs)
    in
    (* For each variable v which has been assigned value d', would like to check
     * that d' satisfied all constraints. *)
    let verify_var v d' =
      let verify_constraint rhs =
        let sigma' x = try LH.find sigma x with Not_found -> D.bot () in
        let theta' x = try GH.find theta x with Not_found -> G.bot () in
        (* First check that each (global) delta is included in the (global)
         * invariant. *)
        let check_local l lv =
          let lv' = sigma' l in
          if not (D.leq lv lv') then
            complain_sidel v l lv' lv
        in
        let check_glob g gv =
          let gv' = theta' g in
          if not (G.leq gv gv') then
            complain_sideg v g gv' gv
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

module Reachability
    (EQSys:GlobConstrSys)
    (LH:Hashtbl.S with type key=EQSys.LVar.t)
    (GH:Hashtbl.S with type key=EQSys.GVar.t)
=
struct
  open EQSys

  let prune (lh:D.t LH.t) (gh:G.t GH.t) (lvs:LVar.t list): unit =
    let reachablel = LH.create (LH.length lh) in

    let rec one_lvar x =
      if not (LH.mem reachablel x) then begin
        LH.replace reachablel x ();
        List.iter one_constraint (system x)
      end
    and one_constraint rhs =
      let getl y =
        one_lvar y;
        try LH.find lh y with Not_found -> D.bot ()
      in
      let getg y = try GH.find gh y with Not_found -> G.bot () in
      let setl y yd = one_lvar y in
      let setg y yd = () in
      ignore (rhs getl setl getg setg)
    in

    List.iter one_lvar lvs;
    LH.filteri_inplace (fun x _ ->
        let r = LH.mem reachablel x in
        if not r then
          ignore (Pretty.printf "Unreachable lvar %a\n" LVar.pretty_trace x);
        r
      ) lh
end
