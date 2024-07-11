(** Construction of a {{!Analyses.MonSystem} constraint system} from an {{!Analyses.Spec} analysis specification} and {{!MyCFG.CfgBackward} CFGs}.
    Transformatons of analysis specifications as functors. *)

open Batteries
open GoblintCil
open MyCFG
open Analyses
open ConstrSys
open GobConfig

module M = Messages


(** Lifts a [Spec] so that the domain is [Hashcons]d *)
module HashconsLifter (S:Spec)
  : Spec with module G = S.G
          and module C = S.C
=
struct
  module HConsedArg =
  struct
    (* We do refine int values on narrow and meet {!IntDomain.IntDomTupleImpl}, which can lead to fixpoint issues if we assume x op x = x *)
    (* see https://github.com/goblint/analyzer/issues/1005 *)
    let assume_idempotent = GobConfig.get_string "ana.int.refinement" = "never"
  end
  module D = Lattice.HConsed (S.D) (HConsedArg)
  module G = S.G
  module C = S.C
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt x = of_elt (D.unlift x)
  end

  let name () = S.name () ^" hashconsed"

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init = S.init
  let finalize = S.finalize

  let startstate v = D.lift (S.startstate v)
  let exitstate  v = D.lift (S.exitstate  v)
  let morphstate v d = D.lift (S.morphstate v (D.unlift d))

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d es -> ctx.split (D.lift d) es )
    }

  let context ctx fd = S.context (conv ctx) fd % D.unlift
  let startcontext () = S.startcontext ()

  let sync ctx reason =
    D.lift @@ S.sync (conv ctx) reason

  let query ctx =
    S.query (conv ctx)

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

  let asm ctx =
    D.lift @@ S.asm (conv ctx)

  let skip ctx =
    D.lift @@ S.skip (conv ctx)

  let enter ctx r f args =
    List.map (fun (x,y) -> D.lift x, D.lift y) @@ S.enter (conv ctx) r f args

  let special ctx r f args =
    D.lift @@ S.special (conv ctx) r f args

  let combine_env ctx r fe f args fc es f_ask =
    D.lift @@ S.combine_env (conv ctx) r fe f args fc (D.unlift es) f_ask

  let combine_assign ctx r fe f args fc es f_ask =
    D.lift @@ S.combine_assign (conv ctx) r fe f args fc (D.unlift es) f_ask

  let threadenter ctx ~multiple lval f args =
    List.map D.lift @@ S.threadenter (conv ctx) ~multiple lval f args

  let threadspawn ctx ~multiple lval f args fctx =
    D.lift @@ S.threadspawn (conv ctx) ~multiple lval f args (conv fctx)

  let paths_as_set ctx =
    List.map (fun x -> D.lift x) @@ S.paths_as_set (conv ctx)

  let event ctx e octx =
    D.lift @@ S.event (conv ctx) e (conv octx)
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
  module V = S.V
  module P = S.P

  let name () = S.name () ^" context hashconsed"

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init = S.init
  let finalize = S.finalize

  let startstate = S.startstate
  let exitstate  = S.exitstate
  let morphstate = S.morphstate

  let conv ctx =
    { ctx with context = (fun () -> C.unlift (ctx.context ())) }

  let context ctx fd = C.lift % S.context (conv ctx) fd
  let startcontext () = C.lift @@ S.startcontext ()

  let sync ctx reason =
    S.sync (conv ctx) reason

  let query ctx (type a) (q: a Queries.t): a Queries.result =
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

  let asm ctx =
    S.asm (conv ctx)

  let skip ctx =
    S.skip (conv ctx)

  let enter ctx r f args =
    S.enter (conv ctx) r f args

  let special ctx r f args =
    S.special (conv ctx) r f args

  let combine_env ctx r fe f args fc es f_ask =
    S.combine_env (conv ctx) r fe f args (Option.map C.unlift fc) es f_ask

  let combine_assign ctx r fe f args fc es f_ask =
    S.combine_assign (conv ctx) r fe f args (Option.map C.unlift fc) es f_ask

  let threadenter ctx ~multiple lval f args =
    S.threadenter (conv ctx) ~multiple lval f args

  let threadspawn ctx ~multiple lval f args fctx =
    S.threadspawn (conv ctx) ~multiple lval f args (conv fctx)

  let paths_as_set ctx = S.paths_as_set (conv ctx)
  let event ctx e octx = S.event (conv ctx) e (conv octx)
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
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  let name () = S.name ()^" level sliced"

  let start_level = ref (`Top)

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init marshal =
    if get_bool "dbg.slice.on" then
      start_level := `Lifted (Int64.of_int (get_int "dbg.slice.n"));
    S.init marshal

  let finalize = S.finalize

  let startstate v = (S.startstate v, !start_level)
  let exitstate  v = (S.exitstate  v, !start_level)
  let morphstate v (d,l) = (S.morphstate v d, l)

  let conv ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es )
    }

  let context ctx fd (d,_) = S.context (conv ctx) fd d
  let startcontext () = S.startcontext ()

  let lift_fun ctx f g h =
    f @@ h (g (conv ctx))

  let enter' ctx r f args =
    let liftmap = List.map (fun (x,y) -> (x, snd ctx.local), (y, snd ctx.local)) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r)

  let lift ctx d = (d, snd ctx.local)
  let lift_start_level d = (d, !start_level)

  let sync ctx reason = lift_fun ctx (lift ctx) S.sync   ((|>) reason)
  let query' ctx (type a) (q: a Queries.t): a Queries.result =
    lift_fun ctx identity   S.query  (fun x -> x q)
  let assign ctx lv e = lift_fun ctx (lift ctx) S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx (lift ctx) S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx (lift ctx) S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx (lift ctx) S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx (lift ctx) S.return ((|>) f % (|>) r)
  let asm ctx         = lift_fun ctx (lift ctx) S.asm    identity
  let skip ctx        = lift_fun ctx (lift ctx) S.skip   identity
  let special ctx r f args        = lift_fun ctx (lift ctx) S.special ((|>) args % (|>) f % (|>) r)
  let combine_env' ctx r fe f args fc es f_ask = lift_fun ctx (lift ctx) S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign' ctx r fe f args fc es f_ask = lift_fun ctx (lift ctx) S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)

  let threadenter ctx ~multiple lval f args = lift_fun ctx (List.map lift_start_level) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval)
  let threadspawn ctx ~multiple lval f args fctx = lift_fun ctx (lift ctx) (S.threadspawn ~multiple) ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)

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

  let paths_as_set ctx =
    let liftmap = List.map (fun x -> (x, snd ctx.local)) in
    lift_fun ctx liftmap S.paths_as_set (Fun.id)

  let event ctx e octx =
    lift_fun ctx (lift ctx) S.event ((|>) (conv octx) % (|>) e)

  let enter ctx r f args =
    let (d,l) = ctx.local in
    if leq0 l then
      [ctx.local, D.bot ()]
    else
      enter' {ctx with local=(d, sub1 l)} r f args

  let combine_env ctx r fe f args fc es f_ask =
    let (d,l) = ctx.local in
    let l = add1 l in
    if leq0 l then
      (d, l)
    else
      let d',_ = combine_env' ctx r fe f args fc es f_ask in
      (d', l)

  let combine_assign ctx r fe f args fc es f_ask =
    let (d,l) = ctx.local in
    (* No need to add1 here, already done in combine_env. *)
    if leq0 l then
      (d, l)
    else
      let d',_ = combine_assign' ctx r fe f args fc es f_ask in
      (d', l)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.EvalFunvar e ->
      let (d,l) = ctx.local in
      if leq0 l then
        Queries.AD.empty ()
      else
        query' ctx (Queries.EvalFunvar e)
    | q -> query' ctx q
end


(** Limits the number of widenings per node. *)
module LimitLifter (S:Spec) =
struct
  include (S : module type of S with module D := S.D and type marshal = S.marshal)

  let name () = S.name ()^" limited"

  let limit = ref 0

  let init marshal =
    limit := get_int "dbg.limit.widen";
    S.init marshal

  module H = MyCFG.NodeH
  let h = H.create 13
  let incr k =
    H.modify_def 1 k (fun v ->
        if v >= !limit then failwith (GobPretty.sprintf "LimitLifter: Reached limit (%d) for node %a" !limit Node.pretty_plain_short (Option.get !MyCFG.current_node));
        v+1
      ) h;
  module D = struct
    include S.D
    let widen x y = Option.may incr !MyCFG.current_node; widen x y (* when is this None? *)
  end
end


(* widening on contexts, keeps contexts for calls only in D *)
module WidenContextLifterSide (S:Spec)
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
  module C = S.C
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end


  let name () = S.name ()^" with widened contexts"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let inj f x = f x, M.bot ()

  let startcontext () = S.startcontext ()
  let startstate = inj S.startstate
  let exitstate  = inj S.exitstate
  let morphstate v (d,m) = S.morphstate v d, m


  let conv ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es )
    }

  let context ctx fd (d,m) = S.context (conv ctx) fd d (* just the child analysis' context *)

  let lift_fun ctx f g = g (f (conv ctx)), snd ctx.local

  let sync ctx reason = lift_fun ctx S.sync   ((|>) reason)
  let query ctx       = S.query (conv ctx)
  let assign ctx lv e = lift_fun ctx S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx S.return ((|>) f % (|>) r)
  let asm ctx         = lift_fun ctx S.asm    identity
  let skip ctx        = lift_fun ctx S.skip   identity
  let special ctx r f args       = lift_fun ctx S.special ((|>) args % (|>) f % (|>) r)

  let event ctx e octx = lift_fun ctx S.event ((|>) (conv octx) % (|>) e)

  let threadenter ctx ~multiple lval f args = S.threadenter (conv ctx) ~multiple lval f args |> List.map (fun d -> (d, snd ctx.local))
  let threadspawn ctx ~multiple lval f args fctx = lift_fun ctx (S.threadspawn ~multiple) ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)

  let enter ctx r f args =
    let m = snd ctx.local in
    let d' v_cur =
      if ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.context.widen" ~keepAttr:"widen" ~removeAttr:"no-widen" f then (
        let v_old = M.find f.svar m in (* S.D.bot () if not found *)
        let v_new = S.D.widen v_old (S.D.join v_old v_cur) in
        Messages.(if tracing && not (S.D.equal v_old v_new) then tracel "widen-context" "enter results in new context for function %s" f.svar.vname);
        v_new, M.add f.svar v_new m
      )
      else
        v_cur, m
    in
    S.enter (conv ctx) r f args
    |> List.map (fun (c,v) -> (c,m), d' v) (* c: caller, v: callee *)

  let paths_as_set ctx =
    let m = snd ctx.local in
    S.paths_as_set (conv ctx) |> List.map (fun v -> (v,m))

  let combine_env ctx r fe f args fc es f_ask = lift_fun ctx S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign ctx r fe f args fc es f_ask = lift_fun ctx S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)
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
  module V = S.V
  module P =
  struct
    include Printable.Option (S.P) (struct let name = "None" end)

    let of_elt = function
      | `Lifted x -> Some (S.P.of_elt x)
      | _ -> None
  end

  let name () = S.name ()^" lifted"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize


  let startcontext () = S.startcontext ()
  let startstate v = `Lifted (S.startstate v)
  let exitstate  v = `Lifted (S.exitstate  v)
  let morphstate v d = try `Lifted (S.morphstate v (D.unlift d)) with Deadcode -> d


  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d es -> ctx.split (D.lift d) es )
    }

  let context ctx fd = S.context (conv ctx) fd % D.unlift

  let lift_fun ctx f g h b =
    try f @@ h (g (conv ctx))
    with Deadcode -> b

  let sync ctx reason = lift_fun ctx D.lift   S.sync   ((|>) reason)      `Bot

  let enter ctx r f args =
    let liftmap = List.map (fun (x,y) -> D.lift x, D.lift y) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r) []

  let paths_as_set ctx =
    let liftmap = List.map (fun x -> D.lift x) in
    lift_fun ctx liftmap S.paths_as_set (Fun.id) [D.bot ()] (* One dead path instead of none, such that combine_env gets called for functions with dead normal return (and thus longjmpy returns can be correctly handled by lifter). *)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    lift_fun ctx identity S.query (fun (x) -> x q) (Queries.Result.bot q)
  let assign ctx lv e = lift_fun ctx D.lift   S.assign ((|>) e % (|>) lv) `Bot
  let vdecl ctx v     = lift_fun ctx D.lift   S.vdecl  ((|>) v)            `Bot
  let branch ctx e tv = lift_fun ctx D.lift   S.branch ((|>) tv % (|>) e) `Bot
  let body ctx f      = lift_fun ctx D.lift   S.body   ((|>) f)            `Bot
  let return ctx r f  = lift_fun ctx D.lift   S.return ((|>) f % (|>) r)  `Bot
  let asm ctx         = lift_fun ctx D.lift   S.asm    identity           `Bot
  let skip ctx        = lift_fun ctx D.lift   S.skip   identity           `Bot
  let special ctx r f args       = lift_fun ctx D.lift S.special ((|>) args % (|>) f % (|>) r)        `Bot
  let combine_env ctx r fe f args fc es f_ask = lift_fun ctx D.lift S.combine_env (fun p -> p r fe f args fc (D.unlift es) f_ask) `Bot
  let combine_assign ctx r fe f args fc es f_ask = lift_fun ctx D.lift S.combine_assign (fun p -> p r fe f args fc (D.unlift es) f_ask) `Bot

  let threadenter ctx ~multiple lval f args = lift_fun ctx (List.map D.lift) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval) []
  let threadspawn ctx ~multiple lval f args fctx = lift_fun ctx D.lift (S.threadspawn ~multiple) ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval) `Bot

  let event (ctx:(D.t,G.t,C.t,V.t) ctx) (e:Events.t) (octx:(D.t,G.t,C.t,V.t) ctx):D.t = lift_fun ctx D.lift S.event ((|>) (conv octx) % (|>) e) `Bot
end

module NoContext = struct let name = "no context" end
module IntConf =
struct
  let n () = max_int
  let names x = Format.asprintf "%d" x
end

(** Lifts a [Spec] with the context gas variable. The gas variable limits the number of context-sensitively analyzed function calls in a call stack.
    For every function call the gas is reduced. If the gas is zero, the remaining function calls are analyzed without context-information *)
module ContextGasLifter (S:Spec)
  : Spec with module D = Lattice.Prod (S.D) (Lattice.Chain (IntConf))
          and module C = Printable.Option (S.C) (NoContext)
          and module G = S.G
=
struct
  include S

  module Context_Gas_Prod (Base1: Lattice.S) (Base2: Lattice.S) =
  struct
    include Lattice.Prod (Base1) (Base2)
    let printXml f (x,y) =
      BatPrintf.fprintf f "\n%a<analysis name=\"context gas value\">\n%a\n</analysis>" Base1.printXml x Base2.printXml y
  end
  module D = Context_Gas_Prod (S.D) (Lattice.Chain (IntConf)) (* Product of S.D and an integer, tracking the context gas value *)
  module C = Printable.Option (S.C) (NoContext)
  module G = S.G
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  (* returns context gas value of the given ctx *)
  let cg_val ctx = snd ctx.local

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize


  let startcontext () = Some (S.startcontext ())
  let name () = S.name ()^" with context gas"
  let startstate v = S.startstate v, get_int "ana.context.gas_value"
  let exitstate v = S.exitstate v, get_int "ana.context.gas_value"
  let morphstate v (d,i) = S.morphstate v d, i

  let conv (ctx:(D.t,G.t,C.t,V.t) ctx): (S.D.t,G.t,S.C.t,V.t)ctx =
    if (cg_val ctx <= 0)
    then {ctx with local = fst ctx.local
                 ; split = (fun d es -> ctx.split (d, cg_val ctx) es)
                 ; context = (fun () -> ctx_failwith "no context (contextGas = 0)")}
    else {ctx with local = fst ctx.local
                 ; split = (fun d es -> ctx.split (d, cg_val ctx) es)
                 ; context = (fun () -> Option.get (ctx.context ()))}

  let context ctx fd (d,i) =
    (* only keep context if the context gas is greater zero *)
    if i <= 0 then None else Some (S.context (conv ctx) fd d)

  let enter ctx r f args =
    (* callee gas = caller gas - 1 *)
    let liftmap_tup = List.map (fun (x,y) -> (x, cg_val ctx), (y, max 0 (cg_val ctx - 1))) in
    liftmap_tup (S.enter (conv ctx) r f args)

  let threadenter ctx ~multiple lval f args =
    let liftmap f = List.map (fun (x) -> (x, max 0 (cg_val ctx - 1))) f in
    liftmap (S.threadenter (conv ctx) ~multiple lval f args)

  let sync ctx reason                             = S.sync (conv ctx) reason, cg_val ctx
  let query ctx q                                 = S.query (conv ctx) q
  let assign ctx lval expr                        = S.assign (conv ctx) lval expr, cg_val ctx
  let vdecl ctx v                                 = S.vdecl (conv ctx) v, cg_val ctx
  let body ctx fundec                             = S.body (conv ctx) fundec, cg_val ctx
  let branch ctx e tv                             = S.branch (conv ctx) e tv, cg_val ctx
  let return ctx r f                              = S.return (conv ctx) r f, cg_val ctx
  let asm ctx                                     = S.asm (conv ctx), cg_val ctx
  let skip ctx                                    = S.skip (conv ctx), cg_val ctx
  let special ctx r f args                        = S.special (conv ctx) r f args, cg_val ctx
  let combine_env ctx r fe f args fc es f_ask     = S.combine_env (conv ctx) r fe f args (Option.bind fc Fun.id) (fst es) f_ask, cg_val ctx
  let combine_assign ctx r fe f args fc es f_ask  = S.combine_assign (conv ctx) r fe f args (Option.bind fc Fun.id) (fst es) f_ask, cg_val ctx
  let paths_as_set ctx                            = List.map (fun (x) -> (x, cg_val ctx)) @@ S.paths_as_set (conv ctx)
  let threadspawn ctx ~multiple lval f args fctx  = S.threadspawn (conv ctx) ~multiple lval f args (conv fctx), cg_val ctx
  let event ctx e octx                            = S.event (conv ctx) e (conv octx), cg_val ctx
end


module type Increment =
sig
  val increment: increment_data option
end


(** The main point of this file---generating a [GlobConstrSys] from a [Spec]. *)
module FromSpec (S:Spec) (Cfg:CfgBackward) (I: Increment)
  : sig
    include GlobConstrSys with module LVar = VarF (S.C)
                           and module GVar = GVarF (S.V)
                           and module D = S.D
                           and module G = GVarG (S.G) (S.C)
  end
=
struct
  type lv = MyCFG.node * S.C.t
  (* type gv = varinfo *)
  type ld = S.D.t
  (* type gd = S.G.t *)
  module LVar = VarF (S.C)
  module GVar = GVarF (S.V)
  module D = S.D
  module G = GVarG (S.G) (S.C)

  (* Two global invariants:
     1. S.V -> S.G  --  used for Spec
     2. fundec -> set of S.C  --  used for IterSysVars Node *)

  let sync ctx =
    match ctx.prev_node, Cfg.prev ctx.prev_node with
    | _, _ :: _ :: _ (* Join in CFG. *)
    | FunctionEntry _, _ -> (* Function entry, also needs sync because partial contexts joined by solver, see 00-sanity/35-join-contexts. *)
      S.sync ctx `Join
    | _, _ -> S.sync ctx `Normal

  let side_context sideg f c =
    if !AnalysisState.postsolving then
      sideg (GVar.contexts f) (G.create_contexts (G.CSet.singleton c))

  let common_ctx var edge prev_node pval (getl:lv -> ld) sidel getg sideg : (D.t, S.G.t, S.C.t, S.V.t) ctx * D.t list ref * (lval option * varinfo * exp list * D.t * bool) list ref =
    let r = ref [] in
    let spawns = ref [] in
    (* now watch this ... *)
    let rec ctx =
      { ask     = (fun (type a) (q: a Queries.t) -> S.query ctx q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = fst var
      ; prev_node = prev_node
      ; control_context = snd var |> Obj.obj
      ; context = snd var |> Obj.obj
      ; edge    = edge
      ; local   = pval
      ; global  = (fun g -> G.spec (getg (GVar.spec g)))
      ; spawn   = spawn
      ; split   = (fun (d:D.t) es -> assert (List.is_empty es); r := d::!r)
      ; sideg   = (fun g d -> sideg (GVar.spec g) (G.create_spec d))
      }
    and spawn ?(multiple=false) lval f args =
      (* TODO: adjust ctx node/edge? *)
      (* TODO: don't repeat for all paths that spawn same *)
      let ds = S.threadenter ~multiple ctx lval f args in
      List.iter (fun d ->
          spawns := (lval, f, args, d, multiple) :: !spawns;
          match Cilfacade.find_varinfo_fundec f with
          | fd ->
            let c = S.context ctx fd d in
            sidel (FunctionEntry fd, c) d;
            ignore (getl (Function fd, c))
          | exception Not_found ->
            (* unknown function *)
            M.error ~category:Imprecise ~tags:[Category Unsound] "Created a thread from unknown function %s" f.vname
            (* actual implementation (e.g. invalidation) is done by threadenter *)
        ) ds
    in
    (* ... nice, right! *)
    let pval = sync ctx in
    { ctx with local = pval }, r, spawns

  let rec bigsqcup = function
    | []    -> D.bot ()
    | [x]   -> x
    | x::xs -> D.join x (bigsqcup xs)

  let thread_spawns ctx d spawns =
    if List.is_empty spawns then
      d
    else
      let rec ctx' =
        { ctx with
          ask = (fun (type a) (q: a Queries.t) -> S.query ctx' q)
        ; local = d
        }
      in
      (* TODO: don't forget path dependencies *)
      let one_spawn (lval, f, args, fd, multiple) =
        let rec fctx =
          { ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query fctx q)
          ; local = fd
          }
        in
        S.threadspawn ctx' ~multiple lval f args fctx
      in
      bigsqcup (List.map one_spawn spawns)

  let common_join ctx d splits spawns =
    thread_spawns ctx (bigsqcup (d :: splits)) spawns

  let common_joins ctx ds splits spawns = common_join ctx (bigsqcup ds) splits spawns

  let tf_assign var edge prev_node lv e getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.assign ctx lv e) !r !spawns

  let tf_vdecl var edge prev_node v getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.vdecl ctx v) !r !spawns

  let normal_return r fd ctx sideg =
    let spawning_return = S.return ctx r fd in
    let nval = S.sync { ctx with local = spawning_return } `Return in
    nval

  let toplevel_kernel_return r fd ctx sideg =
    let st = if fd.svar.vname = MyCFG.dummy_func.svar.vname then ctx.local else S.return ctx r fd in
    let spawning_return = S.return {ctx with local = st} None MyCFG.dummy_func in
    let nval = S.sync { ctx with local = spawning_return } `Return in
    nval

  let tf_ret var edge prev_node ret fd getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let d =
      if (CilType.Fundec.equal fd MyCFG.dummy_func ||
          List.mem fd.svar.vname (get_string_list "mainfun")) &&
         get_bool "kernel"
      then toplevel_kernel_return ret fd ctx sideg
      else normal_return ret fd ctx sideg
    in
    common_join ctx d !r !spawns

  let tf_entry var edge prev_node fd getl sidel getg sideg d =
    (* Side effect function context here instead of at sidel to FunctionEntry,
       because otherwise context for main functions (entrystates) will be missing or pruned during postsolving. *)
    let c: unit -> S.C.t = snd var |> Obj.obj in
    side_context sideg fd (c ());
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.body ctx fd) !r !spawns

  let tf_test var edge prev_node e tv getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.branch ctx e tv) !r !spawns

  let tf_normal_call ctx lv e (f:fundec) args getl sidel getg sideg =
    let combine (cd, fc, fd) =
      if M.tracing then M.traceli "combine" "local: %a" S.D.pretty cd;
      if M.tracing then M.trace "combine" "function: %a" S.D.pretty fd;
      let rec cd_ctx =
        { ctx with
          ask = (fun (type a) (q: a Queries.t) -> S.query cd_ctx q);
          local = cd;
        }
      in
      let fd_ctx =
        (* Inner scope to prevent unsynced fd_ctx from being used. *)
        (* Extra sync in case function has multiple returns.
           Each `Return sync is done before joining, so joined value may be unsound.
           Since sync is normally done before tf (in common_ctx), simulate it here for fd. *)
        (* TODO: don't do this extra sync here *)
        let rec sync_ctx =
          { ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query sync_ctx q);
            local = fd;
            prev_node = Function f;
          }
        in
        (* TODO: more accurate ctx? *)
        let synced = sync sync_ctx in
        let rec fd_ctx =
          { sync_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query fd_ctx q);
            local = synced;
          }
        in
        fd_ctx
      in
      let r = List.fold_left (fun acc fd1 ->
          let rec fd1_ctx =
            { fd_ctx with
              ask = (fun (type a) (q: a Queries.t) -> S.query fd1_ctx q);
              local = fd1;
            }
          in
          let combine_enved = S.combine_env cd_ctx lv e f args fc fd1_ctx.local (Analyses.ask_of_ctx fd1_ctx) in
          let rec combine_assign_ctx =
            { cd_ctx with
              ask = (fun (type a) (q: a Queries.t) -> S.query combine_assign_ctx q);
              local = combine_enved;
            }
          in
          S.D.join acc (S.combine_assign combine_assign_ctx lv e f args fc fd1_ctx.local (Analyses.ask_of_ctx fd1_ctx))
        ) (S.D.bot ()) (S.paths_as_set fd_ctx)
      in
      if M.tracing then M.traceu "combine" "combined local: %a" S.D.pretty r;
      r
    in
    let paths = S.enter ctx lv f args in
    let paths = List.map (fun (c,v) -> (c, S.context ctx f v, v)) paths in
    List.iter (fun (c,fc,v) -> if not (S.D.is_bot v) then sidel (FunctionEntry f, fc) v) paths;
    let paths = List.map (fun (c,fc,v) -> (c, fc, if S.D.is_bot v then v else getl (Function f, fc))) paths in
    (* Don't filter bot paths, otherwise LongjmpLifter is not called. *)
    (* let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in *)
    let paths = List.map (Tuple3.map2 Option.some) paths in
    if M.tracing then M.traceli "combine" "combining";
    let paths = List.map combine paths in
    let r = List.fold_left D.join (D.bot ()) paths in
    if M.tracing then M.traceu "combine" "combined: %a" S.D.pretty r;
    r

  let tf_special_call ctx lv f args = S.special ctx lv f args

  let tf_proc var edge prev_node lv e args getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    let functions =
      match e with
      | Lval (Var v, NoOffset) ->
        (* Handle statically known function call directly.
           Allows deactivating base. *)
        [v]
      | _ ->
        (* Depends on base for query. *)
        let ad = ctx.ask (Queries.EvalFunvar e) in
        Queries.AD.to_var_may ad (* TODO: don't convert, handle UnknownPtr below *)
    in
    let one_function f =
      match f.vtype with
      | TFun (_, params, var_arg, _)  ->
        let arg_length = List.length args in
        let p_length = Option.map_default List.length 0 params in
        (* Check whether number of arguments fits. *)
        (* If params is None, the function or its parameters are not declared, so we still analyze the unknown function call. *)
        if Option.is_none params || p_length = arg_length || (var_arg && arg_length >= p_length) then
          begin Some (match Cilfacade.find_varinfo_fundec f with
              | fd when LibraryFunctions.use_special f.vname ->
                M.info ~category:Analyzer "Using special for defined function %s" f.vname;
                tf_special_call ctx lv f args
              | fd ->
                tf_normal_call ctx lv e fd args getl sidel getg sideg
              | exception Not_found ->
                tf_special_call ctx lv f args)
          end
        else begin
          let geq = if var_arg then ">=" else "" in
          M.warn ~category:Unsound ~tags:[Category Call; CWE 685] "Potential call to function %a with wrong number of arguments (expected: %s%d, actual: %d). This call will be ignored." CilType.Varinfo.pretty f geq p_length arg_length;
          None
        end
      | _ ->
        M.warn ~category:Call "Something that is not a function (%a) is called." CilType.Varinfo.pretty f;
        None
    in
    let funs = List.filter_map one_function functions in
    if [] = funs && not (S.D.is_bot ctx.local) then begin
      M.msg_final Warning ~category:Unsound ~tags:[Category Call] "No suitable function to call";
      M.warn ~category:Unsound ~tags:[Category Call] "No suitable function to be called at call site. Continuing with state before call.";
      d (* because LevelSliceLifter *)
    end else
      common_joins ctx funs !r !spawns

  let tf_asm var edge prev_node getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.asm ctx) !r !spawns

  let tf_skip var edge prev_node getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.skip ctx) !r !spawns

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
    end getl sidel getg sideg d

  type Goblint_backtrace.mark += TfLocation of location

  let () = Goblint_backtrace.register_mark_printer (function
      | TfLocation loc ->
        Some ("transfer function at " ^ CilType.Location.show loc)
      | _ -> None (* for other marks *)
    )

  let tf var getl sidel getg sideg prev_node (_,edge) d (f,t) =
    let old_loc  = !Goblint_tracing.current_loc in
    let old_loc2 = !Goblint_tracing.next_loc in
    Goblint_tracing.current_loc := f;
    Goblint_tracing.next_loc := t;
    Goblint_backtrace.protect ~mark:(fun () -> TfLocation f) ~finally:(fun () ->
        Goblint_tracing.current_loc := old_loc;
        Goblint_tracing.next_loc := old_loc2
      ) (fun () ->
        let d       = tf var getl sidel getg sideg prev_node edge d in
        d
      )

  let tf (v,c) (edges, u) getl sidel getg sideg =
    let pval = getl (u,c) in
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (Node.location v,[]) in
    List.fold_left2 (|>) pval (List.map (tf (v,Obj.repr (fun () -> c)) getl sidel getg sideg u) edges) locs

  let tf (v,c) (e,u) getl sidel getg sideg =
    let old_node = !current_node in
    let old_fd = Option.map Node.find_fundec old_node |? Cil.dummyFunDec in
    let new_fd = Node.find_fundec v in
    if not (CilType.Fundec.equal old_fd new_fd) then
      Timing.Program.enter new_fd.svar.vname;
    let old_context = !M.current_context in
    current_node := Some u;
    M.current_context := Some (Obj.magic c); (* magic is fine because Spec is top-level Control Spec *)
    Fun.protect ~finally:(fun () ->
        current_node := old_node;
        M.current_context := old_context;
        if not (CilType.Fundec.equal old_fd new_fd) then
          Timing.Program.exit new_fd.svar.vname
      ) (fun () ->
        let d       = tf (v,c) (e,u) getl sidel getg sideg in
        d
      )

  let system (v,c) =
    match v with
    | FunctionEntry _ ->
      None
    | _ ->
      let tf getl sidel getg sideg =
        let tf' eu = tf (v,c) eu getl sidel getg sideg in

        match NodeH.find_option CfgTools.node_scc_global v with
        | Some scc when NodeH.mem scc.prev v && NodeH.length scc.prev = 1 ->
          (* Limited to loops with only one entry node. Otherwise unsound as is. *)
          (* TODO: Is it possible to do soundly for multi-entry loops? *)
          let stricts = NodeH.find_default scc.prev v [] in
          let xs_stricts = List.map tf' stricts in
          (* Evaluate non-strict for dead code warnings. See 00-sanity/36-strict-loop-dead. *)
          let equal = [%eq: (CilType.Location.t * Edge.t) list * Node.t] in
          let is_strict eu = List.exists (equal eu) stricts in
          let non_stricts = List.filter (neg is_strict) (Cfg.prev v) in
          let xs_non_stricts = List.map tf' non_stricts in
          if List.for_all S.D.is_bot xs_stricts then
            S.D.bot ()
          else (
            let xs_strict = List.fold_left S.D.join (S.D.bot ()) xs_stricts in
            List.fold_left S.D.join xs_strict xs_non_stricts
          )
        | _ ->
          let xs = List.map tf' (Cfg.prev v) in
          List.fold_left S.D.join (S.D.bot ()) xs
      in
      Some tf

  let iter_vars getl getg vq fl fg =
    (* vars for Spec *)
    let rec ctx =
      { ask    = (fun (type a) (q: a Queries.t) -> S.query ctx q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in query context.")
      ; node   = MyCFG.dummy_node (* TODO maybe ask should take a node (which could be used here) instead of a location *)
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> ctx_failwith "No context in query context.")
      ; context = (fun () -> ctx_failwith "No context in query context.")
      ; edge    = MyCFG.Skip
      ; local  = S.startstate Cil.dummyFunDec.svar (* bot and top both silently raise and catch Deadcode in DeadcodeLifter *)
      ; global = (fun g -> G.spec (getg (GVar.spec g)))
      ; spawn  = (fun ?(multiple=false) v d    -> failwith "Cannot \"spawn\" in query context.")
      ; split  = (fun d es   -> failwith "Cannot \"split\" in query context.")
      ; sideg  = (fun v g    -> failwith "Cannot \"split\" in query context.")
      }
    in
    let f v = fg (GVar.spec (Obj.obj v)) in
    S.query ctx (IterSysVars (vq, f));

    (* node vars for locals *)
    match vq with
    | Node {node; fundec} ->
      let fd = Option.default_delayed (fun () -> Node.find_fundec node) fundec in
      let cs = G.contexts (getg (GVar.contexts fd)) in
      G.CSet.iter (fun c ->
          fl (node, c)
        ) cs
    | _ ->
      ()

  let sys_change getl getg =
    let open CompareCIL in

    let c = match I.increment with
      | Some {changes; _} -> changes
      | None -> empty_change_info ()
    in
    List.(Logs.info "change_info = { unchanged = %d; changed = %d (with unchangedHeader = %d); added = %d; removed = %d }" (length c.unchanged) (length c.changed) (BatList.count_matching (fun c -> c.unchangedHeader) c.changed) (length c.added) (length c.removed));

    let changed_funs = List.filter_map (function
        | {old = {def = Some (Fun f); _}; diff = None; _} ->
          Logs.info "Completely changed function: %s" f.svar.vname;
          Some f
        | _ -> None
      ) c.changed
    in
    let part_changed_funs = List.filter_map (function
        | {old = {def = Some (Fun f); _}; diff = Some nd; _} ->
          Logs.info "Partially changed function: %s" f.svar.vname;
          Some (f, nd.primObsoleteNodes, nd.unchangedNodes)
        | _ -> None
      ) c.changed
    in
    let removed_funs = List.filter_map (function
        | {def = Some (Fun f); _} ->
          Logs.info "Removed function: %s" f.svar.vname;
          Some f
        | _ -> None
      ) c.removed
    in

    let module HM = Hashtbl.Make (Var2 (LVar) (GVar)) in

    let mark_node hm f node =
      iter_vars getl getg (Node {node; fundec = Some f}) (fun v ->
          HM.replace hm (`L v) ()
        ) (fun v ->
          HM.replace hm (`G v) ()
        )
    in

    let reluctant = GobConfig.get_bool "incremental.reluctant.enabled" in
    let reanalyze_entry f =
      (* destabilize the entry points of a changed function when reluctant is off,
         or the function is to be force-reanalyzed  *)
      (not reluctant) || CompareCIL.VarinfoSet.mem f.svar c.exclude_from_rel_destab
    in
    let obsolete_ret = HM.create 103 in
    let obsolete_entry = HM.create 103 in
    let obsolete_prim = HM.create 103 in

    (* When reluctant is on:
       Only add function entry nodes to obsolete_entry if they are in force-reanalyze *)
    List.iter (fun f ->
        if reanalyze_entry f then
          (* collect function entry for eager destabilization *)
          mark_node obsolete_entry f (FunctionEntry f)
        else
          (* collect function return for reluctant analysis *)
          mark_node obsolete_ret f (Function f)
      ) changed_funs;
    (* Primary changed unknowns from partially changed functions need only to be collected for eager destabilization when reluctant is off *)
    (* The return nodes of partially changed functions are collected in obsolete_ret for reluctant analysis *)
    (* We utilize that force-reanalyzed functions are always considered as completely changed (and not partially changed) *)
    List.iter (fun (f, pn, _) ->
        if not reluctant then (
          List.iter (fun n ->
              mark_node obsolete_prim f n
            ) pn
        )
        else
          mark_node obsolete_ret f (Function f)
      ) part_changed_funs;

    let obsolete = Enum.append (HM.keys obsolete_entry) (HM.keys obsolete_prim) |> List.of_enum in
    let reluctant = HM.keys obsolete_ret |> List.of_enum in

    let marked_for_deletion = HM.create 103 in

    let dummy_pseudo_return_node f =
      (* not the same as in CFG, but compares equal because of sid *)
      Node.Statement ({Cil.dummyStmt with sid = Cilfacade.get_pseudo_return_id f})
    in
    let add_nodes_of_fun (functions: fundec list) (withEntry: fundec -> bool) =
      let add_stmts (f: fundec) =
        List.iter (fun s ->
            mark_node marked_for_deletion f (Statement s)
          ) f.sallstmts
      in
      List.iter (fun f ->
          if withEntry f then
            mark_node marked_for_deletion f (FunctionEntry f);
          mark_node marked_for_deletion f (Function f);
          add_stmts f;
          mark_node marked_for_deletion f (dummy_pseudo_return_node f)
        ) functions;
    in

    add_nodes_of_fun changed_funs reanalyze_entry;
    add_nodes_of_fun removed_funs (fun _ -> true);
    (* it is necessary to remove all unknowns for changed pseudo-returns because they have static ids *)
    let add_pseudo_return f un =
      let pseudo = dummy_pseudo_return_node f in
      if not (List.exists (Node.equal pseudo % fst) un) then
        mark_node marked_for_deletion f (dummy_pseudo_return_node f)
    in
    List.iter (fun (f,_,un) ->
        mark_node marked_for_deletion f (Function f);
        add_pseudo_return f un
      ) part_changed_funs;

    let delete = HM.keys marked_for_deletion |> List.of_enum in

    let restart = match I.increment with
      | Some data ->
        let restart = ref [] in
        List.iter (fun g ->
            iter_vars getl getg g (fun v ->
                restart := `L v :: !restart
              ) (fun v ->
                restart := `G v :: !restart
              )
          ) data.restarting;
        !restart
      | None -> []
    in

    {obsolete; delete; reluctant; restart}

  let postmortem leaf =
    match leaf with
    | FunctionEntry fd, c -> [(Function fd, c)]
    | _ -> []
end


(** Add path sensitivity to a analysis *)
module PathSensitive2 (Spec:Spec)
  : Spec
    with module G = Spec.G
     and module C = Spec.C
     and module V = Spec.V
=
struct
  module D =
  struct
    (* TODO is it really worth it to check every time instead of just using sets and joining later? *)
    module R =
    struct
      include Spec.P
      type elt = Spec.D.t
    end
    module J = SetDomain.Joined (Spec.D)
    include DisjointDomain.ProjectiveSet (Spec.D) (J) (R)
    let name () = "PathSensitive (" ^ name () ^ ")"

    let printXml f x =
      let print_one x =
        BatPrintf.fprintf f "\n<path>%a</path>" Spec.D.printXml x
      in
      iter print_one x
  end

  module G = Spec.G
  module C = Spec.C
  module V = Spec.V
  module P = UnitP

  let name () = "PathSensitive2("^Spec.name ()^")"

  type marshal = Spec.marshal
  let init = Spec.init
  let finalize = Spec.finalize

  let startcontext () = Spec.startcontext ()
  let exitstate  v = D.singleton (Spec.exitstate  v)
  let startstate v = D.singleton (Spec.startstate v)
  let morphstate v d = D.map (Spec.morphstate v) d

  let conv ctx x =
    let rec ctx' = { ctx with ask   = (fun (type a) (q: a Queries.t) -> Spec.query ctx' q)
                            ; local = x
                            ; split = (ctx.split % D.singleton) }
    in
    ctx'

  let context ctx fd l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive2.context must be called with a singleton set."
    else
      let x = D.choose l in
      Spec.context (conv ctx x) fd x


  let map ctx f g =
    let h x xs =
      try D.add (g (f (conv ctx x))) xs
      with Deadcode -> xs
    in
    let d = D.fold h ctx.local (D.empty ()) in
    if D.is_bot d then raise Deadcode else d

  let fold' ctx f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    D.fold k ctx.local a

  let assign ctx l e    = map ctx Spec.assign  (fun h -> h l e )
  let vdecl ctx v       = map ctx Spec.vdecl   (fun h -> h v)
  let body   ctx f      = map ctx Spec.body    (fun h -> h f   )
  let return ctx e f    = map ctx Spec.return  (fun h -> h e f )
  let branch ctx e tv   = map ctx Spec.branch  (fun h -> h e tv)
  let asm ctx           = map ctx Spec.asm     identity
  let skip ctx          = map ctx Spec.skip    identity
  let special ctx l f a = map ctx Spec.special (fun h -> h l f a)

  let event ctx e octx =
    let fd1 = D.choose octx.local in
    map ctx Spec.event (fun h -> h e (conv octx fd1))

  let threadenter ctx ~multiple lval f args =
    let g xs ys = (List.map (fun y -> D.singleton y) ys) @ xs in
    fold' ctx (Spec.threadenter ~multiple) (fun h -> h lval f args) g []

  let threadspawn ctx ~multiple lval f args fctx =
    let fd1 = D.choose fctx.local in
    map ctx (Spec.threadspawn ~multiple) (fun h -> h lval f args (conv fctx fd1))

  let sync ctx reason = map ctx Spec.sync (fun h -> h reason)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    (* TODO: handle Invariant path like PathSensitive3? *)
    (* join results so that they are sound for all paths *)
    let module Result = (val Queries.Result.lattice q) in
    fold' ctx Spec.query identity (fun x f -> Result.join x (f q)) (Result.bot ())

  let enter ctx l f a =
    let g xs ys = (List.map (fun (x,y) -> D.singleton x, D.singleton y) ys) @ xs in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let paths_as_set ctx =
    (* Path-sensitivity is only here, not below! *)
    let elems = D.elements ctx.local in
    List.map (D.singleton) elems

  let combine_env ctx l fe f a fc d f_ask =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      if M.tracing then M.traceli "combine" "function: %a" Spec.D.pretty x;
      try
        let r = Spec.combine_env (conv ctx cd) l fe f a fc x f_ask in
        if M.tracing then M.traceu "combine" "combined function: %a" Spec.D.pretty r;
        D.add r y
      with Deadcode ->
        if M.tracing then M.traceu "combine" "combined function: dead";
        y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d

  let combine_assign ctx l fe f a fc d f_ask =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      if M.tracing then M.traceli "combine" "function: %a" Spec.D.pretty x;
      try
        let r = Spec.combine_assign (conv ctx cd) l fe f a fc x f_ask in
        if M.tracing then M.traceu "combine" "combined function: %a" Spec.D.pretty r;
        D.add r y
      with Deadcode ->
        if M.tracing then M.traceu "combine" "combined function: dead";
        y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d
end

module DeadBranchLifter (S: Spec): Spec =
struct
  include S

  let name () = "DeadBranch (" ^ S.name () ^ ")"

  (* Two global invariants:
     1. S.V -> S.G  --  used for S
     2. node -> (exp -> flat bool)  --  used for warnings *)

  module V =
  struct
    include Printable.EitherConf (struct let expand1 = false let expand2 = true end) (S.V) (Node)
    let name () = "DeadBranch"
    let s x = `Left x
    let node x = `Right x
    let is_write_only = function
      | `Left x -> S.V.is_write_only x
      | `Right _ -> true
  end

  module EM =
  struct
    include MapDomain.MapBot (Basetype.CilExp) (BoolDomain.FlatBool)
    let name () = "branches"
  end

  module G =
  struct
    include Lattice.Lift2 (S.G) (EM)
    let name () = "deadbranch"

    let s = function
      | `Bot -> S.G.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "DeadBranchLifter.s"
    let node = function
      | `Bot -> EM.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "DeadBranchLifter.node"
    let create_s s = `Lifted1 s
    let create_node node = `Lifted2 node

    let printXml f = function
      | `Lifted1 x -> S.G.printXml f x
      | `Lifted2 x -> BatPrintf.fprintf f "<analysis name=\"dead-branch\">%a</analysis>" EM.printXml x
      | x -> BatPrintf.fprintf f "<analysis name=\"dead-branch-lifter\">%a</analysis>" printXml x
  end

  let conv (ctx: (_, G.t, _, V.t) ctx): (_, S.G.t, _, S.V.t) ctx =
    { ctx with
      global = (fun v -> G.s (ctx.global (V.s v)));
      sideg = (fun v g -> ctx.sideg (V.s v) (G.create_s g));
    }

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv ctx) (WarnGlobal (Obj.repr g))
        | `Right g ->
          let em = G.node (ctx.global (V.node g)) in
          EM.iter (fun exp tv ->
              match tv with
              | `Lifted tv ->
                let loc = Node.location g in (* TODO: looking up location now doesn't work nicely with incremental *)
                let cilinserted = if loc.synthetic then "(possibly inserted by CIL) " else "" in
                M.warn ~loc:(Node g) ~tags:[CWE (if tv then 571 else 570)] ~category:Deadcode "condition '%a' %sis always %B" d_exp exp cilinserted tv
              | `Bot when not (CilType.Exp.equal exp one) -> (* all branches dead *)
                M.msg_final Error ~category:Analyzer ~tags:[Category Unsound] "Both branches dead";
                M.error ~loc:(Node g) ~category:Analyzer ~tags:[Category Unsound] "both branches over condition '%a' are dead" d_exp exp
              | `Bot (* all branches dead, fine at our inserted Neg(1)-s because no Pos(1) *)
              | `Top -> (* may be both true and false *)
                ()
            ) em;
      end
    | InvariantGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv ctx) (InvariantGlobal (Obj.repr g))
        | `Right g ->
          Queries.Result.top q
      end
    | IterSysVars (vq, vf) ->
      (* vars for S *)
      let vf' x = vf (Obj.repr (V.s (Obj.obj x))) in
      S.query (conv ctx) (IterSysVars (vq, vf'));

      (* node vars for dead branches *)
      begin match vq with
        | Node {node; _} ->
          vf (Obj.repr (V.node node))
        | _ ->
          ()
      end
    | _ ->
      S.query (conv ctx) q


  let branch ctx = S.branch (conv ctx)
  let context ctx = S.context (conv ctx)

  let branch ctx exp tv =
    if !AnalysisState.postsolving then (
      try
        let r = branch ctx exp tv in
        (* branch is live *)
        ctx.sideg (V.node ctx.prev_node) (G.create_node (EM.singleton exp (`Lifted tv))); (* record expression with reached tv *)
        r
      with Deadcode ->
        (* branch is dead *)
        ctx.sideg (V.node ctx.prev_node) (G.create_node (EM.singleton exp `Bot)); (* record expression without reached tv *)
        raise Deadcode
    )
    else (
      ctx.sideg (V.node ctx.prev_node) (G.create_node (EM.bot ())); (* create global variable during solving, to allow postsolving leq hack to pass verify *)
      branch ctx exp tv
    )

  let assign ctx = S.assign (conv ctx)
  let vdecl ctx = S.vdecl (conv ctx)
  let enter ctx = S.enter (conv ctx)
  let paths_as_set ctx = S.paths_as_set (conv ctx)
  let body ctx = S.body (conv ctx)
  let return ctx = S.return (conv ctx)
  let combine_env ctx = S.combine_env (conv ctx)
  let combine_assign ctx = S.combine_assign (conv ctx)
  let special ctx = S.special (conv ctx)
  let threadenter ctx = S.threadenter (conv ctx)
  let threadspawn ctx ~multiple lv f args fctx = S.threadspawn (conv ctx) ~multiple lv f args (conv fctx)
  let sync ctx = S.sync (conv ctx)
  let skip ctx = S.skip (conv ctx)
  let asm ctx = S.asm (conv ctx)
  let event ctx e octx = S.event (conv ctx) e (conv octx)
end

module LongjmpLifter (S: Spec): Spec =
struct
  include S

  let name () = "Longjmp (" ^ S.name () ^ ")"

  module V =
  struct
    include Printable.Either3Conf (struct let expand1 = false let expand2 = true let expand3 = true end) (S.V) (Printable.Prod (Node) (C)) (Printable.Prod (CilType.Fundec) (C))
    let name () = "longjmp"
    let s x = `Left x
    let longjmpto x = `Middle x
    let longjmpret x = `Right x
    let is_write_only = function
      | `Left x -> S.V.is_write_only x
      | _ -> false
  end

  module G =
  struct
    include Lattice.Lift2 (S.G) (S.D)

    let s = function
      | `Bot -> S.G.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "LongjmpLifter.s"
    let local = function
      | `Bot -> S.D.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "LongjmpLifter.local"
    let create_s s = `Lifted1 s
    let create_local local = `Lifted2 local

    let printXml f = function
      | `Lifted1 x -> S.G.printXml f x
      | `Lifted2 x -> BatPrintf.fprintf f "<analysis name=\"longjmp\"><value>%a</value></analysis>" S.D.printXml x
      | x -> BatPrintf.fprintf f "<analysis name=\"longjmp-lifter\">%a</analysis>" printXml x
  end

  let conv (ctx: (_, G.t, _, V.t) ctx): (_, S.G.t, _, S.V.t) ctx =
    { ctx with
      global = (fun v -> G.s (ctx.global (V.s v)));
      sideg = (fun v g -> ctx.sideg (V.s v) (G.create_s g));
    }

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv ctx) (WarnGlobal (Obj.repr g))
        | _ ->
          Queries.Result.top q
      end
    | InvariantGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv ctx) (InvariantGlobal (Obj.repr g))
        | _ ->
          Queries.Result.top q
      end
    | IterSysVars (vq, vf) ->
      (* vars for S *)
      let vf' x = vf (Obj.repr (V.s (Obj.obj x))) in
      S.query (conv ctx) (IterSysVars (vq, vf'));
      (* TODO: vars? *)
    | _ ->
      S.query (conv ctx) q


  let branch ctx = S.branch (conv ctx)
  let assign ctx = S.assign (conv ctx)
  let vdecl ctx = S.vdecl (conv ctx)
  let enter ctx = S.enter (conv ctx)
  let paths_as_set ctx = S.paths_as_set (conv ctx)
  let body ctx = S.body (conv ctx)
  let return ctx = S.return (conv ctx)
  let context ctx = S.context (conv ctx)

  let combine_env ctx lv e f args fc fd f_ask =
    let conv_ctx = conv ctx in
    let current_fundec = Node.find_fundec ctx.node in
    let handle_longjmp (cd, fc, longfd) =
      (* This is called per-path. *)
      let rec cd_ctx =
        { conv_ctx with
          ask = (fun (type a) (q: a Queries.t) -> S.query cd_ctx q);
          local = cd;
        }
      in
      let longfd_ctx =
        (* Inner scope to prevent unsynced longfd_ctx from being used. *)
        (* Extra sync like with normal combine. *)
        let rec sync_ctx =
          { conv_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query sync_ctx q);
            local = longfd;
            prev_node = Function f;
          }
        in
        let synced = S.sync sync_ctx `Join in
        let rec longfd_ctx =
          { sync_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query longfd_ctx q);
            local = synced;
          }
        in
        longfd_ctx
      in
      let combined = lazy ( (* does not depend on target, do at most once *)
        (* Globals are non-problematic here, as they are always carried around without any issues! *)
        (* A combine call is mostly needed to ensure locals have appropriate values. *)
        (* Using f from called function on purpose here! Needed? *)
        S.combine_env cd_ctx None e f args fc longfd_ctx.local (Analyses.ask_of_ctx longfd_ctx) (* no lval because longjmp return skips return value assignment *)
      )
      in
      let returned = lazy ( (* does not depend on target, do at most once *)
        let rec combined_ctx =
          { cd_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query combined_ctx q);
            local = Lazy.force combined;
          }
        in
        S.return combined_ctx None current_fundec
      )
      in
      let (active_targets, _) = longfd_ctx.ask ActiveJumpBuf in
      let valid_targets = cd_ctx.ask ValidLongJmp in
      let handle_target target = match target with
        | JmpBufDomain.BufferEntryOrTop.AllTargets -> () (* The warning is already emitted at the point where the longjmp happens *)
        | Target (target_node, target_context) ->
          let target_fundec = Node.find_fundec target_node in
          if CilType.Fundec.equal target_fundec current_fundec && ControlSpecC.equal target_context (ctx.control_context ()) then (
            if M.tracing then Messages.tracel "longjmp" "Fun: Potentially from same context, side-effect to %a" Node.pretty target_node;
            ctx.sideg (V.longjmpto (target_node, ctx.context ())) (G.create_local (Lazy.force combined))
            (* No need to propagate this outwards here, the set of valid longjumps is part of the context, we can never have the same context setting the longjmp multiple times *)
          )
          (* Appropriate setjmp is not in current function & current context *)
          else if JmpBufDomain.JmpBufSet.mem target valid_targets then
            ctx.sideg (V.longjmpret (current_fundec, ctx.context ())) (G.create_local (Lazy.force returned))
          else
            (* It actually is not handled here but was propagated here spuriously, we already warned at the location where this issue is caused *)
            (* As the validlongjumps inside the callee is a a superset of the ones inside the caller *)
            ()
      in
      JmpBufDomain.JmpBufSet.iter handle_target active_targets
    in
    if M.tracing then M.tracel "longjmp" "longfd getg %a" CilType.Fundec.pretty f;
    let longfd = G.local (ctx.global (V.longjmpret (f, Option.get fc))) in
    if M.tracing then M.tracel "longjmp" "longfd %a" D.pretty longfd;
    if not (D.is_bot longfd) then
      handle_longjmp (ctx.local, fc, longfd);
    S.combine_env (conv_ctx) lv e f args fc fd f_ask

  let combine_assign ctx lv e f args fc fd f_ask =
    S.combine_assign (conv ctx) lv e f args fc fd f_ask

  let special ctx lv f args =
    let conv_ctx = conv ctx in
    match (LibraryFunctions.find f).special args with
    | Setjmp {env} ->
      (* Handling of returning for the first time *)
      let normal_return = S.special conv_ctx lv f args in
      let jmp_return = G.local (ctx.global (V.longjmpto (ctx.prev_node, ctx.context ()))) in
      if S.D.is_bot jmp_return then
        normal_return
      else (
        let rec jmp_ctx =
          { conv_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query jmp_ctx q);
            local = jmp_return;
          }
        in
        let longjmped = S.event jmp_ctx (Events.Longjmped {lval=lv}) jmp_ctx in
        S.D.join normal_return longjmped
      )
    | Longjmp {env; value} ->
      let current_fundec = Node.find_fundec ctx.node in
      let handle_path path = (
        let rec path_ctx =
          { conv_ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query path_ctx q);
            local = path;
          }
        in
        let specialed = lazy ( (* does not depend on target, do at most once *)
          S.special path_ctx lv f args
        )
        in
        let returned = lazy ( (* does not depend on target, do at most once *)
          let rec specialed_ctx =
            { path_ctx with
              ask = (fun (type a) (q: a Queries.t) -> S.query specialed_ctx q);
              local = Lazy.force specialed;
            }
          in
          S.return specialed_ctx None current_fundec
        )
        in
        (* Eval `env` again to avoid having to construct bespoke ctx to ask *)
        let targets = path_ctx.ask (EvalJumpBuf env) in
        let valid_targets = path_ctx.ask ValidLongJmp in
        if M.tracing then Messages.tracel "longjmp" "Jumping to %a" JmpBufDomain.JmpBufSet.pretty targets;
        let handle_target target = match target with
          | JmpBufDomain.BufferEntryOrTop.AllTargets ->
            M.warn ~category:Imprecise "Longjmp to potentially invalid target, as contents of buffer %a may be unknown! (imprecision due to heap?)" d_exp env;
            M.msg_final Error ~category:Unsound ~tags:[Category Imprecise; Category Call] "Longjmp to unknown target ignored"
          | Target (target_node, target_context) ->
            let target_fundec = Node.find_fundec target_node in
            if CilType.Fundec.equal target_fundec current_fundec && ControlSpecC.equal target_context (ctx.control_context ()) then (
              if M.tracing then Messages.tracel "longjmp" "Potentially from same context, side-effect to %a" Node.pretty target_node;
              ctx.sideg (V.longjmpto (target_node, ctx.context ())) (G.create_local (Lazy.force specialed))
            )
            else if JmpBufDomain.JmpBufSet.mem target valid_targets then (
              if M.tracing then Messages.tracel "longjmp" "Longjmp to somewhere else, side-effect to %i" (S.C.hash (ctx.context ()));
              ctx.sideg (V.longjmpret (current_fundec, ctx.context ())) (G.create_local (Lazy.force returned))
            )
            else
              M.warn ~category:(Behavior (Undefined Other)) "Longjmp to potentially invalid target! (Target %a in Function %a which may have already returned or is in a different thread)" Node.pretty target_node CilType.Fundec.pretty target_fundec
        in
        if JmpBufDomain.JmpBufSet.is_empty targets then
          M.warn ~category:(Behavior (Undefined Other)) "Longjmp to potentially invalid target (%a is bot?!)" d_exp env
        else
          JmpBufDomain.JmpBufSet.iter handle_target targets
      )
      in
      List.iter handle_path (S.paths_as_set conv_ctx);
      if !AnalysisState.should_warn && List.mem "termination" @@ get_string_list "ana.activated" then (
        AnalysisState.svcomp_may_not_terminate := true;
        M.warn ~category:Termination "The program might not terminate! (Longjmp)"
      );
      S.D.bot ()
    | _ -> S.special conv_ctx lv f args
  let threadenter ctx = S.threadenter (conv ctx)
  let threadspawn ctx ~multiple lv f args fctx = S.threadspawn (conv ctx) ~multiple lv f args (conv fctx)
  let sync ctx = S.sync (conv ctx)
  let skip ctx = S.skip (conv ctx)
  let asm ctx = S.asm (conv ctx)
  let event ctx e octx = S.event (conv ctx) e (conv octx)
end


(** Add cycle detection in the context-sensitive dynamic function call graph to an analysis *)
module RecursionTermLifter (S: Spec)
  : Spec with module D = S.D
          and module C = S.C
=
(* two global invariants:
   - S.V -> S.G
     Needed to store the previously built global invariants
   - fundec * S.C -> (Set (fundec * S.C))
     The second global invariant maps from the callee fundec and context to a set of caller fundecs and contexts.
     This structure therefore stores the context-sensitive call graph.
     For example:
      let the function f in context c call function g in context c'.
      In the global invariant structure it would be stored like this: (g,c') -> {(f, c)}
*)

struct
  include S

  (* contains all the callee fundecs and contexts *)
  module V = GVarFC(S.V)(S.C)

  (* Tuple containing the fundec and context of a caller *)
  module Call = Printable.Prod (CilType.Fundec) (S.C)

  (* Set containing multiple caller tuples *)
  module CallerSet = SetDomain.Make (Call)

  module G =
  struct
    include Lattice.Lift2 (G) (CallerSet)

    let spec = function
      | `Bot -> G.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "RecursionTermLifter.spec"

    let callers = function
      | `Bot -> CallerSet.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "RecursionTermLifter.callGraph"

    let create_spec spec = `Lifted1 spec
    let create_singleton_caller caller = `Lifted2 (CallerSet.singleton caller)

    let printXml f = function
      | `Lifted1 x -> G.printXml f x
      | `Lifted2 x -> BatPrintf.fprintf f "<analysis name=\"recTerm-context\">%a</analysis>" CallerSet.printXml x
      | x -> BatPrintf.fprintf f "<analysis name=\"recTerm\">%a</analysis>" printXml x

  end

  let name () = "RecursionTermLifter (" ^ S.name () ^ ")"

  let conv (ctx: (_, G.t, _, V.t) ctx): (_, S.G.t, _, S.V.t) ctx =
    { ctx with
      global = (fun v -> G.spec (ctx.global (V.spec v)));
      sideg = (fun v g -> ctx.sideg (V.spec v) (G.create_spec g));
    }

  let cycleDetection ctx call =
    let module LH = Hashtbl.Make (Printable.Prod (CilType.Fundec) (S.C)) in
    let module LS = Set.Make (Printable.Prod (CilType.Fundec) (S.C)) in
    (* find all cycles/SCCs *)
    let global_visited_calls = LH.create 100 in

    (* DFS *)
    let rec iter_call (path_visited_calls: LS.t) ((fundec, _) as call) =
      if LS.mem call path_visited_calls then (
        AnalysisState.svcomp_may_not_terminate := true; (*set the indicator for a non-terminating program for the sv comp*)
        (*Cycle found*)
        let loc = M.Location.CilLocation fundec.svar.vdecl in
        M.warn ~loc ~category:Termination "The program might not terminate! (Fundec %a is contained in a call graph cycle)" CilType.Fundec.pretty fundec) (* output a warning for non-termination*)
      else if not (LH.mem global_visited_calls call) then begin
        LH.replace global_visited_calls call ();
        let new_path_visited_calls = LS.add call path_visited_calls in
        let gvar = V.call call in
        let callers = G.callers (ctx.global gvar) in
        CallerSet.iter (fun to_call ->
            iter_call new_path_visited_calls to_call
          ) callers;
      end
    in
    iter_call LS.empty call

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal v ->
      (* check result of loop analysis *)
      if not (ctx.ask Queries.MustTermAllLoops) then
        AnalysisState.svcomp_may_not_terminate := true;
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v' ->
          S.query (conv ctx) (WarnGlobal (Obj.repr v'))
        | `Right call -> cycleDetection ctx call (* Note: to make it more efficient, one could only execute the cycle detection in case the loop analysis returns true, because otherwise the program will probably not terminate anyway*)
      end
    | InvariantGlobal v ->
      let v: V.t = Obj.obj v in
      begin match v with
        | `Left v ->
          S.query (conv ctx) (InvariantGlobal (Obj.repr v))
        | `Right v ->
          Queries.Result.top q
      end
    | _ -> S.query (conv ctx) q

  let branch ctx = S.branch (conv ctx)
  let assign ctx = S.assign (conv ctx)
  let vdecl ctx = S.vdecl (conv ctx)


  let record_call sideg callee caller =
    sideg (V.call callee) (G.create_singleton_caller caller)

  let enter ctx  = S.enter (conv ctx)
  let context ctx = S.context (conv ctx)
  let paths_as_set ctx = S.paths_as_set (conv ctx)
  let body ctx = S.body (conv ctx)
  let return ctx = S.return (conv ctx)
  let combine_env ctx r fe f args fc es f_ask =
    if !AnalysisState.postsolving then (
      let c_r: S.C.t = ctx.context () in (* Caller context *)
      let nodeF = ctx.node in
      let fd_r : fundec = Node.find_fundec nodeF in (* Caller fundec *)
      let caller: (fundec * S.C.t) = (fd_r, c_r) in
      let c_e: S.C.t = Option.get fc in (* Callee context *)
      let fd_e : fundec = f in (* Callee fundec *)
      let callee = (fd_e, c_e) in
      record_call ctx.sideg callee caller
    );
    S.combine_env (conv ctx) r fe f args fc es f_ask

  let combine_assign ctx = S.combine_assign (conv ctx)
  let special ctx = S.special (conv ctx)
  let threadenter ctx = S.threadenter (conv ctx)
  let threadspawn ctx ~multiple lv f args fctx = S.threadspawn (conv ctx) ~multiple lv f args (conv fctx)
  let sync ctx = S.sync (conv ctx)
  let skip ctx = S.skip (conv ctx)
  let asm ctx = S.asm (conv ctx)
  let event ctx e octx = S.event (conv ctx) e (conv octx)
end

module CompareGlobSys (SpecSys: SpecSys) =
struct
  open SpecSys
  module Sys = EQSys
  module LH = LHT
  module GH = GHT

  open Spec
  module G = Sys.G

  module PP = Hashtbl.Make (Node)

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
        if get_bool "dbg.compare_runs.diff" then
          Logs.info "Global %a is more precise using left:\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v2,v1);
        f_le ()
      end else if b2 then begin
        if get_bool "dbg.compare_runs.diff" then
          Logs.info "Global %a is more precise using right:\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2);
        f_gr ()
      end else begin
        if get_bool "dbg.compare_runs.diff" then (
          Logs.info "Global %a is incomparable (diff):\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2);
          Logs.info "Global %a is incomparable (reverse diff):\n%a" Sys.GVar.pretty_trace k G.pretty_diff (v2,v1);
        );
        f_uk ()
      end
    in
    GH.iter f g1;
    Logs.info "globals:\tequal = %d\tleft = %d\tright = %d\tincomparable = %d" !eq !le !gr !uk

  let compare_locals h1 h2 =
    let eq, le, gr, uk = ref 0, ref 0, ref 0, ref 0 in
    let f k v1 =
      if PP.mem h2 k then
        let v2 = PP.find h2 k in
        let b1 = D.leq v1 v2 in
        let b2 = D.leq v2 v1 in
        if b1 && b2 then
          incr eq
        else if b1 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a @@ %a is more precise using left:\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v2,v1);
          incr le
        end else if b2 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a @@ %a is more precise using right:\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v1,v2);
          incr gr
        end else begin
          if get_bool "dbg.compare_runs.diff" then (
            Logs.info "%a @@ %a is incomparable (diff):\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v1,v2);
            Logs.info "%a @@ %a is incomparable (reverse diff):\n%a" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v2,v1);
          );
          incr uk
        end
    in
    PP.iter f h1;
    (* let k1 = Set.of_enum @@ PP.keys h1 in
       let k2 = Set.of_enum @@ PP.keys h2 in
       let o1 = Set.cardinal @@ Set.diff k1 k2 in
       let o2 = Set.cardinal @@ Set.diff k2 k1 in
       Logs.info "locals: \tequal = %d\tleft = %d[%d]\tright = %d[%d]\tincomparable = %d" !eq !le o1 !gr o2 !uk *)
    Logs.info "locals: \tequal = %d\tleft = %d\tright = %d\tincomparable = %d" !eq !le !gr !uk

  let compare_locals_ctx h1 h2 =
    let eq, le, gr, uk, no2, no1 = ref 0, ref 0, ref 0, ref 0, ref 0, ref 0 in
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
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a is more precise using left:\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v2,v1);
          f_le ()
        end else if b2 then begin
          if get_bool "dbg.compare_runs.diff" then
            Logs.info "%a is more precise using right:\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v1,v2);
          f_gr ()
        end else begin
          if get_bool "dbg.compare_runs.diff" then (
            Logs.info "%a is incomparable (diff):\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v1,v2);
            Logs.info "%a is incomparable (reverse diff):\n%a" Sys.LVar.pretty_trace k D.pretty_diff (v2,v1);
          );
          f_uk ()
        end
    in
    LH.iter f h1;
    let f k v2 =
      if not (LH.mem h1 k) then incr no1
    in
    LH.iter f h2;
    (* let k1 = Set.of_enum @@ PP.keys h1 in *)
    (* let k2 = Set.of_enum @@ PP.keys h2 in *)
    (* let o1 = Set.cardinal @@ Set.diff k1 k2 in *)
    (* let o2 = Set.cardinal @@ Set.diff k2 k1 in *)
    Logs.info "locals_ctx:\tequal = %d\tleft = %d\tright = %d\tincomparable = %d\tno_ctx_in_right = %d\tno_ctx_in_left = %d" !eq !le !gr !uk !no2 !no1

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
    Logs.newline ();
    Logs.info "Comparing GlobConstrSys precision of %s (left) with %s (right):" name1 name2;
    compare_globals g1 g2;
    compare_locals h1 h2;
    compare_locals_ctx l1 l2;
    Logs.newline ();
end

module CompareHashtbl (Var: VarType) (Dom: Lattice.S) (VH: Hashtbl.S with type key = Var.t) =
struct
  module Var =
  struct
    include Printable.Std
    include Var
    let name () = "var"

    let pretty = pretty_trace
    include Printable.SimplePretty (
      struct
        type nonrec t = t
        let pretty = pretty
      end
      )
  end

  include PrecCompare.MakeHashtbl (Var) (Dom) (VH)
end

module CompareEqSys (Sys: EqConstrSys) (VH: Hashtbl.S with type key = Sys.Var.t) =
struct
  module Compare = CompareHashtbl (Sys.Var) (Sys.Dom) (VH)

  let compare (name1, name2) vh1 vh2 =
    Logs.newline ();
    Logs.info "Comparing EqConstrSys precision of %s (left) with %s (right):" name1 name2;
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1 ~name2 vh2 in
    Logs.info "EqConstrSys comparison summary: %t" (fun () -> msg);
    Logs.newline ();
end

module CompareGlobal (GVar: VarType) (G: Lattice.S) (GH: Hashtbl.S with type key = GVar.t) =
struct
  module Compare = CompareHashtbl (GVar) (G) (GH)

  let compare (name1, name2) vh1 vh2 =
    Logs.newline ();
    Logs.info "Comparing globals precision of %s (left) with %s (right):" name1 name2;
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1 ~name2 vh2 in
    Logs.info "Globals comparison summary: %t" (fun () -> msg);
    Logs.newline ();
end

module CompareNode (C: Printable.S) (D: Lattice.S) (LH: Hashtbl.S with type key = VarF (C).t) =
struct
  module Node =
  struct
    include Node
    let var_id _ = "nodes"
    let node x = x
    let is_write_only _ = false
  end
  module NH = Hashtbl.Make (Node)

  module Compare = CompareHashtbl (Node) (D) (NH)

  let join_contexts (lh: D.t LH.t): D.t NH.t =
    let nh = NH.create 113 in
    LH.iter (fun (n, _) d ->
        let d' = try D.join (NH.find nh n) d with Not_found -> d in
        NH.replace nh n d'
      ) lh;
    nh

  let compare (name1, name2) vh1 vh2 =
    Logs.newline ();
    Logs.info "Comparing nodes precision of %s (left) with %s (right):" name1 name2;
    let vh1' = join_contexts vh1 in
    let vh2' = join_contexts vh2 in
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1' ~name2 vh2' in
    Logs.info "Nodes comparison summary: %t" (fun () -> msg);
    Logs.newline ();
end
