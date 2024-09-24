open Batteries
open GoblintCil
open Analyses
open GobConfig


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
