(** Various simple and old analysis lifters. *)

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

  let conv man =
    { man with local = D.unlift man.local
             ; split = (fun d es -> man.split (D.lift d) es )
    }

  let context man fd = S.context (conv man) fd % D.unlift
  let startcontext () = S.startcontext ()

  let sync man reason =
    D.lift @@ S.sync (conv man) reason

  let query man =
    S.query (conv man)

  let assign man lv e =
    D.lift @@ S.assign (conv man) lv e

  let vdecl man v =
    D.lift @@ S.vdecl (conv man) v

  let branch man e tv =
    D.lift @@ S.branch (conv man) e tv

  let body man f =
    D.lift @@ S.body (conv man) f

  let return man r f =
    D.lift @@ S.return (conv man) r f

  let asm man =
    D.lift @@ S.asm (conv man)

  let skip man =
    D.lift @@ S.skip (conv man)

  let enter man r f args =
    List.map (fun (x,y) -> D.lift x, D.lift y) @@ S.enter (conv man) r f args

  let special man r f args =
    D.lift @@ S.special (conv man) r f args

  let combine_env man r fe f args fc es f_ask =
    D.lift @@ S.combine_env (conv man) r fe f args fc (D.unlift es) f_ask

  let combine_assign man r fe f args fc es f_ask =
    D.lift @@ S.combine_assign (conv man) r fe f args fc (D.unlift es) f_ask

  let threadenter man ~multiple lval f args =
    List.map D.lift @@ S.threadenter (conv man) ~multiple lval f args

  let threadspawn man ~multiple lval f args fman =
    D.lift @@ S.threadspawn (conv man) ~multiple lval f args (conv fman)

  let paths_as_set man =
    List.map (fun x -> D.lift x) @@ S.paths_as_set (conv man)

  let event man e oman =
    D.lift @@ S.event (conv man) e (conv oman)
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

  let conv man =
    { man with context = (fun () -> C.unlift (man.context ())) }

  let context man fd = C.lift % S.context (conv man) fd
  let startcontext () = C.lift @@ S.startcontext ()

  let sync man reason =
    S.sync (conv man) reason

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IterPrevVars f ->
      let g i (n, c, j) e = f i (n, Obj.repr (C.lift (Obj.obj c)), j) e in
      S.query (conv man) (Queries.IterPrevVars g)
    | _ -> S.query (conv man) q

  let assign man lv e =
    S.assign (conv man) lv e

  let vdecl man v =
    S.vdecl (conv man) v

  let branch man e tv =
    S.branch (conv man) e tv

  let body man f =
    S.body (conv man) f

  let return man r f =
    S.return (conv man) r f

  let asm man =
    S.asm (conv man)

  let skip man =
    S.skip (conv man)

  let enter man r f args =
    S.enter (conv man) r f args

  let special man r f args =
    S.special (conv man) r f args

  let combine_env man r fe f args fc es f_ask =
    S.combine_env (conv man) r fe f args (Option.map C.unlift fc) es f_ask

  let combine_assign man r fe f args fc es f_ask =
    S.combine_assign (conv man) r fe f args (Option.map C.unlift fc) es f_ask

  let threadenter man ~multiple lval f args =
    S.threadenter (conv man) ~multiple lval f args

  let threadspawn man ~multiple lval f args fman =
    S.threadspawn (conv man) ~multiple lval f args (conv fman)

  let paths_as_set man = S.paths_as_set (conv man)
  let event man e oman = S.event (conv man) e (conv oman)
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

  let conv man =
    { man with local = fst man.local
             ; split = (fun d es -> man.split (d, snd man.local) es )
    }

  let context man fd (d,_) = S.context (conv man) fd d
  let startcontext () = S.startcontext ()

  let lift_fun man f g h =
    f @@ h (g (conv man))

  let enter' man r f args =
    let liftmap = List.map (fun (x,y) -> (x, snd man.local), (y, snd man.local)) in
    lift_fun man liftmap S.enter ((|>) args % (|>) f % (|>) r)

  let lift man d = (d, snd man.local)
  let lift_start_level d = (d, !start_level)

  let sync man reason = lift_fun man (lift man) S.sync   ((|>) reason)
  let query' man (type a) (q: a Queries.t): a Queries.result =
    lift_fun man identity   S.query  (fun x -> x q)
  let assign man lv e = lift_fun man (lift man) S.assign ((|>) e % (|>) lv)
  let vdecl man v     = lift_fun man (lift man) S.vdecl  ((|>) v)
  let branch man e tv = lift_fun man (lift man) S.branch ((|>) tv % (|>) e)
  let body man f      = lift_fun man (lift man) S.body   ((|>) f)
  let return man r f  = lift_fun man (lift man) S.return ((|>) f % (|>) r)
  let asm man         = lift_fun man (lift man) S.asm    identity
  let skip man        = lift_fun man (lift man) S.skip   identity
  let special man r f args        = lift_fun man (lift man) S.special ((|>) args % (|>) f % (|>) r)
  let combine_env' man r fe f args fc es f_ask = lift_fun man (lift man) S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign' man r fe f args fc es f_ask = lift_fun man (lift man) S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)

  let threadenter man ~multiple lval f args = lift_fun man (List.map lift_start_level) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval)
  let threadspawn man ~multiple lval f args fman = lift_fun man (lift man) (S.threadspawn ~multiple) ((|>) (conv fman) % (|>) args % (|>) f % (|>) lval)

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

  let paths_as_set man =
    let liftmap = List.map (fun x -> (x, snd man.local)) in
    lift_fun man liftmap S.paths_as_set (Fun.id)

  let event man e oman =
    lift_fun man (lift man) S.event ((|>) (conv oman) % (|>) e)

  let enter man r f args =
    let (d,l) = man.local in
    if leq0 l then
      [man.local, D.bot ()]
    else
      enter' {man with local=(d, sub1 l)} r f args

  let combine_env man r fe f args fc es f_ask =
    let (d,l) = man.local in
    let l = add1 l in
    if leq0 l then
      (d, l)
    else
      let d',_ = combine_env' man r fe f args fc es f_ask in
      (d', l)

  let combine_assign man r fe f args fc es f_ask =
    let (d,l) = man.local in
    (* No need to add1 here, already done in combine_env. *)
    if leq0 l then
      (d, l)
    else
      let d',_ = combine_assign' man r fe f args fc es f_ask in
      (d', l)

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.EvalFunvar e ->
      let (d,l) = man.local in
      if leq0 l then
        Queries.AD.empty ()
      else
        query' man (Queries.EvalFunvar e)
    | q -> query' man q
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


  let conv man =
    { man with local = fst man.local
             ; split = (fun d es -> man.split (d, snd man.local) es )
    }

  let context man fd (d,m) = S.context (conv man) fd d (* just the child analysis' context *)

  let lift_fun man f g = g (f (conv man)), snd man.local

  let sync man reason = lift_fun man S.sync   ((|>) reason)
  let query man       = S.query (conv man)
  let assign man lv e = lift_fun man S.assign ((|>) e % (|>) lv)
  let vdecl man v     = lift_fun man S.vdecl  ((|>) v)
  let branch man e tv = lift_fun man S.branch ((|>) tv % (|>) e)
  let body man f      = lift_fun man S.body   ((|>) f)
  let return man r f  = lift_fun man S.return ((|>) f % (|>) r)
  let asm man         = lift_fun man S.asm    identity
  let skip man        = lift_fun man S.skip   identity
  let special man r f args       = lift_fun man S.special ((|>) args % (|>) f % (|>) r)

  let event man e oman = lift_fun man S.event ((|>) (conv oman) % (|>) e)

  let threadenter man ~multiple lval f args = S.threadenter (conv man) ~multiple lval f args |> List.map (fun d -> (d, snd man.local))
  let threadspawn man ~multiple lval f args fman = lift_fun man (S.threadspawn ~multiple) ((|>) (conv fman) % (|>) args % (|>) f % (|>) lval)

  let enter man r f args =
    let m = snd man.local in
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
    S.enter (conv man) r f args
    |> List.map (fun (c,v) -> (c,m), d' v) (* c: caller, v: callee *)

  let paths_as_set man =
    let m = snd man.local in
    S.paths_as_set (conv man) |> List.map (fun v -> (v,m))

  let combine_env man r fe f args fc es f_ask = lift_fun man S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign man r fe f args fc es f_ask = lift_fun man S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)
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


  let conv man =
    { man with local = D.unlift man.local
             ; split = (fun d es -> man.split (D.lift d) es )
    }

  let context man fd = S.context (conv man) fd % D.unlift

  let lift_fun man f g h b =
    try f @@ h (g (conv man))
    with Deadcode -> b

  let sync man reason = lift_fun man D.lift   S.sync   ((|>) reason)      `Bot

  let enter man r f args =
    let liftmap = List.map (fun (x,y) -> D.lift x, D.lift y) in
    lift_fun man liftmap S.enter ((|>) args % (|>) f % (|>) r) []

  let paths_as_set man =
    let liftmap = List.map (fun x -> D.lift x) in
    lift_fun man liftmap S.paths_as_set (Fun.id) [D.bot ()] (* One dead path instead of none, such that combine_env gets called for functions with dead normal return (and thus longjmpy returns can be correctly handled by lifter). *)

  let query man (type a) (q: a Queries.t): a Queries.result =
    lift_fun man identity S.query (fun (x) -> x q) (Queries.Result.bot q)
  let assign man lv e = lift_fun man D.lift   S.assign ((|>) e % (|>) lv) `Bot
  let vdecl man v     = lift_fun man D.lift   S.vdecl  ((|>) v)            `Bot
  let branch man e tv = lift_fun man D.lift   S.branch ((|>) tv % (|>) e) `Bot
  let body man f      = lift_fun man D.lift   S.body   ((|>) f)            `Bot
  let return man r f  = lift_fun man D.lift   S.return ((|>) f % (|>) r)  `Bot
  let asm man         = lift_fun man D.lift   S.asm    identity           `Bot
  let skip man        = lift_fun man D.lift   S.skip   identity           `Bot
  let special man r f args       = lift_fun man D.lift S.special ((|>) args % (|>) f % (|>) r)        `Bot
  let combine_env man r fe f args fc es f_ask = lift_fun man D.lift S.combine_env (fun p -> p r fe f args fc (D.unlift es) f_ask) `Bot
  let combine_assign man r fe f args fc es f_ask = lift_fun man D.lift S.combine_assign (fun p -> p r fe f args fc (D.unlift es) f_ask) `Bot

  let threadenter man ~multiple lval f args = lift_fun man (List.map D.lift) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval) []
  let threadspawn man ~multiple lval f args fman = lift_fun man D.lift (S.threadspawn ~multiple) ((|>) (conv fman) % (|>) args % (|>) f % (|>) lval) `Bot

  let event (man:(D.t,G.t,C.t,V.t) man) (e:Events.t) (oman:(D.t,G.t,C.t,V.t) man):D.t = lift_fun man D.lift S.event ((|>) (conv oman) % (|>) e) `Bot
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

  let conv man x =
    let rec man' = { man with ask   = (fun (type a) (q: a Queries.t) -> Spec.query man' q)
                            ; local = x
                            ; split = (man.split % D.singleton) }
    in
    man'

  let context man fd l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive2.context must be called with a singleton set."
    else
      let x = D.choose l in
      Spec.context (conv man x) fd x


  let map man f g =
    let h x xs =
      try D.add (g (f (conv man x))) xs
      with Deadcode -> xs
    in
    let d = D.fold h man.local (D.empty ()) in
    if D.is_bot d then raise Deadcode else d

  let fold' man f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv man x
      with Deadcode -> a
    in
    D.fold k man.local a

  let assign man l e    = map man Spec.assign  (fun h -> h l e )
  let vdecl man v       = map man Spec.vdecl   (fun h -> h v)
  let body   man f      = map man Spec.body    (fun h -> h f   )
  let return man e f    = map man Spec.return  (fun h -> h e f )
  let branch man e tv   = map man Spec.branch  (fun h -> h e tv)
  let asm man           = map man Spec.asm     identity
  let skip man          = map man Spec.skip    identity
  let special man l f a = map man Spec.special (fun h -> h l f a)

  let event man e oman =
    let fd1 = D.choose oman.local in
    map man Spec.event (fun h -> h e (conv oman fd1))

  let threadenter man ~multiple lval f args =
    let g xs ys = (List.map (fun y -> D.singleton y) ys) @ xs in
    fold' man (Spec.threadenter ~multiple) (fun h -> h lval f args) g []

  let threadspawn man ~multiple lval f args fman =
    let fd1 = D.choose fman.local in
    map man (Spec.threadspawn ~multiple) (fun h -> h lval f args (conv fman fd1))

  let sync man reason = map man Spec.sync (fun h -> h reason)

  let query man (type a) (q: a Queries.t): a Queries.result =
    (* TODO: handle Invariant path like PathSensitive3? *)
    (* join results so that they are sound for all paths *)
    let module Result = (val Queries.Result.lattice q) in
    fold' man Spec.query identity (fun x f -> Result.join x (f q)) (Result.bot ())

  let enter man l f a =
    let g xs ys = (List.map (fun (x,y) -> D.singleton x, D.singleton y) ys) @ xs in
    fold' man Spec.enter (fun h -> h l f a) g []

  let paths_as_set man =
    (* Path-sensitivity is only here, not below! *)
    let elems = D.elements man.local in
    List.map (D.singleton) elems

  let combine_env man l fe f a fc d f_ask =
    assert (D.cardinal man.local = 1);
    let cd = D.choose man.local in
    let k x y =
      if M.tracing then M.traceli "combine" "function: %a" Spec.D.pretty x;
      try
        let r = Spec.combine_env (conv man cd) l fe f a fc x f_ask in
        if M.tracing then M.traceu "combine" "combined function: %a" Spec.D.pretty r;
        D.add r y
      with Deadcode ->
        if M.tracing then M.traceu "combine" "combined function: dead";
        y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d

  let combine_assign man l fe f a fc d f_ask =
    assert (D.cardinal man.local = 1);
    let cd = D.choose man.local in
    let k x y =
      if M.tracing then M.traceli "combine" "function: %a" Spec.D.pretty x;
      try
        let r = Spec.combine_assign (conv man cd) l fe f a fc x f_ask in
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

  let init marshal =
    init marshal;
    AnalysisState.unsound_both_branches_dead := Some false

  let conv (man: (_, G.t, _, V.t) man): (_, S.G.t, _, S.V.t) man =
    { man with
      global = (fun v -> G.s (man.global (V.s v)));
      sideg = (fun v g -> man.sideg (V.s v) (G.create_s g));
    }

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | WarnGlobal g ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv man) (WarnGlobal (Obj.repr g))
        | `Right g ->
          let em = G.node (man.global (V.node g)) in
          EM.iter (fun exp tv ->
              match tv with
              | `Lifted tv ->
                let loc = Node.location g in (* TODO: looking up location now doesn't work nicely with incremental *)
                let cilinserted = if loc.synthetic then "(possibly inserted by CIL) " else "" in
                M.warn ~loc:(Node g) ~tags:[CWE (if tv then 571 else 570)] ~category:Deadcode "condition '%a' %sis always %B" d_exp exp cilinserted tv
              | `Bot when not (CilType.Exp.equal exp one) -> (* all branches dead *)
                AnalysisState.unsound_both_branches_dead := Some true;
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
          S.query (conv man) (InvariantGlobal (Obj.repr g))
        | `Right g ->
          Queries.Result.top q
      end
    | YamlEntryGlobal (g, task) ->
      let g: V.t = Obj.obj g in
      begin match g with
        | `Left g ->
          S.query (conv man) (YamlEntryGlobal (Obj.repr g, task))
        | `Right g ->
          Queries.Result.top q
      end
    | IterSysVars (vq, vf) ->
      (* vars for S *)
      let vf' x = vf (Obj.repr (V.s (Obj.obj x))) in
      S.query (conv man) (IterSysVars (vq, vf'));

      (* node vars for dead branches *)
      begin match vq with
        | Node {node; _} ->
          vf (Obj.repr (V.node node))
        | _ ->
          ()
      end
    | _ ->
      S.query (conv man) q


  let branch man = S.branch (conv man)
  let context man = S.context (conv man)

  let branch man exp tv =
    if !AnalysisState.postsolving then (
      try
        let r = branch man exp tv in
        (* branch is live *)
        man.sideg (V.node man.prev_node) (G.create_node (EM.singleton exp (`Lifted tv))); (* record expression with reached tv *)
        r
      with Deadcode ->
        (* branch is dead *)
        man.sideg (V.node man.prev_node) (G.create_node (EM.singleton exp `Bot)); (* record expression without reached tv *)
        raise Deadcode
    )
    else (
      man.sideg (V.node man.prev_node) (G.create_node (EM.bot ())); (* create global variable during solving, to allow postsolving leq hack to pass verify *)
      branch man exp tv
    )

  let assign man = S.assign (conv man)
  let vdecl man = S.vdecl (conv man)
  let enter man = S.enter (conv man)
  let paths_as_set man = S.paths_as_set (conv man)
  let body man = S.body (conv man)
  let return man = S.return (conv man)
  let combine_env man = S.combine_env (conv man)
  let combine_assign man = S.combine_assign (conv man)
  let special man = S.special (conv man)
  let threadenter man = S.threadenter (conv man)
  let threadspawn man ~multiple lv f args fman = S.threadspawn (conv man) ~multiple lv f args (conv fman)
  let sync man = S.sync (conv man)
  let skip man = S.skip (conv man)
  let asm man = S.asm (conv man)
  let event man e oman = S.event (conv man) e (conv oman)
end
