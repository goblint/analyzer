(** How to generate constraints for a solver using specifications described in [Analyses]. *)

open Prelude
open Cil
open MyCFG
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
  module V = S.V

  let name () = S.name () ^" hashconsed"

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init = S.init
  let finalize = S.finalize

  let should_join x y = S.should_join (D.unlift x) (D.unlift y)

  let startstate v = D.lift (S.startstate v)
  let exitstate  v = D.lift (S.exitstate  v)
  let morphstate v d = D.lift (S.morphstate v (D.unlift d))

  let context fd = S.context fd % D.unlift
  let call_descr = S.call_descr

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d es -> ctx.split (D.lift d) es )
    }

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
    List.map D.lift @@ S.threadenter (conv ctx) lval f args

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
  module V = S.V

  let name () = S.name () ^" context hashconsed"

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init = S.init
  let finalize = S.finalize

  let should_join = S.should_join

  let startstate = S.startstate
  let exitstate  = S.exitstate
  let morphstate = S.morphstate

  let context fd = C.lift % S.context fd
  let call_descr f = S.call_descr f % C.unlift

  let conv ctx =
    { ctx with context = (fun () -> C.unlift (ctx.context ())) }

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
    S.combine (conv ctx) r fe f args (Option.map C.unlift fc) es

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
  module V = S.V

  let name () = S.name ()^" level sliced"

  let start_level = ref (`Top)

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init marshal =
    if get_bool "dbg.slice.on" then
      start_level := `Lifted (Int64.of_int (get_int "dbg.slice.n"));
    S.init marshal

  let finalize = S.finalize

  let should_join (x,_) (y,_) = S.should_join x y

  let startstate v = (S.startstate v, !start_level)
  let exitstate  v = (S.exitstate  v, !start_level)
  let morphstate v (d,l) = (S.morphstate v d, l)

  let context fd (d,_) = S.context fd d
  let call_descr f = S.call_descr f

  let conv ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es )
    }

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
  let intrpt ctx      = lift_fun ctx (lift ctx) S.intrpt identity
  let asm ctx         = lift_fun ctx (lift ctx) S.asm    identity
  let skip ctx        = lift_fun ctx (lift ctx) S.skip   identity
  let special ctx r f args        = lift_fun ctx (lift ctx) S.special ((|>) args % (|>) f % (|>) r)
  let combine' ctx r fe f args fc es = lift_fun ctx (lift ctx) S.combine (fun p -> p r fe f args fc (fst es))

  let threadenter ctx lval f args = lift_fun ctx (List.map lift_start_level) S.threadenter ((|>) args % (|>) f % (|>) lval)
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

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.EvalFunvar e ->
      let (d,l) = ctx.local in
      if leq0 l then
        Queries.LS.empty ()
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
        if v >= !limit then failwith ("LimitLifter: Reached limit ("^string_of_int !limit^") for node "^Ana.sprint Node.pretty_plain_short (Option.get !MyCFG.current_node));
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


  let name () = S.name ()^" with widened contexts"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let should_join (x,_) (y,_) = S.should_join x y

  let inj f x = f x, M.bot ()

  let startstate = inj S.startstate
  let exitstate  = inj S.exitstate
  let morphstate v (d,m) = S.morphstate v d, m

  let context fd (d,m) = S.context fd d (* just the child analysis' context *)
  let call_descr = S.call_descr

  let conv ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es )
    }
  let lift_fun ctx f g = g (f (conv ctx)), snd ctx.local

  let sync ctx reason = lift_fun ctx S.sync   ((|>) reason)
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

  let threadenter ctx lval f args = S.threadenter (conv ctx) lval f args |> List.map (fun d -> (d, snd ctx.local))
  let threadspawn ctx lval f args fctx = lift_fun ctx S.threadspawn ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)

  let enter ctx r f args =
    let m = snd ctx.local in
    let d' v_cur =
      if ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.context.widen" ~keepAttr:"widen" ~removeAttr:"no-widen" f then (
        let v_old = M.find f.svar m in (* S.D.bot () if not found *)
        let v_new = S.D.widen v_old (S.D.join v_old v_cur) in
        Messages.(if tracing && not (S.D.equal v_old v_new) then tracel "widen-context" "enter results in new context for function %s\n" f.svar.vname);
        v_new, M.add f.svar v_new m
      )
      else
        v_cur, m
    in
    S.enter (conv ctx) r f args
    |> List.map (fun (c,v) -> (c,m), d' v) (* c: caller, v: callee *)

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
  module V = S.V

  let name () = S.name ()^" lifted"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let should_join x y =
    match x, y with
    | `Lifted a, `Lifted b -> S.should_join a b
    | _ -> true

  let startstate v = `Lifted (S.startstate v)
  let exitstate  v = `Lifted (S.exitstate  v)
  let morphstate v d = try `Lifted (S.morphstate v (D.unlift d)) with Deadcode -> d

  let context fd = S.context fd % D.unlift
  let call_descr f = S.call_descr f

  let conv ctx =
    { ctx with local = D.unlift ctx.local
             ; split = (fun d es -> ctx.split (D.lift d) es )
    }

  let lift_fun ctx f g h b =
    try f @@ h (g (conv ctx))
    with Deadcode -> b

  let sync ctx reason = lift_fun ctx D.lift   S.sync   ((|>) reason)      `Bot

  let enter ctx r f args =
    let liftmap = List.map (fun (x,y) -> D.lift x, D.lift y) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r) []

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    lift_fun ctx identity S.query (fun (x) -> x q) (Queries.Result.bot q)
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

  let threadenter ctx lval f args = lift_fun ctx (List.map D.lift) S.threadenter ((|>) args % (|>) f % (|>) lval) []
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
                           and module GVar = GVarF (S.V)
                           and module D = S.D
                           and module G = S.G
    val tf : MyCFG.node * S.C.t -> (Cil.location * MyCFG.edge) list * MyCFG.node -> ((MyCFG.node * S.C.t) -> S.D.t) -> (MyCFG.node * S.C.t -> S.D.t -> unit) -> (GVar.t -> G.t) -> (GVar.t -> G.t -> unit) -> D.t
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
  module G = S.G

  (* Dummy module. No incremental analysis supported here*)
  let increment = I.increment

  let sync ctx =
    match Cfg.prev ctx.prev_node with
    | _ :: _ :: _ -> S.sync ctx `Join
    | _ -> S.sync ctx `Normal

  let common_ctx var edge prev_node pval (getl:lv -> ld) sidel getg sideg : (D.t, G.t, S.C.t, S.V.t) ctx * D.t list ref * (lval option * varinfo * exp list * D.t) list ref =
    let r = ref [] in
    let spawns = ref [] in
    (* now watch this ... *)
    let rec ctx =
      { ask     = (fun (type a) (q: a Queries.t) -> S.query ctx q)
      ; emit    = (fun _ -> failwith "emit outside MCP")
      ; node    = fst var
      ; prev_node = prev_node
      ; control_context = snd var
      ; context = snd var |> Obj.obj
      ; edge    = edge
      ; local   = pval
      ; global  = getg
      ; presub  = (fun _ -> raise Not_found)
      ; postsub = (fun _ -> raise Not_found)
      ; spawn   = spawn
      ; split   = (fun (d:D.t) es -> assert (List.is_empty es); r := d::!r)
      ; sideg   = sideg
      }
    and spawn lval f args =
      (* TODO: adjust ctx node/edge? *)
      (* TODO: don't repeat for all paths that spawn same *)
      let ds = S.threadenter ctx lval f args in
      List.iter (fun d ->
          spawns := (lval, f, args, d) :: !spawns;
          match Cilfacade.find_varinfo_fundec f with
          | fd ->
            let c = S.context fd d in
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
      let one_spawn (lval, f, args, fd) =
        let rec fctx =
          { ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query fctx q)
          ; local = fd
          }
        in
        S.threadspawn ctx' lval f args fctx
      in
      bigsqcup (List.map one_spawn spawns)

  let common_join ctx d splits spawns =
    thread_spawns ctx (bigsqcup (d :: splits)) spawns

  let common_joins ctx ds splits spawns = common_join ctx (bigsqcup ds) splits spawns

  let tf_loop var edge prev_node getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.intrpt ctx) !r !spawns

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
         (get_bool "kernel" || get_string "ana.osek.oil" <> "")
      then toplevel_kernel_return ret fd ctx sideg
      else normal_return ret fd ctx sideg
    in
    common_join ctx d !r !spawns

  let tf_entry var edge prev_node fd getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.body ctx fd) !r !spawns

  let tf_test var edge prev_node e tv getl sidel getg sideg d =
    let ctx, r, spawns = common_ctx var edge prev_node d getl sidel getg sideg in
    common_join ctx (S.branch ctx e tv) !r !spawns

  let tf_normal_call ctx lv e (f:fundec) args  getl sidel getg sideg =
    let combine (cd, fc, fd) =
      if M.tracing then M.traceli "combine" "local: %a\n" S.D.pretty cd;
      (* Extra sync in case function has multiple returns.
         Each `Return sync is done before joining, so joined value may be unsound.
         Since sync is normally done before tf (in common_ctx), simulate it here for fd. *)
      (* TODO: don't do this extra sync here *)
      let fd =
        (* TODO: more accurate ctx? *)
        let rec sync_ctx = { ctx with
            ask = (fun (type a) (q: a Queries.t) -> S.query sync_ctx q);
            local = fd;
            prev_node = Function f
          }
        in
        sync sync_ctx
      in
      if M.tracing then M.trace "combine" "function: %a\n" S.D.pretty fd;
      let r = S.combine {ctx with local = cd} lv e f args fc fd in
      if M.tracing then M.traceu "combine" "combined local: %a\n" S.D.pretty r;
      r
    in
    let paths = S.enter ctx lv f args in
    let paths = List.map (fun (c,v) -> (c, S.context f v, v)) paths in
    List.iter (fun (c,fc,v) -> if not (S.D.is_bot v) then sidel (FunctionEntry f, fc) v) paths;
    let paths = List.map (fun (c,fc,v) -> (c, fc, if S.D.is_bot v then v else getl (Function f, fc))) paths in
    let paths = List.filter (fun (c,fc,v) -> not (D.is_bot v)) paths in
    let paths = List.map (Tuple3.map2 Option.some) paths in
    if M.tracing then M.traceli "combine" "combining\n";
    let paths = List.map combine paths in
    let r = List.fold_left D.join (D.bot ()) paths in
    if M.tracing then M.traceu "combine" "combined: %a\n" S.D.pretty r;
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
        let ls = ctx.ask (Queries.EvalFunvar e) in
        Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls []
    in
    let one_function f =
      match Cilfacade.find_varinfo_fundec f with
      | fd when LibraryFunctions.use_special f.vname ->
        M.warn "Using special for defined function %s" f.vname;
        tf_special_call ctx lv f args
      | fd ->
        tf_normal_call ctx lv e fd args getl sidel getg sideg
      | exception Not_found ->
        tf_special_call ctx lv f args
    in
    if [] = functions then
      d (* because LevelSliceLifter *)
    else
      let funs = List.map one_function functions in
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
    let _, locs = List.fold_right (fun (f,e) (t,xs) -> f, (f,t)::xs) edges (Node.location v,[]) in
    List.fold_left2 (|>) pval (List.map (tf (v,Obj.repr (fun () -> c)) getl sidel getg sideg u) edges) locs

  let tf (v,c) (e,u) getl sidel getg sideg =
    let old_node = !current_node in
    let old_context = !M.current_context in
    let _       = current_node := Some u in
    M.current_context := Some (Obj.repr c);
    let d       = tf (v,c) (e,u) getl sidel getg sideg in
    let _       = current_node := old_node in
    M.current_context := old_context;
    d

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
          if List.for_all S.D.is_bot xs_stricts then
            S.D.bot ()
          else
            let xs_strict = List.fold_left S.D.join (S.D.bot ()) xs_stricts in
            let equal = [%eq: (CilType.Location.t * Edge.t) list * Node.t] in
            let is_strict eu = List.exists (equal eu) stricts in
            let non_stricts = List.filter (neg is_strict) (Cfg.prev v) in
            let xs_non_stricts = List.map tf' non_stricts in
            List.fold_left S.D.join xs_strict xs_non_stricts
        | _ ->
          let xs = List.map tf' (Cfg.prev v) in
          List.fold_left S.D.join (S.D.bot ()) xs
      in
      Some tf
end

(** Convert a non-incremental solver into an "incremental" solver.
    It will solve from scratch, perform standard postsolving and have no marshal data. *)
module EqIncrSolverFromEqSolver (Sol: GenericEqBoxSolver): GenericEqBoxIncrSolver =
  functor (Arg: IncrSolverArg) (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  struct
    module Sol = Sol (S) (VH)
    module Post = PostSolver.MakeList (PostSolver.ListArgFromStdArg (S) (VH) (Arg))

    type marshal = unit

    let solve box xs vs =
      let vh = Sol.solve box xs vs in
      Post.post xs vs vh;
      (vh, ())
  end

(** Combined variables so that we can also use the more common [EqConstrSys]
    that uses only one kind of a variable. *)
module Var2 (LV:VarType) (GV:VarType)
  : VarType
    with type t = [ `L of LV.t  | `G of GV.t ]
=
struct
  type t = [ `L of LV.t  | `G of GV.t ] [@@deriving eq, ord, hash]
  let relift = function
    | `L x -> `L (LV.relift x)
    | `G x -> `G (GV.relift x)

  let pretty_trace () = function
    | `L a -> LV.pretty_trace () a
    | `G a -> GV.pretty_trace () a

  let printXml f = function
    | `L a -> LV.printXml f a
    | `G a -> GV.printXml f a

  let var_id = function
    | `L a -> LV.var_id a
    | `G a -> GV.var_id a

  let node = function
    | `L a -> LV.node a
    | `G a -> GV.node a
end

(** Translate a [GlobConstrSys] into a [EqConstrSys] *)
module EqConstrSysFromGlobConstrSys (S:GlobConstrSys)
  : EqConstrSys   with type v = Var2(S.LVar)(S.GVar).t
                   and type d = Lattice.Lift2(S.G)(S.D)(Printable.DefaultNames).t
                   and module Var = Var2(S.LVar)(S.GVar)
                   and module Dom = Lattice.Lift2(S.G)(S.D)(Printable.DefaultNames)
=
struct
  module Var = Var2(S.LVar)(S.GVar)
  module Dom =
  struct
    include Lattice.Lift2(S.G)(S.D)(Printable.DefaultNames)
    let printXml f = function
      | `Lifted1 a -> S.G.printXml f a
      | `Lifted2 a -> S.D.printXml f a
      | (`Bot | `Top) as x -> printXml f x
  end
  let increment = S.increment
  type v = Var.t
  type d = Dom.t

  let box f x y = if Dom.leq y x then Dom.narrow x y else Dom.widen x (Dom.join x y)

  let getG = function
    | `Lifted1 x -> x
    | `Bot -> S.G.bot ()
    | `Top -> failwith "EqConstrSysFromGlobConstrSys.getG: global variable has top value"
    | `Lifted2 _ -> failwith "EqConstrSysFromGlobConstrSys.getG: global variable has local value"

  let getL = function
    | `Lifted2 x -> x
    | `Bot -> S.D.bot ()
    | `Top -> failwith "EqConstrSysFromGlobConstrSys.getL: local variable has top value"
    | `Lifted1 _ -> failwith "EqConstrSysFromGlobConstrSys.getL: local variable has global value"

  let l, g = (fun x -> `L x), (fun x -> `G x)
  let lD, gD = (fun x -> `Lifted2 x), (fun x -> `Lifted1 x)

  let conv f get set =
    f (getL % get % l) (fun x v -> set (l x) (lD v))
      (getG % get % g) (fun x v -> set (g x) (gD v))
    |> lD

  let system = function
    | `G _ -> None
    | `L x -> Option.map conv (S.system x)
end

(** Splits a [EqConstrSys] solution into a [GlobConstrSys] solution with given [Hashtbl.S] for the [EqConstrSys]. *)
module GlobConstrSolFromEqConstrSolBase (S: GlobConstrSys) (LH: Hashtbl.S with type key = S.LVar.t) (GH: Hashtbl.S with type key = S.GVar.t) (VH: Hashtbl.S with type key = Var2 (S.LVar) (S.GVar).t) =
struct
  let split_solution hm =
    let l' = LH.create 113 in
    let g' = GH.create 113 in
    let split_vars x d = match x with
      | `L x ->
        begin match d with
          | `Lifted2 d -> LH.replace l' x d
          (* | `Bot -> () *)
          (* Since Verify2 is broken and only checks existing keys, add it with local bottom value.
            This works around some cases, where Verify2 would not detect a problem due to completely missing variable. *)
          | `Bot -> LH.replace l' x (S.D.bot ())
          | `Top -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: local variable has top value"
          | `Lifted1 _ -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: local variable has global value"
        end
      | `G x ->
        begin match d with
          | `Lifted1 d -> GH.replace g' x d
          | `Bot -> ()
          | `Top -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: global variable has top value"
          | `Lifted2 _ -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: global variable has local value"
        end
    in
    VH.iter split_vars hm;
    (l', g')
end

(** Splits a [EqConstrSys] solution into a [GlobConstrSys] solution. *)
module GlobConstrSolFromEqConstrSol (S: GlobConstrSys) (LH: Hashtbl.S with type key = S.LVar.t) (GH: Hashtbl.S with type key = S.GVar.t) =
struct
  module S2 = EqConstrSysFromGlobConstrSys (S)
  module VH = Hashtbl.Make (S2.Var)

  include GlobConstrSolFromEqConstrSolBase (S) (LH) (GH) (VH)
end

(** Transforms a [GenericEqBoxIncrSolver] into a [GenericGlobSolver]. *)
module GlobSolverFromEqSolver (Sol:GenericEqBoxIncrSolverBase)
  : GenericGlobSolver
  = functor (S:GlobConstrSys) ->
    functor (LH:Hashtbl.S with type key=S.LVar.t) ->
    functor (GH:Hashtbl.S with type key=S.GVar.t) ->
    struct
      module EqSys = EqConstrSysFromGlobConstrSys (S)

      module VH : Hashtbl.S with type key=EqSys.v = Hashtbl.Make(EqSys.Var)
      module Sol' = Sol (EqSys) (VH)

      module Splitter = GlobConstrSolFromEqConstrSolBase (S) (LH) (GH) (VH) (* reuse EqSys and VH *)

      type marshal = Sol'.marshal

      let solve ls gs l =
        let vs = List.map (fun (x,v) -> `L x, `Lifted2 v) ls
                 @ List.map (fun (x,v) -> `G x, `Lifted1 v) gs in
        let sv = List.map (fun x -> `L x) l in
        let hm, solver_data = Sol'.solve EqSys.box vs sv in
        Splitter.split_solution hm, solver_data
    end


(** Add path sensitivity to a analysis *)
module PathSensitive2 (Spec:Spec)
  : Spec
    with type D.t = HoareDomain.Set(Spec.D).t
     and module G = Spec.G
     and module C = Spec.C
     and module V = Spec.V
=
struct
  module D =
  struct
    include HoareDomain.Set (Spec.D) (* TODO is it really worth it to check every time instead of just using sets and joining later? *)
    let name () = "PathSensitive (" ^ name () ^ ")"

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
  module V = Spec.V

  let name () = "PathSensitive2("^Spec.name ()^")"

  type marshal = Spec.marshal
  let init = Spec.init
  let finalize = Spec.finalize

  let should_join x y = true

  let exitstate  v = D.singleton (Spec.exitstate  v)
  let startstate v = D.singleton (Spec.startstate v)
  let morphstate v d = D.map (Spec.morphstate v) d

  let call_descr = Spec.call_descr

  let context fd l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive2.context must be called with a singleton set."
    else
      Spec.context fd @@ D.choose l

  let conv ctx x =
    let rec ctx' = { ctx with ask   = (fun (type a) (q: a Queries.t) -> Spec.query ctx' q)
                            ; local = x
                            ; split = (ctx.split % D.singleton) }
    in
    ctx'

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
  let intrpt ctx        = map ctx Spec.intrpt  identity
  let asm ctx           = map ctx Spec.asm     identity
  let skip ctx          = map ctx Spec.skip    identity
  let special ctx l f a = map ctx Spec.special (fun h -> h l f a)

  let threadenter ctx lval f args =
    let g xs ys = (List.map (fun y -> D.singleton y) ys) @ xs in
    fold' ctx Spec.threadenter (fun h -> h lval f args) g []
  let threadspawn ctx lval f args fctx =
    let fd1 = D.choose fctx.local in
    map ctx Spec.threadspawn (fun h -> h lval f args (conv fctx fd1))

    let sync ctx reason = map ctx Spec.sync (fun h -> h reason)

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    (* join results so that they are sound for all paths *)
    let module Result = (val Queries.Result.lattice q) in
    fold' ctx Spec.query identity (fun x f -> Result.join x (f q)) (Result.bot ())

  let enter ctx l f a =
    let g xs ys = (List.map (fun (x,y) -> D.singleton x, D.singleton y) ys) @ xs in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let combine ctx l fe f a fc d =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      if M.tracing then M.traceli "combine" "function: %a\n" Spec.D.pretty x;
      try
        let r = Spec.combine (conv ctx cd) l fe f a fc x in
        if M.tracing then M.traceu "combine" "combined function: %a\n" Spec.D.pretty r;
        D.add r y
      with Deadcode ->
        if M.tracing then M.traceu "combine" "combined function: dead\n";
        y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d
end

module DeadBranchLifter (S: Spec): Spec =
struct
  include S

  let name () = "DeadBranch (" ^ name () ^ ")"

  module Locmap = Deadcode.Locmap

  let dead_branches = function true -> Deadcode.dead_branches_then | false -> Deadcode.dead_branches_else

  let branch ctx exp tv =
    if !GU.postsolving then (
      Locmap.replace Deadcode.dead_branches_cond !Tracing.current_loc exp;
      try
        let r = branch ctx exp tv in
        (* branch is live *)
        Locmap.replace (dead_branches tv) !Tracing.current_loc false; (* set to live (false) *)
        r
      with Deadcode ->
        (* branch is dead *)
        Locmap.modify_def true !Tracing.current_loc Fun.id (dead_branches tv); (* set to dead (true) if not mem, otherwise keep existing (Fun.id) since it may be live (false) in another context *)
        raise Deadcode
    )
    else
      branch ctx exp tv
end

module CompareGlobSys
    (S:Spec)
    (Sys:GlobConstrSys with module LVar = VarF (S.C)
                        and module GVar = GVarF (S.V)
                        and module D = S.D
                        and module G = S.G)
    (LH:Hashtbl.S with type key=Sys.LVar.t)
    (GH:Hashtbl.S with type key=Sys.GVar.t)
=
struct
  open S

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
          ignore (Pretty.printf "Global %a is more precise using left:\n%a\n" Sys.GVar.pretty_trace k G.pretty_diff (v1,v2));
        f_le ()
      end else if b2 then begin
        if get_bool "dbg.compare_runs.diff" then
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
          if get_bool "dbg.compare_runs.diff" then
            ignore (Pretty.printf "%a @@ %a is more precise using left:\n%a\n" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v1,v2));
          incr le
        end else if b2 then begin
          if get_bool "dbg.compare_runs.diff" then
            ignore (Pretty.printf "%a @@ %a is more precise using right:\n%a\n" Node.pretty_plain k CilType.Location.pretty (Node.location k) D.pretty_diff (v1,v2));
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
          (* if get_bool "dbg.compare_runs.diff" then *)
          (*   ignore (Pretty.printf "%a @@ %a is more precise using left:\n%a\n" pretty_node k CilType.Location.pretty (getLoc k) D.pretty_diff (v1,v2)); *)
          f_le ()
        end else if b2 then begin
          (* if get_bool "dbg.compare_runs.diff" then *)
          (*   ignore (Pretty.printf "%a @@ %a is more precise using right:\n%a\n" pretty_node k CilType.Location.pretty (getLoc k) D.pretty_diff (v1,v2)); *)
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
    Printf.printf "\nComparing GlobConstrSys precision of %s (left) with %s (right):\n" name1 name2;
    compare_globals g1 g2;
    compare_locals h1 h2;
    compare_locals_ctx l1 l2;
    print_newline ();
end

module CompareHashtbl (Var: VarType) (Dom: Lattice.S) (VH: Hashtbl.S with type key = Var.t) =
struct
  module Var =
  struct
    include Printable.Std
    include Var

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
    Printf.printf "\nComparing EqConstrSys precision of %s (left) with %s (right):\n" name1 name2;
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1 ~name2 vh2 in
    ignore (Pretty.printf "EqConstrSys comparison summary: %t\n" (fun () -> msg));
    print_newline ();
end

module CompareGlobal (GVar: VarType) (G: Lattice.S) (GH: Hashtbl.S with type key = GVar.t) =
struct
  module Compare = CompareHashtbl (GVar) (G) (GH)

  let compare (name1, name2) vh1 vh2 =
    Printf.printf "\nComparing globals precision of %s (left) with %s (right):\n" name1 name2;
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1 ~name2 vh2 in
    ignore (Pretty.printf "Globals comparison summary: %t\n" (fun () -> msg));
    print_newline ();
end

module CompareNode (C: Printable.S) (D: Lattice.S) (LH: Hashtbl.S with type key = VarF (C).t) =
struct
  module Node =
  struct
    include Node
    let var_id _ = "nodes"
    let node x = x
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
    Printf.printf "\nComparing nodes precision of %s (left) with %s (right):\n" name1 name2;
    let vh1' = join_contexts vh1 in
    let vh2' = join_contexts vh2 in
    let verbose = get_bool "dbg.compare_runs.diff" in
    let (_, msg) = Compare.compare ~verbose ~name1 vh1' ~name2 vh2' in
    ignore (Pretty.printf "Nodes comparison summary: %t\n" (fun () -> msg));
    print_newline ();
end
