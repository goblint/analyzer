(** An analysis specification for witnesses. *)

open Prelude.Ana
open Analyses

module Node: Printable.S with type t = MyCFG.node =
struct
  include Var
  let to_yojson = Node.to_yojson

  (* let short n x = Pretty.sprint n (pretty () x) *)
  (* let short _ x = var_id x *)
  let show = Node.show_cfg
  let pretty = Node.pretty_trace
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let name () = "var"
  let tag _ = failwith "PrintableVar: no tag"
  let arbitrary () = failwith "PrintableVar: no arbitrary"
end

module Edge: Printable.S with type t = MyARG.inline_edge =
struct
  type t = MyARG.inline_edge [@@deriving to_yojson] (* TODO: deriving gets overridden *)

  let equal = Util.equals
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let name () = "edge"

  let pretty = MyARG.pretty_inline_edge
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let tag _ = failwith "Edge: no tag"
  let arbitrary () = failwith "Edge: no arbitrary"
  let relift x = x
end


(** Add path sensitivity to a analysis *)
module PathSensitive3 (Spec:Spec)
  : Spec
    (* with type D.t = SetDomain.ToppedSet(Spec.D)(N).t
     and module G = Spec.G
     and module C = Spec.C *)
=
struct
  (* module I = IntDomain.Integers *)
  module I =
  struct
    include Spec.D
    (* assumes Hashcons inside PathSensitive *)
    let to_int = tag
    let name () = "D"
    let printXml f d = BatPrintf.fprintf f "<value>%a</value>" printXml d
  end
  module CC =
  struct
    include Spec.C
    let name () = "C"
    let printXml f c = BatPrintf.fprintf f "<value>%a</value>" printXml c
  end
  module VI = Printable.Prod3 (Node) (CC) (I)
  module VIE =
  struct
    include Printable.Prod (VI) (Edge)

    let leq ((v, c, x'), e) ((w, d, y'), f) =
      Node.equal v w && Spec.C.equal c d && I.leq x' y' && Edge.equal e f

    (* TODO: join and meet can be implemented, but are they necessary at all? *)
    let join _ _ = failwith "VIE join"
    let meet _ _ = failwith "VIE meet"
    (* widen and narrow are needed for Hoare widen and narrow *)
    (* TODO: use I ops for these if HoareMap gets proper widen *)
    let widen x y = y
    let narrow x y = x
    let top () = failwith "VIE top"
    let is_top _ = failwith "VIE is_top"
    let bot () = failwith "VIE bot"
    let is_bot _ = failwith "VIE is_bot"

    let pretty_diff () (((v, c, x'), e), ((w, d, y'), f)) =
      if not (Node.equal v w) then
        Pretty.dprintf "%a not equal %a" Node.pretty v Node.pretty w
      else if not (Spec.C.equal c d) then
        Pretty.dprintf "%a not equal %a" Spec.C.pretty c Spec.C.pretty d
      else if not (Edge.equal e f) then
        Pretty.dprintf "%a not equal %a" Edge.pretty e Edge.pretty f
      else
        I.pretty_diff () (x', y')
  end
  (* Bot is needed for Hoare widen *)
  (* TODO: could possibly rewrite Hoare to avoid introducing bots in widen which get reduced away anyway? *)
  module VIEB = Lattice.LiftBot (VIE)
  module VIES = HoareDomain.Set (VIEB)

  module R = VIES

  module SpecDMap (R: Lattice.S) =
  struct
    module C =
    struct
      type elt = Spec.D.t
      let cong = Spec.should_join
    end
    module J = MapDomain.Joined (Spec.D) (R)
    include SensitiveDomain.PairwiseMap (Spec.D) (R) (J) (C)
  end

  module Dom =
  struct
    include SpecDMap (R)

    let name () = "PathSensitive (" ^ name () ^ ")"

    let printXml f x =
      let print_one x r =
        (* BatPrintf.fprintf f "\n<path>%a</path>" Spec.D.printXml x *)
        BatPrintf.fprintf f "\n<path>%a<analysis name=\"witness\">%a</analysis></path>" Spec.D.printXml x R.printXml r
      in
      iter print_one x

    (* join elements in the same partition (specified by should_join) *)
    (* let join_reduce a =
      let rec loop js = function
        | [] -> js
        | (x, xr)::xs -> let ((j, jr),r) = List.fold_left (fun ((j, jr),r) (x,xr) ->
            if Spec.should_join x j then (Spec.D.join x j, R.join xr jr), r else (j, jr), (x, xr)::r
          ) ((x, xr),[]) xs in
          loop ((j, jr)::js) r
      in
      apply_list (loop []) a

    let leq a b =
      leq a b || leq (join_reduce a) (join_reduce b)

    let binop op a b = op a b |> join_reduce

    let join = binop join
    let meet = binop meet
    let widen = binop widen
    let narrow = binop narrow *)

    let map_keys f m =
      fold (fun e r acc ->
          add (f e) r acc
        ) m (empty ())
    let choose_key m = fst (choose m)
    let fold_keys f m a = fold (fun e _ acc -> f e acc) m a
  end

  (* Additional dependencies component between values before and after sync.
   * This is required because some analyses (e.g. region) do sideg through local domain diff and sync.
   * sync is automatically applied in FromSpec before any transition, so previous values may change (diff is flushed). *)
  module SyncSet = HoareDomain.Set (Spec.D)
  module Sync = SpecDMap (SyncSet)
  module D =
  struct
    include Lattice.Prod (Dom) (Sync)

    let printXml f (d, _) = Dom.printXml f d
  end

  module G = Spec.G
  module C = Spec.C
  module V = Spec.V

  let name () = "PathSensitive3("^Spec.name ()^")"

  type marshal = Spec.marshal
  let init = Spec.init
  let finalize = Spec.finalize

  let should_join x y = true

  let exitstate  v = (Dom.singleton (Spec.exitstate  v) (R.bot ()), Sync.bot ())
  let startstate v = (Dom.singleton (Spec.startstate v) (R.bot ()), Sync.bot ())
  let morphstate v (d, _) = (Dom.map_keys (Spec.morphstate v) d, Sync.bot ())

  let call_descr = Spec.call_descr

  let context fd (l, _) =
    if Dom.cardinal l <> 1 then
      failwith "PathSensitive3.context must be called with a singleton set."
    else
      Spec.context fd @@ Dom.choose_key l

  let conv ctx x =
    (* TODO: R.bot () isn't right here *)
    let rec ctx' = { ctx with ask   = (fun (type a) (q: a Queries.t) -> Spec.query ctx' q)
                            ; local = x
                            ; split = (ctx.split % (fun x -> (Dom.singleton x (R.bot ()), Sync.bot ()))) }
    in
    ctx'

  let step n c i e = R.singleton (`Lifted ((n, c, i), e))
  let step n c i e sync =
    SyncSet.fold (fun xsync acc ->
        R.join acc (step n c xsync e)
      ) (Sync.find i sync) (R.bot ())
  let step_ctx ctx x e =
    try
      step ctx.prev_node (ctx.context ()) x e (snd ctx.local)
    with Ctx_failure _ ->
      R.bot ()
  let step_ctx_edge ctx x = step_ctx ctx x (CFGEdge ctx.edge)

  let map ctx f g =
    let h x xs =
      try Dom.add (g (f (conv ctx x))) (step_ctx_edge ctx x) xs
      with Deadcode -> xs
    in
    let d = Dom.fold_keys h (fst ctx.local) (Dom.empty ()) in
    if Dom.is_bot d then raise Deadcode else (d, Sync.bot ())

  let fold' ctx f g h a =
    let k x a =
      try h a x @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    Dom.fold_keys k (fst ctx.local) a

  let fold'' ctx f g h a =
    let k x r a =
      try h a x r @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    Dom.fold k (fst ctx.local) a

  let assign ctx l e    = map ctx Spec.assign  (fun h -> h l e )
  let vdecl ctx v       = map ctx Spec.vdecl   (fun h -> h v)
  let body   ctx f      = map ctx Spec.body    (fun h -> h f   )
  let return ctx e f    = map ctx Spec.return  (fun h -> h e f )
  let branch ctx e tv   = map ctx Spec.branch  (fun h -> h e tv)
  let asm ctx           = map ctx Spec.asm     identity
  let skip ctx          = map ctx Spec.skip    identity
  let special ctx l f a = map ctx Spec.special (fun h -> h l f a)

  (* TODO: do additional witness things here *)
  let threadenter ctx lval f args =
    let g xs x' ys =
      let ys' = List.map (fun y ->
          (* R.bot () isn't right here? doesn't actually matter? *)
          let yr = R.bot () in
          (* keep left syncs so combine gets them for no-inline case *)
          (Dom.singleton y yr, Sync.bot ())
        ) ys
      in
      ys' @ xs
    in
    fold' ctx Spec.threadenter (fun h -> h lval f args) g []
  let threadspawn ctx lval f args fctx =
    let fd1 = Dom.choose_key (fst fctx.local) in
    map ctx Spec.threadspawn (fun h -> h lval f args (conv fctx fd1))

  let sync ctx reason =
    fold'' ctx Spec.sync (fun h -> h reason) (fun (a, async) x r a' ->
        (Dom.add a' r a, Sync.add a' (SyncSet.singleton x) async)
      ) (Dom.empty (), Sync.bot ())

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IterPrevVars f ->
      Dom.iter (fun x r ->
          R.iter (function
              | `Lifted ((n, c, j), e) ->
                f (I.to_int x) (n, Obj.repr c, I.to_int j) e
              | `Bot ->
                failwith "PathSensitive3.query: range contains bot"
            ) r
        ) (fst ctx.local);
      (* check that sync mappings don't leak into solution (except Function) *)
      begin match ctx.node with
        | Function _ -> () (* returns post-sync in FromSpec *)
        | _ -> assert (Sync.is_bot (snd ctx.local));
      end;
      ()
    | Queries.IterVars f ->
      Dom.iter (fun x r ->
          f (I.to_int x)
        ) (fst ctx.local);
      ()
    | Queries.Invariant ({path=Some i; _} as c) ->
      (* TODO: optimize indexing, using inner hashcons somehow? *)
      (* let (d, _) = List.at (S.elements s) i in *)
      let (d, _) = List.find (fun (x, _) -> I.to_int x = i) (Dom.bindings (fst ctx.local)) in
      Spec.query (conv ctx d) (Invariant c)
    | _ ->
      (* join results so that they are sound for all paths *)
      let module Result = (val Queries.Result.lattice q) in
      fold' ctx Spec.query identity (fun x _ f -> Result.join x (f q)) (Result.bot ())

  let should_inline f =
    (* (* inline __VERIFIER_error because Control requires the corresponding FunctionEntry node *)
    not (Svcomp.is_special_function f) || Svcomp.is_error_function f *)
    (* TODO: don't inline __VERIFIER functions for CPAchecker, but inlining needed for WP *)
    true

  let enter ctx l f a =
    let g xs x' ys =
      let ys' = List.map (fun (x,y) ->
          (* R.bot () isn't right here? doesn't actually matter? *)
          let yr =
            if should_inline f then
              step_ctx ctx x' (InlineEntry a)
            else
              R.bot ()
          in
          (* keep left syncs so combine gets them for no-inline case *)
          ((Dom.singleton x (R.bot ()), snd ctx.local), (Dom.singleton y yr, Sync.bot ()))
        ) ys
      in
      ys' @ xs
    in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let combine ctx l fe f a fc d =
    assert (Dom.cardinal (fst ctx.local) = 1);
    let cd = Dom.choose_key (fst ctx.local) in
    let k x y =
      let r =
        if should_inline f then
          let nosync = (Sync.singleton x (SyncSet.singleton x)) in
          (* returns already post-sync in FromSpec *)
          step (Function f) (Option.get fc) x (InlineReturn l) nosync (* fc should be Some outside of MCP *)
        else
          step_ctx_edge ctx cd
      in
      try Dom.add (Spec.combine (conv ctx cd) l fe f a fc x) r y
      with Deadcode -> y
    in
    let d = Dom.fold_keys k (fst d) (Dom.bot ()) in
    if Dom.is_bot d then raise Deadcode else (d, Sync.bot ())
end
