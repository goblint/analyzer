(** Analysis specification transformation for ARG construction. *)

open Batteries
open Analyses


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
  module VIE = Printable.Prod (VI) (MyARG.InlineEdgePrintable)
  module VIES = SetDomain.Make (VIE)
  (* even though R is just a set and in solver's [widen old (join old new)] would join the sets of predecessors
     instead of keeping just the last, we are saved by set's narrow bringing that back down to the latest predecessors *)
  module R =
  struct
    include VIES
    (* new predecessors are always the right ones for the latest evaluation *)
    let widen x y = y
    let narrow x y = y
  end

  module SpecDMap (V: Lattice.S) =
  struct
    module R =
    struct
      include Spec.P
      type elt = Spec.D.t
    end
    module J = MapDomain.Joined (Spec.D) (V)
    include DisjointDomain.ProjectiveMap (Spec.D) (V) (J) (R)
  end

  module Dom =
  struct
    module V = R
    include SpecDMap (R)

    let name () = "PathSensitive (" ^ name () ^ ")"

    let printXml f x =
      let print_one x r =
        (* BatPrintf.fprintf f "\n<path>%a</path>" Spec.D.printXml x *)
        BatPrintf.fprintf f "\n<path>%a<analysis name=\"witness\">%a</analysis></path>" Spec.D.printXml x V.printXml r
      in
      iter print_one x

    let map_keys f m =
      fold (fun e r acc ->
          add (f e) r acc
        ) m (empty ())
    let choose_key m = fst (choose m)
    let fold_keys f m a = fold (fun e _ acc -> f e acc) m a
  end

  (* Additional dependencies component between values before and after sync.
     This is required because some analyses (e.g. region) do sideg through local domain diff and sync.
     sync is automatically applied in FromSpec before any transition, so previous values may change (diff is flushed).
     We now use Sync for every tf such that threadspawn after tf could look up state before tf. *)
  module SyncSet =
  struct
    include SetDomain.Make (Spec.D)
    (* new predecessors are always the right ones for the latest evaluation *)
    let widen x y = y
    let narrow x y = y
  end
  module Sync = SpecDMap (SyncSet)
  module D =
  struct
    include Lattice.Prod (Dom) (Sync)

    let printXml f (d, _) = Dom.printXml f d
  end

  module G = Spec.G
  module C = Spec.C
  module V = Spec.V
  module P = UnitP

  let name () = "PathSensitive3("^Spec.name ()^")"

  type marshal = Spec.marshal
  let init = Spec.init
  let finalize = Spec.finalize

  let exitstate  v = (Dom.singleton (Spec.exitstate  v) (R.bot ()), Sync.bot ())
  let startstate v = (Dom.singleton (Spec.startstate v) (R.bot ()), Sync.bot ())
  let morphstate v (d, _) = (Dom.map_keys (Spec.morphstate v) d, Sync.bot ())

  let step n c i e = R.singleton ((n, c, i), e)
  let step n c i e sync =
    match Sync.find i sync with
    | syncs ->
      SyncSet.fold (fun xsync acc ->
          R.join acc (step n c xsync e)
        ) syncs (R.bot ())
    | exception Not_found ->
      M.debug ~category:Witness ~tags:[Category Analyzer] "PathSensitive3 sync predecessor not found";
      R.bot ()
  let step_man man x e =
    try
      step man.prev_node (man.context ()) x e (snd man.local)
    with Man_failure _ ->
      R.bot ()
  let step_man_edge man x = step_man man x (CFGEdge man.edge)
  let step_man_inlined_edge man x = step_man man x (InlinedEdge man.edge)

  let nosync x = Sync.singleton x (SyncSet.singleton x)

  let conv man x =
    let rec man' =
      { man with
        local = x;
        ask = (fun (type a) (q: a Queries.t) -> Spec.query man' q);
        split;
      }
    and split y es =
      let yr = step_man_edge man x in
      man.split (Dom.singleton y yr, Sync.bot ()) es
    in
    man'

  let context man fd (l, _) =
    if Dom.cardinal l <> 1 then
      failwith "PathSensitive3.context must be called with a singleton set."
    else
      let x = Dom.choose_key l in
      Spec.context (conv man x) fd @@ x

  let startcontext = Spec.startcontext

  let map man f g =
    (* we now use Sync for every tf such that threadspawn after tf could look up state before tf *)
    let h x (xs, sync) =
      try
        let x' = g (f (conv man x)) in
        (Dom.add x' (step_man_edge man x) xs, Sync.add x' (SyncSet.singleton x) sync)
      with Deadcode -> (xs, sync)
    in
    let d = Dom.fold_keys h (fst man.local) (Dom.empty (), Sync.bot ()) in
    if Dom.is_bot (fst d) then raise Deadcode else d

  (* TODO???? *)
  let map_event man e =
    (* we now use Sync for every tf such that threadspawn after tf could look up state before tf *)
    let h x (xs, sync) =
      try
        let x' = Spec.event (conv man x) e (conv man x) in
        (Dom.add x' (step_man_edge man x) xs, Sync.add x' (SyncSet.singleton x) sync)
      with Deadcode -> (xs, sync)
    in
    let d = Dom.fold_keys h (fst man.local) (Dom.empty (), Sync.bot ()) in
    if Dom.is_bot (fst d) then raise Deadcode else d


  let fold' man f g h a =
    let k x a =
      try h a x @@ g @@ f @@ conv man x
      with Deadcode -> a
    in
    Dom.fold_keys k (fst man.local) a

  let fold'' man f g h a =
    let k x r a =
      try h a x r @@ g @@ f @@ conv man x
      with Deadcode -> a
    in
    Dom.fold k (fst man.local) a

  let assign man l e    = map man Spec.assign  (fun h -> h l e )
  let vdecl man v       = map man Spec.vdecl   (fun h -> h v)
  let body   man f      = map man Spec.body    (fun h -> h f   )
  let return man e f    = map man Spec.return  (fun h -> h e f )
  let branch man e tv   = map man Spec.branch  (fun h -> h e tv)
  let asm man           = map man Spec.asm     identity
  let skip man          = map man Spec.skip    identity
  let special man l f a = map man Spec.special (fun h -> h l f a)
  let event man e oman = map_event man e (* TODO: ???? *)

  let paths_as_set man =
    let (a,b) = man.local in
    let r = Dom.bindings a in
    List.map (fun (x,v) -> (Dom.singleton x v, b)) r

  let threadenter man ~multiple lval f args =
    let g xs x' ys =
      let ys' = List.map (fun y ->
          let yr = step man.prev_node (man.context ()) x' (ThreadEntry (lval, f, args)) (nosync x') in (* threadenter called on before-sync state *)
          (Dom.singleton y yr, Sync.bot ())
        ) ys
      in
      ys' @ xs
    in
    fold' man (Spec.threadenter ~multiple) (fun h -> h lval f args) g []
  let threadspawn man ~multiple lval f args fman =
    let fd1 = Dom.choose_key (fst fman.local) in
    map man (Spec.threadspawn ~multiple) (fun h -> h lval f args (conv fman fd1))

  let sync man reason =
    fold'' man Spec.sync (fun h -> h reason) (fun (a, async) x r a' ->
        (Dom.add a' r a, Sync.add a' (SyncSet.singleton x) async)
      ) (Dom.empty (), Sync.bot ())

  let query man (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IterPrevVars f ->
      if M.tracing then M.tracei "witness" "IterPrevVars";
      Dom.iter (fun x r ->
          if M.tracing then M.tracei "witness" "x = %a" Spec.D.pretty x;
          R.iter (function ((n, c, j), e) ->
              if M.tracing then M.tracec "witness" "n = %a" Node.pretty_plain n;
              if M.tracing then M.tracec "witness" "c = %a" Spec.C.pretty c;
              if M.tracing then M.tracec "witness" "j = %a" Spec.D.pretty j;
              f (I.to_int x) (n, Obj.repr c, I.to_int j) e
            ) r;
          if M.tracing then M.traceu "witness" "" (* unindent! *)
        ) (fst man.local);
      (* check that sync mappings don't leak into solution (except Function) *)
      (* TODO: disabled because we now use and leave Sync for every tf,
         such that threadspawn after tf could look up state before tf *)
      (* begin match man.node with
           | Function _ -> () (* returns post-sync in FromSpec *)
           | _ -> assert (Sync.is_bot (snd man.local));
         end; *)
      if M.tracing then M.traceu "witness" "";
      ()
    | Queries.IterVars f ->
      Dom.iter (fun x r ->
          f (I.to_int x)
        ) (fst man.local);
      ()
    | Queries.PathQuery (i, q) ->
      (* TODO: optimize indexing, using inner hashcons somehow? *)
      (* let (d, _) = List.at (S.elements s) i in *)
      let (d, _) = List.find (fun (x, _) -> I.to_int x = i) (Dom.bindings (fst man.local)) in
      Spec.query (conv man d) q
    | Queries.Invariant ({path=Some i; _} as c) ->
      (* TODO: optimize indexing, using inner hashcons somehow? *)
      (* let (d, _) = List.at (S.elements s) i in *)
      let (d, _) = List.find (fun (x, _) -> I.to_int x = i) (Dom.bindings (fst man.local)) in
      Spec.query (conv man d) (Invariant c)
    | _ ->
      (* join results so that they are sound for all paths *)
      let module Result = (val Queries.Result.lattice q) in
      fold' man Spec.query identity (fun x _ f -> Result.join x (f q)) (Result.bot ())

  let should_inline f =
    (* (* inline __VERIFIER_error because Control requires the corresponding FunctionEntry node *)
       not (Svcomp.is_special_function f) || Svcomp.is_error_function f *)
    (* TODO: don't inline __VERIFIER functions for CPAchecker, but inlining needed for WP *)
    true

  let enter man l f a =
    let g xs x' ys =
      let ys' = List.map (fun (x,y) ->
          (* R.bot () isn't right here? doesn't actually matter? *)
          let yr =
            if should_inline f then
              step_man man x' (InlineEntry (l, f, a))
            else
              R.bot ()
          in
          (* keep left syncs so combine gets them for no-inline case *)
          (* must lookup and short-circuit because enter may modify first in pair (e.g. abortUnless) *)
          let syncs =
            match Sync.find x' (snd man.local) with
            | syncs -> syncs
            | exception Not_found ->
              M.debug ~category:Witness ~tags:[Category Analyzer] "PathSensitive3 sync predecessor not found";
              SyncSet.bot ()
          in
          ((Dom.singleton x (R.bot ()), Sync.singleton x syncs), (Dom.singleton y yr, Sync.bot ()))
        ) ys
      in
      ys' @ xs
    in
    fold' man Spec.enter (fun h -> h l f a) g []

  let combine_env man l fe f a fc d  f_ask =
    (* Don't yet consider call edge done before assign. *)
    assert (Dom.cardinal (fst man.local) = 1);
    let (cd, cdr) = Dom.choose (fst man.local) in
    let k x (y, sync) =
      try
        let x' = Spec.combine_env (conv man cd) l fe f a fc x f_ask in
        (Dom.add x' cdr y, Sync.add x' (Sync.find cd (snd man.local)) sync) (* keep predecessors and sync from man, sync required for step_man_inlined_edge in combine_assign *)
      with Deadcode -> (y, sync)
    in
    let d = Dom.fold_keys k (fst d) (Dom.bot (), Sync.bot ()) in
    if Dom.is_bot (fst d) then raise Deadcode else d

  let combine_assign man l fe f a fc d  f_ask =
    (* Consider call edge done after entire call-assign. *)
    assert (Dom.cardinal (fst man.local) = 1);
    let cd = Dom.choose_key (fst man.local) in
    let k x (y, sync) =
      let r =
        if should_inline f then
          (* returns already post-sync in FromSpec *)
          let returnr = step (Function f) (Option.get fc) x (InlineReturn (l, f, a)) (nosync x) in (* fc should be Some outside of MCP *)
          let procr = step_man_inlined_edge man cd in
          R.join procr returnr
        else
          step_man_edge man cd
      in
      try
        let x' = Spec.combine_assign (conv man cd) l fe f a fc x f_ask in
        (Dom.add x' r y, Sync.add x' (SyncSet.singleton x) sync)
      with Deadcode -> (y, sync)
    in
    let d = Dom.fold_keys k (fst d) (Dom.bot (), Sync.bot ()) in
    if Dom.is_bot (fst d) then raise Deadcode else d
end
