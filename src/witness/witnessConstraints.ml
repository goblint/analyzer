(** An analysis specification for witnesses. *)

open Prelude.Ana
open Analyses

module PrintableVar =
struct
  include Var
  let to_yojson = MyCFG.node_to_yojson

  let isSimple _ = true
  let pretty_f _ = pretty
  let pretty_diff () (x,y) = dprintf "Unsupported"
  (* let short n x = Pretty.sprint n (pretty () x) *)
  (* let short _ x = var_id x *)
  let short _ x =
    let open MyCFG in
    match x with
    | Statement stmt  -> string_of_int stmt.sid
    | Function f      -> "return of " ^ f.vname ^ "()"
    | FunctionEntry f -> f.vname ^ "()"
  let toXML x =
    let text = short 100 x in
    Xml.Element ("value", [], [Xml.Element ("data", [], [Xml.PCData text])])
  let toXML_f _ = toXML
  let printXml f x =
    BatPrintf.fprintf f "%s" (Xml.to_string (toXML x))
  let name () = "var"
  let invariant _ _ = Invariant.none
  let tag _ = failwith "PrintableVar: no tag"
end

(* TODO: move this to MyCFG *)
module Edge: Printable.S with type t = MyCFG.edge =
struct
  type t = MyCFG.edge [@@deriving to_yojson]

  let equal = Util.equals
  let compare = Pervasives.compare
  let hash = Hashtbl.hash

  let short w x = Pretty.sprint w (MyCFG.pretty_edge () x)
  let name () = "edge"

  include Printable.PrintSimple (
    struct
      type t' = t
      let short = short
      let name = name
    end
    )

  let invariant _ _ = Invariant.none
  let tag _ = failwith "Edge: no tag"
end

module FlatBot (Base: Printable.S) = Lattice.LiftBot (Lattice.Fake (Base))


module WitnessLifter (S:Spec): Spec =
struct
  module V = Printable.Prod (PrintableVar) (S.C)
  module VE = Printable.Prod (V) (Edge)
  module VES = SetDomain.ToppedSet (VE) (struct let topname = "VES top" end)
  module VF = Lattice.Flat (V) (struct let bot_name = "VF bot" let top_name = "VF top" end)
  module W = Lattice.Prod (VES) (VF)

  module D =
  struct
    include Lattice.Prod (S.D) (W)

    let invariant c (d, w) = S.D.invariant c d (* don't return invariants from prev vars contexts *)

    let printXml f (d, w) =
      (* BatPrintf.fprintf f "%a<path><analysis name=\"witness\">%a</analysis></path>" S.D.printXml d W.printXml w *)
      BatPrintf.fprintf f "%a<analysis name=\"witness\">%a</analysis>" S.D.printXml d W.printXml w
  end
  module G = S.G
  module C = S.C

  let get_context ctx = ctx.context2 ()

  let set_of_flat (x:VF.t) (edge:Edge.t): VES.t = match x with
    | `Lifted x -> VES.singleton (x, edge)
    | `Bot -> VES.bot ()
    | `Top -> VES.top ()

  let step (from:VF.t) (edge:Edge.t) (to_node:V.t): W.t =
    let prev = set_of_flat from edge in
    (* ignore (Pretty.printf "from: %a, prev: %a -> to_node: %a\n" W.pretty from VS.pretty prev V.pretty to_node); *)
    (prev, `Lifted to_node)

  let step_witness (from:W.t) = step (snd from)

  let step_ctx ctx =
    try
      let context = get_context ctx in
      let prev_node_witness = snd (snd ctx.local) in
      let prev_node_ctx = `Lifted (ctx.prev_node, context) in
      (* assert (VF.equal prev_node_witness prev_node_ctx); *)
      if not (VF.equal prev_node_witness prev_node_ctx) then begin
        let extract_node: VF.t -> MyCFG.node = function
          | `Lifted (node, _) -> node
          | _ -> MyCFG.dummy_node
        in
        let s = Pretty.sprint 80 (Pretty.dprintf "WitnessLifter: prev_node mismatch at %a via %a: %a vs %a" MyCFG.pretty_node ctx.node Edge.pretty ctx.edge MyCFG.pretty_node (extract_node prev_node_witness) MyCFG.pretty_node (extract_node prev_node_ctx)) in
        (* M.waitWhat s; *)
        failwith s;
      end;
      step prev_node_witness ctx.edge (ctx.node, context)
    with Ctx_failure _ ->
      W.bot ()

  (* let strict (d, w) = if S.D.is_bot d then D.bot () else (d, w) *)
  let strict (d, w) = (d, w) (* analysis is strict as long as witness lifter inside dead code lifter *)

  let should_inline f =
    let loc = f.vdecl in
    let is_svcomp = String.ends_with loc.file "sv-comp.c" in (* only includes/sv-comp.c functions, not __VERIFIER_assert in benchmark *)
    let is_verifier = String.starts_with f.vname "__VERIFIER" in
    (* inline __VERIFIER_error because Control requires the corresponding FunctionEntry node *)
    let should_not_inline = is_svcomp && is_verifier && f.vname <> Svcomp.verifier_error in
    not should_not_inline

  let name () = S.name () ^ " witnessed"

  let init = S.init
  let finalize = S.finalize

  let startstate v = (S.startstate v, W.bot ())
  let morphstate v (d, w) = (S.morphstate v d, w)
  let exitstate v = (S.exitstate v, W.bot ())
  let otherstate v = (S.otherstate v, W.bot ())

  let should_join (x, _) (y, _) = S.should_join x y
  let val_of c = (S.val_of c, W.bot ())
  let context (d, _) = S.context d
  let call_descr = S.call_descr

  let unlift_ctx (ctx:(D.t, 'g, 'c) Analyses.ctx) =
    let w = snd ctx.local in
    { ctx with
      local = fst ctx.local;
      spawn = (fun v d ->
          (* like enter *)
          (* TODO: don't duplicate logic with enter *)
          let to_node = (MyCFG.FunctionEntry v, S.context d) in
          let w' =
            if should_inline v then
              step_witness w MyCFG.Skip to_node
            else
              (VES.bot (), `Lifted to_node)
          in
          ctx.spawn v (strict (d, w'))
        );
      split = (fun d e tv -> ctx.split (strict (d, w)) e tv)
    }
  let part_access ctx = S.part_access (unlift_ctx ctx)

  let sync ctx =
    let (d, l) = S.sync (unlift_ctx ctx) in
    (* let w = step_ctx ctx in *)
    let w = snd ctx.local in
    (strict (d, w), l)

  let query ctx q =
    match q with
    | Queries.IterPrevVars f ->
      begin match fst (snd ctx.local) with
        | VES.All ->
          failwith (Pretty.sprint 80 (Pretty.dprintf "WitnessLifter: witness messed up! prev vars top at %a" MyCFG.pretty_node ctx.node))
        | VES.Set s ->
          VES.S.iter (fun ((n, c), e) ->
              f 0 (n, Obj.repr c, 0) e
            ) s
      end;
      `Bot
    | _ -> S.query (unlift_ctx ctx) q

  (* TODO: handle Bailure during tf step? *)

  let assign ctx lv e =
    let d = S.assign (unlift_ctx ctx) lv e in
    let w = step_ctx ctx in
    strict (d, w)

  let branch ctx e tv =
    let d = S.branch (unlift_ctx ctx) e tv in
    let w = step_ctx ctx in
    strict (d, w)

  let body ctx f =
    let d = S.body (unlift_ctx ctx) f in
    let w = step_ctx ctx in
    strict (d, w)

  let return ctx r f =
    let d = S.return (unlift_ctx ctx) r f in
    let w = step_ctx ctx in
    strict (d, w)

  let intrpt ctx =
    let d = S.intrpt (unlift_ctx ctx) in
    let w = snd ctx.local in (* interrupt is a self-loop and doesn't step to next node *)
    strict (d, w)

  let asm ctx =
    let d = S.asm (unlift_ctx ctx) in
    let w = step_ctx ctx in
    strict (d, w)

  let skip ctx =
    let d = S.skip (unlift_ctx ctx) in
    let w = step_ctx ctx in
    strict (d, w)

  let special ctx r f args =
    let d = S.special (unlift_ctx ctx) r f args in
    let w = step_ctx ctx in
    strict (d, w)

  let enter ctx r f args =
    let ddl = S.enter (unlift_ctx ctx) r f args in
    let w = snd ctx.local in
    List.map (fun (d1, d2) ->
        let to_node = (MyCFG.FunctionEntry f, S.context d2) in
        let w' =
          if should_inline f then
            step_witness w MyCFG.Skip to_node
          else
            (VES.bot (), `Lifted to_node)
        in
        (strict (d1, w), strict (d2, w'))
      ) ddl

  let combine ctx r fe f args (d', w') =
    let d = S.combine (unlift_ctx ctx) r fe f args d' in
    let w =
      if should_inline f then
        step_witness w' MyCFG.Skip (ctx.node, get_context ctx)
      else
        step_ctx ctx
    in
    strict (d, w)
end



module N = struct let topname = "Top" end
(** Add path sensitivity to a analysis *)
module PathSensitive3 (Spec:Spec)
  : Spec
    (* with type D.t = SetDomain.ToppedSet(Spec.D)(N).t
     and module G = Spec.G
     and module C = Spec.C *)
=
struct
  module I = IntDomain.Integers
  module VI = Printable.Prod3 (PrintableVar) (Spec.C) (I)
  module VIE = Printable.Prod (VI) (Edge)
  module VIES = SetDomain.ToppedSet (VIE) (struct let topname = "VIES top" end)

  module R = VIES

  module D =
  struct
    module SpecDGroupable =
    struct
      include Printable.Std
      include Spec.D
    end
    include MapDomain.MapBot_LiftTop (SpecDGroupable) (R)

    (* TODO: get rid of these value-ignoring set-mimicing hacks *)
    let cardinal (s: t): int = match s with
      | `Top -> failwith "cardinal"
      | `Lifted s -> M.M.cardinal s
    let choose (s: t): Spec.D.t = match s with
      | `Top -> failwith "choose"
      | `Lifted s -> fst (M.M.choose s)
    let filter (p: key -> bool) (s: t): t = filter (fun x _ -> p x) s
    let iter' = iter
    let iter (f: key -> unit) (s: t): unit = iter (fun x _ -> f x) s
    let for_all (p: key -> bool) (s: t): bool = for_all (fun x _ -> p x) s
    let fold (f: key -> 'a -> 'a) (s: t) (acc: 'a): 'a = fold (fun x _ acc -> f x acc) s acc
    let singleton (x: key) (r: R.t): t = `Lifted (M.M.singleton x r)
    let empty (): t = `Lifted M.M.empty
    let add (x: key) (r: R.t) (s: t): t = add x r s
    let map (f: key -> key) (s: t): t = match s with
      | `Top -> `Top
      | `Lifted s -> `Lifted (M.fold (fun x v acc -> M.M.add (f x) (R.join v (M.find (f x) acc)) acc) s (M.M.empty))

    module S =
    struct
      let exists (p: key -> bool) (s: M.t): bool = M.M.exists (fun x _ -> p x) s
      let elements (s: M.t): (key * R.t) list = M.M.bindings s
      let of_list (l: (key * R.t) list): M.t = M.add_list l M.M.empty
    end

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
      let print_one x r =
        (* BatPrintf.fprintf f "\n<path>%a</path>" Spec.D.printXml x *)
        BatPrintf.fprintf f "\n<path>%a<analysis name=\"witness\">%a</analysis></path>" Spec.D.printXml x R.printXml r
      in
      iter' print_one x

    (* copied from SetDomain.Hoare *)
    let mem x = function
      | `Top -> true
      | `Lifted s -> S.exists (Spec.D.leq x) s
    let leq a b =
      match a with
      | `Top -> b = `Top
      | _ -> for_all (fun x -> mem x b) a (* mem uses B.leq! *)
    let apply_list f = function
      | `Top -> `Top
      | `Lifted s -> `Lifted (S.elements s |> f |> S.of_list)
    (* join elements in the same partition (specified by should_join) *)
    let join_reduce a =
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
    let narrow = binop narrow

    let invariant c s = fold (fun x a ->
        Invariant.(a || Spec.D.invariant c x) (* TODO: || correct? *)
      ) s Invariant.none
  end

  module G = Spec.G
  module C = Spec.C

  let name () = "PathSensitive3("^Spec.name ()^")"

  let init = Spec.init
  let finalize = Spec.finalize

  let should_join x y = true

  let otherstate v = D.singleton (Spec.otherstate v) (R.bot ())
  let exitstate  v = D.singleton (Spec.exitstate  v) (R.bot ())
  let startstate v = D.singleton (Spec.startstate v) (R.bot ())
  let morphstate v d = D.map (Spec.morphstate v) d

  let call_descr = Spec.call_descr

  let val_of c = D.singleton (Spec.val_of c) (R.bot ())
  let context l =
    if D.cardinal l <> 1 then
      failwith "PathSensitive3.context must be called with a singleton set."
    else
      Spec.context @@ D.choose l

  let conv ctx x =
    (* TODO: R.bot () isn't right here *)
    let rec ctx' = { ctx with ask   = query
                            ; local = x
                            ; spawn = (fun v -> ctx.spawn v % (fun x -> D.singleton x (R.bot ())) )
                            ; split = (ctx.split % (fun x -> D.singleton x (R.bot ()))) }
    and query x = Spec.query ctx' x in
    ctx'

  let get_context ctx = ctx.context2 ()

  let map ctx f g =
    let h x (i, xs) =
      let r =
        try
          R.singleton ((ctx.prev_node, get_context ctx, Int64.of_int i), ctx.edge)
        with Ctx_failure _ ->
          R.bot ()
      in
      try (succ i, D.add (g (f (conv ctx x))) r xs)
      with Deadcode -> (succ i, xs)
    in
    let (_, d) = D.fold h ctx.local (0, D.empty ()) in
    if D.is_bot d then raise Deadcode else d

  let assign ctx l e    = map ctx Spec.assign  (fun h -> h l e )
  let body   ctx f      = map ctx Spec.body    (fun h -> h f   )
  let return ctx e f    = map ctx Spec.return  (fun h -> h e f )
  let branch ctx e tv   = map ctx Spec.branch  (fun h -> h e tv)
  let intrpt ctx        = map ctx Spec.intrpt  identity
  let asm ctx           = map ctx Spec.asm     identity
  let skip ctx          = map ctx Spec.skip    identity
  let special ctx l f a = map ctx Spec.special (fun h -> h l f a)

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
    (* TODO: R.bot () isn't right here *)
    fold' ctx Spec.sync identity (fun (a,b) (a',b') -> D.add a' (R.bot ()) a, b'@b) (D.empty (), [])

  let query ctx q =
    match q with
    | Queries.IterPrevVars f ->
      begin match ctx.local with
        | `Lifted s ->
          D.S.elements s
          |> List.iteri (fun i (x, r) ->
              R.iter (fun ((n, c, j), e) ->
                f i (n, Obj.repr c, Int64.to_int j) e
              ) r
            )
        | `Top -> failwith "prev messed up: top"
      end;
      `Bot
    | _ ->
      fold' ctx Spec.query identity (fun x f -> Queries.Result.meet x (f q)) `Top

  let enter ctx l f a =
    (* R.bot () isn't right here *)
    let g xs ys = (List.map (fun (x,y) -> D.singleton x (R.bot ()), D.singleton y (R.bot ())) ys) @ xs in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let combine ctx l fe f a d =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      (* TODO: R.bot () isn't right here *)
      try D.add (Spec.combine (conv ctx cd) l fe f a x) (R.bot ()) y
      with Deadcode -> y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d

  let part_access _ _ _ _ =
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
end
