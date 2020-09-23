(** An analysis specification for witnesses. *)

open Prelude.Ana
open Analyses

module Node: Printable.S with type t = MyCFG.node =
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

module Edge: Printable.S with type t = MyARG.inline_edge =
struct
  type t = MyARG.inline_edge [@@deriving to_yojson]

  let equal = Util.equals
  let compare = Stdlib.compare
  let hash = Hashtbl.hash

  let short w x = Pretty.sprint w (MyARG.pretty_inline_edge () x)
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

(* TODO: weaken R to Lattice.S ? *)
module HoareMap (SpecD:Lattice.S) (R:SetDomain.S) =
struct
  module SpecDGroupable =
  struct
    include Printable.Std
    include SpecD
  end
  module MM = MapDomain.MapBot_LiftTop (SpecDGroupable) (R)
  include MM

  (* TODO: get rid of these value-ignoring set-mimicing hacks *)
  let cardinal (s: t): int = match s with
    | `Top -> failwith "cardinal"
    | `Lifted s -> M.M.cardinal s
  let choose (s: t): SpecD.t = match s with
    | `Top -> failwith "choose"
    | `Lifted s -> fst (M.M.choose s)
  let filter (p: key -> bool) (s: t): t = filter (fun x _ -> p x) s
  let iter' = iter
  let iter (f: key -> unit) (s: t): unit = iter (fun x _ -> f x) s
  let for_all' = for_all
  let for_all (p: key -> bool) (s: t): bool = for_all (fun x _ -> p x) s
  let fold' = fold
  let fold (f: key -> 'a -> 'a) (s: t) (acc: 'a): 'a = fold (fun x _ acc -> f x acc) s acc
  let singleton (x: key) (r: R.t): t = `Lifted (M.M.singleton x r)
  let empty (): t = `Lifted M.M.empty
  let add (x: key) (r: R.t) (s: t): t = match s with
    | `Top -> `Top
    | `Lifted s -> `Lifted (M.M.add x (R.join r (M.find x s)) s)
  let map (f: key -> key) (s: t): t = match s with
    | `Top -> `Top
    | `Lifted s -> `Lifted (M.fold (fun x v acc -> M.M.add (f x) (R.join v (M.find (f x) acc)) acc) s (M.M.empty))

  module S =
  struct
    let exists (p: key -> bool) (s: M.t): bool = M.M.exists (fun x _ -> p x) s
    let elements (s: M.t): (key * R.t) list = M.M.bindings s
    let of_list (l: (key * R.t) list): M.t = List.fold_left (fun acc (x, r) -> M.M.add x (R.join r (M.find x acc)) acc) M.M.empty l
  end


  (* copied & modified from SetDomain.Hoare *)
  let mem x xr = function
    | `Top -> true
    (* | `Lifted s -> S.exists (Spec.D.leq x) s *)
    (* exists check per previous VIE.t in R.t *)
    (* seems to be necessary for correct ARG but why? *)
    (* | `Lifted s -> R.for_all (fun vie -> M.M.exists (fun y yr -> Spec.D.leq x y && R.mem vie yr) s) xr *)
    (* | `Lifted s -> R.for_all (fun vie -> M.M.exists (fun y yr -> Spec.D.leq x y && R.exists (fun vie' -> VIE.leq vie vie') yr) s) xr *)
    | `Lifted s -> R.for_all (fun vie -> M.M.exists (fun y yr -> SpecD.leq x y && R.mem vie yr) s) xr
  let leq a b =
    match a with
    | `Top -> b = `Top
    | _ -> for_all' (fun x xr -> mem x xr b) a (* mem uses B.leq! *)

  let apply_list f = function
    | `Top -> `Top
    | `Lifted s -> `Lifted (S.elements s |> f |> S.of_list)
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
  (* module I = IntDomain.Integers *)
  module I =
  struct
    include Spec.D
    (* assumes Hashcons inside PathSensitive *)
    let to_int = tag
  end
  module VI = Printable.Prod3 (Node) (Spec.C) (I)
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
  end
  (* Bot is needed for Hoare widen *)
  (* TODO: could possibly rewrite Hoare to avoid introducing bots in widen which get reduced away anyway? *)
  module VIEB = Lattice.LiftBot (VIE)
  module VIES = SetDomain.Hoare (VIEB) (struct let topname = "VIES top" end)

  module R = VIES

  module D =
  struct
    include HoareMap (Spec.D) (R)

    let name () = "PathSensitive (" ^ name () ^ ")"

    let pretty_diff () ((s1:t),(s2:t)): Pretty.doc =
      if leq s1 s2 then dprintf "%s (%d and %d paths): These are fine!" (name ()) (cardinal s1) (cardinal s2) else begin
        try
          let p t = not (MM.mem t s2) in
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

    (* TODO: fix these operators by implementing corresponding HoareMap ones *)
    let join = binop join
    let meet = binop meet
    let widen = join
    let narrow a b = a

    let invariant c s =
      match s with
      | `Top -> failwith "invariant Top"
      | `Lifted s ->
        (* TODO: optimize indexing, using inner hashcons somehow? *)
        (* let (d, _) = List.at (S.elements s) c.Invariant.i in *)
        let (d, _) = List.find (fun (x, _) -> I.to_int x = c.Invariant.i) (S.elements s) in
        Spec.D.invariant c d
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
                            ; spawn = (fun v -> ctx.spawn v % (fun x -> D.singleton x (R.bot ())) ) (* TODO: use enter-like behavior for spawn, as in WitnessLifter *)
                            ; split = (ctx.split % (fun x -> D.singleton x (R.bot ()))) }
    and query x = Spec.query ctx' x in
    ctx'

  let step n c i e = R.singleton (`Lifted ((n, c, i), e))
  let step_ctx ctx x e =
    try
      step ctx.prev_node (ctx.context ()) x e
    with Ctx_failure _ ->
      R.bot ()
  let step_ctx_edge ctx x = step_ctx ctx x (CFGEdge ctx.edge)

  let map ctx f g =
    let h x xs =
      try D.add (g (f (conv ctx x))) (step_ctx_edge ctx x) xs
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

  let fold ctx f g h a =
    let k x a =
      try h a @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    let d = D.fold k ctx.local a in
    if D.is_bot d then raise Deadcode else d

  let fold' ctx f g h a =
    let k x a =
      try h a x @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    D.fold k ctx.local a

  let fold'' ctx f g h a =
    let k x r a =
      try h a x r @@ g @@ f @@ conv ctx x
      with Deadcode -> a
    in
    D.fold' k ctx.local a

  let sync ctx =
    (* TODO: no idea if this is right *)
    fold'' ctx Spec.sync identity (fun (a,b) _ r (a',b') -> D.add a' r a, b'@b) (D.empty (), [])

  let query ctx q =
    match q with
    | Queries.IterPrevVars f ->
      D.iter' (fun x r ->
          R.iter (function
              | `Lifted ((n, c, j), e) ->
                f (I.to_int x) (n, Obj.repr c, I.to_int j) e
              | `Bot ->
                failwith "PathSensitive3.query: range contains bot"
            ) r
        ) ctx.local;
      `Bot
    | Queries.IterVars f ->
      D.iter' (fun x r ->
          f (I.to_int x)
        ) ctx.local;
      `Bot
    | _ ->
      fold' ctx Spec.query identity (fun x _ f -> Queries.Result.meet x (f q)) `Top

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
          (D.singleton x (R.bot ()), D.singleton y yr)
        ) ys
      in
      ys' @ xs
    in
    fold' ctx Spec.enter (fun h -> h l f a) g []

  let combine ctx l fe f a fc d =
    assert (D.cardinal ctx.local = 1);
    let cd = D.choose ctx.local in
    let k x y =
      let r =
        if should_inline f then
          step (Function f) fc x (InlineReturn l)
        else
          step_ctx_edge ctx cd
      in
      try D.add (Spec.combine (conv ctx cd) l fe f a fc x) r y
      with Deadcode -> y
    in
    let d = D.fold k d (D.bot ()) in
    if D.is_bot d then raise Deadcode else d

  let part_access _ _ _ _ =
    (Access.LSSSet.singleton (Access.LSSet.empty ()), Access.LSSet.empty ())
end
