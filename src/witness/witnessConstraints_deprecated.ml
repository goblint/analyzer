open Prelude.Ana
open Analyses

open WitnessConstraints (* for Node, Edge *)

(* Deprecated context-sensitive dependency tracking *)
(* Cannot handle dependencies path-sensitive set elements *)
(* PathSensitive3 combines them *)
module WitnessLifter (S:Spec): Spec =
struct
  module V = Printable.Prod (Node) (S.C)
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

  let get_context ctx = ctx.context ()

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
        let s = Pretty.sprint 80 (Pretty.dprintf "WitnessLifter: prev_node mismatch at %a via %a: %a vs %a" MyCFG.pretty_node ctx.node MyCFG.pretty_edge ctx.edge MyCFG.pretty_node (extract_node prev_node_witness) MyCFG.pretty_node (extract_node prev_node_ctx)) in
        (* M.waitWhat s; *)
        failwith s;
      end;
      step prev_node_witness (CFGEdge ctx.edge) (ctx.node, context)
    with Ctx_failure _ ->
      W.bot ()

  let should_inline f =
    (* inline __VERIFIER_error because Control requires the corresponding FunctionEntry node *)
    not (Svcomp.is_special_function f) || Svcomp.is_error_function f

  let name () = S.name () ^ " witnessed"

  let init = S.init
  let finalize = S.finalize

  let startstate v = (S.startstate v, W.bot ())
  let morphstate v (d, w) = (S.morphstate v d, w)
  let exitstate v = (S.exitstate v, W.bot ())

  let should_join (x, _) (y, _) = S.should_join x y
  let val_of c = (S.val_of c, W.bot ())
  let context (d, _) = S.context d
  let call_descr = S.call_descr

  let unlift_ctx (ctx:(D.t, 'g, 'c) Analyses.ctx) =
    let w = snd ctx.local in
    { ctx with
      local = fst ctx.local;
      (* spawn = (fun v d ->
          (* like enter *)
          (* TODO: don't duplicate logic with enter *)
          let to_node = (MyCFG.FunctionEntry v, S.context d) in
          let w' =
            if should_inline v then
              step_witness w (InlineEntry []) to_node (* TODO: args *)
            else
              (VES.bot (), `Lifted to_node)
          in
          ctx.spawn v (d, w')
        ); *)
      split = (fun d e tv -> ctx.split (d, w) e tv)
    }
  let part_access ctx = S.part_access (unlift_ctx ctx)

  let sync ctx =
    let (d, l) = S.sync (unlift_ctx ctx) in
    (* let w = step_ctx ctx in *)
    let w = snd ctx.local in
    (d, w), l

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
    d, w

  let vdecl ctx v =
    let d = S.vdecl (unlift_ctx ctx) v in
    let w = step_ctx ctx in
    d, w

  let branch ctx e tv =
    let d = S.branch (unlift_ctx ctx) e tv in
    let w = step_ctx ctx in
    d, w

  let body ctx f =
    let d = S.body (unlift_ctx ctx) f in
    let w = step_ctx ctx in
    d, w

  let return ctx r f =
    let d = S.return (unlift_ctx ctx) r f in
    let w = step_ctx ctx in
    d, w

  let intrpt ctx =
    let d = S.intrpt (unlift_ctx ctx) in
    let w = snd ctx.local in (* interrupt is a self-loop and doesn't step to next node *)
    d, w

  let asm ctx =
    let d = S.asm (unlift_ctx ctx) in
    let w = step_ctx ctx in
    d, w

  let skip ctx =
    let d = S.skip (unlift_ctx ctx) in
    let w = step_ctx ctx in
    d, w

  let special ctx r f args =
    let d = S.special (unlift_ctx ctx) r f args in
    let w = step_ctx ctx in
    d, w

  let enter ctx r f args =
    let ddl = S.enter (unlift_ctx ctx) r f args in
    let w = snd ctx.local in
    List.map (fun (d1, d2) ->
        let to_node = (MyCFG.FunctionEntry f, S.context d2) in
        let w' =
          if should_inline f then
            step_witness w (InlineEntry args) to_node
          else
            (VES.bot (), `Lifted to_node)
        in
        (d1, w), (d2, w')
      ) ddl

  let combine ctx r fe f args fc (d', w') =
    let d = S.combine (unlift_ctx ctx) r fe f args fc d' in
    let w =
      if should_inline f then
        step_witness w' (InlineReturn r) (ctx.node, get_context ctx)
      else
        step_ctx ctx
    in
    d, w

  let threadenter ctx f args =
    let d = S.threadenter (unlift_ctx ctx) f args in
    let w = step_ctx ctx in
    d, w

  let threadcombine ctx f args fd =
    let d = S.threadcombine (unlift_ctx ctx) f args (fst fd) in
    let w = step_ctx ctx in
    d, w
end