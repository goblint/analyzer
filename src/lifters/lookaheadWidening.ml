(** Lookahead widening.

    @see <https://doi.org/10.1007/11817963_41> Gopan, D., Reps, T. Lookahead Widening. *)

open Batteries
open Lattice
open Analyses

module LookaheadConf (C: Printable.ProdConfiguration) (Base: S) =
struct
  include Printable.ProdConf (C) (Base) (Base)

  let bot () = (Base.bot (), Base.bot())
  let is_bot (m, p) = Base.is_bot m
  let top () = (Base.top (), Base.top ())
  let is_top (m, p) = Base.is_top m && Base.is_top p

  let leq (m1,p1) (m2,p2) = Base.leq m1 m2 && (not (Base.equal m1 m2) || Base.leq p1 p2)

  let pretty_diff () ((m1,p1:t),(m2,p2:t)): Pretty.doc =
    if Base.leq m1 m2 then
      Base.pretty_diff () (p1,p2)
    else
      Base.pretty_diff () (m1,m2)

  let op_scheme op1 op2 (m1,p1) (m2,p2) = (op1 m1 m2, op2 p1 p2)
  let join x y = if !AnalysisState.widening then y else op_scheme Base.join Base.join x y
  let meet = op_scheme Base.meet Base.meet (** Might not be correct *)
  let narrow = op_scheme Base.narrow Base.narrow (** Might not be correct *)
  let widen (m1,p1) (m2,p2) =
    if leq (m2,p2) (m1,p1) then (m1,p1)
    else if Base.leq p2 p1 then (p2,p2)
    else op_scheme Base.join (fun y x -> if Base.leq x y then y else Base.widen y (Base.join x y)) (m1,p1) (m2,p2)

  let printXml f (x,y) =
    BatPrintf.fprintf f "%a%a" Base.printXml x Base.printXml y
end

module Lookahead = LookaheadConf (struct let expand_fst = true let expand_snd = true end)
module LookaheadSimple = LookaheadConf (struct let expand_fst = false let expand_snd = false end)


module Lifter (S:Spec)
  : Spec with module D = Lookahead (S.D)
          and module G = S.G
          and module C = S.C
=
struct
  module D = Lookahead (S.D)
  module G = S.G
  module C = S.C
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  let name () = S.name ()^" lookahead"

  let start_level = ref (`Top)

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init marshal =
    S.init marshal

  let finalize = S.finalize

  let startstate v = (S.startstate v, S.startstate v)
  let exitstate  v = (S.exitstate  v, S.exitstate  v)
  let morphstate v (d,l) = (S.morphstate v d, S.morphstate v l)

  let conv1 (ctx: (D.t, G.t, C.t, V.t) ctx) : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es )
    }
  let conv2 (ctx: (D.t, G.t, C.t, V.t) ctx) : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
    { ctx with local = snd ctx.local
             ; split = (fun d es -> ctx.split (fst ctx.local, d) es )
    }

  let context ctx fd (d,_) = S.context (conv1 ctx) fd d
  let startcontext () = S.startcontext ()

  let lift_fun (ctx: (D.t, G.t, C.t, V.t) ctx) f g h =
    let main = h (g (conv1 ctx)) in
    if S.D.is_bot main then D.bot () else
      (main, h (g (conv2 ctx)))
  let lift_fun' (ctx: (D.t, G.t, C.t, V.t) ctx) f g h =
    let main = h (g (conv1 ctx)) in
    (main, h (g (conv2 ctx)))
  let lift_fun2 (ctx: (D.t, G.t, C.t, V.t) ctx) f g h1 h2 =
    let main = h1 (g (conv1 ctx)) in
    if S.D.is_bot main then D.bot () else
      (main, h2 (g (conv2 ctx)))

  let enter ctx r f args =
    let liftmap = List.map (fun (x,y) -> (x, snd ctx.local), (fst ctx.local, y)) in
    let (l1, l2) = lift_fun' ctx liftmap S.enter ((|>) args % (|>) f % (|>) r) in
    List.map2 (fun (x1, y1) (x2, y2) -> ((x1, x2), (y1, y2))) l1 l2

  let lift ctx d = (d, snd ctx.local)
  let rec lift' ctx dl =
    match dl with
    | [] -> []
    | d::l -> (d, snd ctx.local)::(lift' ctx l)
  let lift_start_level d = (d, !start_level)

  let sync ctx reason = lift_fun ctx (lift ctx) S.sync   ((|>) reason)
  let query ctx (type a) (q: a Queries.t): a Queries.result = S.query (conv1 ctx) q
  let assign ctx lv e = lift_fun ctx (lift ctx) S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx (lift ctx) S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx (lift ctx) S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx (lift ctx) S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx (lift ctx) S.return ((|>) f % (|>) r)
  let asm ctx         = lift_fun ctx (lift ctx) S.asm    identity
  let skip ctx        = lift_fun ctx (lift ctx) S.skip identity
  let special ctx r f args        = lift_fun ctx (lift ctx) S.special ((|>) args % (|>) f % (|>) r)
  let combine_env ctx r fe f args fc es f_ask = lift_fun ctx (lift ctx) S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign ctx r fe f args fc es f_ask = lift_fun ctx (lift ctx) S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)

  let threadenter ctx ~multiple lval f args =
    let (l1, l2) = lift_fun' ctx Fun.id (S.threadenter~multiple) ((|>) args % (|>) f % (|>) lval) in
    List.combine l1 l2
  let threadspawn ctx ~multiple lval f args fctx = lift_fun2 ctx (lift ctx) (S.threadspawn ~multiple) ((|>) (conv1 fctx) % (|>) args % (|>) f % (|>) lval) ((|>) (conv2 fctx) % (|>) args % (|>) f % (|>) lval)

  let paths_as_set ctx = (*[ctx.local]*) (** Trivial, may be incorrect. *)
    let (l1, l2) = lift_fun' ctx Fun.id S.paths_as_set Fun.id in
    List.combine l1 l2

  let event ctx e octx =
    lift_fun2 ctx (lift ctx) S.event ((|>) (conv1 octx) % (|>) e) ((|>) (conv2 octx) % (|>) e)
end
