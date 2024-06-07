(** Lookahead widening.

    @see <https://doi.org/10.1007/11817963_41> Gopan, D., Reps, T. Lookahead Widening. *)

open Batteries
open Lattice
open Analyses

module Dom (Base: S) =
struct
  include Printable.Prod (Base) (Base)

  let bot () = (Base.bot (), Base.bot ())
  let is_bot (m, p) = Base.is_bot m
  let top () = (Base.top (), Base.top ())
  let is_top (m, p) = Base.is_top m && Base.is_top p

  let leq (m1, p1) (m2, p2) = Base.leq m1 m2 && (not (Base.equal m1 m2) || Base.leq p1 p2)

  let op_scheme mop pop (m1, p1) (m2, p2) = (mop m1 m2, pop p1 p2)
  let join x y =
    if !AnalysisState.widening then
      y
    else
      op_scheme Base.join Base.join x y
  let meet = op_scheme Base.meet Base.meet (** TODO: Might not be correct *)
  let widen ((m1, p1) as x) ((m2, p2) as y) =
    if leq y x then
      x
    else if Base.leq p2 p1 then
      (p2, p2)
    else
      op_scheme Base.join (fun p1 p2 ->
          if Base.leq p2 p1 then
            p1
          else
            Base.widen p1 (Base.join p1 p2)
        ) x y
  let narrow = op_scheme Base.narrow Base.narrow (** TODO: Might not be correct *)

  let pretty_diff () ((m1, p1), (m2, p2)) =
    if Base.leq m1 m2 then
      Base.pretty_diff () (p1, p2)
    else
      Base.pretty_diff () (m1, m2)
end


module Lifter (S: Spec): Spec =
struct
  module D =
  struct
    include Dom (S.D)

    let printXml f (m, p) =
      BatPrintf.fprintf f "%a%a" S.D.printXml m S.D.printXml p
  end
  module G = S.G
  module C = S.C
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  let name () = S.name () ^ " with lookahead widening"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let startstate v = (S.startstate v, S.startstate v)
  let exitstate  v = (S.exitstate  v, S.exitstate  v)
  let morphstate v (m, p) = (S.morphstate v m, S.morphstate v p)

  let convm (ctx: (D.t, G.t, C.t, V.t) ctx): (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es)
    }
  let convp (ctx: (D.t, G.t, C.t, V.t) ctx): (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
    { ctx with local = snd ctx.local
             ; split = (fun d es -> ctx.split (fst ctx.local, d) es)
    }

  let context ctx fd (m, _) = S.context (convm ctx) fd m
  let startcontext () = S.startcontext ()

  let lift_fun (ctx: (D.t, G.t, C.t, V.t) ctx) g h =
    let main = h (g (convm ctx)) in
    if S.D.is_bot main then D.bot () else
      (main, h (g (convp ctx)))
  let lift_fun' (ctx: (D.t, G.t, C.t, V.t) ctx) g h =
    let main = h (g (convm ctx)) in
    (main, h (g (convp ctx)))
  let lift_fun2 (ctx: (D.t, G.t, C.t, V.t) ctx) g h1 h2 =
    let main = h1 (g (convm ctx)) in
    if S.D.is_bot main then D.bot () else
      (main, h2 (g (convp ctx)))

  let sync ctx reason = lift_fun ctx S.sync   ((|>) reason)
  let query ctx (type a) (q: a Queries.t): a Queries.result = S.query (convm ctx) q
  let assign ctx lv e = lift_fun ctx S.assign ((|>) e % (|>) lv)
  let vdecl ctx v     = lift_fun ctx S.vdecl  ((|>) v)
  let branch ctx e tv = lift_fun ctx S.branch ((|>) tv % (|>) e)
  let body ctx f      = lift_fun ctx S.body   ((|>) f)
  let return ctx r f  = lift_fun ctx S.return ((|>) f % (|>) r)
  let asm ctx         = lift_fun ctx S.asm    identity
  let skip ctx        = lift_fun ctx S.skip identity
  let special ctx r f args = lift_fun ctx S.special ((|>) args % (|>) f % (|>) r)

  let enter ctx r f args =
    let (l1, l2) = lift_fun' ctx S.enter ((|>) args % (|>) f % (|>) r) in
    List.map2 (fun (m1, m2) (p1, p2) -> ((m1, p1), (m2, p2))) l1 l2
  let combine_env ctx r fe f args fc es f_ask = lift_fun ctx S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign ctx r fe f args fc es f_ask = lift_fun ctx S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)

  let threadenter ctx ~multiple lval f args =
    let (l1, l2) = lift_fun' ctx (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval) in
    List.combine l1 l2
  let threadspawn ctx ~multiple lval f args fctx =
    lift_fun2 ctx (S.threadspawn ~multiple) ((|>) (convm fctx) % (|>) args % (|>) f % (|>) lval) ((|>) (convp fctx) % (|>) args % (|>) f % (|>) lval)

  let paths_as_set ctx =
    let (l1, l2) = lift_fun' ctx S.paths_as_set Fun.id in
    List.combine l1 l2

  let event ctx e octx =
    lift_fun2 ctx S.event ((|>) (convm octx) % (|>) e) ((|>) (convp octx) % (|>) e)
end
