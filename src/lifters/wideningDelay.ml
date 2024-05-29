(** Standard widening delay with counters.

    Abstract elements are paired with an integer counter, indicating how many times it has been widened.
    Lifted abstract elements are only widened if the counter exceeds a predefined limit. *)

open Batteries
open Lattice
open Analyses

module Dom (Base: S) =
struct
  module ChainParams =
  struct
    let n () = GobConfig.get_int "ana.widen.delay"
    let names = string_of_int
  end
  module Chain = Printable.Chain (ChainParams)

  include Printable.Prod (Base) (Chain)

  let bot () = (Base.bot (), 0)
  let is_bot (b, _) = Base.is_bot b
  let top () = (Base.top (), ChainParams.n ())
  let is_top (b, _) = Base.is_top b

  let leq (b1, _) (b2, _) = Base.leq b1 b2

  (** All operations keep maximal counter. *)
  let join (b1, i1) (b2, i2) = (Base.join b1 b2, max i1 i2)
  let meet (b1, i1) (b2, i2) = (Base.meet b1 b2, max i1 i2)
  let widen (b1, i1) (b2, i2) =
    let i' = max i1 i2 in
    if i' < ChainParams.n () then
      (Base.join b1 b2, i' + 1)
    else
      (Base.widen b1 b2, i') (* Don't increase beyond n, otherwise TD3 will not reach fixpoint w.r.t. equal. *)
  let narrow (b1, i1) (b2, i2) = (Base.narrow b1 b2, max i1 i2)

  let pretty_diff () ((b1, _), (b2, _)) =
    Base.pretty_diff () (b1, b2) (* Counters cannot violate leq. *)
end


module Lifter (S:Spec)
  : Spec with module D = Dom (S.D)
          and module G = S.G
          and module C = S.C
=
struct
  module D =
  struct
    include Dom (S.D)

    let printXml f (b, i) =
      BatPrintf.fprintf f "%a<analysis name=\"widen-delay\">%a</analysis>" S.D.printXml b Chain.printXml i
  end
  module G = S.G
  module C = S.C
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  let name () = S.name ()^" delayed"

  let start_level = ref (`Top)

  type marshal = S.marshal (* TODO: should hashcons table be in here to avoid relift altogether? *)
  let init marshal =
    S.init marshal

  let finalize = S.finalize

  let startstate v = (S.startstate v, 0)
  let exitstate  v = (S.exitstate  v, D.ChainParams.n ())
  let morphstate v (d,l) = (S.morphstate v d, l)

  let conv (ctx: (D.t, G.t, C.t, V.t) ctx) : (S.D.t, S.G.t, S.C.t, S.V.t) ctx =
    { ctx with local = fst ctx.local
             ; split = (fun d es -> ctx.split (d, snd ctx.local) es )
    }

  let context ctx fd (d,_) = S.context (conv ctx) fd d
  let startcontext () = S.startcontext ()

  let lift_fun ctx f g h =
    f @@ h (g (conv ctx))

  let enter ctx r f args =
    let liftmap = List.map (fun (x,y) -> (x, snd ctx.local), (y, snd ctx.local)) in
    lift_fun ctx liftmap S.enter ((|>) args % (|>) f % (|>) r)

  let lift ctx d = (d, 0)

  (* Used only in the threadspawn function. *)
  let rec lift' ctx dl =
    match dl with
    | [] -> []
    | d::l -> (d, 0)::(lift' ctx l)

  let sync ctx reason = lift_fun ctx (lift ctx) S.sync   ((|>) reason)
  let query ctx (type a) (q: a Queries.t): a Queries.result = S.query (conv ctx) q
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

  let threadenter ctx ~multiple lval f args = lift_fun ctx (lift' ctx) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval)
  let threadspawn ctx ~multiple lval f args fctx = lift_fun ctx (lift ctx) (S.threadspawn ~multiple) ((|>) (conv fctx) % (|>) args % (|>) f % (|>) lval)


  let paths_as_set ctx =
    let liftmap = List.map (fun x -> (x, snd ctx.local)) in
    lift_fun ctx liftmap S.paths_as_set (Fun.id)

  let event ctx e octx =
    lift_fun ctx (lift ctx) S.event ((|>) (conv octx) % (|>) e)
end