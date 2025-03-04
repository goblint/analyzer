(** Standard widening delay with counters.

    Abstract elements are paired with an integer counter, indicating how many times it has been widened.
    Lifted abstract elements are only widened if the counter exceeds a predefined limit. *)

open Batteries
open Lattice
open Analyses

module LocalChainParams: Printable.ChainParams =
struct
  let n () = GobConfig.get_int "ana.widen.delay.local"
  let names = string_of_int
end

module GlobalChainParams: Printable.ChainParams =
struct
  let n () = GobConfig.get_int "ana.widen.delay.global"
  let names = string_of_int
end

module Dom (Base: S) (ChainParams: Printable.ChainParams) =
struct
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


(** Lift {!S} to use widening delay for local states.

    All transfer functions reset the counter to 0, so counting only happens between old and new values at a local unknown. *)
module DLifter (S: Spec): Spec =
struct
  module D =
  struct
    include Dom (S.D) (LocalChainParams)

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

  let name () = S.name () ^ " with widening delay"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let startstate v = (S.startstate v, 0)
  let exitstate  v = (S.exitstate  v, 0)
  let morphstate v (d, l) = (S.morphstate v d, l)

  let conv (man: (D.t, G.t, C.t, V.t) man): (S.D.t, S.G.t, S.C.t, S.V.t) man =
    { man with local = fst man.local
             ; split = (fun d es -> man.split (d, 0) es)
    }

  let context man fd (d, _) = S.context (conv man) fd d
  let startcontext () = S.startcontext ()

  let lift_fun man f g h =
    f @@ h (g (conv man))

  let lift d = (d, 0)

  let sync man reason = lift_fun man lift S.sync   ((|>) reason)
  let query man (type a) (q: a Queries.t): a Queries.result = S.query (conv man) q
  let assign man lv e = lift_fun man lift S.assign ((|>) e % (|>) lv)
  let vdecl man v     = lift_fun man lift S.vdecl  ((|>) v)
  let branch man e tv = lift_fun man lift S.branch ((|>) tv % (|>) e)
  let body man f      = lift_fun man lift S.body   ((|>) f)
  let return man r f  = lift_fun man lift S.return ((|>) f % (|>) r)
  let asm man         = lift_fun man lift S.asm    identity
  let skip man        = lift_fun man lift S.skip identity
  let special man r f args = lift_fun man lift S.special ((|>) args % (|>) f % (|>) r)

  let enter man r f args =
    let liftmap = List.map (Tuple2.mapn lift) in
    lift_fun man liftmap S.enter ((|>) args % (|>) f % (|>) r)
  let combine_env man r fe f args fc es f_ask = lift_fun man lift S.combine_env (fun p -> p r fe f args fc (fst es) f_ask)
  let combine_assign man r fe f args fc es f_ask = lift_fun man lift S.combine_assign (fun p -> p r fe f args fc (fst es) f_ask)

  let threadenter man ~multiple lval f args = lift_fun man (List.map lift) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval)
  let threadspawn man ~multiple lval f args fman = lift_fun man lift (S.threadspawn ~multiple) ((|>) (conv fman) % (|>) args % (|>) f % (|>) lval)

  let paths_as_set man =
    let liftmap = List.map (fun x -> (x, snd man.local)) in
    lift_fun man liftmap S.paths_as_set Fun.id

  let event man e oman =
    lift_fun man lift S.event ((|>) (conv oman) % (|>) e)
end

(** Lift {!S} to use widening delay for global unknowns. *)
module GLifter (S: Spec): Spec =
struct
  module D = S.D
  module G =
  struct
    include Dom (S.G) (GlobalChainParams)

    let printXml f (b, i) =
      BatPrintf.fprintf f "%a<analysis name=\"widen-delay\">%a</analysis>" S.G.printXml b Chain.printXml i
  end
  module C = S.C
  module V = S.V
  module P = S.P

  let name () = S.name () ^ " with widening delay"

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize

  let startstate v = S.startstate v
  let exitstate  v = S.exitstate  v
  let morphstate v d = S.morphstate v d

  let conv (man: (D.t, G.t, C.t, V.t) man): (S.D.t, S.G.t, S.C.t, S.V.t) man =
    { man with global = (fun v -> fst (man.global v))
             ; sideg = (fun v g -> man.sideg v (g, 0))
    }

  let context man fd d = S.context (conv man) fd d
  let startcontext () = S.startcontext ()

  let lift_fun man f g h =
    f @@ h (g (conv man))

  let lift d = d

  let sync man reason = lift_fun man lift S.sync   ((|>) reason)
  let query man (type a) (q: a Queries.t): a Queries.result = S.query (conv man) q
  let assign man lv e = lift_fun man lift S.assign ((|>) e % (|>) lv)
  let vdecl man v     = lift_fun man lift S.vdecl  ((|>) v)
  let branch man e tv = lift_fun man lift S.branch ((|>) tv % (|>) e)
  let body man f      = lift_fun man lift S.body   ((|>) f)
  let return man r f  = lift_fun man lift S.return ((|>) f % (|>) r)
  let asm man         = lift_fun man lift S.asm    identity
  let skip man        = lift_fun man lift S.skip identity
  let special man r f args = lift_fun man lift S.special ((|>) args % (|>) f % (|>) r)

  let enter man r f args =
    let liftmap = List.map (Tuple2.mapn lift) in
    lift_fun man liftmap S.enter ((|>) args % (|>) f % (|>) r)
  let combine_env man r fe f args fc es f_ask = lift_fun man lift S.combine_env (fun p -> p r fe f args fc es f_ask)
  let combine_assign man r fe f args fc es f_ask = lift_fun man lift S.combine_assign (fun p -> p r fe f args fc es f_ask)

  let threadenter man ~multiple lval f args = lift_fun man (List.map lift) (S.threadenter ~multiple) ((|>) args % (|>) f % (|>) lval)
  let threadspawn man ~multiple lval f args fman = lift_fun man lift (S.threadspawn ~multiple) ((|>) (conv fman) % (|>) args % (|>) f % (|>) lval)

  let paths_as_set man =
    lift_fun man Fun.id S.paths_as_set Fun.id

  let event man e oman =
    lift_fun man lift S.event ((|>) (conv oman) % (|>) e)
end
