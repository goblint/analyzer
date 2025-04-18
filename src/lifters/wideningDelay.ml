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

  let lift d = (d, 0)
  let unlift (d, _) = d

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
  module DD (D: Lattice.S) =
  struct
    include Dom (D) (LocalChainParams)

    let printXml f (b, i) =
      BatPrintf.fprintf f "%a<analysis name=\"widen-delay\">%a</analysis>" D.printXml b Chain.printXml i
  end

  module NameLifter =
  struct
    let lift_name x = x ^ " with widening delay"
  end
  include SpecLifters.DomainLifter (NameLifter) (DD) (S)

  let morphstate v (d, l) = (S.morphstate v d, l)

  let paths_as_set man =
    let liftmap = List.map (fun (x, _) -> (x, snd man.local)) in
    (* TODO: expose conv from DomainLifter? *)
    liftmap (paths_as_set man)
end

(** Lift {!S} to use widening delay for global unknowns. *)
module GLifter (S: Spec): Spec =
struct
  module GG (G: Lattice.S) =
  struct
    include Dom (G) (GlobalChainParams)

    let printXml f (b, i) =
      BatPrintf.fprintf f "%a<analysis name=\"widen-delay\">%a</analysis>" G.printXml b Chain.printXml i
  end

  module NameLifter =
  struct
    let lift_name x = x ^ " with widening delay"
  end
  include SpecLifters.GlobalDomainLifter (NameLifter) (GG) (S)
end
