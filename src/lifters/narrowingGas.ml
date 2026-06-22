(** narrowing delay with counters.

    Abstract elements are paired with an integer counter, indicating how many times narrowing has been delayed.
    Lifted abstract elements are only narrowed if the counter exceeds a predefined limit (gas). *)

open Batteries
open Lattice
open Analyses

module LocalChainParams: Printable.ChainParams =
struct
  let n () = GobConfig.get_int "ana.narrowing.gas"
  let names = string_of_int
end

module GlobalChainParams: Printable.ChainParams =
struct
  let n () = GobConfig.get_int "ana.narrowing.gas"
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
  let widen (b1, i1) (b2, i2) = (Base.widen b1 b2, max i1 i2) 
  let narrow (b1, i1) (b2, i2) = 
    let i' = max i1 i2 in
    if i' < ChainParams.n () then
      (Base.meet b1 b2, i' + 1)  (* Stop narrowing when counter exceeds limit. *)
    else
      (Base.narrow b1 b2, i')

  let pretty_diff () ((b1, _), (b2, _)) =
    Base.pretty_diff () (b1, b2) (* Counters cannot violate leq. *)
end


(** Lift {!S} to use widening delay for local states.

    All transfer functions reset the counter to 0, so counting only happens between old and new values at a local unknown. *)
module DLifter (S: Spec) (C: Printable.ChainParams): Spec =
struct
  module DD (D: Lattice.S) =
  struct
    include Dom (D) (C)

    let printXml f (b, i) =
      BatPrintf.fprintf f "%a<analysis name=\"narrow-count\">%a</analysis>" D.printXml b Chain.printXml i
  end

  module NameLifter =
  struct
    let lift_name x = x ^ " with narrowing count"
  end
  include SpecLifters.DomainLifter (NameLifter) (DD) (S)

  (* Redefine morphstate and paths_as_set to keep counter instead of resetting to 0. *)

  let morphstate v (d, l) = (S.morphstate v d, l)

  let paths_as_set man =
    List.map (fun x -> (x, snd man.local)) @@ S.paths_as_set (conv man)
end

(** Lift {!S} to use widening delay for global unknowns. *)
module GLifter (S: Spec): Spec =
struct
  module GG (G: Lattice.S) =
  struct
    include Dom (G) (GlobalChainParams)

    let printXml f (b, i) =
      BatPrintf.fprintf f "%a<analysis name=\"narrow-count\">%a</analysis>" G.printXml b Chain.printXml i
  end

  module NameLifter =
  struct
    let lift_name x = x ^ " with narrowing count"
  end
  include SpecLifters.GlobalDomainLifter (NameLifter) (GG) (S)
end
