module type AbstractFunction =
sig
  type c
  type a
  val abstract: c -> a
  val check_leq: c -> a -> bool
end

module AbstractTest (CD: Lattice.S) (AD: Lattice.S) =
struct
  include DomainProperties.DomainTest (AD)

  let arb = CD.arbitrary ()

  let make ~name =
    let domain_name = CD.name () ^ " -> " ^ AD.name () in
    make ~domain_name ~name
end

module Monotone (CD: Lattice.S) (AD: Lattice.S) (AF: AbstractFunction with type c := CD.t and type a := AD.t): DomainProperties.S = (* Destructive Substitution *)
struct
  open QCheck

  include AbstractTest (CD) (AD)

  let monotone = make ~name:"monotone" (QCheck.pair arb arb) (fun (a, b) ->
      CD.leq a b ==> AD.leq (AF.abstract a) (AF.abstract b)
    )

  let tests = [
    monotone
  ]
end

module ValidTest (CD: Lattice.S) (AD: Lattice.S) (AF: AbstractFunction with type c := CD.t and type a := AD.t) = (* Destructive Substitution *)
struct
  include AbstractTest (CD) (AD)

  let make_valid ~name arb ?(cond=fun _ -> true) cf abstract2 af =
    let full_name = "valid " ^ name in
    make ~name:full_name arb QCheck.(fun a ->
        assume (cond a); (* assume is lazy, ==> is eager *)
        AF.check_leq (cf a) (af (abstract2 a))
      )
  let make_valid1 ?cond cf af = make_valid arb ?cond cf AF.abstract af
  let make_valid2 ?cond cf af = make_valid (QCheck.pair arb arb) ?cond (Batteries.uncurry cf) (BatTuple.Tuple2.mapn AF.abstract) (Batteries.uncurry af)
end
