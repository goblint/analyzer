module type AbstractFunction =
sig
  type c
  type a
  val abstract: c -> a
end

module ValidTest (CD: Lattice.S) (AD: Lattice.S) (AF: AbstractFunction with type c := CD.t and type a := AD.t) = (* Destructive Substitution *)
struct
  include DomainProperties.DomainTest (AD)

  let arb = CD.arbitrary ()

  let make_valid ~name arb cf abstract2 af =
    let full_name = "valid " ^ name in
    make ~name:full_name arb (fun a ->
        AD.leq (AF.abstract (cf a)) (af (abstract2 a))
      )
  let make_valid1 ~name cf af = make_valid ~name arb cf AF.abstract af
  let make_valid2 ~name cf af = make_valid ~name (QCheck.pair arb arb) (Batteries.uncurry cf) (BatTuple.Tuple2.mapn AF.abstract) (Batteries.uncurry af)
end